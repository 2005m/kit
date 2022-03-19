/*
 * kit : Useful R Functions Implemented in C
 * Copyright (C) 2020-2021  Morgan Jacob
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "kit.h"

SEXP psumR(SEXP na, SEXP args) {
  if (!IS_BOOL(na)) {
    error("Argument 'na.rm' must be TRUE or FALSE and length 1.");
  }
  const int n=length(args);
  if (n < 1) {
    error("Please supply at least 1 argument. (%d argument supplied)", n);
  }
  const SEXP args0 = PTR_ETL(args, 0);
  SEXPTYPE anstype = UTYPEOF(args0);
  SEXPTYPE type0 = anstype;
  const R_xlen_t len0 = xlength(args0);
  Rboolean hasFactor = isFactor(args0);
  if (anstype != INTSXP && anstype != REALSXP && anstype != CPLXSXP) {
    error("Argument %d is of type %s. Only integer, double and complex types are supported."
          "Data.frame (of the previous types) is also supported as a single input. ",
          1, type2char(anstype));
  }
  for (int i = 1; i < n; ++i) {
    SEXPTYPE type = UTYPEOF(PTR_ETL(args, i));
    R_xlen_t len1 = xlength(PTR_ETL(args, i));
    if (type != INTSXP && type != REALSXP && type != CPLXSXP) {
      error("Argument %d is of type %s. Only integer, double and complex types are supported. " , i+1, type2char(type));
    }
    if (len1 != len0) {
      error("Argument %d is of length %zu but argument %d is of length %zu. "
      "If you wish to 'recycle' your argument, please use rep() to make this intent "
      "clear to the readers of your code.", i+1, len1, 1, len0);
    }
    if (type > anstype) {
      anstype = type;
    }
    hasFactor = hasFactor ? TRUE : isFactor(PTR_ETL(args, i));
  }
  if (hasFactor) {
    error("Function 'psum' is not meaningful for factors.");
  }
  int nprotect=1;
  SEXP ans = (anstype != type0) ? PROTECT(coerceVector(args0, anstype)) : PROTECT(duplicate(args0));
  const bool narm = asLogical(na);
  switch(anstype) {
  case INTSXP: {
    int *restrict pans =INTEGER(ans);
    if(narm) {
      for (ssize_t j = 0; j < len0; ++j) {
        if (pans[j] == NA_INTEGER) {
          pans[j] = 0; 
        }
      }
    }
    for (int i = 1; i < n; ++i) {
      int *pa = INTEGER(PTR_ETL(args, i));
      if (narm) {
        for (ssize_t j = 0; j < len0; ++j) {
          pans[j] = pa[j]==NA_INTEGER ? pans[j] : (pans[j] + pa[j]);
        }
      } else {
        for (ssize_t j = 0; j < len0; ++j) {
          pans[j] = (pans[j] == NA_INTEGER || pa[j] == NA_INTEGER) ? NA_INTEGER : (pans[j] + pa[j]);
        }
      }
    }
  } break;
  case REALSXP: {
    double *restrict pans = REAL(ans);
    SEXP dbl_a = R_NilValue;
    PROTECT_INDEX Idbl;
    PROTECT_WITH_INDEX(dbl_a, &Idbl); nprotect++;
    if(narm) {
      for (ssize_t j = 0; j < len0; ++j) {
        if (ISNAN(pans[j])) {
          pans[j] = 0; 
        }
      }
    }
    for (int i = 1; i < n; ++i) {
      SEXPTYPE targsi = UTYPEOF(PTR_ETL(args, i));
      if (targsi != anstype) {
        REPROTECT(dbl_a = coerceVector(PTR_ETL(args, i), anstype), Idbl);
      } else {
        REPROTECT(dbl_a = PTR_ETL(args, i), Idbl);
      }
      double *pa = REAL(dbl_a);
      if (narm) {
        for (ssize_t j = 0; j < len0; ++j) {
          pans[j] = ISNAN(pa[j]) ? pans[j] : (pans[j] + pa[j]);
        }
      } else {
        for (ssize_t j = 0; j < len0; ++j) {
          pans[j] += pa[j];
        }
      }
    }
  } break;
  case CPLXSXP: {
    Rcomplex *restrict pans = COMPLEX(ans);
    SEXP cpl_a = R_NilValue;
    PROTECT_INDEX Icpl;
    PROTECT_WITH_INDEX(cpl_a, &Icpl); nprotect++;
    if(narm) {
      for (ssize_t j = 0; j < len0; ++j) {
        if (ISNAN_COMPLEX(pans[j])) {
          pans[j].r = 0;
          pans[j].i = 0;
        }
      }
    }
    for (int i = 1; i < n; ++i) {
      SEXPTYPE targsi = UTYPEOF(PTR_ETL(args, i));
      if (targsi != anstype) {
        REPROTECT(cpl_a=coerceVector(PTR_ETL(args, i), anstype), Icpl);
      } else {
        REPROTECT(cpl_a=PTR_ETL(args, i), Icpl);
      }
      Rcomplex *pa = COMPLEX(cpl_a);
      if (narm) {
        for (ssize_t j = 0; j < len0; ++j) {
          pans[j].r = ISNAN_COMPLEX(pa[j]) ? pans[j].r : (pans[j].r + pa[j].r);
          pans[j].i = ISNAN_COMPLEX(pa[j]) ? pans[j].i : (pans[j].i + pa[j].i);
        }
      } else {
        for (ssize_t j = 0; j < len0; ++j) {
          pans[j].r += pa[j].r;
          pans[j].i += pa[j].i;
        }
      }
    }
  } break;
  }
  UNPROTECT(nprotect);
  return ans;
}

SEXP pprodR(SEXP na, SEXP args) {
  if (!IS_BOOL(na)) {
    error("Argument 'na.rm' must be TRUE or FALSE and length 1.");
  }
  const int n=length(args);
  if (n < 1) {
    error("Please supply at least 1 argument. (%d argument supplied)", n);
  }
  const SEXP args0 = PTR_ETL(args, 0);
  SEXPTYPE anstype = UTYPEOF(args0);
  SEXPTYPE type0 = anstype;
  const R_xlen_t len0 = xlength(args0);
  Rboolean hasFactor = isFactor(args0);
  if (anstype != INTSXP && anstype != REALSXP && anstype != CPLXSXP) {
    error("Argument %d is of type %s. Only integer, double and complex types are supported."
          "Data.frame (of the previous types) is also supported as a single input. ",
          1, type2char(anstype));
  }
  for (int i = 1; i < n; ++i) {
    SEXPTYPE type = UTYPEOF(PTR_ETL(args, i));
    R_xlen_t len1 = xlength(PTR_ETL(args, i));
    if (type != INTSXP && type != REALSXP && type != CPLXSXP) {
      error("Argument %d is of type %s. Only integer, double and complex types are supported.", i+1, type2char(type));
    }
    if (len1 != len0) {
      error("Argument %d is of length %zu but argument %d is of length %zu. "
      "If you wish to 'recycle' your argument, please use rep() to make this intent "
      "clear to the readers of your code.", i+1, len1, 1, len0);
    }
    if (type > anstype) {
      anstype = type;
    }
    hasFactor = hasFactor ? TRUE : isFactor(PTR_ETL(args, i));
  }
  if (hasFactor) {
    error("Function 'pprod' is not meaningful for factors.");
  }
  int nprotect=1;
  SEXP ans = anstype != type0 ? PROTECT(coerceVector(args0, anstype)) : PROTECT(duplicate(args0));
  const bool narm = asLogical(na);
  switch(anstype) {
  case INTSXP: {
    int *restrict pans =INTEGER(ans);
    if(narm) {
      for (ssize_t j = 0; j < len0; ++j) {
        if (pans[j] == NA_INTEGER) {
          pans[j] = 1; 
        }
      }
    }
    for (int i = 1; i < n; ++i) {
      int *pa = INTEGER(PTR_ETL(args, i));
      if (narm) {
        for (ssize_t j = 0; j < len0; ++j) {
          pans[j] = pa[j]==NA_INTEGER ? pans[j] : (pans[j] * pa[j]);
        }
      } else {
        for (ssize_t j = 0; j < len0; ++j) {
          pans[j] = (pans[j] == NA_INTEGER || pa[j] == NA_INTEGER) ? NA_INTEGER : (pans[j] * pa[j]);
        }
      }
    }
  } break;
  case REALSXP: {
    double *restrict pans = REAL(ans);
    SEXP dbl_a = R_NilValue;
    PROTECT_INDEX Idbl;
    PROTECT_WITH_INDEX(dbl_a, &Idbl); nprotect++;
    if(narm) {
      for (ssize_t j = 0; j < len0; ++j) {
        if (ISNAN(pans[j])) {
          pans[j] = 1; 
        }
      }
    }
    for (int i = 1; i < n; ++i) {
      SEXPTYPE targsi = UTYPEOF(PTR_ETL(args, i));
      if (targsi != anstype) {
        REPROTECT(dbl_a = coerceVector(PTR_ETL(args, i), anstype), Idbl);
      } else {
        REPROTECT(dbl_a = PTR_ETL(args, i), Idbl);
      }
      double *pa = REAL(dbl_a);
      if (narm) {
        for (ssize_t j = 0; j < len0; ++j) {
          pans[j] = ISNAN(pa[j]) ? pans[j] : (pans[j] * pa[j]);
        }
      } else {
        for (ssize_t j = 0; j < len0; ++j) {
          pans[j] *= pa[j];
        }
      }
    }
  } break;
  case CPLXSXP: {
    Rcomplex *restrict pans = COMPLEX(ans);
    SEXP cpl_a = R_NilValue;
    PROTECT_INDEX Icpl;
    PROTECT_WITH_INDEX(cpl_a, &Icpl); nprotect++;
    if(narm) {
      for (ssize_t j = 0; j < len0; ++j) {
        if (ISNAN_COMPLEX(pans[j])) {
          pans[j].r = 1;
          pans[j].i = 0;
        }
      }
    }
    for (int i = 1; i < n; ++i) {
      SEXPTYPE targsi = UTYPEOF(PTR_ETL(args, i));
      if (targsi != anstype) {
        REPROTECT(cpl_a = coerceVector(PTR_ETL(args, i), anstype), Icpl);
      } else {
        REPROTECT(cpl_a = PTR_ETL(args, i), Icpl);
      }
      Rcomplex *pa = COMPLEX(cpl_a);
      if (narm) {
        for (ssize_t j = 0; j < len0; ++j) {
          pans[j].r = ISNAN_COMPLEX(pa[j]) ? pans[j].r : (pans[j].r * pa[j].r - pans[j].i * pa[j].i);
          pans[j].i = ISNAN_COMPLEX(pa[j]) ? pans[j].i : (pans[j].r * pa[j].i + pans[j].i * pa[j].r);
        }
      } else {
        for (ssize_t j = 0; j < len0; ++j) {
          pans[j].r = pans[j].r * pa[j].r - pans[j].i * pa[j].i;
          pans[j].i = pans[j].r * pa[j].i + pans[j].i * pa[j].r;
        }
      }
    }
  } break;
  }
  UNPROTECT(nprotect);
  return ans;
}

SEXP pallR(SEXP na, SEXP args) {
  if (!IS_BOOL(na)) {
    error("Argument 'na.rm' must be TRUE or FALSE and length 1.");
  }
  const int n=length(args);
  if (n < 1) {
    error("Please supply at least 1 argument. (%d argument supplied)", n);
  }
  const SEXP args0 = PROTECT(PTR_ETL(args, 0));
  SEXPTYPE anstype = UTYPEOF(args0);
  const R_xlen_t len0 = xlength(args0);
  if (anstype != LGLSXP) {
    error("Argument %d is of type %s. Only logical type is supported."
          "Data.frame (of logical vectors) is also supported as a single input. ",
          1, type2char(anstype));
  }
  for (int i = 1; i < n; ++i) {
    SEXPTYPE type = UTYPEOF(PTR_ETL(args, i));
    R_xlen_t len1 = xlength(PTR_ETL(args, i));
    if (type != LGLSXP) {
      error("Argument %d is of type %s. Only logical type is supported.", i+1, type2char(type));
    }
    if (len1 != len0) {
      error("Argument %d is of length %zu but argument %d is of length %zu. "
              "If you wish to 'recycle' your argument, please use rep() to make this intent "
              "clear to the readers of your code.", i+1, len1, 1, len0);
    }
  }
  SEXP ans = R_NilValue;
  const bool narm = asLogical(na);
  int *pans;
  if (narm) {
    ans = PROTECT(allocVector(LGLSXP, len0));
    pans = LOGICAL(ans);
    const int *restrict pargs0 = LOGICAL(args0);
    for (ssize_t j = 0; j < len0; ++j) {
      pans[j] = pargs0[j]==NA_LOGICAL ? 1 : pargs0[j]; 
    }
  } else {
    ans = PROTECT(duplicate(args0));
    pans = LOGICAL(ans);
  }
  for (int i = 1; i < n; ++i) {
    int *pa = LOGICAL(PTR_ETL(args, i));
    if (narm) {
      for (ssize_t j = 0; j < len0; ++j) {
        pans[j] = pans[j]==0 ? pans[j] :(pa[j]==NA_LOGICAL ? 1 : pa[j]);
      }
    } else {
      for (ssize_t j = 0; j < len0; ++j) {
        pans[j] = (pans[j] == 0 || pa[j] == 0) ? 0 : ((pans[j]==NA_LOGICAL || pa[j]==NA_LOGICAL) ? NA_LOGICAL : 1);
      }
    }
  }
  UNPROTECT(2);
  return ans;
}

SEXP panyR(SEXP na, SEXP args) {
  if (!IS_BOOL(na)) {
    error("Argument 'na.rm' must be TRUE or FALSE and length 1.");
  }
  const int n=length(args);
  if (n < 1) {
    error("Please supply at least 1 argument. (%d argument supplied)", n);
  }
  const SEXP args0 = PROTECT(PTR_ETL(args, 0));
  SEXPTYPE anstype = UTYPEOF(args0);
  const R_xlen_t len0 = xlength(args0);
  if (anstype != LGLSXP) {
    error("Argument %d is of type %s. Only logical type is supported."
          "Data.frame (of logical vectors) is also supported as a single input. ",
          1, type2char(anstype));
  }
  for (int i = 1; i < n; ++i) {
    SEXPTYPE type = UTYPEOF(PTR_ETL(args, i));
    R_xlen_t len1 = xlength(PTR_ETL(args, i));
    if (type != LGLSXP) {
      error("Argument %d is of type %s. Only logical type is supported.", i+1, type2char(type));
    }
    if (len1 != len0) {
      error("Argument %d is of length %zu but argument %d is of length %zu. "
              "If you wish to 'recycle' your argument, please use rep() to make this intent "
              "clear to the readers of your code.", i+1, len1, 1, len0);
    }
  }
  SEXP ans = R_NilValue;
  const bool narm = asLogical(na);
  int *pans;
  if (narm && n==1) {
    ans = PROTECT(allocVector(LGLSXP, len0));
    pans = LOGICAL(ans);
    const int *restrict pargs0 = LOGICAL(args0);
    for (ssize_t j = 0; j < len0; ++j) {
      pans[j] = pargs0[j]==NA_LOGICAL ? 1 : pargs0[j]; 
    }
  } else {
    ans = PROTECT(duplicate(args0));
    pans = LOGICAL(ans);
  }
  for (int i = 1; i < n; ++i) {
    int *pa = LOGICAL(PTR_ETL(args, i));
    if (narm) {
      for (ssize_t j = 0; j < len0; ++j) {
        pans[j] = (pans[j] == 1 || pa[j] == 1) ? 1 : 0;
      }
    } else {
      for (ssize_t j = 0; j < len0; ++j) {
        pans[j] = (pans[j] == 1 || pa[j] == 1) ? 1 : ((pans[j] == NA_LOGICAL || pa[j] == NA_LOGICAL) ? NA_LOGICAL:0);
      }
    }
  }
  UNPROTECT(2);
  return ans;
}

SEXP pmeanR(SEXP na, SEXP args) {
  if (!IS_BOOL(na)) {
    error("Argument 'na.rm' must be TRUE or FALSE and length 1.");
  }
  const int n=length(args);
  if (n < 1) {
    error("Please supply at least 1 argument. (%d argument supplied)", n);
  }
  const SEXP args0 = PTR_ETL(args, 0);
  SEXPTYPE anstype = UTYPEOF(args0);
  SEXPTYPE type0 = REALSXP;
  const R_xlen_t len0 = xlength(args0);
  Rboolean hasFactor = isFactor(args0);
  if (anstype != INTSXP && anstype != REALSXP) {
    error("Argument %d is of type %s. Only integer and double types are supported."
          "Data.frame (of the previous types) is also supported as a single input. ",
          1, type2char(anstype));
  }
  for (int i = 1; i < n; ++i) {
    SEXPTYPE type = UTYPEOF(PTR_ETL(args, i));
    R_xlen_t len1 = xlength(PTR_ETL(args, i));
    if (type != INTSXP && type != REALSXP) {
      error("Argument %d is of type %s. Only integer and double types are supported.", i+1, type2char(type));
    }
    if (len1 != len0) {
      error("Argument %d is of length %zu but argument %d is of length %zu. "
              "If you wish to 'recycle' your argument, please use rep() to make this intent "
              "clear to the readers of your code.", i+1, len1, 1, len0);
    }
    hasFactor = hasFactor ? TRUE : isFactor(PTR_ETL(args, i));
  }
  if (hasFactor) {
    error("Function 'pmean' is not meaningful for factors.");
  }
  int nprotect=2;
  SEXP ans = anstype != type0 ? PROTECT(coerceVector(args0, type0)) : PROTECT(duplicate(args0));
  const bool narm = asLogical(na);
  SEXP den = PROTECT(allocVector(REALSXP, len0));
  double *restrict pden = REAL(den);
  memset(pden, 0, (unsigned)len0*sizeof(double));
  double *restrict pans = REAL(ans);
  SEXP dbl_a = R_NilValue;
  PROTECT_INDEX Idbl;
  PROTECT_WITH_INDEX(dbl_a, &Idbl); nprotect++;
  if (narm) {
    for (ssize_t j = 0; j < len0; ++j) {
      if (ISNAN(pans[j])) {
        pans[j] = 0;
        pden[j]++;
      }
    }
  }
  for (int i = 1; i < n; ++i) {
    SEXPTYPE targsi = UTYPEOF(PTR_ETL(args, i));
    if (targsi != type0) {
      REPROTECT(dbl_a = coerceVector(PTR_ETL(args, i), type0), Idbl);
    } else {
      REPROTECT(dbl_a = PTR_ETL(args, i), Idbl);
    }
    double *pa = REAL(dbl_a);
    if (narm) {
      for (ssize_t j = 0; j < len0; ++j) {
        if (ISNAN(pa[j])) {
          pden[j]++;
        }
        pans[j] = ISNAN(pa[j]) ? pans[j] : (pans[j] + pa[j]);
      }
    } else {
      for (ssize_t j = 0; j < len0; ++j) {
        pans[j] += pa[j];
      }
    }
  }
  if (narm) {
    for (ssize_t j = 0; j < len0; ++j) {
      pans[j] = n==pden[j] ? R_NaN : pans[j]/(n-pden[j]);
    }
  } else {
    for (ssize_t j = 0; j < len0; ++j) {
      pans[j] = pans[j]/n;
    }
  }
  UNPROTECT(nprotect);
  return ans;
}

SEXP pcountR(SEXP x, SEXP args) {
  if (xlength(x) != 1 || isNull(x)) {
    error("Argument 'value' must be non NULL and length 1.");
  }
  const int n=length(args);
  if (n < 1) {
    error("Please supply at least 1 argument. (%d argument supplied)", n);
  }
  const SEXP args0 = PTR_ETL(args, 0);
  SEXPTYPE anstype = UTYPEOF(args0);
  const R_xlen_t len0 = xlength(args0);
  if (anstype != LGLSXP && anstype != INTSXP && anstype != REALSXP &&
      anstype != CPLXSXP && anstype != STRSXP) {
    error("Argument %d is of type %s. Only logical, integer, double, complex and"
          " character types are supported.", 1, type2char(anstype));
  }
  SEXPTYPE tx = UTYPEOF(x);
  if (anstype != tx) {
    error("Type of 'value' (%s) is different than type of Argument %d (%s). "
          "Please make sure both have the same type.", type2char(tx), 1, type2char(anstype));
  }
  SEXP classx = PROTECT(getAttrib(x, R_ClassSymbol));
  if(!R_compute_identical(PROTECT(getAttrib(args0, R_ClassSymbol)), classx, 0)) {
    error("Class of 'value' is different than class of Argument %d. "
            "Please make sure both have the same class.", 1);
  }
  UNPROTECT(1);
  int nprotect = 0;
  const bool xf = isFactor(x);
  SEXP levelsx;
  if (xf) {
    levelsx = PROTECT(getAttrib(x, R_LevelsSymbol)); nprotect++;
    if (!R_compute_identical(levelsx, PROTECT(getAttrib(args0, R_LevelsSymbol)), 0)) {
      error("Levels of 'value' are different than levels of Argument %d. "
              "Please make sure both have the same levels.", 1);
    }
    UNPROTECT(1);
  }
  for (int i = 1; i < n; ++i) {
    SEXPTYPE type = UTYPEOF(PTR_ETL(args, i));
    R_xlen_t len1 = xlength(PTR_ETL(args, i));
    if (type != anstype) {
      error("Type of argument %d is %s but argument %d is of type %s. "
            "Please make sure both have the same type.", i+1,
            type2char(type), 1, type2char(anstype));
    }
    if(!R_compute_identical(PROTECT(getAttrib(PTR_ETL(args, i), R_ClassSymbol)), classx, 0)) {
      error("Class of 'value' is different than class of Argument %d. "
              "Please make sure both have the same class.", i+1);
    }
    UNPROTECT(1);
    if (xf) {
      if (!R_compute_identical(levelsx, PROTECT(getAttrib(PTR_ETL(args, i), R_LevelsSymbol)), 0)) {
        error("Levels of 'value' are different than levels of Argument %d. "
                "Please make sure both have the same levels.", i + 1);
      }
      UNPROTECT(1);
    }
    if (len1 != len0) {
      error("Argument %d is of length %zu but argument %d is of length %zu. "
      "If you wish to 'recycle' your argument, please use rep() to make this intent "
      "clear to the readers of your code.", i+1, len1, 1, len0);
    }
  }
  UNPROTECT(1 + nprotect);
  SEXP ans;
  if (len0 > INT_MAX) {
    ans = PROTECT(allocVector(REALSXP, len0)); // # nocov start
    double *restrict pans = REAL(ans);
    memset(pans, 0, (unsigned)len0*sizeof(double));
    switch(anstype) {
    case LGLSXP: {
      const int px = LOGICAL(x)[0];
      for (int i = 0; i < n; ++i) {
        int *pa = LOGICAL(PTR_ETL(args, i));
        for (ssize_t j = 0; j < len0; ++j) {
          if (pa[j] == px) {
            pans[j]++;
          }
        }
      }
    } break;
    case INTSXP: {
      const int px = INTEGER(x)[0];
      for (int i = 0; i < n; ++i) {
        int *pa = INTEGER(PTR_ETL(args, i));
        for (ssize_t j = 0; j < len0; ++j) {
          if (pa[j] == px) {
            pans[j]++;
          }
        }
      }
    } break;
    case REALSXP: {
      const double px = REAL(x)[0];
      for (int i = 0; i < n; ++i) {
        double *pa = REAL(PTR_ETL(args, i));
        for (ssize_t j = 0; j < len0; ++j) {
          if (pa[j] == px) {
            pans[j]++;
          }
        }
      }
    } break;
    case CPLXSXP: {
      const Rcomplex px = COMPLEX(x)[0];
      for (int i = 0; i < n; ++i) {
        Rcomplex *pa = COMPLEX(PTR_ETL(args, i));
        for (ssize_t j = 0; j < len0; ++j) {
          if (EQUAL_CPLX(pa[j], px)) {
            pans[j]++;
          }
        }
      }
    } break;
    case STRSXP: {
      const char* px = RCHAR(x, 0);
      for (int i = 0; i < n; ++i) {
        const SEXP pa = PTR_ETL(args, i);
        for (ssize_t j = 0; j < len0; ++j) {
          if (RCHAR(pa, j) == px) {
            pans[j]++;
          }
        }
      }
    } break;
    } // # nocov end
  } else {
    ans = PROTECT(allocVector(INTSXP, len0));
    int *restrict pans = INTEGER(ans);
    memset(pans, 0, (unsigned)len0*sizeof(int));
    switch(anstype) {
    case LGLSXP: {
      const int px = LOGICAL(x)[0];
      for (int i = 0; i < n; ++i) {
        int *pa = LOGICAL(PTR_ETL(args, i));
        for (ssize_t j = 0; j < len0; ++j) {
          if (pa[j] == px) {
            pans[j]++;
          }
        }
      }
    } break;
    case INTSXP: {
      const int px = INTEGER(x)[0];
      for (int i = 0; i < n; ++i) {
        int *pa = INTEGER(PTR_ETL(args, i));
        for (ssize_t j = 0; j < len0; ++j) {
          if (pa[j] == px) {
            pans[j]++;
          }
        }
      }
    } break;
    case REALSXP: {
      const double px = REAL(x)[0];
      for (int i = 0; i < n; ++i) {
        double *pa = REAL(PTR_ETL(args, i));
        for (ssize_t j = 0; j < len0; ++j) {
          if (pa[j] == px) {
            pans[j]++;
          }
        }
      }
    } break;
    case CPLXSXP: {
      const Rcomplex px = COMPLEX(x)[0];
      for (int i = 0; i < n; ++i) {
        Rcomplex *pa = COMPLEX(PTR_ETL(args, i));
        for (ssize_t j = 0; j < len0; ++j) {
          if (EQUAL_CPLX(pa[j], px)) {
            pans[j]++;
          }
        }
      }
    } break;
    case STRSXP: {
      const char* px = RCHAR(x, 0);
      for (int i = 0; i < n; ++i) {
        const SEXP pa = PTR_ETL(args, i);
        for (ssize_t j = 0; j < len0; ++j) {
          if (RCHAR(pa, j) == px) {
            pans[j]++;
          }
        }
      }
    } break;
    }
  }
  UNPROTECT(1);
  return ans;
}

SEXP pcountNAR(SEXP x, SEXP args) {
  if (xlength(x) != 1 || isNull(x)) {
    error("Argument 'value' must be non NULL and length 1.");
  }
  const int n=length(args);
  if (n < 1) {
    error("Please supply at least 1 argument. (%d argument supplied)", n);
  }
  const SEXP args0 = PTR_ETL(args, 0);
  SEXPTYPE anstype = UTYPEOF(args0);
  const R_xlen_t len0 = xlength(args0);
  if (anstype != LGLSXP && anstype != INTSXP && anstype != REALSXP &&
      anstype != CPLXSXP && anstype != STRSXP) {
    error("Argument %d is of type %s. Only logical, integer, double, complex and"
            " character types are supported.", 1, type2char(anstype));
  }
  SEXPTYPE tx = UTYPEOF(x);
  if (anstype != tx) {
    error("Type of 'value' (%s) is different than type of Argument %d (%s). "
            "Please make sure both have the same type.", type2char(tx), 1, type2char(anstype));
  }
  SEXP classx = PROTECT(getAttrib(x, R_ClassSymbol));
  if(!R_compute_identical(PROTECT(getAttrib(args0, R_ClassSymbol)), classx, 0)) {
    error("Class of 'value' is different than class of Argument %d. "
            "Please make sure both have the same class.", 1);
  }
  UNPROTECT(1);
  int nprotect = 0;
  const bool xf = isFactor(x);
  SEXP levelsx;
  if (xf) {
    levelsx = PROTECT(getAttrib(x, R_LevelsSymbol)); nprotect++;
    if (!R_compute_identical(levelsx, PROTECT(getAttrib(args0, R_LevelsSymbol)), 0)) {
      error("Levels of 'value' are different than levels of Argument %d. "
              "Please make sure both have the same levels.", 1);
    }
    UNPROTECT(1);
  }
  for (int i = 1; i < n; ++i) {
    SEXPTYPE type = UTYPEOF(PTR_ETL(args, i));
    R_xlen_t len1 = xlength(PTR_ETL(args, i));
    if (type != anstype) {
      error("Type of argument %d is %s but argument %d is of type %s. "
              "Please make sure both have the same type.", i+1,
              type2char(type), 1, type2char(anstype));
    }
    if(!R_compute_identical(PROTECT(getAttrib(PTR_ETL(args, i), R_ClassSymbol)), classx, 0)) {
      error("Class of 'value' is different than class of Argument %d. "
              "Please make sure both have the same class.", i+1);
    }
    UNPROTECT(1);
    if (xf) {
      if (!R_compute_identical(levelsx, PROTECT(getAttrib(PTR_ETL(args, i), R_LevelsSymbol)), 0)) {
        error("Levels of 'value' are different than levels of Argument %d. "
                "Please make sure both have the same levels.", i + 1);
      }
      UNPROTECT(1);
    }
    if (len1 != len0) {
      error("Argument %d is of length %zu but argument %d is of length %zu. "
              "If you wish to 'recycle' your argument, please use rep() to make this intent "
              "clear to the readers of your code.", i+1, len1, 1, len0);
    }
  }
  UNPROTECT(1 + nprotect);
  SEXP ans;
  if (len0 > INT_MAX) {
    ans = PROTECT(allocVector(REALSXP, len0)); // # nocov start
    double *restrict pans = REAL(ans);
    memset(pans, 0, (unsigned)len0*sizeof(double));
    switch(anstype) {
    case LGLSXP: {
      for (int i = 0; i < n; ++i) {
        int *pa = LOGICAL(PTR_ETL(args, i));
        for (ssize_t j = 0; j < len0; ++j) {
          if (pa[j] == NA_LOGICAL) {
            pans[j]++;
          }
        }
      }
    } break;
    case INTSXP: {
      for (int i = 0; i < n; ++i) {
        int *pa = INTEGER(PTR_ETL(args, i));
        for (ssize_t j = 0; j < len0; ++j) {
          if (pa[j] == NA_INTEGER) {
            pans[j]++;
          }
        }
      }
    } break;
    case REALSXP: {
      for (int i = 0; i < n; ++i) {
        double *pa = REAL(PTR_ETL(args, i));
        for (ssize_t j = 0; j < len0; ++j) {
          if (ISNAN(pa[j])) {
            pans[j]++;
          }
        }
      }
    } break;
    case CPLXSXP: {
      for (int i = 0; i < n; ++i) {
        Rcomplex *pa = COMPLEX(PTR_ETL(args, i));
        for (ssize_t j = 0; j < len0; ++j) {
          if (ISNAN_COMPLEX(pa[j])) {
            pans[j]++;
          }
        }
      }
    } break;
    case STRSXP: {
      for (int i = 0; i < n; ++i) {
        const SEXP pa = PTR_ETL(args, i);
        const SEXP *restrict px = STRING_PTR(pa);
        for (ssize_t j = 0; j < len0; ++j) {
          if (px[j] == NA_STRING) {
            pans[j]++;
          }
        }
      }
    } break;
    } // # nocov end
  } else {
    ans = PROTECT(allocVector(INTSXP, len0));
    int *restrict pans = INTEGER(ans);
    memset(pans, 0, (unsigned)len0*sizeof(int));
    switch(anstype) {
    case LGLSXP: {
      for (int i = 0; i < n; ++i) {
        int *pa = LOGICAL(PTR_ETL(args, i));
        for (ssize_t j = 0; j < len0; ++j) {
          if (pa[j] == NA_LOGICAL) {
            pans[j]++;
          }
        }
      }
    } break;
    case INTSXP: {
      for (int i = 0; i < n; ++i) {
        int *pa = INTEGER(PTR_ETL(args, i));
        for (ssize_t j = 0; j < len0; ++j) {
          if (pa[j] == NA_INTEGER) {
            pans[j]++;
          }
        }
      }
    } break;
    case REALSXP: {
      for (int i = 0; i < n; ++i) {
        double *pa = REAL(PTR_ETL(args, i));
        for (ssize_t j = 0; j < len0; ++j) {
          if (ISNAN(pa[j])) {
            pans[j]++;
          }
        }
      }
    } break;
    case CPLXSXP: {
      for (int i = 0; i < n; ++i) {
        Rcomplex *pa = COMPLEX(PTR_ETL(args, i));
        for (ssize_t j = 0; j < len0; ++j) {
          if (ISNAN_COMPLEX(pa[j])) {
            pans[j]++;
          }
        }
      }
    } break;
    case STRSXP: {
      for (int i = 0; i < n; ++i) {
        const SEXP pa = PTR_ETL(args, i);
        const SEXP *restrict px = STRING_PTR(pa);
        for (ssize_t j = 0; j < len0; ++j) {
          if (px[j] == NA_STRING) {
            pans[j]++;
          }
        }
      }
    } break;
    }
  }
  UNPROTECT(1);
  return ans;
}
