/*
 * kit : Useful R Functions Implemented in C
 * Copyright (C) 2020-2024  Morgan Jacob
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

SEXP nswitchR(SEXP x, SEXP na, SEXP nthreads, SEXP chkenc, SEXP args) {
  if (!IS_BOOL(chkenc)) {
    error("Argument 'checkEnc' must be TRUE or FALSE and length 1.");
  }
  if (isS4(x)) {
    error("S4 class objects for argument 'x' are not supported.");
  }
  if (isS4(na)) {
    error("S4 class objects for argument 'na' are not supported.");
  }
  int nth = asInteger(nthreads);
  nth = nth > max_thread ? max_thread : (nth < min_thread ? min_thread : nth); //revisit this
  R_len_t n = length(args);
  if (n % 2) {
    error("Received %d inputs; please supply an even number of arguments in ... "
            "consisting of target value, resulting output pairs (in that order). "
            "Note that argument 'default' must be named explicitly (e.g.: default=0)", n);
  }
  n = n / 2;
  const bool pchkenc = asLogical(chkenc);
  const bool nonna = !isNull(na);
  const bool ifact = isFactor(PTR_ETL(args, 1));
  const R_xlen_t len_na = xlength(na);
  const R_xlen_t len_x = xlength(x);
  const SEXPTYPE type0 = UTYPEOF(PTR_ETL(args, 1));
  const SEXPTYPE type1 = UTYPEOF(PTR_ETL(args, 0));
  const SEXPTYPE type_x = UTYPEOF(x);
  if (type_x != type1) {
    error("Type of 'x' and 'values' are different. Please make sure they are the same.");
  }
  SEXP out0c = PROTECT(getAttrib(PTR_ETL(args, 1), R_ClassSymbol));
  SEXP out0l = PROTECT(getAttrib(PTR_ETL(args, 1), R_LevelsSymbol));
  if (nonna) {
    if (len_na != 1 && len_na != len_x) {
      error("Length of 'default' must either be 1 or length of 'x'."); // maybe improve message
    }
    SEXPTYPE tn = UTYPEOF(na);
    if (tn != type0) {
      error("Resulting value is of type %s but 'default' is of type %s. "
              "Please make sure that both arguments have the same type.",
              type2char(type0), type2char(tn));
    }
    if (!R_compute_identical(out0c, PROTECT(getAttrib(na, R_ClassSymbol)), 0)) {
      error("Resulting value has different class than 'default'. "
              "Please make sure that both arguments have the same class.");
    }
    UNPROTECT(1);
    if (ifact) {
      if (!R_compute_identical(out0l, PROTECT(getAttrib(na, R_LevelsSymbol)), 0)) {
        error("Resulting value and 'default' are both type factor but their levels are different.");
      }
      UNPROTECT(1);
    }
  }
  ssize_t amask[n];
  for (ssize_t i=0; i<n;++i) {
    if (isS4(PTR_ETL(args, 2*i+1))) {
      error("S4 class objects for argument '...' (item %zu) are not supported.", i+1);
    }
    if (type1 != UTYPEOF(PTR_ETL(args,2*i))) {
      error("Item 1 and %zu of '...' are not of the same type.",2*i+1);
    }
    if (type0 != UTYPEOF(PTR_ETL(args,2*i+1))) {
      error("Item 2 and %zu of '...' are not of the same type.",2*i+2);
    }
    R_xlen_t len_i = xlength(PTR_ETL(args, 2*i+1));
    if (len_i != len_x && len_i != 1) {
      error("Length of item %zu of '...' is different than 1 and length of 'x'. "
              "Please make sure that all items of 'output' have length 1 or length of 'x'(%zu).",
              2*i+2, len_x);
    }
    if (xlength(PTR_ETL(args, 2*i)) != 1) {
      error("Length of item %zu of '...' is different than 1. Please make sure it has length 1.",2*i+1);
    }
    if (!R_compute_identical(out0c, PROTECT(getAttrib(PTR_ETL(args, 2*i+1), R_ClassSymbol)), 0)) {
      error("Items 2 and  %zu of '...' must have same class.", 2*i+2);
    }
    UNPROTECT(1);
    if (ifact) {
      if (!R_compute_identical(out0l, PROTECT(getAttrib(PTR_ETL(args, 2*i+1), R_LevelsSymbol)), 0)) {
        error("Items 2 and  %zu of '...' are both factor but their levels are different.", 2*i+2);
      }
      UNPROTECT(1);
    }
    amask[i] = len_i>1 ? SSIZE_MAX : 0;
  }
  SEXP xans = R_NilValue, vans = R_NilValue;
  int nprotect = 0;
  bool utfcon = false;
  if (pchkenc && type_x == STRSXP) {
    if (!isMixEnc(x)) {
      const cetype_t cx = getCharCE(STRING_PTR_RO(x)[0]);
      for (ssize_t i = 0; i < n; ++i) {
        if(cx != getCharCE(STRING_PTR_RO(PTR_ETL(args,2*i))[0])) {
          utfcon = true;
          break;
        }
      }
    } else {
      utfcon = true;
    }
    if (utfcon) {
      xans = PROTECT(enc2UTF8(x));
      vans = PROTECT(allocVector(STRSXP, n));
      nprotect = 2;
      for (ssize_t i = 0; i < n; ++i) {
        SET_STRING_ELT(vans, i, STRING_PTR_RO(enc2UTF8(PTR_ETL(args,2*i)))[0]);
      }
    }
  }
  SEXP ans = PROTECT(allocVector(type0, len_x));
  copyMostAttrib(PTR_ETL(args, 1), ans);
  switch(type0) {
  /*
   *  This part is for LOGICAL
   */
  case LGLSXP:{
    int *restrict pans = LOGICAL(ans);
    const int *restrict pna = nonna ? LOGICAL(na) : NULL; 
    ssize_t namask = len_na>1 ? SSIZE_MAX : 0;
    OMP_PARALLEL_FOR(nth)
    for (ssize_t j=0; j<len_x; ++j) {
      pans[j]= nonna ? pna[j & namask] : NA_LOGICAL;
    }
    switch(type_x) {
    case LGLSXP: {
      const int *restrict px = LOGICAL(x);
      OMP_PARALLEL_FOR(nth)
      for (ssize_t i = 0; i < n; ++i) {
        const int *restrict pto = LOGICAL(PTR_ETL(args, 2*i+1));
        const int *restrict pvalues = LOGICAL(PTR_ETL(args, 2*i));
        for (ssize_t j = 0; j < len_x; ++j) {
          if (px[j] == pvalues[0]) {
            pans[j] = pto[j & amask[i]];
          }
        }
      }
    } break;
    case INTSXP: {
      const int *restrict px = INTEGER(x);
      OMP_PARALLEL_FOR(nth)
      for (ssize_t i = 0; i < n; ++i) {
        const int *restrict pto = LOGICAL(PTR_ETL(args, 2*i+1));
        const int *restrict pvalues = INTEGER(PTR_ETL(args, 2*i));
        for (ssize_t j = 0; j < len_x; ++j) {
          if (px[j] == pvalues[0]) {
            pans[j] = pto[j & amask[i]];
          }
        }
      }
    } break;
    case REALSXP: {
      const double *restrict px = REAL(x);
      OMP_PARALLEL_FOR(nth)
      for (ssize_t i = 0; i < n; ++i) {
        const int *restrict pto = LOGICAL(PTR_ETL(args, 2*i+1));
        const double *restrict pvalues = REAL(PTR_ETL(args, 2*i));
        for (ssize_t j = 0; j < len_x; ++j) {
          if (px[j] == pvalues[0]) {
            pans[j] = pto[j & amask[i]];
          }
        }
      }
    } break;
    case CPLXSXP: {
      const Rcomplex *restrict px = COMPLEX(x);
      OMP_PARALLEL_FOR(nth)
      for (ssize_t i = 0; i < n; ++i) {
        const int *restrict pto = LOGICAL(PTR_ETL(args, 2*i+1));
        const Rcomplex *restrict pvalues = COMPLEX(PTR_ETL(args, 2*i));
        for (ssize_t j = 0; j < len_x; ++j) {
          if (EQUAL_CPLX(px[j], pvalues[0])) {
            pans[j] = pto[j & amask[i]];
          }
        }
      }
    } break;
    case STRSXP: {
      const SEXP *restrict px = STRING_PTR_RO(utfcon ? xans : x);
      OMP_PARALLEL_FOR(nth)
      for (ssize_t i = 0; i < n; ++i) {
        const int *restrict pto = LOGICAL(PTR_ETL(args, 2*i+1));
        const SEXP *restrict pvalues = STRING_PTR_RO(utfcon ? vans : PTR_ETL(args, 2*i));
        for (ssize_t j = 0; j < len_x; ++j) {
          if (px[j] == pvalues[0]) {
            pans[j] = pto[j & amask[i]];
          }
        }
      }
    } break;
    case VECSXP: {
      const SEXP *restrict px = SEXPPTR_RO(x);
      for (ssize_t i = 0; i < n; ++i) {
        const int *restrict pto = LOGICAL(PTR_ETL(args, 2*i+1));
        const SEXP *restrict pvalues = SEXPPTR_RO(PTR_ETL(args, 2*i));
        for (ssize_t j = 0; j < len_x; ++j) {
          if (R_compute_identical(px[j],pvalues[0],0)) {
            pans[j] = pto[i & amask[i]];
          }
        }
      }    
    } break;
    default:
      error("Type %s is not supported for argument 'x'.", type2char(type_x));
    }
  } break;
    /*
     *  This part is for INTEGER
     */
  case INTSXP:{
    int *restrict pans = INTEGER(ans);
    const int *restrict pna = nonna ? INTEGER(na) : NULL; 
    ssize_t namask = len_na>1 ? SSIZE_MAX : 0;
    OMP_PARALLEL_FOR(nth)
    for (ssize_t j=0; j<len_x; ++j) {
      pans[j]= nonna ? pna[j & namask] : NA_INTEGER;
    }
    switch(type_x) {
    case LGLSXP: {
      const int *restrict px = LOGICAL(x);
      OMP_PARALLEL_FOR(nth)
      for (ssize_t i = 0; i < n; ++i) {
        const int *restrict pto = INTEGER(PTR_ETL(args, 2*i+1));
        const int *restrict pvalues = LOGICAL(PTR_ETL(args, 2*i));
        for (ssize_t j = 0; j < len_x; ++j) {
          if (px[j] == pvalues[0]) {
            pans[j] = pto[j & amask[i]];
          }
        }
      }
    } break;
    case INTSXP: {
      const int *restrict px = INTEGER(x);
      OMP_PARALLEL_FOR(nth)
      for (ssize_t i = 0; i < n; ++i) {
        const int *restrict pto = INTEGER(PTR_ETL(args, 2*i+1));
        const int *restrict pvalues = INTEGER(PTR_ETL(args, 2*i));
        for (ssize_t j = 0; j < len_x; ++j) {
          if (px[j] == pvalues[0]) {
            pans[j] = pto[j & amask[i]];
          }
        }
      }
    } break;
    case REALSXP: {
      const double *restrict px = REAL(x);
      OMP_PARALLEL_FOR(nth)
      for (ssize_t i = 0; i < n; ++i) {
        const int *restrict pto = INTEGER(PTR_ETL(args, 2*i+1));
        const double *restrict pvalues = REAL(PTR_ETL(args, 2*i));
        for (ssize_t j = 0; j < len_x; ++j) {
          if (px[j] == pvalues[0]) {
            pans[j] = pto[j & amask[i]];
          }
        }
      }
    } break;
    case CPLXSXP: {
      const Rcomplex *restrict px = COMPLEX(x);
      OMP_PARALLEL_FOR(nth)
      for (ssize_t i = 0; i < n; ++i) {
        const int *restrict pto = INTEGER(PTR_ETL(args, 2*i+1));
        const Rcomplex *restrict pvalues = COMPLEX(PTR_ETL(args, 2*i));
        for (ssize_t j = 0; j < len_x; ++j) {
          if (EQUAL_CPLX(px[j], pvalues[0])) {
            pans[j] = pto[j & amask[i]];
          }
        }
      }
    } break;
    case STRSXP: {
      const SEXP *restrict px = STRING_PTR_RO(utfcon ? xans : x);
      OMP_PARALLEL_FOR(nth)
      for (ssize_t i = 0; i < n; ++i) {
        const int *restrict pto = INTEGER(PTR_ETL(args, 2*i+1));
        const SEXP *restrict pvalues = STRING_PTR_RO(utfcon ? vans : PTR_ETL(args, 2*i));
        for (ssize_t j = 0; j < len_x; ++j) {
          if (px[j] == pvalues[0]) {
            pans[j] = pto[j & amask[i]];
          }
        }
      }
    } break;
    case VECSXP: {
      const SEXP *restrict px = SEXPPTR_RO(x);
      for (ssize_t i = 0; i < n; ++i) {
        const int *restrict pto = INTEGER(PTR_ETL(args, 2*i+1));
        const SEXP *restrict pvalues = SEXPPTR_RO(PTR_ETL(args, 2*i));
        for (ssize_t j = 0; j < len_x; ++j) {
          if (R_compute_identical(px[j],pvalues[0],0)) {
            pans[j] = pto[j & amask[i]];
          }
        }
      }
    } break;
    default:
      error("Type %s is not supported for argument 'x'.", type2char(type_x));
    }
  } break;
    /*
     *  This part is for REAL
     */
  case REALSXP:{
    double *restrict pans = REAL(ans);
    const double *restrict pna = nonna ? REAL(na) : NULL; 
    ssize_t namask = len_na>1 ? SSIZE_MAX : 0;
    OMP_PARALLEL_FOR(nth)
    for (ssize_t j=0; j<len_x; ++j) {
      pans[j]= nonna ? pna[j & namask] : NA_REAL;
    }
    switch(type_x) {
    case LGLSXP: {
      const int *restrict px = LOGICAL(x);
      OMP_PARALLEL_FOR(nth)
      for (ssize_t i = 0; i < n; ++i) {
        const double *restrict pto = REAL(PTR_ETL(args, 2*i+1));
        const int *restrict pvalues = LOGICAL(PTR_ETL(args, 2*i));
        for (ssize_t j = 0; j < len_x; ++j) {
          if (px[j] == pvalues[0]) {
            pans[j] = pto[j & amask[i]];
          }
        }
      }
    } break;
    case INTSXP: {
      const int *restrict px = INTEGER(x);
      OMP_PARALLEL_FOR(nth)
      for (ssize_t i = 0; i < n; ++i) {
        const double *restrict pto = REAL(PTR_ETL(args, 2*i+1));
        const int *restrict pvalues = INTEGER(PTR_ETL(args, 2*i));
        for (ssize_t j = 0; j < len_x; ++j) {
          if (px[j] == pvalues[0]) {
            pans[j] = pto[j & amask[i]];
          }
        }
      }
    } break;
    case REALSXP: {
      const double *restrict px = REAL(x);
      OMP_PARALLEL_FOR(nth)
      for (ssize_t i = 0; i < n; ++i) {
        const double *restrict pto = REAL(PTR_ETL(args, 2*i+1));
        const double *restrict pvalues = REAL(PTR_ETL(args, 2*i));
        for (ssize_t j = 0; j < len_x; ++j) {
          if (px[j] == pvalues[0]) {
            pans[j] = pto[j & amask[i]];
          }
        }
      }
    } break;
    case CPLXSXP: {
      const Rcomplex *restrict px = COMPLEX(x);
      OMP_PARALLEL_FOR(nth)
      for (ssize_t i = 0; i < n; ++i) {
        const double *restrict pto = REAL(PTR_ETL(args, 2*i+1));
        const Rcomplex *restrict pvalues = COMPLEX(PTR_ETL(args, 2*i));
        for (ssize_t j = 0; j < len_x; ++j) {
          if (EQUAL_CPLX(px[j], pvalues[0])) {
            pans[j] = pto[j & amask[i]];
          }
        }
      }
    } break;
    case STRSXP: {
      const SEXP *restrict px = STRING_PTR_RO(utfcon ? xans : x);
      OMP_PARALLEL_FOR(nth)
      for (ssize_t i = 0; i < n; ++i) {
        const double *restrict pto = REAL(PTR_ETL(args, 2*i+1));
        const SEXP *restrict pvalues = STRING_PTR_RO(utfcon ? vans : PTR_ETL(args, 2*i));
        for (ssize_t j = 0; j < len_x; ++j) {
          if (px[j] == pvalues[0]) {
            pans[j] = pto[j & amask[i]];
          }
        }
      }
    } break;
    case VECSXP: {
      const SEXP *restrict px = SEXPPTR_RO(x);
      for (ssize_t i = 0; i < n; ++i) {
        const double *restrict pto = REAL(PTR_ETL(args, 2*i+1));
        const SEXP *restrict pvalues = SEXPPTR_RO(PTR_ETL(args, 2*i));
        for (ssize_t j = 0; j < len_x; ++j) {
          if (R_compute_identical(px[j],pvalues[0],0)) {
            pans[j] = pto[j & amask[i]];
          }
        }
      }
    } break;
    default:
      error("Type %s is not supported for argument 'x'.", type2char(type_x));
    }
  } break;
    /*
     *  This part is for COMPLEX
     */
  case CPLXSXP:{
    Rcomplex *restrict pans = COMPLEX(ans);
    const Rcomplex *restrict pna = nonna ? COMPLEX(na) : NULL; 
    ssize_t namask = len_na>1 ? SSIZE_MAX : 0;
    Rcomplex NA_CPLX; NA_CPLX.r = NA_REAL; NA_CPLX.i = NA_REAL; // deal with that across all functions
    OMP_PARALLEL_FOR(nth)
    for (ssize_t j=0; j<len_x; ++j) {
      pans[j]= nonna ? pna[j & namask] : NA_CPLX;
    }
    switch(type_x) {
    case LGLSXP: {
      const int *restrict px = LOGICAL(x);
      OMP_PARALLEL_FOR(nth)
      for (ssize_t i = 0; i < n; ++i) {
        const Rcomplex *restrict pto = COMPLEX(PTR_ETL(args, 2*i+1));
        const int *restrict pvalues = LOGICAL(PTR_ETL(args, 2*i));
        for (ssize_t j = 0; j < len_x; ++j) {
          if (px[j] == pvalues[0]) {
            pans[j] = pto[j & amask[i]];
          }
        }
      }
    } break;
    case INTSXP: {
      const int *restrict px = INTEGER(x);
      OMP_PARALLEL_FOR(nth)
      for (ssize_t i = 0; i < n; ++i) {
        const Rcomplex *restrict pto = COMPLEX(PTR_ETL(args, 2*i+1));
        const int *restrict pvalues = INTEGER(PTR_ETL(args, 2*i));
        for (ssize_t j = 0; j < len_x; ++j) {
          if (px[j] == pvalues[0]) {
            pans[j] = pto[j & amask[i]];
          }
        }
      }
    } break;
    case REALSXP: {
      const double *restrict px = REAL(x);
      OMP_PARALLEL_FOR(nth)
      for (ssize_t i = 0; i < n; ++i) {
        const Rcomplex *restrict pto = COMPLEX(PTR_ETL(args, 2*i+1));
        const double *restrict pvalues = REAL(PTR_ETL(args, 2*i));
        for (ssize_t j = 0; j < len_x; ++j) {
          if (px[j] == pvalues[0]) {
            pans[j] = pto[j & amask[i]];
          }
        }
      }
    } break;
    case CPLXSXP: {
      const Rcomplex *restrict px = COMPLEX(x);
      OMP_PARALLEL_FOR(nth)
      for (ssize_t i = 0; i < n; ++i) {
        const Rcomplex *restrict pto = COMPLEX(PTR_ETL(args, 2*i+1));
        const Rcomplex *restrict pvalues = COMPLEX(PTR_ETL(args, 2*i));
        for (ssize_t j = 0; j < len_x; ++j) {
          if (EQUAL_CPLX(px[j],pvalues[0])) {
            pans[j] = pto[j & amask[i]];
          }
        }
      }
    } break;
    case STRSXP: {
      const SEXP *restrict px = STRING_PTR_RO(utfcon ? xans : x);
      OMP_PARALLEL_FOR(nth)
      for (ssize_t i = 0; i < n; ++i) {
        const Rcomplex *restrict pto = COMPLEX(PTR_ETL(args, 2*i+1));
        const SEXP *restrict pvalues = STRING_PTR_RO(utfcon ? vans : PTR_ETL(args, 2*i));
        for (ssize_t j = 0; j < len_x; ++j) {
          if (px[j] == pvalues[0]) {
            pans[j] = pto[j & amask[i]];
          }
        }
      }
    } break;
    case VECSXP: {
      const SEXP *restrict px = SEXPPTR_RO(x);
      for (ssize_t i = 0; i < n; ++i) {
        const Rcomplex *restrict pto = COMPLEX(PTR_ETL(args, 2*i+1));
        const SEXP *restrict pvalues = SEXPPTR_RO(PTR_ETL(args, 2*i));
        for (ssize_t j = 0; j < len_x; ++j) {
          if (R_compute_identical(px[j],pvalues[0],0)) {
            pans[j] = pto[j & amask[i]];
          }
        }
      }
    } break;
    default:
      error("Type %s is not supported for argument 'x'.", type2char(type_x));
    }
  } break;
    /*
     *  This part is for STRING
     */
  case STRSXP:{
    const SEXP *restrict pna = nonna ? STRING_PTR_RO(na) : NULL; 
    ssize_t namask = len_na>1 ? SSIZE_MAX : 0;
    for (ssize_t j=0; j<len_x; ++j) {
      SET_STRING_ELT(ans, j, nonna ? pna[j & namask] : NA_STRING);
    }
    switch(type_x) {
    case LGLSXP: {
      const int *restrict px = LOGICAL(x);
      for (ssize_t i = 0; i < n; ++i) {
        const SEXP *restrict pto = STRING_PTR_RO(PTR_ETL(args, 2*i+1));
        const int *restrict pvalues = LOGICAL(PTR_ETL(args, 2*i));
        for (ssize_t j = 0; j < len_x; ++j) {
          if (px[j] == pvalues[0]) {
            SET_STRING_ELT(ans, j, pto[j & amask[i]]);
          }
        }
      }
    } break;
    case INTSXP: {
      const int *restrict px = INTEGER(x);
      for (ssize_t i = 0; i < n; ++i) {
        const SEXP *restrict pto = STRING_PTR_RO(PTR_ETL(args, 2*i+1));
        const int *restrict pvalues = INTEGER(PTR_ETL(args, 2*i));
        for (ssize_t j = 0; j < len_x; ++j) {
          if (px[j] == pvalues[0]) {
            SET_STRING_ELT(ans, j, pto[j & amask[i]]);
          }
        }
      }
    } break;
    case REALSXP: {
      const double *restrict px = REAL(x);
      for (ssize_t i = 0; i < n; ++i) {
        const SEXP *restrict pto = STRING_PTR_RO(PTR_ETL(args, 2*i+1));
        const double *restrict pvalues = REAL(PTR_ETL(args, 2*i));
        for (ssize_t j = 0; j < len_x; ++j) {
          if (px[j] == pvalues[0]) {
            SET_STRING_ELT(ans, j, pto[j & amask[i]]);
          }
        }
      }
    } break;
    case CPLXSXP: {
      const Rcomplex *restrict px = COMPLEX(x);
      for (ssize_t i = 0; i < n; ++i) {
        const SEXP *restrict pto = STRING_PTR_RO(PTR_ETL(args, 2*i+1));
        const Rcomplex *restrict pvalues = COMPLEX(PTR_ETL(args, 2*i));
        for (ssize_t j = 0; j < len_x; ++j) {
          if (EQUAL_CPLX(px[j],pvalues[0])) {
            SET_STRING_ELT(ans, j, pto[j & amask[i]]);
          }
        }
      }
    } break;
    case STRSXP: {
      const SEXP *restrict px = STRING_PTR_RO(utfcon ? xans : x);
      for (ssize_t i = 0; i < n; ++i) {
        const SEXP *restrict pto = STRING_PTR_RO(PTR_ETL(args, 2*i+1));
        const SEXP *restrict pvalues = STRING_PTR_RO(utfcon ? vans : PTR_ETL(args, 2*i));
        for (ssize_t j = 0; j < len_x; ++j) {
          if (px[j] == pvalues[0]) {
            SET_STRING_ELT(ans, j, pto[j & amask[i]]);
          }
        }
      }
    } break;
    case VECSXP: {
      const SEXP *restrict px = SEXPPTR_RO(x);
      for (ssize_t i = 0; i < n; ++i) {
        const SEXP *restrict pto = STRING_PTR_RO(PTR_ETL(args, 2*i+1));
        const SEXP *restrict pvalues = SEXPPTR_RO(PTR_ETL(args, 2*i));
        for (ssize_t j = 0; j < len_x; ++j) {
          if (R_compute_identical(px[j],pvalues[0],0)) {
            SET_STRING_ELT(ans, j, pto[j & amask[i]]);
          }
        }
      }
    } break;
    default:
      error("Type %s is not supported for argument 'x'.", type2char(type_x));
    }
  } break;
    /*
     *  This part is for LIST
     */
  case VECSXP:{
    const SEXP *restrict pna = nonna ? SEXPPTR_RO(na) : NULL; 
    ssize_t namask = len_na>1 ? SSIZE_MAX : 0;
    for (ssize_t j=0; j<len_x; ++j) {
      if (nonna) {
        SET_VECTOR_ELT(ans, j, pna[j & namask]); 
      }
    }
    switch(type_x) {
    case LGLSXP: {
      const int *restrict px = LOGICAL(x);
      for (ssize_t i = 0; i < n; ++i) {
        const SEXP *restrict pto = SEXPPTR_RO(PTR_ETL(args, 2*i+1));
        const int *restrict pvalues = LOGICAL(PTR_ETL(args, 2*i));
        for (ssize_t j = 0; j < len_x; ++j) {
          if (px[j] == pvalues[0]) {
            SET_VECTOR_ELT(ans, j, pto[j & amask[i]]);
          }
        }
      }
    } break;
    case INTSXP: {
      const int *restrict px = INTEGER(x);
      for (ssize_t i = 0; i < n; ++i) {
        const SEXP *restrict pto = SEXPPTR_RO(PTR_ETL(args, 2*i+1));
        const int *restrict pvalues = INTEGER(PTR_ETL(args, 2*i));
        for (ssize_t j = 0; j < len_x; ++j) {
          if (px[j] == pvalues[0]) {
            SET_VECTOR_ELT(ans, j, pto[j & amask[i]]);
          }
        }
      }
    } break;
    case REALSXP: {
      const double *restrict px = REAL(x);
      for (ssize_t i = 0; i < n; ++i) {
        const SEXP *restrict pto = SEXPPTR_RO(PTR_ETL(args, 2*i+1));
        const double *restrict pvalues = REAL(PTR_ETL(args, 2*i));
        for (ssize_t j = 0; j < len_x; ++j) {
          if (px[j] == pvalues[0]) {
            SET_VECTOR_ELT(ans, j, pto[j & amask[i]]);
          }
        }
      }
    } break;
    case CPLXSXP: {
      const Rcomplex *restrict px = COMPLEX(x);
      for (ssize_t i = 0; i < n; ++i) {
        const SEXP *restrict pto = SEXPPTR_RO(PTR_ETL(args, 2*i+1));
        const Rcomplex *restrict pvalues = COMPLEX(PTR_ETL(args, 2*i));
        for (ssize_t j = 0; j < len_x; ++j) {
          if (EQUAL_CPLX(px[j], pvalues[0])) {
            SET_VECTOR_ELT(ans, j, pto[j & amask[i]]);
          }
        }
      }
    } break;
    case STRSXP: {
      const SEXP *restrict px = STRING_PTR_RO(utfcon ? xans : x);
      for (ssize_t i = 0; i < n; ++i) {
        const SEXP *restrict pto = SEXPPTR_RO(PTR_ETL(args, 2*i+1));
        const SEXP *restrict pvalues = STRING_PTR_RO(utfcon ? vans : PTR_ETL(args, 2*i));
        for (ssize_t j = 0; j < len_x; ++j) {
          if (px[j] == pvalues[0]) {
            SET_VECTOR_ELT(ans, j, pto[j & amask[i]]);
          }
        }
      }
    } break;
    case VECSXP: {
      const SEXP *restrict px = SEXPPTR_RO(x);
      for (ssize_t i = 0; i < n; ++i) {
        const SEXP *restrict pto = SEXPPTR_RO(PTR_ETL(args, 2*i+1));
        const SEXP *restrict pvalues = SEXPPTR_RO(PTR_ETL(args, 2*i));
        for (ssize_t j = 0; j < len_x; ++j) {
          if (R_compute_identical(px[j],pvalues[0],0)) {
            SET_VECTOR_ELT(ans, j, pto[j & amask[i]]);
          }
        }
      }
    } break;
    default:
      error("Type %s is not supported for argument 'x'.", type2char(type_x));
    }
  } break;
  default :
    error("Type %s is not supported for argument 'outputs'", type2char(type0));
  }
  UNPROTECT(nprotect + 3);
  return ans;
}
