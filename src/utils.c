/*
 * kit : Useful R Functions Implemented in C
 * Copyright (C) 2020  Morgan Jacob
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

SEXP ompEnabledR() {
  return omp_enabled ? ScalarLogical(TRUE) : ScalarLogical(FALSE);
}

SEXP setlevelsR(SEXP x, SEXP old_lvl, SEXP new_lvl, SEXP skip_absent) {
  if (!IS_BOOL(skip_absent)) {
    error("Argument 'skip_absent' must be TRUE or FALSE and length 1.");
  }
  if (any_duplicated(old_lvl, FALSE)) {
    error("'old' has duplicated value. Please make sure no duplicated values are introduced.");
  }
  if (any_duplicated(new_lvl, FALSE)) {
    error("'new' has duplicated value. Please make sure no duplicated values are introduced.");
  }
  if (!isFactor(x)) {
    error("'setlevels' must be passed a factor.");
  }
  if (UTYPEOF(old_lvl) != STRSXP) {
    error("Type of 'old' must be character.");
  }
  if (UTYPEOF(new_lvl) != STRSXP) {
    error("Type of 'new' must be character.");
  }
  const R_xlen_t nlvl = xlength(old_lvl);
  if (nlvl != xlength(new_lvl)) {
    error("'old' and 'new' are not the same length.");
  }
  const bool absent = !LOGICAL(skip_absent)[0];
  SEXP xchar = PROTECT(getAttrib(x, R_LevelsSymbol));
  const R_xlen_t nx = xlength(xchar);
  for (ssize_t j=0; j<nlvl; ++j) {
    for (ssize_t i=0; i<nx; ++i) {
      if (STRING_ELT(xchar, i) == STRING_ELT(old_lvl, j)) {
        SET_STRING_ELT(xchar, i, STRING_ELT(new_lvl, j));
        goto label;
      }
    }
    if (absent) {
      error("Element '%s' of 'old' does not exist in 'x'.", RCHAR(old_lvl, j));
    }
    label:;
  }
  SEXP ans = PROTECT(duplicate(x));
  setAttrib(ans, R_LevelsSymbol, xchar);
  UNPROTECT(2);
  return ans;
}

/*SEXP removeNA(SEXP x) {
  const R_xlen_t len_x = xlength(x);
  SEXPTYPE tx = UTYPEOF(x);
  R_xlen_t nb = countNA(x);
  if (nb == 0) {
    return x;
  }
  SEXP ans = PROTECT(allocVector(tx, len_x - (unsigned)nb));
  size_t j=0;
  switch(tx) {
  case LGLSXP: {
    const int *restrict px = LOGICAL(x);
    int *restrict pans = LOGICAL(ans);
    for (ssize_t i=0; i<len_x; ++i) {
      if (px[i]== NA_LOGICAL) {
        continue;
      } else {
        pans[j++]=px[i];
      }
    }
  } break;
  case INTSXP: {
    const int *restrict px = INTEGER(x);
    int *restrict pans = INTEGER(ans);
    for (ssize_t i=0; i<len_x; ++i) {
      if (px[i]== NA_INTEGER) {
        continue;
      } else {
        pans[j++]=px[i];
      }
    }
  } break;
  case REALSXP: {
    const double *restrict px = REAL(x);
    double *restrict pans = REAL(ans);
    for (ssize_t i=0; i<len_x; ++i) {
      if (ISNAN(px[i])) {
        continue;
      } else {
        pans[j++]=px[i];
      }
    }
  } break;
  case CPLXSXP: {
    const Rcomplex *restrict px = COMPLEX(x);
    Rcomplex *restrict pans = COMPLEX(ans);
    for (ssize_t i=0; i<len_x; ++i) {
      if (ISNAN_COMPLEX(px[i])) {
        continue;
      } else {
        pans[j++]=px[i];
      }
    }
  } break;
  case STRSXP: {
    const SEXP *restrict px = STRING_PTR(x);
    for (ssize_t i=0; i<len_x; ++i) {
      if (px[i] == NA_STRING) {
        continue;
      } else {
        SET_STRING_ELT(ans, (signed)j++, px[i]);
      }
    }
  } break;
  default:
    error("Type %s is not supported.", type2char(tx));
  }
  UNPROTECT(1);
  return ans;
}*/

SEXP countNAR(SEXP x) {
  const R_xlen_t len_x = xlength(x);
  SEXPTYPE tx = UTYPEOF(x);
  R_xlen_t cnt = 0;
  switch(tx) {
  case LGLSXP: {
    const int *restrict px = LOGICAL(x);
    for (ssize_t i=0; i<len_x; ++i) {
      if (px[i]== NA_LOGICAL) {
        cnt++;
      }
    }
  } break;
  case INTSXP: {
    const int *restrict px = INTEGER(x);
    for (ssize_t i=0; i<len_x; ++i) {
      if (px[i]== NA_INTEGER) {
        cnt++;
      }
    }
  } break;
  case REALSXP: {
    const double *restrict px = REAL(x);
    for (ssize_t i=0; i<len_x; ++i) {
      if (ISNAN(px[i])) {
        cnt++;
      }
    }
  } break;
  case CPLXSXP: {
    const Rcomplex *restrict px = COMPLEX(x);
    for (ssize_t i=0; i<len_x; ++i) {
      if (ISNAN_COMPLEX(px[i])) {
        cnt++;
      }
    }
  } break;
  case STRSXP: {
    const SEXP *restrict px = STRING_PTR(x);
    for (ssize_t i=0; i<len_x; ++i) {
      if (px[i] == NA_STRING) {
        cnt++;
      }
    }
  } break;
  default:
    error("Type %s is not supported.", type2char(tx));
  }
  return cnt > INT_MAX ? ScalarReal(cnt) : ScalarInteger(cnt);
}

/*Rboolean hasNA(SEXP x) {
  const R_xlen_t len_x = xlength(x);
  SEXPTYPE tx = UTYPEOF(x);
  Rboolean na = FALSE;
  switch(tx) {
  case LGLSXP: {
    const int *restrict px = LOGICAL(x);
    for (ssize_t i=0; i<len_x; ++i) {
      if (px[i]== NA_LOGICAL) {
        na = TRUE;
        break;
      }
    }
  } break;
  case INTSXP: {
    const int *restrict px = INTEGER(x);
    for (ssize_t i=0; i<len_x; ++i) {
      if (px[i]== NA_INTEGER) {
        na = TRUE;
        break;
      }
    }
  } break;
  case REALSXP: {
    const double *restrict px = REAL(x);
    for (ssize_t i=0; i<len_x; ++i) {
      if (ISNAN(px[i])) {
        na = TRUE;
        break;
      }
    }
  } break;
  case CPLXSXP: {
    const Rcomplex *restrict px = COMPLEX(x);
    for (ssize_t i=0; i<len_x; ++i) {
      if (ISNAN_COMPLEX(px[i])) {
        na = TRUE;
        break;
      }
    }
  } break;
  case STRSXP: {
    const SEXP *restrict px = STRING_PTR(x);
    for (ssize_t i=0; i<len_x; ++i) {
      if (px[i] == NA_STRING) {
        na = TRUE;
        break;
      }
    }
  } break;
  default:
    error("Type %s is not supported.", type2char(tx));
  }
  return na;
}*/

SEXP uniquePR(SEXP x) {
  const R_xlen_t xlen=xlength(x);
  const SEXP lg=PROTECT(duplicated(x, FALSE));
  const int *restrict plg=LOGICAL(lg);
  R_xlen_t k=0;
  const bool isLong = xlen > INT_MAX;
  SEXP pos, ans=R_NilValue;
  if (isLong) {
    pos = PROTECT(allocVector(REALSXP, xlen));
    double *restrict p=REAL(pos);
    for (R_xlen_t i=0; i<xlen; ++i) {
      if (plg[i]==0) {
        p[k++]=i+1;
      }
    }
    if(k==xlen) {
      UNPROTECT(2);
      return pos;
    }
    ans = PROTECT(allocVector(REALSXP, k));
    memcpy(REAL(ans), p, (unsigned)k*sizeof(double));
  } else {
    pos = PROTECT(allocVector(INTSXP, xlen));
    int *restrict p=INTEGER(pos);
    for (int i=0; i<xlen; ++i) {
      if (plg[i]==0) {
        p[k++]=i+1;
      }
    }
    if(k==xlen) {
      UNPROTECT(2);
      return pos;
    }
    ans = PROTECT(allocVector(INTSXP, k));
    memcpy(INTEGER(ans), p, (unsigned)k*sizeof(int));
  }
  UNPROTECT(3);
  return ans;
}
  