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

SEXP countR(SEXP x, SEXP y) {
  const R_xlen_t len_x = xlength(x);
  const R_xlen_t len_y = xlength(y);
  if (len_y != 1 || isNull(y)) {
    error("Argument 'value' must be non NULL and length 1.");
  }
  SEXPTYPE tx = UTYPEOF(x);
  SEXPTYPE ty = UTYPEOF(y);
  if (tx != ty) {
    error("Type of 'value' (%s) is different than type of 'x' (%s). Please make sure both have the same type.", type2char(ty), type2char(tx));
  }
  if(!R_compute_identical(PROTECT(getAttrib(x, R_ClassSymbol)), PROTECT(getAttrib(y, R_ClassSymbol)), 0)) {
    error("'x' has different class than 'y'. Please make sure that both arguments have the same class.");
  }
  UNPROTECT(2);
  if (isFactor(x)) {
    if (!R_compute_identical(PROTECT(getAttrib(x, R_LevelsSymbol)), PROTECT(getAttrib(y, R_LevelsSymbol)), 0)) {
      error("'x' and 'y' are both type factor but their levels are different.");
    }
    UNPROTECT(2);
  }
  R_xlen_t cnt = 0;
  switch(tx) {
  case LGLSXP: {
    const int *restrict px = LOGICAL(x);
    const int py = LOGICAL(y)[0];
    for (ssize_t i=0; i<len_x; ++i) {
      if (px[i]== py) {
        cnt++;
      }
    }
  } break;
  case INTSXP: {
    const int *restrict px = INTEGER(x);
    const int py = INTEGER(y)[0];
    for (ssize_t i=0; i<len_x; ++i) {
      if (px[i]== py) {
        cnt++;
      }
    }
  } break;
  case REALSXP: {
    const double *restrict px = REAL(x);
    const double py = REAL(y)[0];
    for (ssize_t i=0; i<len_x; ++i) {
      if (px[i]== py) {
        cnt++;
      }
    }
  } break;
  case CPLXSXP: {
    const Rcomplex *restrict px = COMPLEX(x);
    const Rcomplex py = COMPLEX(y)[0];
    for (ssize_t i=0; i<len_x; ++i) {
      if (EQUAL_CPLX(px[i], py)) {
        cnt++;
      }
    }
  } break;
  case STRSXP: {
    const char* py = RCHAR(y, 0);
    for (ssize_t i=0; i<len_x; ++i) {
      if (RCHAR(x, i) == py) {
        cnt++;
      }
    }
  } break;
  default:
    error("Type %s is not supported.", type2char(tx));
  }
  return cnt > INT_MAX ? ScalarReal(cnt) : ScalarInteger((int)cnt);
}

SEXP countNAR(SEXP x) {
  const R_xlen_t len_x = xlength(x);
  SEXPTYPE tx = UTYPEOF(x);
  R_xlen_t cnt = 0;
  switch(tx) {
  case NILSXP: break;
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
  case VECSXP: {
    const SEXP *restrict px = SEXPPTR_RO(x);
    SEXP ans = PROTECT(allocVector(VECSXP, len_x));
    for (ssize_t i=0; i<len_x; ++i) {
      SET_VECTOR_ELT(ans, i, countNAR(px[i]));
    }
    UNPROTECT(1);
    return ans;
  } break;
  default:
    error("Type %s is not supported.", type2char(tx));
  }
  return cnt > INT_MAX ? ScalarReal(cnt) : ScalarInteger((int)cnt);
}

SEXP subSetRow(SEXP df, SEXP rws) {
  const SEXP *restrict pdf = SEXPPTR_RO(df);
  const int *restrict prws = INTEGER(rws);
  const R_xlen_t len_df = xlength(df);
  const R_xlen_t len_rws = xlength(rws);
  SEXP dfo = PROTECT(allocVector(VECSXP, len_df));
  classgets(dfo, STR_DF);
  setAttrib(dfo, R_NamesSymbol, PROTECT(getAttrib(df, R_NamesSymbol)));
  SEXP rownam = PROTECT(allocVector(INTSXP, 2));
  INTEGER(rownam)[0] = NA_INTEGER;
  INTEGER(rownam)[1] = -(int)len_rws;
  setAttrib(dfo, R_RowNamesSymbol, rownam);
  for (R_xlen_t i = 0; i < len_df; ++i) {
    switch(UTYPEOF(pdf[i])) {
    case LGLSXP : {
      const int *restrict ptmp = LOGICAL(pdf[i]);
      SEXP TYPECOL = PROTECT(allocVector(LGLSXP, len_rws));
      int *restrict pc = LOGICAL(TYPECOL);
      for (R_xlen_t j = 0; j < len_rws; ++j) {
        pc[j] = ptmp[prws[j]];
      }
      SET_VECTOR_ELT(dfo, i, TYPECOL);
      UNPROTECT(1);
    } break;
    case INTSXP : {
      const int *restrict ptmp = INTEGER(pdf[i]);
      SEXP TYPECOL = PROTECT(allocVector(INTSXP, len_rws));
      int *restrict pc = INTEGER(TYPECOL);
      for (R_xlen_t j = 0; j < len_rws; ++j) {
        pc[j] = ptmp[prws[j]];
      }
      if (isFactor(pdf[i])) {
        copyMostAttrib(pdf[i], TYPECOL);
      }
      SET_VECTOR_ELT(dfo, i, TYPECOL);
      UNPROTECT(1);
    } break;
    case REALSXP : {
      const double *restrict ptmp = REAL(pdf[i]);
      SEXP TYPECOL = PROTECT(allocVector(REALSXP, len_rws));
      double *restrict pc = REAL(TYPECOL);
      for (R_xlen_t j = 0; j < len_rws; ++j) {
        pc[j] = ptmp[prws[j]];
      }
      copyMostAttrib(pdf[i], TYPECOL);
      SET_VECTOR_ELT(dfo, i, TYPECOL);
      UNPROTECT(1);
    } break;
    case CPLXSXP : {
      const Rcomplex *restrict ptmp = COMPLEX(pdf[i]);
      SEXP TYPECOL = PROTECT(allocVector(CPLXSXP, len_rws));
      Rcomplex *restrict pc = COMPLEX(TYPECOL);
      for (R_xlen_t j = 0; j < len_rws; ++j) {
        pc[j] = ptmp[prws[j]];
      }
      SET_VECTOR_ELT(dfo, i, TYPECOL);
      UNPROTECT(1);
    } break;
    case STRSXP : {
      const SEXP *restrict ptmp = STRING_PTR(pdf[i]);
      SEXP TYPECOL = PROTECT(allocVector(STRSXP, len_rws));
      SEXP *restrict pc = STRING_PTR(TYPECOL);
      for (R_xlen_t j = 0; j < len_rws; ++j) {
        pc[j] = ptmp[prws[j]];
      }
      SET_VECTOR_ELT(dfo, i, TYPECOL);
      UNPROTECT(1);
    } break;
    default:
      error("Type %s is not supported.", type2char(UTYPEOF(pdf[i]))); // add Raw type
    }
  }
  UNPROTECT(3);
  return dfo;
}
