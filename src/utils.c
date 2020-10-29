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
  return cnt > INT_MAX ? ScalarReal((double)cnt) : ScalarInteger((int)cnt);
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
  return cnt > INT_MAX ? ScalarReal((double)cnt) : ScalarInteger((int)cnt);
}

SEXP subSetRowDataFrame(SEXP df, SEXP rws) {
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
      copyMostAttrib(pdf[i], TYPECOL);
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
      error("Type %s is not supported.", type2char(UTYPEOF(pdf[i]))); // add Raw type ?
    }
  }
  UNPROTECT(3);
  return dfo;
}

SEXP subSetRowMatrix(SEXP mat, SEXP rws) {
  const int *restrict prws = INTEGER(rws);
  const int col_mat = ncols(mat);
  const int row_mat = nrows(mat);
  const int len_rws = length(rws);
  SEXP mato;
  switch(UTYPEOF(mat)) {
  case LGLSXP : {
    mato = PROTECT(allocMatrix(LGLSXP, len_rws, col_mat));
    const int *restrict pmat = LOGICAL(mat);
    int *restrict pmato = LOGICAL(mato);
    for (int i = 0; i < col_mat; ++i) {
      for (int j = 0; j < len_rws; ++j) {
        pmato[j+len_rws*i] = pmat[prws[j]+row_mat*i];
      }
    }
  } break;
  case INTSXP : {
    mato = PROTECT(allocMatrix(INTSXP, len_rws, col_mat));
    const int *restrict pmat = INTEGER(mat);
    int *restrict pmato = INTEGER(mato);
    for (int i = 0; i < col_mat; ++i) {
      for (int j = 0; j < len_rws; ++j) {
        pmato[j+len_rws*i] = pmat[prws[j]+row_mat*i];
      }
    }
  } break;
  case REALSXP : {
    mato = PROTECT(allocMatrix(REALSXP, len_rws, col_mat));
    const double *restrict pmat = REAL(mat);
    double *restrict pmato = REAL(mato);
    for (int i = 0; i < col_mat; ++i) {
      for (int j = 0; j < len_rws; ++j) {
        pmato[j+len_rws*i] = pmat[prws[j]+row_mat*i];
      }
    }
  } break;
  case CPLXSXP : {
    mato = PROTECT(allocMatrix(CPLXSXP, len_rws, col_mat));
    const Rcomplex *restrict pmat = COMPLEX(mat);
    Rcomplex *restrict pmato = COMPLEX(mato);
    for (int i = 0; i < col_mat; ++i) {
      for (int j = 0; j < len_rws; ++j) {
        pmato[j+len_rws*i] = pmat[prws[j]+row_mat*i];
      }
    }
  } break;
  case STRSXP : {
    mato = PROTECT(allocMatrix(STRSXP, len_rws, col_mat));
    const SEXP *restrict pmat = STRING_PTR(mat);
    for (int i = 0; i < col_mat; ++i) {
      for (int j = 0; j < len_rws; ++j) {
        SET_STRING_ELT(mato, j+len_rws*i, pmat[prws[j]+row_mat*i]);
      }
    }
  } break;
  default:
    error("Type %s is not supported.", type2char(UTYPEOF(mat))); // add Raw type ?
  }
  UNPROTECT(1);
  return mato;
}

// No checks in this functions
SEXP subSetColDataFrame(SEXP df, SEXP str) {
  SEXP nm = PROTECT(getAttrib(df, R_NamesSymbol));
  const int len = length(str);
  const int cnm = length(nm);
  if (len == 1) {
    int i = 0;
    const SEXP pstr = STRING_ELT(str, 0);
    for (; i < cnm; ++i) {
      if (STRING_ELT(nm, i) == pstr) {
        break;
      }
    }
    if (i == cnm) {
      error("Column '%s' is not in data.frame.", RCHAR(str, 0));
    }
    UNPROTECT(1);
    return VECTOR_ELT(df, i);
  }
  SEXP dfo = PROTECT(allocVector(VECSXP, len));
  int ct = 0;
  for (int i = 0; ct < len; ++i) {
    if (i == cnm) {
      error("Column '%s' is not in data.frame.", RCHAR(str, ct));
    }
	  if (STRING_ELT(nm, i) == STRING_ELT(str, ct)) {
      SET_VECTOR_ELT(dfo, ct++, VECTOR_ELT(df, i));
	    i = -1;
	  }
  }
  classgets(dfo, STR_DF);
  namesgets(dfo, str);
  SEXP rownam = PROTECT(allocVector(INTSXP, 2));
  INTEGER(rownam)[0] = NA_INTEGER;
  INTEGER(rownam)[1] = -(int)length(VECTOR_ELT(df, 0));
  setAttrib(dfo, R_RowNamesSymbol, rownam);
  UNPROTECT(3);
  return dfo;
}

// No checks in this functions (subset just one column)
SEXP subSetColMatrix(SEXP x, R_xlen_t idx) {
  const R_xlen_t len_i = nrows(x);
  SEXPTYPE xt = UTYPEOF(x);
  SEXP ans = PROTECT(allocVector(xt, len_i));
  const R_xlen_t pidx = idx * len_i;
  switch(xt) {
  case LGLSXP : {
    memcpy(LOGICAL(ans), LOGICAL(x)+pidx, (unsigned)len_i*sizeof(Rboolean));
  } break;
  case INTSXP : {
    memcpy(INTEGER(ans), INTEGER(x)+pidx, (unsigned)len_i*sizeof(int));
  } break;
  case REALSXP : {
    memcpy(REAL(ans), REAL(x)+pidx, (unsigned)len_i*sizeof(double));
  } break;
  case CPLXSXP : {
    memcpy(COMPLEX(ans), COMPLEX(x)+pidx, (unsigned)len_i*sizeof(Rcomplex));
  } break;
  case STRSXP : {
    const SEXP *restrict px = STRING_PTR(x);
    for (R_xlen_t i = 0; i < len_i; ++i) {
      SET_STRING_ELT(ans, i, px[i + pidx]);
    }
  } break;
  default:
    error("Matrix of type %s are not supported.", type2char(xt));
  }
  UNPROTECT(1);
  return ans;
}

// This function does not do any check
SEXP addColToDataFrame(SEXP df, SEXP mcol, SEXP coln) {
  const R_xlen_t len_df = xlength(df);
  const R_xlen_t len_col = xlength(mcol);
  SEXP dfo = R_NilValue;
  if (UTYPEOF(mcol) != VECSXP) {
    dfo = PROTECT(allocVector(VECSXP, len_df + 1));
	  for (int i = 0; i < len_df; ++i) {
      SET_VECTOR_ELT(dfo, i, VECTOR_ELT(df, i));
    }
    SET_VECTOR_ELT(dfo, len_df, mcol);
    classgets(dfo, STR_DF);
    SEXP nam = PROTECT(allocVector(STRSXP, len_df + 1));
    SEXP oldnam = PROTECT(getAttrib(df, R_NamesSymbol));
    for (int i = 0; i < len_df; ++i) {
      SET_STRING_ELT(nam, i, STRING_ELT(oldnam, i));
    }
    SET_STRING_ELT(nam, len_df, STRING_ELT(coln, 0));
    namesgets(dfo, nam);
    SEXP rownam = PROTECT(allocVector(INTSXP, 2));
    INTEGER(rownam)[0] = NA_INTEGER;
    INTEGER(rownam)[1] = -(int)len_col;
	  setAttrib(dfo, R_RowNamesSymbol, rownam);
  } else {
    const R_xlen_t len_row = xlength(VECTOR_ELT(df, 0));
	  dfo = PROTECT(allocVector(VECSXP, len_df + len_col));
    for (int i = 0; i < len_df; ++i) {
      SET_VECTOR_ELT(dfo, i, VECTOR_ELT(df, i));
    }
    for (int i = 0; i < len_col; ++i) {
      SET_VECTOR_ELT(dfo, len_df + i, VECTOR_ELT(mcol, i));
    }
    classgets(dfo, STR_DF);
    SEXP nam = PROTECT(allocVector(STRSXP, len_df + len_col));
    SEXP oldnam = PROTECT(getAttrib(df, R_NamesSymbol));
    for (int i = 0; i < len_df; ++i) {
      SET_STRING_ELT(nam, i, STRING_ELT(oldnam, i));
    }
    for (int i = 0; i < len_col; ++i) {
      SET_STRING_ELT(nam, len_df + i, STRING_ELT(coln, i));
    }
    namesgets(dfo, nam);
    SEXP rownam = PROTECT(allocVector(INTSXP, 2));
    INTEGER(rownam)[0] = NA_INTEGER;
    INTEGER(rownam)[1] = -(int)len_row;
	  setAttrib(dfo, R_RowNamesSymbol, rownam);
  }
  UNPROTECT(4);
  return dfo;
}

// TRy to improve this by removing element in the loop or initilising at 0 

SEXP countOccurR(SEXP x) { // can be improved for factors
  if (isFrame(x)) {
    SEXP ans = PROTECT(countOccurDataFrameR(x));
    UNPROTECT(1);
    return ans;
  }
  if (isArray(x)) {
    error("Array are not yet supported.");
  }
  const R_xlen_t n = xlength(x);
  const SEXPTYPE tx = UTYPEOF(x);
  int K;
  size_t M;
  if (tx == INTSXP || tx == STRSXP || tx == REALSXP || tx == CPLXSXP ) {
    if(n >= 1073741824) {
      error("Length of 'x' is too large. (Long vector not supported yet)");
    }
    const size_t n2 = 2U * (size_t) n;
    M = 256;
    K = 8;
    while (M < n2) {
      M *= 2;
      K++;
    }
  } else if (tx == LGLSXP) {
    M = 4;
    K = 2;
  } else {
    error("Type %s is not supported.", type2char(tx));
  }
  R_xlen_t count = 0;
  int *restrict h = (int*)calloc(M, sizeof(int));
  //SEXP ans_l = PROTECT(allocVector(LGLSXP, n));
  SEXP ans_ct = PROTECT(allocVector(INTSXP, n));
  //int *restrict pans_l = LOGICAL(ans_l);
  int *restrict pans_l = (int*)calloc(n, sizeof(int));
  int *restrict pans_ct = INTEGER(ans_ct);
  SEXP ans_f = PROTECT(allocVector(VECSXP, 2));
  switch (tx) {
  case LGLSXP: {
    const int *restrict px = LOGICAL(x);
    size_t id = 0;
    for (int i = 0; i < n; ++i) {
      id = (px[i] == NA_LOGICAL) ? 2U : (size_t) px[i];
      while (h[id]) {
        if (px[h[id]-1]==px[i]) {
          pans_ct[h[id]-1]++;
          goto lbl;
        }
        id++; id %= M;
      }
      h[id] = (int) i + 1;
      pans_l[i]++;
      pans_ct[i] = 1;
      count++;
      lbl:;
    }
    SET_VECTOR_ELT(ans_f, 0, PROTECT(allocVector(tx, count)));
    SET_VECTOR_ELT(ans_f, 1, PROTECT(allocVector(INTSXP, count)));
    R_xlen_t ct = 0;
    int *restrict py = LOGICAL(PTR_ETL(ans_f, 0));
    int *restrict pw = INTEGER(PTR_ETL(ans_f, 1));
    for (int i = 0; ct < count; ++i) {
      if (pans_l[i]) {
        pw[ct] = pans_ct[i];
        py[ct++] = px[i];
      }
    }
  } break;
  case INTSXP: { // think about factor and levels number
    const int *restrict px = INTEGER(x);
    size_t id = 0;
    for (int i = 0; i < n; ++i) {
      id = (px[i] == NA_INTEGER) ? 0 : HASH(px[i], K);
      while (h[id]) {
        if (px[h[id]-1]==px[i]) {
          pans_ct[h[id]-1]++;
          goto ibl;
        }
        id++; id %= M;
      }
      h[id] = (int) i + 1;
      pans_l[i]++;
      pans_ct[i] = 1;
      count++;
      ibl:;
    }
    SET_VECTOR_ELT(ans_f, 0, PROTECT(allocVector(tx, count)));
    SET_VECTOR_ELT(ans_f, 1, PROTECT(allocVector(INTSXP, count)));
    R_xlen_t ct = 0;
    int *restrict py = INTEGER(PTR_ETL(ans_f, 0));
    int *restrict pw = INTEGER(PTR_ETL(ans_f, 1));
    for (int i = 0; ct < count; ++i) {
      if (pans_l[i]) {
        pw[ct] = pans_ct[i];
        py[ct++] = px[i];
      }
    }
  } break;
  case REALSXP: {
    const double *restrict px = REAL(x);
    size_t id = 0;
    union uno tpv;
    for (int i = 0; i < n; ++i) {
      tpv.d = R_IsNA(px[i]) ? NA_REAL : (R_IsNaN(px[i]) ? R_NaN :px[i]);
      id = HASH(tpv.u[0] + tpv.u[1], K);
      while (h[id]) {
        if (REQUAL(px[h[id] - 1], px[i])) {
          pans_ct[h[id]-1]++;
          goto rbl;
        }
        id++; id %= M;
      }
      h[id] = (int) i + 1;
      pans_l[i]++;
      pans_ct[i] = 1;
      count++;
      rbl:;
    }
    SET_VECTOR_ELT(ans_f, 0, PROTECT(allocVector(tx, count)));
    SET_VECTOR_ELT(ans_f, 1, PROTECT(allocVector(INTSXP, count)));
    R_xlen_t ct = 0;
    double *restrict py = REAL(PTR_ETL(ans_f, 0));
    int *restrict pw = INTEGER(PTR_ETL(ans_f, 1));
    for (int i = 0; ct < count; ++i) {
      if (pans_l[i]) {
        pw[ct] = pans_ct[i];
        py[ct++] = px[i];
      }
    }
  } break;
  case CPLXSXP: {
    const Rcomplex *restrict px = COMPLEX(x);
    size_t id = 0;
    unsigned int u;
    union uno tpv;
    Rcomplex tmp;
    for (int i = 0; i < n; ++i) {
      tmp.r = (px[i].r == 0.0) ? 0.0 : px[i].r;
      tmp.i = (px[i].i == 0.0) ? 0.0 : px[i].i;
      if (C_IsNA(tmp)) {
        tmp.r = tmp.i = NA_REAL;
      } else if (C_IsNaN(tmp)) {
        tmp.r = tmp.i = R_NaN;
      }
      tpv.d = tmp.r;
      u = tpv.u[0] ^ tpv.u[1];
      tpv.d = tmp.i;
      u ^= tpv.u[0] ^ tpv.u[1];
      id = HASH(u, K);
      while (h[id]) {
        if (CEQUAL(px[h[id]-1],px[i])) {
          pans_ct[h[id]-1]++;
          goto cbl;
        }
        id++; id %= M;
      }
      h[id] = (int) i + 1;
      pans_l[i]++;
      pans_ct[i] = 1;
      count++;
      cbl:;
    }
    SET_VECTOR_ELT(ans_f, 0, PROTECT(allocVector(tx, count)));
    SET_VECTOR_ELT(ans_f, 1, PROTECT(allocVector(INTSXP, count)));
    R_xlen_t ct = 0;
    Rcomplex *restrict py = COMPLEX(PTR_ETL(ans_f, 0));
    int *restrict pw = INTEGER(PTR_ETL(ans_f, 1));
    for (int i = 0; ct < count; ++i) {
      if (pans_l[i]) {
        pw[ct] = pans_ct[i];
        py[ct++] = px[i];
      }
    }
  } break;
  case STRSXP: {
    const SEXP *restrict px = STRING_PTR(x);
    size_t id = 0;
    for (int i = 0; i < n; ++i) {
      id = HASH(((intptr_t) px[i] & 0xffffffff), K);
      while (h[id]) {
        if (px[h[id]-1]==px[i]) {
          pans_ct[h[id]-1]++;
          goto sbl;
        }
        id++; id %= M;
      }
      h[id] = (int) i + 1;
      pans_l[i]++;
      pans_ct[i] = 1;
      count++;
      sbl:;
    }
    SET_VECTOR_ELT(ans_f, 0, PROTECT(allocVector(tx, count)));
    SET_VECTOR_ELT(ans_f, 1, PROTECT(allocVector(INTSXP, count)));
    R_xlen_t ct = 0;
    int *restrict pw = INTEGER(PTR_ETL(ans_f, 1));
    SEXP p0 = PTR_ETL(ans_f, 0);
    for (int i = 0; ct < count; ++i) {
      if (pans_l[i]) {
        pw[ct] = pans_ct[i];
        SET_STRING_ELT(p0, ct++, px[i]);
      }
    }
  } break;
  }
  free(pans_l);
  free(h);
  copyMostAttrib(x, PTR_ETL(ans_f, 0));
  classgets(ans_f, STR_DF);
  SEXP nam = PROTECT(allocVector(STRSXP, 2));
  SET_STRING_ELT(nam, 0, mkChar("Variable"));
  SET_STRING_ELT(nam, 1, mkChar("Count"));
  namesgets(ans_f, nam);
  SEXP rownam = PROTECT(allocVector(INTSXP, 2));
  INTEGER(rownam)[0] = NA_INTEGER;
  INTEGER(rownam)[1] = -(int)count;
  setAttrib(ans_f, R_RowNamesSymbol, rownam);
  UNPROTECT(6);
  return ans_f;
}

SEXP countOccurDataFrameR(SEXP x) { // move to matrix if possible (change hash algo)
  const SEXP *restrict px = SEXPPTR_RO(x);
  const R_xlen_t len_x = xlength(x);
  const R_xlen_t len_i = xlength(px[0]);
  SEXP mlv = PROTECT(allocMatrix(INTSXP, (int)len_i, (int)len_x));
  for (R_xlen_t i = 0; i < len_x; ++i) {
    memcpy(INTEGER(mlv)+i*len_i, INTEGER(PROTECT(dupVecIndexOnlyR(px[i]))), (unsigned)len_i*sizeof(int));
  }
  UNPROTECT((int)len_x);
  const size_t n2 = 2U * (size_t) len_i;
  size_t M = 256;
  int K = 8;
  while (M < n2) {
    M *= 2;
    K++;
  }
  R_xlen_t count = 0;
  int *restrict h = (int*)calloc(M, sizeof(int));
  //SEXP ans_l = PROTECT(allocVector(LGLSXP, len_i));
  SEXP ans_ct = PROTECT(allocVector(INTSXP, len_i));
  //int *restrict pans_l = LOGICAL(ans_l);
  int *restrict pans_l = (int*)calloc(len_i, sizeof(int));
  int *restrict pans_ct = INTEGER(ans_ct);
  const int *restrict v = INTEGER(mlv);
  size_t id = 0;
  for (R_xlen_t i = 0; i < len_i; ++i) {
    R_xlen_t key = 0;
    for (R_xlen_t j = 0; j < len_x; ++j) {
      key ^= HASH(v[i+j*len_i],K)*97*(j+1);
    }
    id = HASH(key, K);
    while (h[id]) {
      for (R_xlen_t j = 0; j < len_x; ++j) {
        if (v[h[id]-1+j*len_i] != v[i+j*len_i]) {
          goto label1;
        }
      }
      pans_ct[h[id]-1]++;
      goto label2;
      label1:;
      id++; id %= M;
    }
    h[id] = (int) i + 1;
    pans_l[i]++;
    pans_ct[i] = 1;
    count++;
    label2:;
  }
  free(h);
  SEXP indx = PROTECT(allocVector(INTSXP, count));
  SEXP cntr = PROTECT(allocVector(INTSXP, count));
  R_xlen_t ct = 0;
  int *restrict py = INTEGER(indx);
  int *restrict pw = INTEGER(cntr);
  for (int i = 0; ct < count; ++i) {
    if (pans_l[i]) {
      pw[ct] = pans_ct[i];
      py[ct++] = i;
    }
  }
  free(pans_l);
  SEXP output = PROTECT(addColToDataFrame(PROTECT(subSetRowDataFrame(x, indx)),cntr, mkString("Count")));
  UNPROTECT(6);
  return output;
}

// All columns must be of the same type
SEXP dfToMatrix(SEXP df) {
  const SEXP *restrict px = SEXPPTR_RO(df);
  const R_xlen_t len_x = xlength(df);
  const R_xlen_t len_i = xlength(px[0]);
  const SEXPTYPE tx = UTYPEOF(px[0]);
  SEXP mlv = PROTECT(allocMatrix(tx, (int)len_i, (int)len_x));
  switch(tx) {
  case LGLSXP :{
    int *restrict pmlv = LOGICAL(mlv);
    for (int i = 0; i < len_x; ++i) {
      const int *restrict ppx = LOGICAL(px[i]);
      const int ct = i*len_i;
      for (int j = 0; j < len_i; ++j) {
        pmlv[j+ct] = ppx[j];
      }
    }
  } break;
  case INTSXP :{
    int *restrict pmlv = INTEGER(mlv);
    for (int i = 0; i < len_x; ++i) {
      const int *restrict ppx = INTEGER(px[i]);
      const int ct = i*len_i;
      for (int j = 0; j < len_i; ++j) {
        pmlv[j+ct] = ppx[j];
      }
    }
  } break;
  case REALSXP :{
    double *restrict pmlv = REAL(mlv);
    for (int i = 0; i < len_x; ++i) {
      const double *restrict ppx = REAL(px[i]);
      const int ct = i*len_i;
      for (int j = 0; j < len_i; ++j) {
        pmlv[j+ct] = ppx[j];
      }
    }
  } break;
  case CPLXSXP :{
    Rcomplex *restrict pmlv = COMPLEX(mlv);
    for (int i = 0; i < len_x; ++i) {
      const Rcomplex *restrict ppx = COMPLEX(px[i]);
      const int ct = i*len_i;
      for (int j = 0; j < len_i; ++j) {
        pmlv[j+ct] = ppx[j];
      }
    }
  } break;
  case STRSXP :{
    for (int i = 0; i < len_x; ++i) {
      const SEXP *restrict ppx = STRING_PTR(px[i]);
      const int ct = i*len_i;
      for (int j = 0; j < len_i; ++j) {
        SET_STRING_ELT(mlv, j+ct, ppx[j]);
      }
    }
  } break;
  }
  UNPROTECT(1);
  return mlv;
}

bool isMixEnc(SEXP x) {
  const R_xlen_t len = xlength(x);
  SEXP *px = STRING_PTR(x);
  const cetype_t ces = getCharCE(px[0]);
  for (R_xlen_t i = 1; i < len; ++i)
    if(getCharCE(px[i]) != ces)
      return true;
  return false;
}

SEXP enc2UTF8(SEXP x) {
  SEXP *px = STRING_PTR(x);
  const R_xlen_t len = xlength(x);
  if (getCharCE(px[0]) != CE_UTF8) {
    SEXP ans = PROTECT(allocVector(STRSXP, len));
    for (R_xlen_t i = 0; i < len; ++i) {
      SET_STRING_ELT(ans, i, mkCharCE(translateCharUTF8(px[i]), CE_UTF8));
    }
    UNPROTECT(1);
    return ans;
  }
  return x;
}
