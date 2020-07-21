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

/*
 *  Main Function
 */

SEXP dupR(SEXP x, SEXP uniq) {
  if (isFrame(x)) {
    SEXP ans = PROTECT(dupDataFrameR(x, uniq));
    UNPROTECT(1);
    return ans;
  }
  if (isMatrix(x)) {
    SEXP ans = PROTECT(dupMatrixR(x, uniq));
    UNPROTECT(1);
    return ans;
  }
  if (isArray(x)) {
    error("Arrays are not yet supported. (please raise a feature request if needed)");
  }
  SEXP ans = PROTECT(dupVecR(x, uniq));
  UNPROTECT(1);
  return ans;
}

/*
 *  Data.Frame
 */

SEXP dupDataFrameR(SEXP x, SEXP uniq) { // move to matrix if possible
  const SEXP *restrict px = SEXPPTR_RO(x);
  const R_xlen_t len_x = xlength(x);
  const R_xlen_t len_i = xlength(px[0]);
  SEXP ans = PROTECT(allocVector(LGLSXP, len_i));
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
  int *h = (int*) calloc(M, sizeof(int));
  const int *restrict v = INTEGER(mlv);
  int *restrict pans = LOGICAL(ans);
  size_t id = 0;
  for (R_xlen_t i = 0; i < len_i; ++i) {
    R_xlen_t key = 0;
    for (R_xlen_t j = 0; j < len_x; ++j) {
      key ^=  HASH(v[i+j*len_i],K)*97;
    }
    id = HASH(key, K);
    while (h[id]) {
      for (R_xlen_t j = 0; j < len_x; ++j) {
        if (v[h[id]-1+j*len_i] != v[i+j*len_i]) {
          goto label1;
        }
      }
      pans[i] = 1; goto label2;
      label1:;
      id++; id %= M;
    }
    h[id] = (int) i + 1;
    pans[i] = 0;
    count++;
    label2:;
  }
  free(h);
  if (asLogical(uniq)) {
    UNPROTECT(1);
    SEXP indx = PROTECT(allocVector(INTSXP, count));
    int ct = 0;
    int *restrict py = INTEGER(indx);
    for (int i = 0; ct < count; ++i) {
      if (pans[i] == 0) {
        py[ct++] = i;
      }
    }
    SEXP output = PROTECT(subSetRowDataFrame(x, indx));
    UNPROTECT(3);
    return output;
  }
  UNPROTECT(2);
  return ans;
}

/*
 *  Matrix
 */

SEXP dupMatrixR(SEXP x, SEXP uniq) {
  const R_xlen_t len_x = ncols(x);
  const R_xlen_t len_i = nrows(x);
  SEXP ans = PROTECT(allocVector(LGLSXP, len_i));
  const size_t n2 = 2U * (size_t) len_i;
  size_t M = 256;
  int K = 8;
  while (M < n2) {
    M *= 2;
    K++;
  }
  R_xlen_t count = 0;
  int *h = (int*) calloc(M, sizeof(int));
  int *restrict pans = LOGICAL(ans);
  size_t id = 0;
  switch(UTYPEOF(x)) {
  case LGLSXP : {
    const int *restrict px = LOGICAL(x);
    for (R_xlen_t i = 0; i < len_i; ++i) {
      id = 0;
      for (R_xlen_t j = 0; j < len_x; ++j) {
        id ^= ((unsigned)(j+1) * ((px[i+j*len_i] == NA_LOGICAL) ? 2U : (size_t) px[i+j*len_i]))*97;
      }
      id = HASH(id, K);
      while (h[id]) {
        for (R_xlen_t j = 0; j < len_x; ++j) {
          if (px[h[id]-1+j*len_i] != px[i+j*len_i]) {
            goto labelml1;
          }
        }
        pans[i] = 1; goto labelml2;
        labelml1:;
        id++; id %= M;
      }
      h[id] = (int) i + 1;
      pans[i] = 0;
      count++;
      labelml2:;
    }
  } break;
  case INTSXP : {
    const int *restrict px = INTEGER(x);
    for (R_xlen_t i = 0; i < len_i; ++i) {
      R_xlen_t key = 0;
      for (R_xlen_t j = 0; j < len_x; ++j) {
        key ^= HASH(((px[i+j*len_i] == NA_INTEGER) ? 0 : px[i+j*len_i]),K)*97;
      }
      id = HASH(key, K);
      while (h[id]) {
        for (R_xlen_t j = 0; j < len_x; ++j) {
          if (px[h[id]-1+j*len_i] != px[i+j*len_i]) {
            goto labelmi1;
          }
        }
        pans[i] = 1; goto labelmi2;
        labelmi1:;
        id++; id %= M;
      }
      h[id] = (int) i + 1;
      pans[i] = 0;
      count++;
      labelmi2:;
    }
  } break;
  case REALSXP : {
    const double *restrict px = REAL(x);
    union uno tpv;
    for (R_xlen_t i = 0; i < len_i; ++i) {
      R_xlen_t key = 0;
      for (R_xlen_t j = 0; j < len_x; ++j) {
        tpv.d = px[i+j*len_i];
        key ^= HASH(tpv.u[0] + tpv.u[1],K)*97;
      }
      tpv.d = key;
      id = HASH(tpv.u[0] + tpv.u[1], K);
      while (h[id]) {
        for (R_xlen_t j = 0; j < len_x; ++j) {
          if (!REQUAL(px[h[id]-1+j*len_i], px[i+j*len_i])) {
            goto labelmr1;
          }
        }
        pans[i] = 1; goto labelmr2;
        labelmr1:;
        id++; id %= M;
      }
      h[id] = (int) i + 1;
      pans[i] = 0;
      count++;
      labelmr2:;
    }
  } break;
  case CPLXSXP : {
    const Rcomplex *restrict px = COMPLEX(x);
    unsigned int u;
    union uno tpv;
    Rcomplex tmp;
    for (R_xlen_t i = 0; i < len_i; ++i) {
      R_xlen_t key = 0;
      for (R_xlen_t j = 0; j < len_x; ++j) {
        tmp.r = (px[i+j*len_i].r == 0.0) ? 0.0 : px[i+j*len_i].r;
        tmp.i = (px[i+j*len_i].i == 0.0) ? 0.0 : px[i+j*len_i].i;
        if (C_IsNA(tmp)) {
          tmp.r = tmp.i = NA_REAL;
        } else if (C_IsNaN(tmp)) {
          tmp.r = tmp.i = R_NaN;
        }
        tpv.d = tmp.r;
        u = tpv.u[0] ^ tpv.u[1];
        tpv.d = tmp.i;
        u ^= tpv.u[0] ^ tpv.u[1];
        key ^= HASH(u, K)*97;
      }
      id = HASH(key, K);
      while (h[id]) {
        for (R_xlen_t j = 0; j < len_x; ++j) {
          if (!CEQUAL(px[h[id]-1+j*len_i], px[i+j*len_i])) {
            goto labelmc1;
          }
        }
        pans[i] = 1; goto labelmc2;
        labelmc1:;
        id++; id %= M;
      }
      h[id] = (int) i + 1;
      pans[i] = 0;
      count++;
      labelmc2:;
    }
  } break;
  case STRSXP : {
    const SEXP *restrict px = STRING_PTR(x);
    for (R_xlen_t i = 0; i < len_i; ++i) {
      R_xlen_t key = 0;
      for (R_xlen_t j = 0; j < len_x; ++j) {
        key ^= HASH(((intptr_t) px[i+j*len_i] & 0xffffffff) ^ 0,K)*97;
      }
      id = HASH(key, K);
      while (h[id]) {
        for (R_xlen_t j = 0; j < len_x; ++j) {
          if (px[h[id]-1+j*len_i] != px[i+j*len_i]) {
            goto labelms1;
          }
        }
        pans[i] = 1; goto labelms2;
        labelms1:;
        id++; id %= M;
      }
      h[id] = (int) i + 1;
      pans[i] = 0;
      count++;
      labelms2:;
    }
  } break;
  default:
    error("Matrix of type %s are not supported.", type2char(UTYPEOF(x)));
  }
  free(h);
  if (asLogical(uniq)) {
    SEXP indx = PROTECT(allocVector(INTSXP, count));
    int ct = 0;
    int *restrict py = INTEGER(indx);
    for (int i = 0; ct < count; ++i) {
      if (pans[i] == 0) {
        py[ct++] = i;
      }
    }
    SEXP output = PROTECT(subSetRowMatrix(x, indx));
    UNPROTECT(3);
    return output;
  }
  UNPROTECT(1);
  return ans;
}

/*
 *  Vector
 */

SEXP dupVecR(SEXP x, SEXP uniq) {
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
  int *h = (int*)calloc(M, sizeof(int));
  SEXP ans = PROTECT(allocVector(LGLSXP, n));
  int *restrict pans = LOGICAL(ans);
  switch (tx) {
  case LGLSXP: {
    const int *restrict px = LOGICAL(x);
    size_t id = 0;
    for (int i = 0; i < n; ++i) {
      id = (px[i] == NA_LOGICAL) ? 2U : (size_t) px[i];
      while (h[id]) {
        if (px[h[id]-1]==px[i]) {
          pans[i] = 1;
          goto lbl;
        }
        id++; id %= M;
      }
      h[id] = (int) i + 1;
      pans[i] = 0;
      count++;
      lbl:;
    }
    free(h);
    if (asLogical(uniq)) {
      SEXP indx = PROTECT(allocVector(tx, count));
      size_t ct = 0;
      int *restrict py = LOGICAL(indx);
      for (int i = 0; i < n; ++i) {
        if (pans[i] == 0) {
          py[ct++] = px[i];
        }
      }
      UNPROTECT(2);
      return indx;
    }
  } break;
  case INTSXP: { // think about factor and levels number
    const int *restrict px = INTEGER(x);
    size_t id = 0;
    for (int i = 0; i < n; ++i) {
      id = (px[i] == NA_INTEGER) ? 0 : HASH(px[i], K);
      while (h[id]) {
        if (px[h[id]-1]==px[i]) {
          pans[i] = 1;
          goto ibl;
        }
        id++; id %= M;
      }
      h[id] = (int) i + 1;
      pans[i] =  0;
      count++;
      ibl:;
    }
    free(h);
    if (asLogical(uniq)) {
      SEXP indx = PROTECT(allocVector(tx, count));
      size_t ct = 0;
      int *restrict py = INTEGER(indx);
      for (R_xlen_t i = 0; i < n; ++i) {
        if (pans[i] == 0) {
          py[ct++] = px[i];
        }
      }
      copyMostAttrib(x, indx);
      UNPROTECT(2);
      return indx;
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
        if (REQUAL(px[h[id]-1], px[i])) {
          pans[i] = 1;
          goto rbl;
        }
        id++; id %= M;
      }
      h[id] = (int) i + 1;
      pans[i] = 0;
      count++;
      rbl:;
    }
    free(h);
    if (asLogical(uniq)) {
      SEXP indx = PROTECT(allocVector(tx, count));
      size_t ct = 0;
      double *restrict py = REAL(indx);
      for (R_xlen_t i = 0; i < n; ++i) {
        if (pans[i] == 0) {
          py[ct++] = px[i];
        }
      }
      copyMostAttrib(x, indx);
      UNPROTECT(2);
      return indx;
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
        if (CEQUAL(px[h[id] - 1],px[i])) {
          pans[i] = 1;
          goto cbl;
        }
        id++; id %= M;
      }
      h[id] = (int) i + 1;
      pans[i] = 0;
      count++;
      cbl:;
    }
    free(h);
    if (asLogical(uniq)) {
      SEXP indx = PROTECT(allocVector(tx, count));
      size_t ct = 0;
      Rcomplex *restrict py = COMPLEX(indx);
      for (int i = 0; i < n; ++i) {
        if (pans[i] == 0) {
          py[ct++] = px[i];
        }
      }
      UNPROTECT(2);
      return indx;
    }
  } break;
  case STRSXP: {
    const SEXP *restrict px = STRING_PTR(x);
    size_t id = 0;
    for (int i = 0; i < n; ++i) {
      id = HASH(((intptr_t) px[i] & 0xffffffff) ^ 0, K);
      while (h[id]) {
        if (px[h[id] - 1]==px[i]) {
          pans[i] = 1;
          goto sbl;
        }
        id++; id %= M;
      }
      h[id] = (int) i + 1;
      pans[i] = 0;
      count++;
      sbl:;
    }
    free(h);
    if (asLogical(uniq)) {
      SEXP indx = PROTECT(allocVector(tx, count));
      R_xlen_t ct = 0;
      for (int i = 0; i < n; ++i) {
        if (pans[i] == 0) {
          SET_STRING_ELT(indx, ct++, px[i]);
        }
      }
      UNPROTECT(2);
      return indx;
    }
  } break;
  }
  UNPROTECT(1);
  return ans;
}

/*
 *  Vector Index
 */

SEXP dupVecIndexR(SEXP x) {
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
  int *h = (int*)calloc(M, sizeof(int));
  SEXP ans_l = PROTECT(allocVector(LGLSXP, n));
  SEXP ans_f = PROTECT(allocVector(VECSXP, 2));
  SET_VECTOR_ELT(ans_f, 0, PROTECT(allocVector(INTSXP, n)));
  int *restrict pans_l = LOGICAL(ans_l);
  int *restrict pans_i = INTEGER(PTR_ETL(ans_f, 0));
  switch (tx) {
  case LGLSXP: {
    const int *restrict px = LOGICAL(x);
    size_t id = 0;
    for (int i = 0; i < n; ++i) {
      id = (px[i] == NA_LOGICAL) ? 2U : (size_t) px[i];
      while (h[id]) {
        if (px[h[id]-1]==px[i]) {
          pans_l[i] = 1;
          pans_i[i] = h[id];
          goto lbl;
        }
        id++; id %= M;
      }
      h[id] = (int) i + 1;
      pans_l[i] = 0;
      pans_i[i] = h[id];
      count++;
      lbl:;
    }
  } break;
  case INTSXP: { // think about factor and levels number
    const int *restrict px = INTEGER(x);
    size_t id = 0;
    for (int i = 0; i < n; ++i) {
      id = (px[i] == NA_INTEGER) ? 0 : HASH(px[i], K);
      while (h[id]) {
        if (px[h[id]-1]==px[i]) {
          pans_l[i] = 1;
          pans_i[i] = h[id];
          goto ibl;
        }
        id++; id %= M;
      }
      h[id] = (int) i + 1;
      pans_l[i] = 0;
      pans_i[i] = h[id];
      count++;
      ibl:;
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
        if (REQUAL(px[h[id]-1], px[i])) {
          pans_l[i] = 1;
          pans_i[i] = h[id];
          goto rbl;
        }
        id++; id %= M;
      }
      h[id] = (int) i + 1;
      pans_l[i] = 0;
      pans_i[i] = h[id];
      count++;
      rbl:;
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
          pans_l[i] = 1;
          pans_i[i] = h[id];
          goto cbl;
        }
        id++; id %= M;
      }
      h[id] = (int) i + 1;
      pans_l[i] = 0;
      pans_i[i] = h[id];
      count++;
      cbl:;
    }
  } break;
  case STRSXP: {
    const SEXP *restrict px = STRING_PTR(x);
    size_t id = 0;
    for (int i = 0; i < n; ++i) {
      id = HASH(((intptr_t) px[i] & 0xffffffff) ^ 0, K);
      while (h[id]) {
        if (px[h[id]-1]==px[i]) {
          pans_l[i] = 1;
          pans_i[i] = h[id];
          goto sbl;
        }
        id++; id %= M;
      }
      h[id] = (int) i + 1;
      pans_l[i] = 0;
      pans_i[i] = h[id];
      count++;
      sbl:;
    }
  } break;
  }
  SET_VECTOR_ELT(ans_f, 1, PROTECT(allocVector(INTSXP, count)));
  R_xlen_t ct = 0;
  int *restrict py = INTEGER(PTR_ETL(ans_f, 1));
  for (int i = 0; ct < count; ++i) {
    if (pans_l[i] == 0) {
      py[ct++] = i+1;
    }
  }
  free(h);
  UNPROTECT(4);
  return ans_f;
}

/*
 *  Data.Frame Index
 */

SEXP dupDataFrameIndexR(SEXP x) { // move to matrix if possible
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
  int *h = (int*)calloc(M, sizeof(int));
  SEXP ans_l = PROTECT(allocVector(LGLSXP, len_i));
  SEXP ans_f = PROTECT(allocVector(VECSXP, 2));
  SET_VECTOR_ELT(ans_f, 0, PROTECT(allocVector(INTSXP, len_i)));
  int *restrict pans_l = LOGICAL(ans_l);
  int *restrict pans_i = INTEGER(PTR_ETL(ans_f, 0));
  const int *restrict v = INTEGER(mlv);
  size_t id = 0;
  for (int i = 0; i < len_i; ++i) {
    R_xlen_t key = 0;
    for (int j = 0; j < len_x; ++j) {
      key ^=  HASH(v[i+j*len_i],K)*97;
    }
    id = HASH(key, K);
    while (h[id]) {
      for (R_xlen_t j = 0; j < len_x; ++j) {
        if (v[h[id]-1+j*len_i] != v[i+j*len_i]) {
          goto label1;
        }
      }
      pans_l[i] = 1;
      pans_i[i] = h[id];
      goto label2;
      label1:;
      id++; id %= M;
    }
    h[id] = (int) i + 1;
    pans_l[i] = 0;
    pans_i[i] = h[id];
    count++;
    label2:;
  }
  SET_VECTOR_ELT(ans_f, 1, PROTECT(allocVector(INTSXP, count)));
  R_xlen_t ct = 0;
  int *restrict py = INTEGER(PTR_ETL(ans_f, 1));
  for (int i = 0; ct < count; ++i) {
    if (pans_l[i] == 0) {
      py[ct++] = i+1;
    }
  }
  free(h);
  UNPROTECT(5);
  return ans_f;
}

/*
 *  Matrix Index  (change hash algo)
 */

SEXP dupMatrixIndexR(SEXP x) {
  const R_xlen_t len_x = ncols(x);
  const R_xlen_t len_i = nrows(x);
  const size_t n2 = 2U * (size_t) len_i;
  size_t M = 256;
  int K = 8;
  while (M < n2) {
    M *= 2;
    K++;
  }
  R_xlen_t count = 0;
  int *h = (int*)calloc(M, sizeof(int));
  SEXP ans_l = PROTECT(allocVector(LGLSXP, len_i));
  SEXP ans_f = PROTECT(allocVector(VECSXP, 2));
  SET_VECTOR_ELT(ans_f, 0, PROTECT(allocVector(INTSXP, len_i)));
  int *restrict pans_l = LOGICAL(ans_l);
  int *restrict pans_i = INTEGER(PTR_ETL(ans_f, 0));

  size_t id = 0;
  switch(UTYPEOF(x)) {
  case LGLSXP : {
    const int *restrict px = LOGICAL(x);
    for (R_xlen_t i = 0; i < len_i; ++i) {
      id = 0;
      for (R_xlen_t j = 0; j < len_x; ++j) {
        id += (unsigned)(j+1) * ((px[i+j*len_i] == NA_LOGICAL) ? 2U : (size_t) px[i+j*len_i]);
      }
      while (h[id]) {
        for (R_xlen_t j = 0; j < len_x; ++j) {
          if (px[h[id]-1+j*len_i] != px[i+j*len_i]) {
            goto labelml1;
          }
        }
        pans_l[i] = 1;
        pans_i[i] = h[id];
        goto labelml2;
        labelml1:;
        id++; id %= M;
      }
      h[id] = (int) i + 1;
      pans_l[i] = 0;
      pans_i[i] = h[id];
      count++;
      labelml2:;
    }
  } break;
  case INTSXP : {
    const int *restrict px = INTEGER(x);
    for (R_xlen_t i = 0; i < len_i; ++i) {
      R_xlen_t key = 0;
      for (R_xlen_t j = 0; j < len_x; ++j) {
        key += (j+1) * ((px[i+j*len_i] == NA_INTEGER) ? 0 : px[i+j*len_i]);
      }
      id = HASH(key, K);
      while (h[id] != -1) {
        for (R_xlen_t j = 0; j < len_x; ++j) {
          if (px[h[id]-1+j*len_i] != px[i+j*len_i]) {
            goto labelmi1;
          }
        }
        pans_l[i] = 1;
        pans_i[i] = h[id];
        goto labelmi2;
        labelmi1:;
        id++; id %= M;
      }
      h[id] = (int) i + 1;
      pans_l[i] = 0;
      pans_i[i] = h[id];
      count++;
      labelmi2:;
    }
  } break;
  case REALSXP : {
    const double *restrict px = REAL(x);
    union uno tpv;
    for (R_xlen_t i = 0; i < len_i; ++i) {
      double key = 0;
      for (R_xlen_t j = 0; j < len_x; ++j) {
        //key += (j+1) * (R_IsNA(px[i+j*len_i]) ? NA_REAL : (R_IsNaN((px[i+j*len_i]) ? R_NaN :(px[i+j*len_i]))));
        key += (j+1) * px[i+j*len_i];
      }
      tpv.d = key;
      id = HASH(tpv.u[0] + tpv.u[1], K);
      while (h[id]) {
        for (R_xlen_t j = 0; j < len_x; ++j) {
          //if (!REQUAL(px[h[id]+j*len_i], px[i+j*len_i])) {
          if (px[h[id]-1+j*len_i] != px[i+j*len_i]) {
            goto labelmr1;
          }
        }
        pans_l[i] = 1;
        pans_i[i] = h[id];
        goto labelmr2;
        labelmr1:;
        id++; id %= M;
      }
      h[id] = (int) i + 1;
      pans_l[i] = 0;
      pans_i[i] = h[id];
      count++;
      labelmr2:;
    }
  } break;
  case CPLXSXP : {
    const Rcomplex *restrict px = COMPLEX(x);
    unsigned int u;
    union uno tpv;
    Rcomplex tmp;
    for (R_xlen_t i = 0; i < len_i; ++i) {
      R_xlen_t key = 0;
      for (R_xlen_t j = 0; j < len_x; ++j) {
        tmp.r = (px[i+j*len_i].r == 0.0) ? 0.0 : px[i+j*len_i].r;
        tmp.i = (px[i+j*len_i].i == 0.0) ? 0.0 : px[i+j*len_i].i;
        if (C_IsNA(tmp)) {
          tmp.r = tmp.i = NA_REAL;
        } else if (C_IsNaN(tmp)) {
          tmp.r = tmp.i = R_NaN;
        }
        tpv.d = tmp.r;
        u = tpv.u[0] ^ tpv.u[1];
        tpv.d = tmp.i;
        u ^= tpv.u[0] ^ tpv.u[1];
        key += (j+1) * u;
      }
      id = HASH(key, K);
      while (h[id]) {
        for (R_xlen_t j = 0; j < len_x; ++j) {
          if (!CEQUAL(px[h[id]-1+j*len_i], px[i+j*len_i])) {
            goto labelmc1;
          }
        }
        pans_l[i] = 1;
        pans_i[i] = h[id];
        goto labelmc2;
        labelmc1:;
        id++; id %= M;
      }
      h[id] = (int) i + 1;
      pans_l[i] = 0;
      pans_i[i] = h[id];
      count++;
      labelmc2:;
    }
  } break;
  case STRSXP : {
    const SEXP *restrict px = STRING_PTR(x);
    for (R_xlen_t i = 0; i < len_i; ++i) {
      R_xlen_t key = 0;
      for (R_xlen_t j = 0; j < len_x; ++j) {
        key += (j+1) * ((intptr_t) px[i+j*len_i] & 0xffffffff) ^ 0;
      }
      id = HASH(key, K);
      while (h[id]) {
        for (R_xlen_t j = 0; j < len_x; ++j) {
          if (px[h[id]-1+j*len_i] != px[i+j*len_i]) {
            goto labelms1;
          }
        }
        pans_l[i] = 1;
        pans_i[i] = h[id]+1;
        goto labelms2;
        labelms1:;
        id++; id %= M;
      }
      h[id] = (int) i + 1;
      pans_l[i] = 0;
      pans_i[i] = h[id];
      count++;
      labelms2:;
    }
  } break;
  default:
    error("Matrix of type %s are not supported.", type2char(UTYPEOF(x)));
  }
  SET_VECTOR_ELT(ans_f, 1, PROTECT(allocVector(INTSXP, count)));
  R_xlen_t ct = 0;
  int *restrict py = INTEGER(PTR_ETL(ans_f, 1));
  for (int i = 0; ct < count; ++i) {
    if (pans_l[i] == 0) {
      py[ct++] = i+1;
    }
  }
  free(h);
  UNPROTECT(4);
  return ans_f;
}

/*
 * Vector Index Only
 */

SEXP dupVecIndexOnlyR(SEXP x) {
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
  int *h = (int*)calloc(M, sizeof(int));
  SEXP ans_i = PROTECT(allocVector(INTSXP, n));
  int *restrict pans_i = INTEGER(ans_i);
  switch (tx) {
  case LGLSXP: {
    const int *restrict px = LOGICAL(x);
    size_t id = 0;
    for (int i = 0; i < n; ++i) {
      id = (px[i] == NA_LOGICAL) ? 2U : (size_t) px[i];
      while (h[id]) {
        if (px[h[id]-1]==px[i]) {
          pans_i[i] = h[id];
          goto lbl;
        }
        id++; id %= M;
      }
      h[id] = (int) i + 1;
      pans_i[i] = h[id];
      count++;
      lbl:;
    }
  } break;
  case INTSXP: { // think about factor and levels number
    const int *restrict px = INTEGER(x);
    size_t id = 0;
    for (int i = 0; i < n; ++i) {
      id = (px[i] == NA_INTEGER) ? 0 : HASH(px[i], K);
      while (h[id]) {
        if (px[h[id]-1]==px[i]) {
          pans_i[i] = h[id];
          goto ibl;
        }
        id++; id %= M;
      }
      h[id] = (int) i + 1;
      pans_i[i] = h[id];
      count++;
      ibl:;
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
        if (REQUAL(px[h[id]-1], px[i])) {
          pans_i[i] = h[id];
          goto rbl;
        }
        id++; id %= M;
      }
      h[id] = (int) i + 1;
      pans_i[i] = h[id];
      count++;
      rbl:;
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
          pans_i[i] = h[id];
          goto cbl;
        }
        id++; id %= M;
      }
      h[id] = (int) i + 1;
      pans_i[i] = h[id];
      count++;
      cbl:;
    }
  } break;
  case STRSXP: {
    const SEXP *restrict px = STRING_PTR(x);
    size_t id = 0;
    for (int i = 0; i < n; ++i) {
      id = HASH(((intptr_t) px[i] & 0xffffffff) ^ 0, K);
      while (h[id]) {
        if (px[h[id]-1]==px[i]) {
          pans_i[i] = h[id];
          goto sbl;
        }
        id++; id %= M;
      }
      h[id] = (int) i + 1;
      pans_i[i] = h[id];
      count++;
      sbl:;
    }
  } break;
  }
  free(h);
  UNPROTECT(1);
  return ans_i;
}

/*
 * Vector Index and count
 */

SEXP dupVecIndexCountR(SEXP x) {
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
  int *h = (int*)calloc(M, sizeof(int));
  SEXP ans_l = PROTECT(allocVector(LGLSXP, n)); // check if needed if not can be moved to calloc + speed up
  SEXP ans_ct = PROTECT(allocVector(INTSXP, n));
  SEXP ans_f = PROTECT(allocVector(VECSXP, 3));
  SET_VECTOR_ELT(ans_f, 0, PROTECT(allocVector(INTSXP, n)));
  int *restrict pans_l = LOGICAL(ans_l);
  int *restrict pans_ct = INTEGER(ans_ct);
  int *restrict pans_i = INTEGER(PTR_ETL(ans_f, 0));
  switch (tx) {
  case LGLSXP: {
    const int *restrict px = LOGICAL(x);
    size_t id = 0;
    for (int i = 0; i < n; ++i) {
      id = (px[i] == NA_LOGICAL) ? 2U : (size_t) px[i];
      while (h[id]) {
        if (px[h[id]-1]==px[i]) {
          pans_l[i] = 1;
          pans_i[i] = h[id];
          pans_ct[h[id]-1]++;
          goto lbl;
        }
        id++; id %= M;
      }
      h[id] = (int) i + 1;
      pans_l[i] = 0;
      pans_i[i] = h[id];
      pans_ct[i] = 1;
      count++;
      lbl:;
    }
  } break;
  case INTSXP: { // think about factor and levels number
    const int *restrict px = INTEGER(x);
    size_t id = 0;
    for (int i = 0; i < n; ++i) {
      id = (px[i] == NA_INTEGER) ? 0 : HASH(px[i], K);
      while (h[id]) {
        if (px[h[id]-1]==px[i]) {
          pans_l[i] = 1;
          pans_i[i] = h[id];
          pans_ct[h[id]-1]++;
          goto ibl;
        }
        id++; id %= M;
      }
      h[id] = (int) i + 1;
      pans_l[i] = 0;
      pans_i[i] = h[id];
      pans_ct[i] = 1;
      count++;
      ibl:;
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
        if (REQUAL(px[h[id]-1], px[i])) {
          pans_l[i] = 1;
          pans_i[i] = h[id];
          pans_ct[h[id]-1]++;
          goto rbl;
        }
        id++; id %= M;
      }
      h[id] = (int) i + 1;
      pans_l[i] = 0;
      pans_i[i] = h[id];
      pans_ct[i] = 1;
      count++;
      rbl:;
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
          pans_l[i] = 1;
          pans_i[i] = h[id];
          pans_ct[h[id]-1]++;
          goto cbl;
        }
        id++; id %= M;
      }
      h[id] = (int) i + 1;
      pans_l[i] = 0;
      pans_i[i] = h[id];
      pans_ct[i] = 1;
      count++;
      cbl:;
    }
  } break;
  case STRSXP: {
    const SEXP *restrict px = STRING_PTR(x);
    size_t id = 0;
    for (int i = 0; i < n; ++i) {
      id = HASH(((intptr_t) px[i] & 0xffffffff) ^ 0, K);
      while (h[id]) {
        if (px[h[id]-1]==px[i]) {
          pans_l[i] = 1;
          pans_i[i] = h[id];
          pans_ct[h[id]-1]++;
          goto sbl;
        }
        id++; id %= M;
      }
      h[id] = (int) i + 1;
      pans_l[i] = 0;
      pans_i[i] = h[id];
      pans_ct[i] = 1;
      count++;
      sbl:;
    }
  } break;
  }
  SET_VECTOR_ELT(ans_f, 1, PROTECT(allocVector(INTSXP, count)));
  SET_VECTOR_ELT(ans_f, 2, PROTECT(allocVector(INTSXP, count)));
  R_xlen_t ct = 0;
  int *restrict py = INTEGER(PTR_ETL(ans_f, 1));
  int *restrict pw = INTEGER(PTR_ETL(ans_f, 2));
  for (int i = 0; ct < count; ++i) {
    if (pans_l[i] == 0) {
      pw[ct] = pans_ct[i];
      py[ct++] = i+1;
    }
  }
  free(h);
  UNPROTECT(6);
  return ans_f;
}

/*
 *  Data.Frame Index and Count
 */

SEXP dupDataFrameIndexCountR(SEXP x) { // move to matrix if possible
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
  int *h = (int*)calloc(M, sizeof(int));
  SEXP ans_l = PROTECT(allocVector(LGLSXP, len_i)); // check if needed if not can be moved to calloc + speed up
  SEXP ans_ct = PROTECT(allocVector(INTSXP, len_i));
  SEXP ans_f = PROTECT(allocVector(VECSXP, 3));
  SET_VECTOR_ELT(ans_f, 0, PROTECT(allocVector(INTSXP, len_i)));
  int *restrict pans_l = LOGICAL(ans_l);
  int *restrict pans_ct = INTEGER(ans_ct);
  int *restrict pans_i = INTEGER(PTR_ETL(ans_f, 0));
  const int *restrict v = INTEGER(mlv);
  size_t id = 0;
  for (R_xlen_t i = 0; i < len_i; ++i) {
    R_xlen_t key = 0;
    for (R_xlen_t j = 0; j < len_x; ++j) {
      key ^=  HASH(v[i+j*len_i],K)*97;
    }
    id = HASH(key, K);
    while (h[id]) {
      for (R_xlen_t j = 0; j < len_x; ++j) {
        if (v[h[id]-1+j*len_i] != v[i+j*len_i]) {
          goto label1;
        }
      }
      pans_l[i] = 1;
      pans_i[i] = h[id];
      pans_ct[h[id]-1]++;
      goto label2;
      label1:;
      id++; id %= M;
    }
    h[id] = (int) i + 1;
    pans_l[i] = 0;
    pans_i[i] = (int) h[id];
    pans_ct[i] = 1;
    count++;
    label2:;
  }
  SET_VECTOR_ELT(ans_f, 1, PROTECT(allocVector(INTSXP, count)));
  SET_VECTOR_ELT(ans_f, 2, PROTECT(allocVector(INTSXP, count)));
  R_xlen_t ct = 0;
  int *restrict py = INTEGER(PTR_ETL(ans_f, 1));
  int *restrict pw = INTEGER(PTR_ETL(ans_f, 2));
  for (int i = 0; ct < count; ++i) {
    if (pans_l[i] == 0) {
      pw[ct] = pans_ct[i];
      py[ct++] = i+1;
    }
  }
  free(h);
  UNPROTECT(7);
  return ans_f;
}

/*
 * Grouping element function (Vector)
 */

SEXP groupEltVectR(SEXP x) {
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
  int *h = (int*)calloc(M, sizeof(int));
  int *pans_l = (int*)calloc(n, sizeof(int));
  SEXP ans_ct = PROTECT(allocVector(INTSXP, n));
  SEXP ans_i = PROTECT(allocVector(INTSXP, n));
  int *restrict pans_ct = INTEGER(ans_ct);
  int *restrict pans_i = INTEGER(ans_i);
  switch (tx) {
  case LGLSXP: {
    const int *restrict px = LOGICAL(x);
    size_t id = 0;
    for (int i = 0; i < n; ++i) {
      id = (px[i] == NA_LOGICAL) ? 2U : (size_t) px[i];
      while (h[id]) {
        if (px[h[id]-1]==px[i]) {
          pans_l[i] = 1;
          pans_i[i] = pans_i[h[id]-1];
          pans_ct[h[id]-1]++;
          goto lbl;
        }
        id++; id %= M;
      }
      h[id] = (int) i + 1;
      pans_i[i] = count;
      pans_ct[i] = 1;
      count++;
      lbl:;
    }
  } break;
  case INTSXP: { // think about factor and levels number
    const int *restrict px = INTEGER(x);
    size_t id = 0;
    for (int i = 0; i < n; ++i) {
      id = (px[i] == NA_INTEGER) ? 0 : HASH(px[i], K);
      while (h[id]) {
        if (px[h[id]-1]==px[i]) {
          pans_l[i] = 1;
          pans_i[i] = pans_i[h[id]-1];
          pans_ct[h[id]-1]++;
          goto ibl;
        }
        id++; id %= M;
      }
      h[id] = (int) i + 1;
      pans_i[i] = count;
      pans_ct[i] = 1;
      count++;
      ibl:;
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
        if (REQUAL(px[h[id]-1], px[i])) {
          pans_l[i] = 1;
          pans_i[i] = pans_i[h[id]-1];
          pans_ct[h[id]-1]++;
          goto rbl;
        }
        id++; id %= M;
      }
      h[id] = (int) i + 1;
      pans_i[i] = count;
      pans_ct[i] = 1;
      count++;
      rbl:;
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
          pans_l[i] = 1;
          pans_i[i] = pans_i[h[id]-1];
          pans_ct[h[id]-1]++;
          goto cbl;
        }
        id++; id %= M;
      }
      h[id] = (int) i + 1;
      pans_i[i] = count;
      pans_ct[i] = 1;
      count++;
      cbl:;
    }
  } break;
  case STRSXP: {
    const SEXP *restrict px = STRING_PTR(x);
    size_t id = 0;
    for (int i = 0; i < n; ++i) {
      id = HASH(((intptr_t) px[i] & 0xffffffff) ^ 0, K);
      while (h[id]) {
        if (px[h[id]-1]==px[i]) {
          pans_l[i] = 1;
          pans_i[i] = pans_i[h[id]-1];
          pans_ct[h[id]-1]++;
          goto sbl;
        }
        id++; id %= M;
      }
      h[id] = (int) i + 1;
      pans_i[i] = count;
      pans_ct[i] = 1;
      count++;
      sbl:;
    }
  } break;
  }
  SEXP ans = PROTECT(allocVector(INTSXP, n));
  SEXP ans_uniq_ct = PROTECT(allocVector(INTSXP, count));
  SEXP ans_sp = PROTECT(allocVector(INTSXP, count));
  int *restrict pans = INTEGER(ans);
  int *restrict pw = INTEGER(ans_uniq_ct);
  int *restrict ps = INTEGER(ans_sp);
  R_xlen_t ct = 0;
  for (R_xlen_t i = 0; ct < count; ++i) {
   if (pans_l[i] == 0) {
     pw[ct++] = pans_ct[i];
   }
  }
  ps[0] = 0;
  for (R_xlen_t i = 1; i < count; ++i) {
    ps[i] = pw[i-1] + ps[i-1];
  }
  for (R_xlen_t i = 0; i < n; ++i) {
    pans[ps[pans_i[i]]++] = i + 1;
  }
  free(h);
  free(pans_l);
  UNPROTECT(5);
  return ans;
}

/*
 * Grouping element function (data.frame)
 */

SEXP groupEltDataFrameR(SEXP x) { // move to matrix if possible
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
  int *h = (int*)calloc(M, sizeof(int));
  int *pans_l = (int*)calloc(len_i, sizeof(int));
  SEXP ans_ct = PROTECT(allocVector(INTSXP, len_i));
  SEXP ans_i = PROTECT(allocVector(INTSXP, len_i));
  int *restrict pans_ct = INTEGER(ans_ct);
  int *restrict pans_i = INTEGER(ans_i);
  const int *restrict v = INTEGER(mlv);
  size_t id = 0;
  for (R_xlen_t i = 0; i < len_i; ++i) {
    R_xlen_t key = 0;
    for (R_xlen_t j = 0; j < len_x; ++j) {
      key ^=  HASH(v[i+j*len_i],K)*97;
    }
    id = HASH(key, K);
    while (h[id]) {
      for (R_xlen_t j = 0; j < len_x; ++j) {
        if (v[h[id]-1+j*len_i] != v[i+j*len_i]) {
          goto label1;
        }
      }
      pans_l[i] = 1;
      pans_i[i] = pans_i[h[id]-1];
      pans_ct[h[id]-1]++;
      goto label2;
      label1:;
      id++; id %= M;
    }
    h[id] = (int) i + 1;
    pans_i[i] = count;
    pans_ct[i] = 1;
    count++;
    label2:;
  }
  SEXP ans = PROTECT(allocVector(INTSXP, len_i));
  SEXP ans_uniq_ct = PROTECT(allocVector(INTSXP, count));
  SEXP ans_sp = PROTECT(allocVector(INTSXP, count));
  int *restrict pans = INTEGER(ans);
  int *restrict pw = INTEGER(ans_uniq_ct);
  int *restrict ps = INTEGER(ans_sp);
  R_xlen_t ct = 0;
  for (R_xlen_t i = 0; ct < count; ++i) {
   if (pans_l[i] == 0) {
     pw[ct++] = pans_ct[i];
   }
  }
  ps[0] = 0;
  for (R_xlen_t i = 1; i < count; ++i) {
    ps[i] = pw[i-1] + ps[i-1];
  }
  for (R_xlen_t i = 0; i < len_i; ++i) {
    pans[ps[pans_i[i]]++] = i + 1;
  }
  free(h);
  free(pans_l);
  UNPROTECT(6);
  return ans;
}
