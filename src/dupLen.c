/*
 * kit : Useful R Functions Implemented in C
 * Copyright (C) 2020-2025  Morgan Jacob
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

SEXP dupLenR(SEXP x) {
  if (isDataFrame(x)) {
    SEXP ans = PROTECT(dupLenDataFrameR(x));
    UNPROTECT(1);
    return ans;
  }
  if (isMatrix(x)) {
    SEXP ans = PROTECT(dupLenMatrixR(x));
    UNPROTECT(1);
    return ans;
  }
  if (isArray(x)) {
    error("Arrays are not yet supported. (please raise a feature request if needed)");
  }
  SEXP ans = PROTECT(dupLenVecR(x));
  UNPROTECT(1);
  return ans;
}

/*
 *  Data.Frame
 */

SEXP dupLenDataFrameR(SEXP x) {
  const SEXP *restrict px = SEXPPTR_RO(x);
  const R_xlen_t len_x = xlength(x);
  bool allT = true;
  const SEXPTYPE t0 = UTYPEOF(px[0]); 
  for (int i = 1; i < len_x; ++i) {
    if (UTYPEOF(px[i]) != t0) {
      allT = false;
      break;
    }
  }
  if (allT) {
    SEXP output = PROTECT(dupLenMatrixR(PROTECT(dfToMatrix(x))));
    UNPROTECT(2);
    return output;
  }
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
  int *restrict h = (int*) R_Calloc(M, int);
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
      goto label2;
      label1:;
      id++; id %= M;
    }
    h[id] = (int) i + 1;
    count++;
    label2:;
  }
  R_Free(h);
  UNPROTECT(1);
  return ScalarInteger(count);
}

/*
 *  Matrix
 */

SEXP dupLenMatrixR(SEXP x) {
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
  int *restrict h = (int*) R_Calloc(M, int);
  size_t id = 0;
  switch(UTYPEOF(x)) {
  case LGLSXP : {
    const int *restrict px = LOGICAL(x);
    for (R_xlen_t i = 0; i < len_i; ++i) {
      id = 0;
      for (R_xlen_t j = 0; j < len_x; ++j) {
        id ^= ((unsigned)(j+1) * ((px[i+j*len_i] == NA_LOGICAL) ? 2U : (size_t) px[i+j*len_i]))*97*(j+1);
      }
      id = HASH(id, K);
      while (h[id]) {
        for (R_xlen_t j = 0; j < len_x; ++j) {
          if (px[h[id]-1+j*len_i] != px[i+j*len_i]) {
            goto labelml1; // # nocov
          }
        }
        goto labelml2;
        labelml1:;// # nocov
        id++; id %= M; // # nocov
      }
      h[id] = (int) i + 1;
      count++;
      labelml2:;
    }
  } break;
  case INTSXP : {
    const int *restrict px = INTEGER(x);
    for (R_xlen_t i = 0; i < len_i; ++i) {
      R_xlen_t key = 0;
      for (R_xlen_t j = 0; j < len_x; ++j) {
        key ^= HASH(((px[i+j*len_i] == NA_INTEGER) ? 0 : px[i+j*len_i]),K)*97*(j+1);
      }
      id = HASH(key, K);
      while (h[id]) {
        for (R_xlen_t j = 0; j < len_x; ++j) {
          if (px[h[id]-1+j*len_i] != px[i+j*len_i]) {
            goto labelmi1; // # nocov
          }
        }
        goto labelmi2;
        labelmi1:;
        id++; id %= M; // # nocov
      }
      h[id] = (int) i + 1;
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
        key ^= HASH(tpv.u[0] + tpv.u[1],K)*97*(j+1);
      }
      tpv.d = key;
      id = HASH(tpv.u[0] + tpv.u[1], K);
      while (h[id]) {
        for (R_xlen_t j = 0; j < len_x; ++j) {
          if (!REQUAL(px[h[id]-1+j*len_i], px[i+j*len_i])) {
            goto labelmr1;
          }
        }
        goto labelmr2;
        labelmr1:;
        id++; id %= M;
      }
      h[id] = (int) i + 1;
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
        key ^= HASH(u, K)*97*(j+1);
      }
      id = HASH(key, K);
      while (h[id]) {
        for (R_xlen_t j = 0; j < len_x; ++j) {
          if (!CEQUAL(px[h[id]-1+j*len_i], px[i+j*len_i])) {
            goto labelmc1;
          }
        }
        goto labelmc2;
        labelmc1:;
        id++; id %= M;
      }
      h[id] = (int) i + 1;
      count++;
      labelmc2:;
    }
  } break;
  case STRSXP : {
    const SEXP *restrict px = STRING_PTR_RO(x);
    for (R_xlen_t i = 0; i < len_i; ++i) {
      R_xlen_t key = 0;
      for (R_xlen_t j = 0; j < len_x; ++j) {
        key ^= HASH(((intptr_t) px[i+j*len_i] & 0xffffffff),K)*97*(j+1);
      }
      id = HASH(key, K);
      while (h[id]) {
        for (R_xlen_t j = 0; j < len_x; ++j) {
          if (px[h[id]-1+j*len_i] != px[i+j*len_i]) {
            goto labelms1; // # nocov
          }
        }
        goto labelms2;
        labelms1:; // # nocov
        id++; id %= M; // # nocov
      }
      h[id] = (int) i + 1;
      count++;
      labelms2:;
    }
  } break;
  default: {
    R_Free(h);
    error("Matrix of type %s are not supported.", type2char(UTYPEOF(x)));
  }
  }
  R_Free(h);
  return ScalarInteger(count);
}

/*
 *  Vector
 */

SEXP dupLenVecR(SEXP x) {
  if (isFactor(x)) {
    const int len = LENGTH(PROTECT(getAttrib(x, R_LevelsSymbol)));
    UNPROTECT(1);
    bool *restrict count = (bool*)R_Calloc(len+1,bool);
    const int *restrict px = INTEGER(x);
    const int xlen = LENGTH(x);
    int j = 0;
    for (int i = 0; i < xlen; ++i) {
      if (!count[px[i]]) {
        j++;
        if (j == len)
          break;
        count[px[i]] = true;
      }
    }
    R_Free(count);
    return ScalarInteger(j);
  }
  if (isLogical(x)) {
    bool *restrict count = (bool*)R_Calloc(3,bool);
    const int *restrict px = LOGICAL(x);
    const int xlen = LENGTH(x);
    int j = 0;
    for (int i = 0; i < xlen; ++i) {
      const int cs = px[i] == NA_LOGICAL ? 2 : px[i];
      if (!count[cs]) {
        j++;
        if (j == 3)
          break;
        count[cs] = true;
      }
    }
    R_Free(count);
    return ScalarInteger(j);
  }
  const R_xlen_t n = xlength(x);
  const SEXPTYPE tx = UTYPEOF(x);
  int K;
  size_t M;
  if (tx == INTSXP || tx == STRSXP || tx == REALSXP || tx == CPLXSXP ) {
    if(n >= 1073741824) {
      error("Length of 'x' is too large. (Long vector not supported yet)"); // # nocov
    }
    const size_t n2 = 2U * (size_t) n;
    M = 256;
    K = 8;
    while (M < n2) {
      M *= 2;
      K++;
    }
  } else {
    error("Type %s is not supported.", type2char(tx)); // # nocov
  }
  R_xlen_t count = 0;
  int *restrict h = (int*)R_Calloc(M, int);
  switch (tx) {
  case INTSXP: {
    const int *restrict px = INTEGER(x);
    size_t id = 0;
    for (int i = 0; i < n; ++i) {
      id = (px[i] == NA_INTEGER) ? 0 : HASH(px[i], K);
      while (h[id]) {
        if (px[h[id]-1]==px[i]) {
          goto ibl;
        }
        id++; id %= M; // # nocov
      }
      h[id] = (int) i + 1;
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
          goto rbl;
        }
        id++; id %= M;
      }
      h[id] = (int) i + 1;
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
        if (CEQUAL(px[h[id] - 1],px[i])) {
          goto cbl;
        }
        id++; id %= M;
      }
      h[id] = (int) i + 1;
      count++;
      cbl:;
    }
  } break;
  case STRSXP: {
    const SEXP *restrict px = STRING_PTR_RO(x);
    size_t id = 0;
    for (int i = 0; i < n; ++i) {
      id = HASH(((intptr_t) px[i] & 0xffffffff), K);
      while (h[id]) {
        if (px[h[id] - 1]==px[i]) {
          goto sbl;
        }
        id++; id %= M;
      }
      h[id] = (int) i + 1;
      count++;
      sbl:;
    }
  } break;
  }
  R_Free(h);
  return ScalarInteger(count);
}
