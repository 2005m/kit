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

SEXP dupR(SEXP x, SEXP uniq, SEXP ridx, SEXP pos) { // add matrix support?
  if (isFrame(x)) {
    const SEXP *restrict px = SEXPPTR_RO(x);
    const R_xlen_t len_x = xlength(x);
    const R_xlen_t len_i = xlength(px[0]);
    SEXP ans = PROTECT(allocVector(LGLSXP, len_i));
    SEXP mlv = PROTECT(allocMatrix(INTSXP, (int)len_i, (int)len_x));
    for (R_xlen_t i = 0; i < len_x; ++i) {
      memcpy(INTEGER(mlv)+i*len_i, INTEGER(PROTECT(dupR(px[i], SEXP_F, SEXP_T, SEXP_F))), (unsigned)len_i*sizeof(int));
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
    SEXP TBL = PROTECT(allocVector(INTSXP, (R_xlen_t) M));
    int *restrict h = INTEGER(TBL);
    for (size_t i = 0; i < M; ++i) {
      h[i] = -1;
    }
    const int *restrict v = INTEGER(mlv);
    int *restrict pans = LOGICAL(ans);
    size_t id = 0;
    for (R_xlen_t i = 0; i < len_i; ++i) {
      R_xlen_t key = 0;
      for (R_xlen_t j = 0; j < len_x; ++j) {
        key += (j+1) * v[i+j*len_i];
      }
      id = HASH(key, K);
      while (h[id] != -1) {
        for (R_xlen_t j = 0; j < len_x; ++j) {
          if (v[h[id]+j*len_i] != v[i+j*len_i]) {
            goto label1;
          }
        }
        pans[i] = 1; goto label2;
        label1:;
        id++; id %= M;
      }
      h[id] = (int) i;
      pans[i] = 0;
      count++;
      label2:;
    }
    if (asLogical(uniq)) { // add duplicated index, will need it down the road
      UNPROTECT(2);
      SEXP indx = PROTECT(allocVector(INTSXP, count));
      int ct = 0;
      int *restrict py = INTEGER(indx);
      for (int i = 0; i < len_i; ++i) {
        if (pans[i] == 0) {
          py[ct++] = i;
        }
      }
      SEXP output = PROTECT(subSetRow(x, indx));
      UNPROTECT(3);
      return output; //UNPROTECT(2); //return indx;
    }
    UNPROTECT(3);
    return ans;
  }
  if (isMatrix(x) || isArray(x)) {
    error("Matrix and Array are not yet supported. (Maybe for the next release)");
  }
  const R_xlen_t n = xlength(x);
  const SEXPTYPE tx = UTYPEOF(x);
  const bool cridx = asLogical(ridx);
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
  SEXP TBL = PROTECT(allocVector(INTSXP, (R_xlen_t) M));
  int *restrict h = INTEGER(TBL);
  for (size_t i = 0; i < M; ++i) {
    h[i] = -1;
  }
  SEXP ans = PROTECT(allocVector(cridx ? INTSXP : LGLSXP, n));
  int *restrict pans = cridx ? INTEGER(ans) : LOGICAL(ans);
  switch (tx) {
  case LGLSXP: {
    const int *restrict px = LOGICAL(x);
    size_t id = 0;
    for (int i = 0; i < n; ++i) {
      id = (px[i] == NA_LOGICAL) ? 2U : (size_t) px[i];
      while (h[id] != -1) {
        if (px[h[id]]==px[i]) {
          pans[i] = cridx ? h[id]+1 : 1;
          goto lbl;
        }
        id++; id %= M;
      }
      h[id] = (int) i;
      pans[i] = cridx ? i+1 : 0;
      count++;
      lbl:;
    }
    if (asLogical(uniq)) {
      if (asLogical(pos)) {
        SEXP indx = PROTECT(allocVector(INTSXP, count));
        size_t ct = 0;
        int *restrict py = INTEGER(indx);
        for (int i = 0; i < n; ++i) {
          if (pans[i] == 0) {
            py[ct++] = i+1;
          }
        }
        UNPROTECT(3);
        return indx;
      } else {
        SEXP indx = PROTECT(allocVector(tx, count));
        size_t ct = 0;
        int *restrict py = LOGICAL(indx);
        for (int i = 0; i < n; ++i) {
          if (pans[i] == 0) {
            py[ct++] = px[i];
          }
        }
        UNPROTECT(3);
        return indx;
      }
    }
  } break;
  case INTSXP: { // think about factor and levels number
    const int *restrict px = INTEGER(x);
    size_t id = 0;
    for (int i = 0; i < n; ++i) {
      id = (px[i] == NA_INTEGER) ? 0 : HASH(px[i], K);
      while (h[id] != -1) {
        if (px[h[id]]==px[i]) {
          pans[i] = cridx ? h[id]+1 : 1;
          goto ibl;
        }
        id++; id %= M;
      }
      h[id] = (int) i;
      pans[i] = cridx ? i+1 : 0;
      count++;
      ibl:;
    }
    if (asLogical(uniq)) {
      if (asLogical(pos)) {
        SEXP indx = PROTECT(allocVector(INTSXP, count));
        size_t ct = 0;
        int *restrict py = INTEGER(indx);
        for (int i = 0; i < n; ++i) {
          if (pans[i] == 0) {
            py[ct++] = i+1;
          }
        }
        UNPROTECT(3);
        return indx;
      } else {
        SEXP indx = PROTECT(allocVector(tx, count));
        size_t ct = 0;
        int *restrict py = INTEGER(indx);
        for (R_xlen_t i = 0; i < n; ++i) {
          if (pans[i] == 0) {
            py[ct++] = px[i];
          }
        }
        copyMostAttrib(x, indx);
        UNPROTECT(3);
        return indx;
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
      while (h[id] != -1) {
        if (REQUAL(px[h[id]], px[i])) {
          pans[i] = cridx ? h[id]+1 : 1;
          goto rbl;
        }
        id++; id %= M;
      }
      h[id] = (int) i;
      pans[i] = cridx ? i+1 : 0;
      count++;
      rbl:;
    }
    if (asLogical(uniq)) {
      if (asLogical(pos)) {
        SEXP indx = PROTECT(allocVector(INTSXP, count));
        size_t ct = 0;
        int *restrict py = INTEGER(indx);
        for (int i = 0; i < n; ++i) {
          if (pans[i] == 0) {
            py[ct++] = i+1;
          }
        }
        UNPROTECT(3);
        return indx;
      } else {
        SEXP indx = PROTECT(allocVector(tx, count));
        size_t ct = 0;
        double *restrict py = REAL(indx);
        for (R_xlen_t i = 0; i < n; ++i) {
          if (pans[i] == 0) {
            py[ct++] = px[i];
          }
        }
        copyMostAttrib(x, indx);
        UNPROTECT(3);
        return indx;
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
      while (h[id] != -1) {
        if (CEQUAL(px[h[id]],px[i])) {
          pans[i] = cridx ? h[id]+1 : 1;
          goto cbl;
        }
        id++; id %= M;
      }
      h[id] = (int) i;
      pans[i] = cridx ? i+1 : 0;
      count++;
      cbl:;
    }
    if (asLogical(uniq)) {
      if (asLogical(pos)) {
        SEXP indx = PROTECT(allocVector(INTSXP, count));
        size_t ct = 0;
        int *restrict py = INTEGER(indx);
        for (int i = 0; i < n; ++i) {
          if (pans[i] == 0) {
            py[ct++] = i+1;
          }
        }
        UNPROTECT(3);
        return indx;
      } else {
        SEXP indx = PROTECT(allocVector(tx, count));
        size_t ct = 0;
        Rcomplex *restrict py = COMPLEX(indx);
        for (int i = 0; i < n; ++i) {
          if (pans[i] == 0) {
            py[ct++] = px[i];
          }
        }
        UNPROTECT(3);
        return indx;
      }
    }
  } break;
  case STRSXP: {
    const SEXP *restrict px = STRING_PTR(x);
    size_t id = 0;
    for (int i = 0; i < n; ++i) {
      id = HASH(((intptr_t) px[i] & 0xffffffff) ^ 0, K);
      while (h[id] != -1) {
        if (px[h[id]]==px[i]) {
          pans[i] = cridx ? h[id]+1 : 1;
          goto sbl;
        }
        id++; id %= M;
      }
      h[id] = (int) i;
      pans[i] = cridx ? i+1 : 0;
      count++;
      sbl:;
    }
    if (asLogical(uniq)) {
      if (asLogical(pos)) {
        SEXP indx = PROTECT(allocVector(INTSXP, count));
        size_t ct = 0;
        int *restrict py = INTEGER(indx);
        for (int i = 0; i < n; ++i) {
          if (pans[i] == 0) {
            py[ct++] = i+1;
          }
        }
        UNPROTECT(3);
        return indx;
      } else {
        SEXP indx = PROTECT(allocVector(tx, count));
        R_xlen_t ct = 0;
        for (int i = 0; i < n; ++i) {
          if (pans[i] == 0) {
            SET_STRING_ELT(indx,ct++, px[i]);
          }
        }
        UNPROTECT(3);
        return indx;
      }
    }
  } break;
  }
  UNPROTECT(2);
  return ans;
}
