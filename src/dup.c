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

/*
 *  Main Function
 */

SEXP dupR(SEXP x, SEXP uniq, SEXP fromLast) {
  if (isFrame(x)) {
    SEXP ans = PROTECT(dupDataFrameR(x, uniq, fromLast));
    UNPROTECT(1);
    return ans;
  }
  if (isMatrix(x)) {
    SEXP ans = PROTECT(dupMatrixR(x, uniq, FALSE, fromLast));
    UNPROTECT(1);
    return ans;
  }
  if (isArray(x)) {
    error("Arrays are not yet supported. (please raise a feature request if needed)");
  }
  SEXP ans = PROTECT(dupVecR(x, uniq, fromLast));
  UNPROTECT(1);
  return ans;
}

/*
 *  Data.Frame
 */

SEXP dupDataFrameR(SEXP x, SEXP uniq, SEXP fromLast) { // move to matrix if possible
  if(!IS_BOOL(fromLast)) {
    error("Argument 'fromLast' must be TRUE or FALSE and length 1.");
  }
  const bool pfromLast = asLogical(fromLast);
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
  const bool buniq = asLogical(uniq);
  if (allT) {
    SEXP output = buniq ? PROTECT(subSetRowDataFrame(x, PROTECT(dupMatrixR(PROTECT(dfToMatrix(x)), uniq, TRUE, fromLast)))) : PROTECT(dupMatrixR(PROTECT(dfToMatrix(x)), uniq, FALSE, fromLast));
    UNPROTECT(buniq ? 3 : 2);
    return output;
  }
  const R_xlen_t len_i = xlength(px[0]);
  SEXP ans = buniq ? R_NilValue: PROTECT(allocVector(LGLSXP, len_i));
  SEXP mlv = PROTECT(allocMatrix(INTSXP, (int)len_i, (int)len_x));
  for (R_xlen_t i = 0; i < len_x; ++i) {
    memcpy(INTEGER(mlv)+i*len_i, INTEGER(PROTECT(dupVecIndexOnlyR(px[i], fromLast))), (unsigned)len_i*sizeof(int));
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
  int *restrict pans = buniq ? (int*) calloc(len_i, sizeof(int)) : LOGICAL(ans);
  size_t id = 0;
  if (buniq) {
    if (pfromLast) {
      for (R_xlen_t i = len_i-1; i > -1; --i) {
        R_xlen_t key = 0;
        for (R_xlen_t j = 0; j < len_x; ++j) {
          key ^= HASH(v[i+j*len_i],K)*97*(j+1);
        }
        id = HASH(key, K);
        while (h[id]) {
          for (R_xlen_t j = 0; j < len_x; ++j) {
            if (v[h[id]-1+j*len_i] != v[i+j*len_i]) {
              goto label1t;
            }
          }
          goto label2t;
          label1t:;
          id++; id %= M;
        }
        h[id] = (int) i + 1;
        pans[i]++;
        count++;
        label2t:;
      }
    } else {
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
        pans[i]++;
        count++;
        label2:;
      }
    }
    free(h);
    UNPROTECT(1);
    SEXP indx = PROTECT(allocVector(INTSXP, count));
    int ct = 0;
    int *restrict py = INTEGER(indx);
    for (int i = 0; ct < count; ++i) {
      if (pans[i]) {
        py[ct++] = i;
      }
    }
    SEXP output = PROTECT(subSetRowDataFrame(x, indx));
    free(pans);
    UNPROTECT(2);
    return output;
  }
  if (pfromLast) {
    for (R_xlen_t i = len_i-1; i > -1; --i) {
      R_xlen_t key = 0;
      for (R_xlen_t j = 0; j < len_x; ++j) {
        key ^= HASH(v[i+j*len_i],K)*97*(j+1);
      }
      id = HASH(key, K);
      while (h[id]) {
        for (R_xlen_t j = 0; j < len_x; ++j) {
          if (v[h[id]-1+j*len_i] != v[i+j*len_i]) {
            goto label1bt;
          }
        }
        pans[i] = 1;
        goto label2bt;
        label1bt:;
        id++; id %= M;
      }
      h[id] = (int) i + 1;
      pans[i] = 0;
      count++;
      label2bt:;
    }
  } else {
    for (R_xlen_t i = 0; i < len_i; ++i) {
      R_xlen_t key = 0;
      for (R_xlen_t j = 0; j < len_x; ++j) {
        key ^= HASH(v[i+j*len_i],K)*97*(j+1);
      }
      id = HASH(key, K);
      while (h[id]) {
        for (R_xlen_t j = 0; j < len_x; ++j) {
          if (v[h[id]-1+j*len_i] != v[i+j*len_i]) {
            goto label1b;
          }
        }
        pans[i] = 1;
        goto label2b;
        label1b:;
        id++; id %= M;
      }
      h[id] = (int) i + 1;
      pans[i] = 0;
      count++;
      label2b:;
    }
  }
  free(h);
  UNPROTECT(2);
  return ans;
}

/*
 *  Matrix
 */

SEXP dupMatrixR(SEXP x, SEXP uniq, Rboolean idx, SEXP fromLast) {
  if(!IS_BOOL(fromLast)) {
    error("Argument 'fromLast' must be TRUE or FALSE and length 1.");
  }
  const bool pfromLast = asLogical(fromLast);
  const R_xlen_t len_x = ncols(x);
  const R_xlen_t len_i = nrows(x);
  const bool buniq = asLogical(uniq);
  SEXP ans = buniq ? R_NilValue : PROTECT(allocVector(LGLSXP, len_i));
  const size_t n2 = 2U * (size_t) len_i;
  size_t M = 256;
  int K = 8;
  while (M < n2) {
    M *= 2;
    K++;
  }
  R_xlen_t count = 0;
  int *h = (int*) calloc(M, sizeof(int));
  int *restrict pans = buniq ? (int*) calloc(len_i, sizeof(int)) : LOGICAL(ans);
  size_t id = 0;
  switch(UTYPEOF(x)) {
  case LGLSXP : {
    const int *restrict px = LOGICAL(x);
    if (buniq) {
      if (pfromLast) {
        for (R_xlen_t i = len_i-1; i > -1; --i) {
          id = 0;
          for (R_xlen_t j = 0; j < len_x; ++j) {
            id ^= ((unsigned)(j+1) * ((px[i+j*len_i] == NA_LOGICAL) ? 2U : (size_t) px[i+j*len_i]))*97*(j+1);
          }
          id = HASH(id, K);
          while (h[id]) {
            for (R_xlen_t j = 0; j < len_x; ++j) {
              if (px[h[id]-1+j*len_i] != px[i+j*len_i]) {
                goto labelml1t; // # nocov
              }
            }
            goto labelml2t;
            labelml1t:;
            id++; id %= M; // # nocov
          }
          h[id] = (int) i + 1;
          pans[i]++;
          count++;
          labelml2t:;
        }
      } else {
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
            labelml1:;
            id++; id %= M; // # nocov
          }
          h[id] = (int) i + 1;
          pans[i]++;
          count++;
          labelml2:;
        }
      }
    } else {
      if (pfromLast) {
        for (R_xlen_t i = len_i-1; i >-1; --i) {
          id = 0;
          for (R_xlen_t j = 0; j < len_x; ++j) {
            id ^= ((unsigned)(j+1) * ((px[i+j*len_i] == NA_LOGICAL) ? 2U : (size_t) px[i+j*len_i]))*97*(j+1);
          }
          id = HASH(id, K);
          while (h[id]) {
            for (R_xlen_t j = 0; j < len_x; ++j) {
              if (px[h[id]-1+j*len_i] != px[i+j*len_i]) {
                goto labelml1bt; // # nocov
              }
            }
            pans[i] = 1;
            goto labelml2bt;
            labelml1bt:;
            id++; id %= M; // # nocov
          }
          h[id] = (int) i + 1;
          pans[i] = 0;
          count++;
          labelml2bt:;
        }
      } else {
        for (R_xlen_t i = 0; i < len_i; ++i) {
          id = 0;
          for (R_xlen_t j = 0; j < len_x; ++j) {
            id ^= ((unsigned)(j+1) * ((px[i+j*len_i] == NA_LOGICAL) ? 2U : (size_t) px[i+j*len_i]))*97*(j+1);
          }
          id = HASH(id, K);
          while (h[id]) {
            for (R_xlen_t j = 0; j < len_x; ++j) {
              if (px[h[id]-1+j*len_i] != px[i+j*len_i]) {
                goto labelml1b; // # nocov
              }
            }
            pans[i] = 1;
            goto labelml2b;
            labelml1b:;
            id++; id %= M; // # nocov
          }
          h[id] = (int) i + 1;
          pans[i] = 0;
          count++;
          labelml2b:;
        }
      }
    }
  } break;
  case INTSXP : {
    const int *restrict px = INTEGER(x);
    if (buniq) {
      if (pfromLast) {
        for (R_xlen_t i = len_i-1; i > -1; --i) {
          R_xlen_t key = 0;
          for (R_xlen_t j = 0; j < len_x; ++j) {
            key ^= HASH(((px[i+j*len_i] == NA_INTEGER) ? 0 : px[i+j*len_i]),K)*97*(j+1);
          }
          id = HASH(key, K);
          while (h[id]) {
            for (R_xlen_t j = 0; j < len_x; ++j) {
              if (px[h[id]-1+j*len_i] != px[i+j*len_i]) {
                goto labelmi1t;
              }
            }
            goto labelmi2t;
            labelmi1t:;
            id++; id %= M;
          }
          h[id] = (int) i + 1;
          pans[i]++;
          count++;
          labelmi2t:;
        }
      } else {
        for (R_xlen_t i = 0; i < len_i; ++i) {
          R_xlen_t key = 0;
          for (R_xlen_t j = 0; j < len_x; ++j) {
            key ^= HASH(((px[i+j*len_i] == NA_INTEGER) ? 0 : px[i+j*len_i]),K)*97*(j+1);
          }
          id = HASH(key, K);
          while (h[id]) {
            for (R_xlen_t j = 0; j < len_x; ++j) {
              if (px[h[id]-1+j*len_i] != px[i+j*len_i]) {
                goto labelmi1;
              }
            }
            goto labelmi2;
            labelmi1:;
            id++; id %= M;
          }
          h[id] = (int) i + 1;
          pans[i]++;
          count++;
          labelmi2:;
        }
      }
    } else {
      if (pfromLast) {
        for (R_xlen_t i = len_i-1; i > -1; --i) {
          R_xlen_t key = 0;
          for (R_xlen_t j = 0; j < len_x; ++j) {
            key ^= HASH(((px[i+j*len_i] == NA_INTEGER) ? 0 : px[i+j*len_i]),K)*97*(j+1);
          }
          id = HASH(key, K);
          while (h[id]) {
            for (R_xlen_t j = 0; j < len_x; ++j) {
              if (px[h[id]-1+j*len_i] != px[i+j*len_i]) {
                goto labelmi1bt;
              }
            }
            pans[i] = 1;
            goto labelmi2bt;
            labelmi1bt:;
            id++; id %= M;
          }
          h[id] = (int) i + 1;
          pans[i] = 0;
          count++;
          labelmi2bt:;
        }
      } else {
        for (R_xlen_t i = 0; i < len_i; ++i) {
          R_xlen_t key = 0;
          for (R_xlen_t j = 0; j < len_x; ++j) {
            key ^= HASH(((px[i+j*len_i] == NA_INTEGER) ? 0 : px[i+j*len_i]),K)*97*(j+1);
          }
          id = HASH(key, K);
          while (h[id]) {
            for (R_xlen_t j = 0; j < len_x; ++j) {
              if (px[h[id]-1+j*len_i] != px[i+j*len_i]) {
                goto labelmi1b;
              }
            }
            pans[i] = 1;
            goto labelmi2b;
            labelmi1b:;
            id++; id %= M;
          }
          h[id] = (int) i + 1;
          pans[i] = 0;
          count++;
          labelmi2b:;
        }
      }
    }
  } break;
  case REALSXP : {
    const double *restrict px = REAL(x);
    union uno tpv;
    if (buniq) {
      if (pfromLast) {
        for (R_xlen_t i = len_i-1; i > -1; --i) {
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
                goto labelmr1t;
              }
            }
            goto labelmr2t;
            labelmr1t:;
            id++; id %= M;
          }
          h[id] = (int) i + 1;
          pans[i]++;
          count++;
          labelmr2t:;
        }
      } else {
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
          pans[i]++;
          count++;
          labelmr2:;
        }
      }
    } else {
      if (pfromLast) {
        for (R_xlen_t i = len_i-1; i > -1; --i) {
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
                goto labelmr1bt;
              }
            }
            pans[i] = 1;
            goto labelmr2bt;
            labelmr1bt:;
            id++; id %= M;
          }
          h[id] = (int) i + 1;
          pans[i] = 0;
          count++;
          labelmr2bt:;
        }
      } else {
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
                goto labelmr1b;
              }
            }
            pans[i] = 1;
            goto labelmr2b;
            labelmr1b:;
            id++; id %= M;
          }
          h[id] = (int) i + 1;
          pans[i] = 0;
          count++;
          labelmr2b:;
        }
      }
    }
  } break;
  case CPLXSXP : {
    const Rcomplex *restrict px = COMPLEX(x);
    unsigned int u;
    union uno tpv;
    Rcomplex tmp;
    if (buniq) {
      if (pfromLast) {
        for (R_xlen_t i = len_i-1; i > -1; --i) {
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
                goto labelmc1t;
              }
            }
            goto labelmc2t;
            labelmc1t:;
            id++; id %= M;
          }
          h[id] = (int) i + 1;
          pans[i]++;
          count++;
          labelmc2t:;
        }
      } else {
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
          pans[i]++;
          count++;
          labelmc2:;
        }
      }
    } else {
      if (pfromLast) {
        for (R_xlen_t i = len_i-1; i > -1; --i) {
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
                goto labelmc1bt;
              }
            }
            pans[i] = 1;
            goto labelmc2bt;
            labelmc1bt:;
            id++; id %= M;
          }
          h[id] = (int) i + 1;
          pans[i] = 0;
          count++;
          labelmc2bt:;
        }
      } else {
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
                goto labelmc1b;
              }
            }
            pans[i] = 1;
            goto labelmc2b;
            labelmc1b:;
            id++; id %= M;
          }
          h[id] = (int) i + 1;
          pans[i] = 0;
          count++;
          labelmc2b:;
        }
      }
    }
  } break;
  case STRSXP : {
    const SEXP *restrict px = STRING_PTR(x);
    if (buniq) {
      if (pfromLast) {
        for (R_xlen_t i = len_i-1; i > -1; --i) {
          R_xlen_t key = 0;
          for (R_xlen_t j = 0; j < len_x; ++j) {
            key ^= HASH(((intptr_t) px[i+j*len_i] & 0xffffffff),K)*97*(j+1);
          }
          id = HASH(key, K);
          while (h[id]) {
            for (R_xlen_t j = 0; j < len_x; ++j) {
              if (px[h[id]-1+j*len_i] != px[i+j*len_i]) {
                goto labelms1t;
              }
            }
            goto labelms2t; // # nocov
            labelms1t:;
            id++; id %= M;
          }
          h[id] = (int) i + 1;
          pans[i]++;
          count++;
          labelms2t:;
        }
      } else {
        for (R_xlen_t i = 0; i < len_i; ++i) {
          R_xlen_t key = 0;
          for (R_xlen_t j = 0; j < len_x; ++j) {
            key ^= HASH(((intptr_t) px[i+j*len_i] & 0xffffffff),K)*97*(j+1);
          }
          id = HASH(key, K);
          while (h[id]) {
            for (R_xlen_t j = 0; j < len_x; ++j) {
              if (px[h[id]-1+j*len_i] != px[i+j*len_i]) {
                goto labelms1;
              }
            }
            goto labelms2;
            labelms1:;
            id++; id %= M;
          }
          h[id] = (int) i + 1;
          pans[i]++;
          count++;
          labelms2:;
        }
      }
    } else {
      if (pfromLast) {
        for (R_xlen_t i = len_i-1; i > -1; --i) {
          R_xlen_t key = 0;
          for (R_xlen_t j = 0; j < len_x; ++j) {
            key ^= HASH(((intptr_t) px[i+j*len_i] & 0xffffffff),K)*97*(j+1);
          }
          id = HASH(key, K);
          while (h[id]) {
            for (R_xlen_t j = 0; j < len_x; ++j) {
              if (px[h[id]-1+j*len_i] != px[i+j*len_i]) {
                goto labelms1bt; // # nocov
              }
            }
            pans[i] = 1;
            goto labelms2bt;
            labelms1bt:;
            id++; id %= M; // # nocov
          }
          h[id] = (int) i + 1;
          pans[i] = 0;
          count++;
          labelms2bt:;
        }
      } else {
        for (R_xlen_t i = 0; i < len_i; ++i) {
          R_xlen_t key = 0;
          for (R_xlen_t j = 0; j < len_x; ++j) {
            key ^= HASH(((intptr_t) px[i+j*len_i] & 0xffffffff),K)*97*(j+1);
          }
          id = HASH(key, K);
          while (h[id]) {
            for (R_xlen_t j = 0; j < len_x; ++j) {
              if (px[h[id]-1+j*len_i] != px[i+j*len_i]) {
                goto labelms1b; // # nocov
              }
            }
            pans[i] = 1;
            goto labelms2b;
            labelms1b:;
            id++; id %= M; // # nocov
          }
          h[id] = (int) i + 1;
          pans[i] = 0;
          count++;
          labelms2b:;
        }
      }
    }
  } break;
  default:
    error("Matrix of type %s are not supported.", type2char(UTYPEOF(x)));
  }
  free(h);
  if (buniq) {
    SEXP indx = PROTECT(allocVector(INTSXP, count));
    int ct = 0;
    int *restrict py = INTEGER(indx);
    for (int i = 0; ct < count; ++i) {
      if (pans[i]) {
        py[ct++] = i;
      }
    }
    free(pans);
    if (idx) {
      UNPROTECT(1);
      return indx;
    }
    SEXP output = PROTECT(subSetRowMatrix(x, indx));
    UNPROTECT(2);
    return output;
  }
  UNPROTECT(1);
  return ans;
}

/*
 *  Vector
 */

SEXP dupVecR(SEXP x, SEXP uniq, SEXP fromLast) {
  const bool buniq = asLogical(uniq);
  if(!IS_BOOL(fromLast)) {
    error("Argument 'fromLast' must be TRUE or FALSE and length 1.");
  }
  const bool pfromLast = asLogical(fromLast);
  if (isFactor(x) && buniq) {
    const int len = LENGTH(PROTECT(getAttrib(x, R_LevelsSymbol)));
    UNPROTECT(1);
    bool *restrict count = (bool*)calloc(len,sizeof(bool));
    const int *restrict px = INTEGER(x);
    const int xlen = LENGTH(x);
    SEXP ans = PROTECT(allocVector(INTSXP, len));
    copyMostAttrib(x, ans);
    int *restrict pans = INTEGER(ans);
    if (pfromLast) {
      int j = len-1;
      for (int i = xlen-1; i >= 0; --i) {
        if (!count[px[i]]) {
          pans[j--] = px[i];
          if (j == -1)
            break;
          count[px[i]] = true;
        }
      }
      if (j != -1) {
        SEXP ans2 = PROTECT(allocVector(INTSXP, len-j-1));
        copyMostAttrib(x, ans2);
        memcpy(INTEGER(ans2),pans+j+1,(len-j-1)*sizeof(int));
        free(count);
        UNPROTECT(2);
        return ans2;
      }
    } else {
      int j = 0;
      for (int i = 0; i < xlen; ++i) {
        if (!count[px[i]]) {
          pans[j++] = px[i];
          if (j == len)
            break;
          count[px[i]] = true;
        }
      }
      if (j != len) {
        SETLENGTH(ans, j);
      }
    }
    free(count);
    UNPROTECT(1);
    return ans;
  }
  if (isLogical(x) && buniq) {
    bool *restrict count = (bool*)calloc(3,sizeof(bool));
    const int *restrict px = LOGICAL(x);
    const int xlen = LENGTH(x);
    SEXP ans = PROTECT(allocVector(LGLSXP, 3));
    copyMostAttrib(x, ans);
    int *restrict pans = LOGICAL(ans);
    if (pfromLast) {
      int j = 2;
      for (int i = xlen-1; i >= 0; --i) {
        const int cs = px[i] == NA_LOGICAL ? 2 : px[i];
        if (!count[cs]) {
          pans[j--] = cs == 2 ? NA_LOGICAL : px[i];
          if (j == -1)
            break;
          count[cs] = true;
        }
      }
      if (j != -1) {
        SEXP ans2 = PROTECT(allocVector(LGLSXP, 2-j));
        copyMostAttrib(x, ans2);
        memcpy(LOGICAL(ans2),pans+j+1,(2-j)*sizeof(int));
        free(count);
        UNPROTECT(2);
        return ans2;
      }
    } else {
      int j = 0;
      for (int i = 0; i < xlen; ++i) {
        const int cs = px[i] == NA_LOGICAL ? 2 : px[i];
        if (!count[cs]) {
          pans[j++] = cs == 2 ? NA_LOGICAL : px[i];
          if (j == 3)
            break;
          count[cs] = true;
        }
      }
      if (j != 3) {
        SETLENGTH(ans, j);
      }
    }
    free(count);
    UNPROTECT(1);
    return ans;
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
  } else if (tx == LGLSXP) {
    M = 4;
    K = 2;
  } else {
    error("Type %s is not supported.", type2char(tx));
  }
  R_xlen_t count = 0;
  int *h = (int*)calloc(M, sizeof(int));
  SEXP ans = buniq ? R_NilValue : PROTECT(allocVector(LGLSXP, n));
  int *restrict pans = buniq ? (int*)calloc(n, sizeof(int)) : LOGICAL(ans);
  switch (tx) {
  case LGLSXP: {
    const int *restrict px = LOGICAL(x);
    size_t id = 0;
    if (pfromLast) {
      for (int i = n-1; i > -1; --i) {
        id = (px[i] == NA_LOGICAL) ? 2U : (size_t) px[i];
        while (h[id]) {
          if (px[h[id]-1]==px[i]) {
            pans[i]=1;
            goto lbldt;
          }
          id++; id %= M; // # nocov
        }
        h[id] = (int) i + 1;
        pans[i] = 0;
        count++;
        lbldt:;
      }
    } else {
      for (int i = 0; i < n; ++i) {
        id = (px[i] == NA_LOGICAL) ? 2U : (size_t) px[i];
        while (h[id]) {
          if (px[h[id]-1]==px[i]) {
            pans[i]=1;
            goto lbld;
          }
          id++; id %= M; // # nocov
        }
        h[id] = (int) i + 1;
        pans[i] = 0;
        count++;
        lbld:;
      }
    }
    free(h);
  } break;
  case INTSXP: { // think about factor and levels number
    const int *restrict px = INTEGER(x);
    size_t id = 0;
    if (buniq) {
      if (pfromLast) {
        for (int i = n-1; i > -1; --i) {
          id = (px[i] == NA_INTEGER) ? 0 : HASH(px[i], K);
          while (h[id]) {
            if (px[h[id]-1]==px[i]) {
              goto iblt;
            }
            id++; id %= M;
          }
          h[id] = (int) i + 1;
          pans[i]++;
          count++;
          iblt:;
        }
      } else {
        for (int i = 0; i < n; ++i) {
          id = (px[i] == NA_INTEGER) ? 0 : HASH(px[i], K);
          while (h[id]) {
            if (px[h[id]-1]==px[i]) {
              goto ibl;
            }
            id++; id %= M;
          }
          h[id] = (int) i + 1;
          pans[i]++;
          count++;
          ibl:;
        }
      }
      free(h);
      SEXP indx = PROTECT(allocVector(tx, count));
      size_t ct = 0;
      int *restrict py = INTEGER(indx);
      for (R_xlen_t i = 0; ct < count; ++i) {
        if (pans[i]) {
          py[ct++] = px[i];
        }
      }
      free(pans);
      copyMostAttrib(x, indx);
      UNPROTECT(1);
      return indx;
    } else {
      if (pfromLast) {
        for (int i = n-1; i > -1; --i) {
          id = (px[i] == NA_INTEGER) ? 0 : HASH(px[i], K);
          while (h[id]) {
            if (px[h[id]-1]==px[i]) {
              pans[i]=1;
              goto ibldt;
            }
            id++; id %= M;
          }
          h[id] = (int) i + 1;
          pans[i] = 0;
          count++;
          ibldt:;
        }
      } else {
        for (int i = 0; i < n; ++i) {
          id = (px[i] == NA_INTEGER) ? 0 : HASH(px[i], K);
          while (h[id]) {
            if (px[h[id]-1]==px[i]) {
              pans[i]=1;
              goto ibld;
            }
            id++; id %= M;
          }
          h[id] = (int) i + 1;
          pans[i] = 0;
          count++;
          ibld:;
        }
      }
      free(h);
    }
  } break;
  case REALSXP: {
    const double *restrict px = REAL(x);
    size_t id = 0;
    union uno tpv;
    if (buniq) {
      if (pfromLast) {
        for (int i = n-1; i > -1; --i) {
          tpv.d = R_IsNA(px[i]) ? NA_REAL : (R_IsNaN(px[i]) ? R_NaN :px[i]);
          id = HASH(tpv.u[0] + tpv.u[1], K);
          while (h[id]) {
            if (REQUAL(px[h[id]-1], px[i])) {
              goto rblt;
            }
            id++; id %= M;
          }
          h[id] = (int) i + 1;
          pans[i]++;
          count++;
          rblt:;
        }
      } else {
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
          pans[i]++;
          count++;
          rbl:;
        }
      }
      free(h);
      SEXP indx = PROTECT(allocVector(tx, count));
      size_t ct = 0;
      double *restrict py = REAL(indx);
      for (R_xlen_t i = 0; ct < count; ++i) {
        if (pans[i]) {
          py[ct++] = px[i];
        }
      }
      free(pans);
      copyMostAttrib(x, indx);
      UNPROTECT(1);
      return indx;
    } else {
      if (pfromLast) {
        for (int i = n-1; i > -1; --i) {
          tpv.d = R_IsNA(px[i]) ? NA_REAL : (R_IsNaN(px[i]) ? R_NaN :px[i]);
          id = HASH(tpv.u[0] + tpv.u[1], K);
          while (h[id]) {
            if (REQUAL(px[h[id]-1], px[i])) {
              pans[i]=1;
              goto rbldt;
            }
            id++; id %= M;
          }
          h[id] = (int) i + 1;
          pans[i]=0;
          count++;
          rbldt:;
        }
      } else {
        for (int i = 0; i < n; ++i) {
          tpv.d = R_IsNA(px[i]) ? NA_REAL : (R_IsNaN(px[i]) ? R_NaN :px[i]);
          id = HASH(tpv.u[0] + tpv.u[1], K);
          while (h[id]) {
            if (REQUAL(px[h[id]-1], px[i])) {
              pans[i]=1;
              goto rbld;
            }
            id++; id %= M;
          }
          h[id] = (int) i + 1;
          pans[i]=0;
          count++;
          rbld:;
        }
      }
      free(h);
    }
  } break;
  case CPLXSXP: {
    const Rcomplex *restrict px = COMPLEX(x);
    size_t id = 0;
    unsigned int u;
    union uno tpv;
    Rcomplex tmp;
    if (buniq) {
      if (pfromLast) {
        for (int i = n-1; i > -1; --i) {
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
              goto cblt;
            }
            id++; id %= M;
          }
          h[id] = (int) i + 1;
          pans[i]++;
          count++;
          cblt:;
        }
      } else {
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
          pans[i]++;
          count++;
          cbl:;
        }
      }
      free(h);
      SEXP indx = PROTECT(allocVector(tx, count));
      size_t ct = 0;
      Rcomplex *restrict py = COMPLEX(indx);
      for (int i = 0; ct < count; ++i) {
        if (pans[i]) {
          py[ct++] = px[i];
        }
      }
      free(pans);
      copyMostAttrib(x, indx);
      UNPROTECT(1);
      return indx;
    } else {
      if (pfromLast) {
        for (int i = n-1; i > -1; --i) {
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
              pans[i]=1;
              goto cbldt;
            }
            id++; id %= M;
          }
          h[id] = (int) i + 1;
          pans[i]=0;
          count++;
          cbldt:;
        }
      } else {
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
              pans[i]=1;
              goto cbld;
            }
            id++; id %= M;
          }
          h[id] = (int) i + 1;
          pans[i]=0;
          count++;
          cbld:;
        }
      }
      free(h);
    }
  } break;
  case STRSXP: {
    const SEXP *restrict px = STRING_PTR(x);
    size_t id = 0;
    if (buniq) {
      if (pfromLast) {
        for (int i = n-1; i > -1; --i) {
          id = HASH(((intptr_t) px[i] & 0xffffffff), K);
          while (h[id]) {
            if (px[h[id] - 1]==px[i]) {
              goto sblt;
            }
            id++; id %= M;
          }
          h[id] = (int) i + 1;
          pans[i]++;
          count++;
          sblt:;
        }
      } else {
        for (int i = 0; i < n; ++i) {
          id = HASH(((intptr_t) px[i] & 0xffffffff), K);
          while (h[id]) {
            if (px[h[id] - 1]==px[i]) {
              goto sbl;
            }
            id++; id %= M;
          }
          h[id] = (int) i + 1;
          pans[i]++;
          count++;
          sbl:;
        }
      }
      free(h);
      SEXP indx = PROTECT(allocVector(tx, count));
      R_xlen_t ct = 0;
      for (int i = 0; ct < count; ++i) {
        if (pans[i]) {
          SET_STRING_ELT(indx, ct++, px[i]);
        }
      }
      free(pans);
      copyMostAttrib(x, indx);
      UNPROTECT(1);
      return indx;
    } else {
      if (pfromLast) {
        for (int i = n-1; i > -1; --i) {
          id = HASH(((intptr_t) px[i] & 0xffffffff), K);
          while (h[id]) {
            if (px[h[id] - 1]==px[i]) {
              pans[i]=1;
              goto sbldt;
            }
            id++; id %= M;
          }
          h[id] = (int) i + 1;
          pans[i]=0;
          count++;
          sbldt:;
        }
      } else {
        for (int i = 0; i < n; ++i) {
          id = HASH(((intptr_t) px[i] & 0xffffffff), K);
          while (h[id]) {
            if (px[h[id] - 1]==px[i]) {
              pans[i]=1;
              goto sbld;
            }
            id++; id %= M;
          }
          h[id] = (int) i + 1;
          pans[i]=0;
          count++;
          sbld:;
        }
      }
      free(h);
    }
  } break;
  }
  UNPROTECT(1);
  return ans;
}

/*
 * Vector Index Only
 */

SEXP dupVecIndexOnlyR(SEXP x, SEXP fromLast) {
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
  } else if (tx == LGLSXP) {
    M = 4;
    K = 2;
  } else {
    error("Type %s is not supported.", type2char(tx)); // # nocov
  }
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
        id++; id %= M; // # nocov
      }
      h[id] = (int) i + 1;
      pans_i[i] = h[id];
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
        id++; id %= M; // # nocov
      }
      h[id] = (int) i + 1;
      pans_i[i] = h[id];
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
        id++; id %= M; // # nocov
      }
      h[id] = (int) i + 1;
      pans_i[i] = h[id];
      cbl:;
    }
  } break;
  case STRSXP: {
    const SEXP *restrict px = STRING_PTR(x);
    size_t id = 0;
    for (int i = 0; i < n; ++i) {
      id = HASH(((intptr_t) px[i] & 0xffffffff), K);
      while (h[id]) {
        if (px[h[id]-1]==px[i]) {
          pans_i[i] = h[id];
          goto sbl;
        }
        id++; id %= M; // # nocov
      }
      h[id] = (int) i + 1;
      pans_i[i] = h[id];
      sbl:;
    }
  } break;
  }
  free(h);
  UNPROTECT(1);
  return ans_i;
}
