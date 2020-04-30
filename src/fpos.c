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

SEXP fposR(SEXP ndle, SEXP hsk, SEXP all, SEXP overlap) {
  if (!IS_BOOL(all)) {
    error("Argument 'all' must be TRUE or FALSE and length 1.");
  }
  if (!IS_BOOL(overlap)) {
    error("Argument 'overlap' must be TRUE or FALSE and length 1.");
  }
  if (isS4(hsk) || isS4(ndle)) {
    error("S4 class objects are not supported.");
  }
  SEXPTYPE thsk = UTYPEOF(hsk);
  SEXPTYPE tndle = UTYPEOF(ndle);
  if (thsk != INTSXP && thsk != REALSXP && thsk != LGLSXP &&
      thsk != CPLXSXP && thsk != STRSXP) {
    error("Type %s for 'haystack' is not supported.", type2char(thsk));
  }
  if (tndle != INTSXP && tndle != REALSXP && tndle != LGLSXP &&
      tndle != CPLXSXP && tndle != STRSXP) {
    error("Type %s for 'needle' is not supported.", type2char(tndle));
  }
  const int n = nrows(hsk);
  const int m = ncols(hsk);
  const int k = nrows(ndle);
  const int l = ncols(ndle);
  if (k > n || l > m) {
    error("One of the dimension of the small matrix is greater than the large matrix.");
  }
  int nprotect = 0;
  if (thsk != tndle) {
    if (tndle == INTSXP && thsk == REALSXP) {
      ndle = PROTECT(coerceVector(ndle, thsk)); nprotect++;
      tndle = thsk;
    } else if (tndle == REALSXP && thsk == INTSXP) {
      hsk = PROTECT(coerceVector(hsk, tndle)); nprotect++;
      thsk = tndle;
    } else {
      error("Haystack type (%s) and needle type (%s) are different."
              " Please make sure that they have the same type.",
              type2char(thsk), type2char(tndle));
    }
  }
  const int lim_x = n - k + 1;
  const int lim_y = m - l + 1;
  const int sz = lim_x * lim_y;
  int i, j, p, q, id, tj = 0, ti = 0;
  int pos_h = 0, pos_n = 0, x = 0;
  SEXP col = PROTECT(allocVector(INTSXP, sz)); nprotect++;
  SEXP row = PROTECT(allocVector(INTSXP, sz)); nprotect++;
  int *restrict pcol = INTEGER(col);
  int *restrict prow = INTEGER(row);
  const int pall = !LOGICAL(all)[0];
  const int poverlap = !LOGICAL(overlap)[0];
  switch(thsk) {
  case LGLSXP: {
    const int *restrict int_h = LOGICAL(hsk);
    const int *restrict int_n = LOGICAL(ndle);
    for (i = 0; i < lim_y; ++i) {
      for (j = 0; j < lim_x; ++j) {
        id = 1;
        if (i < ti && j < tj) {
          continue;
        }
        for (p = 0; p < l; ++p) {
          pos_h = (i+p) * n  + j;
          pos_n = p * k;
          for (q = 0; q < k; ++q) {
            if (int_h[pos_h + q] != int_n[pos_n + q]) {
              id = 0;
              break;
            }
          }
          if (!id) {
            break;
          }
        }
        if (id) {
          prow[x] = j + 1;
          pcol[x++] = i + 1;
          if (pall) {
            goto label;
          }
          if (poverlap) {
            ti = i + l;
            tj = j + k;
          }
        }
      }
    }
  } break;
  case INTSXP: {
    const int *restrict int_h = INTEGER(hsk);
    const int *restrict int_n = INTEGER(ndle);
    for (i = 0; i < lim_y; ++i) {
      for (j = 0; j < lim_x; ++j) {
        id = 1;
        if (i < ti && j < tj) {
          continue;
        }
        for (p = 0; p < l; ++p) {
          pos_h = (i+p) * n  + j;
          pos_n = p * k;
          for (q = 0; q < k; ++q) {
            if (int_h[pos_h + q] != int_n[pos_n + q]) {
              id = 0;
              break;
            }
          }
          if (!id) {
            break;
          }
        }
        if (id) {
          prow[x] = j + 1;
          pcol[x++] = i + 1;
          if (pall) {
            goto label;
          }
          if (poverlap) {
            ti = i + l;
            tj = j + k;
          }
        }
      }
    }
  } break;
  case REALSXP: {
    const double *restrict dbl_h = REAL(hsk);
    const double *restrict dbl_n = REAL(ndle);
    for (i = 0; i < lim_y; ++i) {
      for (j = 0; j < lim_x; ++j) {
        id = 1;
        if (i < ti && j < tj) {
          continue;
        }
        for (p = 0; p < l; ++p)
        {
          pos_h = (i+p) * n  + j;
          pos_n = p * k;
          for (q = 0; q < k; ++q) {
            if (dbl_h[pos_h + q] != dbl_n[pos_n + q] &&
                (!(ISNAN(dbl_h[pos_h + q]) || ISNAN(dbl_n[pos_n + q])))) {
              id = 0;
              break;
            }
          }
          if (!id) {
            break;
          }
        }
        if (id) {
          prow[x] = j + 1;
          pcol[x++] = i + 1;
          if (pall) {
            goto label;
          }
          if (poverlap) {
            ti = i + l;
            tj = j + k;
          }
        }
      }
    }
  } break;
  case CPLXSXP: {
    const Rcomplex *restrict cpl_h = COMPLEX(hsk);
    const Rcomplex *restrict cpl_n = COMPLEX(ndle);
    for (i = 0; i < lim_y; ++i) {
      for (j = 0; j < lim_x; ++j) {
        id = 1;
        if (i < ti && j < tj) {
          continue;
        }
        for (p = 0; p < l; ++p) {
          pos_h = (i+p) * n  + j;
          pos_n = p * k;
          for (q = 0; q < k; ++q) {
            if ((cpl_h[pos_h + q].r != cpl_n[pos_n + q].r || cpl_h[pos_h + q].i != cpl_n[pos_n + q].i) &&
                (!(ISNAN_COMPLEX(cpl_h[pos_h + q])  || ISNAN_COMPLEX(cpl_n[pos_n + q])) )) {
              id = 0;
              break;
            }
          }
          if (!id) {
            break;
          }
        }
        if (id) {
          prow[x] = j + 1;
          pcol[x++] = i + 1;
          if (pall) {
            goto label;
          }
          if (poverlap) {
            ti = i + l;
            tj = j + k;
          }
        }
      }
    }
  } break;
  case STRSXP: {
    for (i = 0; i < lim_y; ++i) {
    for (j = 0; j < lim_x; ++j) {
      id = 1;
      if (i < ti && j < tj) {
        continue;
      }
      for (p = 0; p < l; ++p) {
        pos_h = (i+p) * n  + j;
        pos_n = p * k;
        for (q = 0; q < k; ++q) {
          if (RCHAR(hsk, pos_h + q) != RCHAR(ndle, pos_n + q)) {
            id = 0;
            break;
          }
        }
        if (!id) {
          break;
        }
      }
      if (id) {
        prow[x] = j + 1;
        pcol[x++] = i + 1;
        if (pall) {
          goto label;
        }
        if (poverlap) {
          ti = i + l;
          tj = j + k;
        }
      }
    }
  }
  } break;
  }
  label:;
  if (x == 0) {
    UNPROTECT(nprotect);
    return R_NilValue;
  }
  SEXP ans = PROTECT(allocMatrix(INTSXP, x, 2)); nprotect++;
  memcpy(INTEGER(ans), prow, (unsigned)x*sizeof(int));
  memcpy(INTEGER(ans)+x, pcol, (unsigned)x*sizeof(int));
  UNPROTECT(nprotect);
  return ans;
}
