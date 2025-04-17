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

SEXP topnR(SEXP vec, SEXP n, SEXP dec, SEXP hasna, SEXP env) {
  int len0 = asInteger(n);
  const R_xlen_t len1 = xlength(vec);
  if (isS4(vec)) {
    error("S4 class objects are not supported.");
  }
  if (len0 > len1) {
    warning("'n' is larger than length of 'vec'. 'n' will be set to length of 'vec'.");
    len0 = (int)len1;
  }
  if (len0 < 1) {
    error("Please enter a positive integer larger or equal to 1.");
  }
  if (!IS_BOOL(dec)) {
    error("Argument 'decreasing' must be TRUE or FALSE and length 1.");
  }
  if (!IS_BOOL(hasna)) {
    error("Argument 'hasna' must be TRUE or FALSE and length 1.");
  }
  const Rboolean dcr = asLogical(dec);
  const SEXPTYPE tvec = UTYPEOF(vec);
  const Rboolean vhasna = asLogical(hasna);
  if ( ((len0 > 2000 && vhasna == FALSE) || (len0 > 1500 && vhasna == TRUE)) && (tvec == INTSXP || tvec == REALSXP)) {
    SEXP prem = PROTECT(callToOrder(vec, "radix", dcr, TRUE, env));
    SEXP ans = PROTECT(allocVector(UTYPEOF(prem), len0));
    switch(UTYPEOF(prem)) {
    case INTSXP: {
      memcpy(INTEGER(ans), INTEGER(prem), len0 *sizeof(int));
    } break;
    case REALSXP: {
      memcpy(REAL(ans), REAL(prem), len0 *sizeof(double));
    } break;
    }
    UNPROTECT(2);
    return ans;
  }
  SEXP ans = PROTECT(allocVector(INTSXP, len0));
  int *restrict pans = INTEGER(ans);
  int tmp;
  if (dcr) {
    switch(tvec) {
    case INTSXP: {
      int i, j, idx = 0;
      const int *restrict pvec = INTEGER(vec);
      int min_value = pvec[0];
      if (vhasna) {
        for (i = 0; i < len0; ++i) {
          pans[i] = i;
          if (pvec[i] <= min_value || pvec[i] == NA_INTEGER) {
            min_value = pvec[i];
            idx = i;
          }
        }
        for (i = len0; i < len1; ++i) {
          if (pvec[i] == NA_INTEGER) {
            continue;
          }
          if (pvec[i] > min_value) {
            min_value = pvec[i];
            pans[idx] = i;
            for (j = 0; j <len0; ++j) {
              if ((min_value > pvec[pans[j]] || (min_value == pvec[pans[j]] && pans[idx] < pans[j])) || pvec[pans[j]] == NA_INTEGER) {
                min_value = pvec[pans[j]];
                idx = j;
              }
            }
          }
        }
        for (i = 0; i < len0; ++i) {
          tmp = pans[i];
          for (j = i; j > 0 && (pvec[tmp] > pvec[pans[j-1]] || (pvec[tmp] == pvec[pans[j-1]] && tmp < pans[j-1])); --j) {
            pans[j] = pans[j-1];
          }
          pans[j] = tmp;
        }
        for (i =0; i < len0; ++i) {
          pans[i]++;
        }
      } else {
        for (i = 0; i < len0; ++i) {
          pans[i] = i;
          if (pvec[i] <= min_value) {
            min_value = pvec[i];
            idx = i;
          }
        }
        for (i = len0; i < len1; ++i) {
          if (pvec[i] > min_value) {
            min_value = pvec[i];
            pans[idx] = i;
            for (j = 0; j <len0; ++j) {
              if (min_value > pvec[pans[j]] || (min_value == pvec[pans[j]] && pans[idx] < pans[j])) {
                min_value = pvec[pans[j]];
                idx = j;
              }
            }
          }
        }
        for (i = 0; i < len0; ++i) {
          tmp = pans[i];
          for (j = i; j > 0 && (pvec[tmp] > pvec[pans[j-1]] || (pvec[tmp] == pvec[pans[j-1]] && tmp < pans[j-1])); --j) {
            pans[j] = pans[j-1];
          }
          pans[j] = tmp;
        }
        for (i =0; i < len0; ++i) {
          pans[i]++;
        }
      }
    } break;
    case REALSXP: {
      int i, j, idx = 0;
      const double *restrict pvec = REAL(vec);
      double min_value = pvec[0];
      if (vhasna) {
        for (i = 0; i < len0; ++i) {
          pans[i] = i;
          if (pvec[i] <= min_value || ISNAN(pvec[i])) {
            min_value = pvec[i];
            idx = i;
          }
        }
        for (i = len0; i < len1; ++i) {
          if (ISNAN(pvec[i])) {
            continue;
          }
          if (pvec[i] > min_value || ISNAN(min_value)) {
            min_value = pvec[i];
            pans[idx] = i;
            for (j = 0; j <len0; ++j) {
              if ((min_value > pvec[pans[j]] || (min_value == pvec[pans[j]] && pans[idx] < pans[j])) || ISNAN(pvec[pans[j]])) {
                min_value = pvec[pans[j]];
                idx = j;
              }
            }
          }
        }
        for (i = 0; i < len0; ++i) {
          tmp = pans[i];
          for (j = i; j > 0 && (pvec[tmp] > pvec[pans[j-1]] || (pvec[tmp] == pvec[pans[j-1]] && tmp < pans[j-1]) || (!ISNAN(pvec[tmp]) && ISNAN(pvec[pans[j-1]]))); --j) {
            pans[j] = pans[j-1];
          }
          pans[j] = tmp;
        }
        for (i =0; i < len0; ++i) {
          pans[i]++;
        }
      } else {
        for (i = 0; i < len0; ++i) {
          pans[i] = i;
          if (pvec[i] <= min_value) {
            min_value = pvec[i];
            idx = i;
          }
        }
        for (i = len0; i < len1; ++i) {
          if (pvec[i] > min_value) {
            min_value = pvec[i];
            pans[idx] = i;
            for (j = 0; j <len0; ++j) {
              if (min_value > pvec[pans[j]] || (min_value == pvec[pans[j]] && pans[idx] < pans[j])) {
                min_value = pvec[pans[j]];
                idx = j;
              }
            }
          }
        }
        for (i = 0; i < len0; ++i) {
          tmp = pans[i];
          for (j = i; j > 0 && (pvec[tmp] > pvec[pans[j-1]] || (pvec[tmp] == pvec[pans[j-1]] && tmp < pans[j-1])); --j) {
            pans[j] = pans[j-1];
          }
          pans[j] = tmp;
        }
        for (i =0; i < len0; ++i) {
          pans[i]++;
        }
      }
    } break;
    default:
      error("Type %s is not supported.", type2char(tvec));
    }
  } else {
    switch(tvec) {
    case INTSXP: {
      int i, j, idx = 0;
      const int *restrict pvec = INTEGER(vec);
      int min_value = pvec[0];
      if (vhasna) {
        for (i = 0; i < len0; ++i) {
          pans[i] = i;
          if ((pvec[i] >= min_value && min_value != NA_INTEGER) || pvec[i] == NA_INTEGER) {
            min_value = pvec[i];
            idx = i;
          }
        }
        for (i = len0; i < len1; ++i) {
          if (pvec[i] == NA_INTEGER) {
            continue;
          }
          if (pvec[i] < min_value || min_value == NA_INTEGER) {
            min_value = pvec[i];
            pans[idx] = i;
            for (j = 0; j <len0; ++j) {
              if (((min_value < pvec[pans[j]] || (min_value == pvec[pans[j]] && pans[idx] < pans[j])) && min_value != NA_INTEGER) || pvec[pans[j]] == NA_INTEGER) {
                min_value = pvec[pans[j]];
                idx = j;
              }
            }
          }
        }
        for (i = 0; i < len0; ++i) {
          tmp = pans[i];
          if (pvec[tmp] == NA_INTEGER) {
            continue;
          }
          for (j = i; j > 0 && (pvec[tmp] < pvec[pans[j-1]] || (pvec[tmp] == pvec[pans[j-1]] && tmp < pans[j-1]) || pvec[pans[j-1]] == NA_INTEGER); --j) {
            pans[j] = pans[j-1];
          }
          pans[j] = tmp;
        }
        for (i =0; i < len0; ++i) {
          pans[i]++;
        }
      } else {
        for (i = 0; i < len0; ++i) {
          pans[i] = i;
          if (pvec[i] >= min_value) {
            min_value = pvec[i];
            idx = i;
          }
        }
        for (i = len0; i < len1; ++i) {
          if (pvec[i] < min_value) {
            min_value = pvec[i];
            pans[idx] = i;
            for (j = 0; j <len0; ++j) {
              if (min_value < pvec[pans[j]] || (min_value == pvec[pans[j]] && pans[idx] < pans[j])) {
                min_value = pvec[pans[j]];
                idx = j;
              }
            }
          }
        }
        for (i = 0; i < len0; ++i) {
          tmp = pans[i];
          for (j = i; j > 0 && (pvec[tmp] < pvec[pans[j-1]] || (pvec[tmp] == pvec[pans[j-1]] && tmp < pans[j-1])); --j) {
            pans[j] = pans[j-1];
          }
          pans[j] = tmp;
        }
        for (i =0; i < len0; ++i) {
          pans[i]++;
        }
      }
    } break;
    case REALSXP: {
      int i, j, idx = 0;
      const double *restrict pvec = REAL(vec);
      double min_value = pvec[0];
      if (vhasna) {
        for (i = 0; i < len0; ++i) {
          pans[i] = i;
          if (pvec[i] >= min_value || ISNAN(pvec[i])) {
            min_value = pvec[i];
            idx = i;
          }
        }
        for (i = len0; i < len1; ++i) {
          if (ISNAN(pvec[i])) {
            continue;
          }
          if (pvec[i] < min_value || ISNAN(min_value)) {
            min_value = pvec[i];
            pans[idx] = i;
            for (j = 0; j <len0; ++j) {
              if ((min_value < pvec[pans[j]] || (min_value == pvec[pans[j]] && pans[idx] < pans[j])) || ISNAN(pvec[pans[j]])) {
                min_value = pvec[pans[j]];
                idx = j;
              }
            }
          }
        }
        for (i = 0; i < len0; ++i) {
          tmp = pans[i];
          for (j = i; j > 0 && (pvec[tmp] < pvec[pans[j-1]] || (pvec[tmp] == pvec[pans[j-1]] && tmp < pans[j-1]) || (!ISNAN(pvec[tmp]) && ISNAN(pvec[pans[j-1]]))); --j) {
            pans[j] = pans[j-1];
          }
          pans[j] = tmp;
        }
        for (i =0; i < len0; ++i) {
          pans[i]++;
        }
      } else {
        for (i = 0; i < len0; ++i) {
          pans[i] = i;
          if (pvec[i] >= min_value) {
            min_value = pvec[i];
            idx = i;
          }
        }
        for (i = len0; i < len1; ++i) {
          if (pvec[i] < min_value) {
            min_value = pvec[i];
            pans[idx] = i;
            for (j = 0; j <len0; ++j) {
              if (min_value < pvec[pans[j]] || (min_value == pvec[pans[j]] && pans[idx] < pans[j])) {
                min_value = pvec[pans[j]];
                idx = j;
              }
            }
          }
        }
        for (i = 0; i < len0; ++i) {
          tmp = pans[i];
          for (j = i; j > 0 && (pvec[tmp] < pvec[pans[j-1]] || (pvec[tmp] == pvec[pans[j-1]] && tmp < pans[j-1])); --j) {
            pans[j] = pans[j-1];
          }
          pans[j] = tmp;
        }
        for (i =0; i < len0; ++i) {
          pans[i]++;
        }
      }
    } break;
    default:
      error("Type %s is not supported.", type2char(tvec));
    }
  }
  UNPROTECT(1);
  return ans;
}
