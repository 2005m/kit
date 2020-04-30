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

SEXP nifR(SEXP na, SEXP rho, SEXP md, SEXP args) {
  R_len_t n=length(args);
  if (n % 2) {
    error("Received %d inputs; please supply an even number of arguments in ... "
            "consisting of logical condition, resulting value pairs (in that order). "
            "Note that argument 'default' must be named explicitly (e.g.: default=0)", n);
  }
  int nprotect = 0;
  R_xlen_t len0=0, len1=0, len2=0, len_na=xlength(na), idx=0, l=0;
  SEXP ans = R_NilValue, out0c = R_NilValue, out0l = R_NilValue, cons = R_NilValue, outs = R_NilValue;
  PROTECT_INDEX Icons, Iouts;
  PROTECT_WITH_INDEX(cons, &Icons); nprotect++;
  PROTECT_WITH_INDEX(outs, &Iouts); nprotect++;
  SEXPTYPE type0, type1;
  const bool nonna = !isNull(na); 
  bool imask = true;
  bool ifact = false;
  R_xlen_t *restrict p = NULL;
  n = n/2;
  const int dn = n-1;
  const bool bmd = asLogical(md);
  for (int i=0; i<n; ++i) {
    if (bmd) {
      REPROTECT(cons = eval(PTR_ETL(args, 2*i), rho), Icons);
      REPROTECT(outs = eval(PTR_ETL(args, 2*i+1), rho), Iouts); 
    } else {
      REPROTECT(cons = PTR_ETL(args, 2*i), Icons);
      REPROTECT(outs = PTR_ETL(args, 2*i+1), Iouts);
    }
    if (isS4(outs)) {
      error("S4 class objects are not supported.");
    }
    if (!isLogical(cons)) {
      error("Argument #%d must be logical.", 2*i+1);
    }
    const int *restrict pcons = LOGICAL(cons);
    if (i == 0) {
      len0 = xlength(cons);
      len2 = len0;
      type0 = UTYPEOF(outs);
      type1 = type0;
      out0c = PROTECT(getAttrib(outs, R_ClassSymbol)); nprotect++;
      out0l = PROTECT(getAttrib(outs, R_LevelsSymbol)); nprotect++;
      ifact = isFactor(outs);
      if (nonna) {
        if (len_na != 1 && len_na != len0) {
          error("Length of 'default' must either be 1 or length of logical condition."); // maybe improve message
        }
        SEXPTYPE tn = UTYPEOF(na);
        if (tn != type0) {
          error("Resulting value is of type %s but 'default' is of type %s. "
                  "Please make sure that both arguments have the same type.", type2char(type0), type2char(tn));
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
      ans = PROTECT(allocVector(type0, len0)); nprotect++;
      p = (R_xlen_t*)malloc(sizeof(R_xlen_t)*(size_t)len0);
      copyMostAttrib(outs, ans);
    } else {
      imask = false;
      l = 0;
      if (xlength(cons) != len0) {
        error("Argument #%d has a different length than argument #1. "
                "Please make sure all logical conditions have the same length.",
                i*2+1);
      }
      type1 = UTYPEOF(outs);
      if (type1 != type0) {
        error("Argument #%d is of type %s, however argument #2 is of type %s. "
                "Please make sure all output values have the same type.",
                i*2+2, type2char(type1), type2char(type0));
      }
      if (!R_compute_identical(out0c, PROTECT(getAttrib(outs, R_ClassSymbol)), 0)) {
        error("Argument #%d has different class than argument #2, "
                "Please make sure all output values have the same class.", i*2+2);
      }
      UNPROTECT(1);
      if (ifact) {
        if (!R_compute_identical(out0l, PROTECT(getAttrib(outs, R_LevelsSymbol)), 0)) {
          error("Argument #2 and argument #%d are both factor but their levels are different.", i*2+2);
        }
        UNPROTECT(1);
      }
    }
    len1 = xlength(outs);
    if (len1 != len0 && len1 != 1) {
      error("Length of output value #%d must either be 1 or length of logical condition.", i*2+2);
    }
    ssize_t amask = len1>1 ? SSIZE_MAX : 0;
    switch(type1) {
    case LGLSXP: {
      const int *restrict pouts = LOGICAL(outs);
      int *restrict pans = LOGICAL(ans);
      for (ssize_t j=0; j<len2; ++j) {
        idx = imask ? j : p[j];
        if (pcons[idx]==1) {
          pans[idx] = pouts[idx & amask];
        } else {
          p[l++] = idx;
        }
      }
      if (i==dn) {
        ssize_t bmask = len_na>1 ? SSIZE_MAX : 0;
        const int *restrict pna = nonna ? LOGICAL(na) : NULL;
        for (ssize_t j=0; j<l; ++j) {
          pans[p[j]] = nonna ? pna[p[j] & bmask] : NA_LOGICAL;
        }
      }
    } break;
    case INTSXP: {
      const int *restrict pouts = INTEGER(outs);
      int *restrict pans = INTEGER(ans);
      for (ssize_t j=0; j<len2; ++j) {
        idx = imask ? j : p[j];
        if (pcons[idx]==1) {
          pans[idx] = pouts[idx & amask];
        } else {
          p[l++] = idx;
        }
      }
      if (i==dn) {
        ssize_t bmask = len_na>1 ? SSIZE_MAX : 0;
        const int *restrict pna = nonna ? INTEGER(na) : NULL;
        for (ssize_t j=0; j<l; ++j) {
          pans[p[j]] = nonna ? pna[p[j] & bmask] : NA_INTEGER;
        }
      }
    } break;
    case REALSXP: {
      const double *restrict pouts = REAL(outs);
      double *restrict pans = REAL(ans);
      for (ssize_t j=0; j<len2; ++j) {
        idx = imask ? j : p[j];
        if (pcons[idx]==1) {
          pans[idx] = pouts[idx & amask];
        } else {
          p[l++] = idx;
        }
      }
      if (i==dn) {
        ssize_t bmask = len_na>1 ? SSIZE_MAX : 0;
        const double *restrict pna = nonna ? REAL(na) : NULL;
        for (ssize_t j=0; j<l; ++j) {
          pans[p[j]] = nonna ? pna[p[j] & bmask] : NA_REAL;
        }
      }
    } break;
    case CPLXSXP: {
      const Rcomplex *restrict pouts = COMPLEX(outs);
      Rcomplex *restrict pans = COMPLEX(ans);
      for (ssize_t j=0; j<len2; ++j) {
        idx = imask ? j : p[j];
        if (pcons[idx]==1) {
          pans[idx] = pouts[idx & amask];
        } else {
          p[l++] = idx;
        }
      }
      if (i==dn) {
        ssize_t bmask = len_na>1 ? SSIZE_MAX : 0;
        Rcomplex NA_CPLX; NA_CPLX.r = NA_REAL; NA_CPLX.i = NA_REAL; // deal with that across all functions
        const Rcomplex *restrict pna = nonna ? COMPLEX(na) : NULL;
        for (ssize_t j=0; j<l; ++j) {
          pans[p[j]] = nonna ? pna[p[j] & bmask] : NA_CPLX;
        }
      }
    } break;
    case STRSXP: {
      const SEXP *restrict pouts = STRING_PTR(outs);
      for (ssize_t j=0; j<len2; ++j) {
        idx = imask ? j : p[j];
        if (pcons[idx]==1) {
          SET_STRING_ELT(ans, idx, pouts[idx & amask]);
        } else {
          p[l++] = idx;
        }
      }
      if (i==dn) {
        ssize_t bmask = len_na>1 ? SSIZE_MAX : 0;
        const SEXP *restrict pna = nonna ? STRING_PTR(na) : NULL;
        for (ssize_t j=0; j<l; ++j) {
          SET_STRING_ELT(ans, p[j], nonna ? pna[p[j] & bmask] : NA_STRING);
        }
      }
    } break;
    case VECSXP: {
      const SEXP *restrict pouts = SEXPPTR_RO(outs);
      for (ssize_t j=0; j<len2; ++j) {
        idx = imask ? j : p[j];
        if (pcons[idx]==1) {
          SET_VECTOR_ELT(ans, idx, pouts[idx & amask]);
        } else {
          p[l++] = idx;
        }
      }
      if (i==dn) {
        ssize_t bmask = len_na>1 ? SSIZE_MAX : 0;
        const SEXP *restrict pna = nonna ? SEXPPTR_RO(na) : NULL;
        for (ssize_t j=0; j<l; ++j) {
          if (nonna) {
            SET_VECTOR_ELT(ans, p[j], pna[p[j] & bmask]);  
          }
        }
      }
    } break;
    default:
      error("Type %s is not supported.", type2char(type1));
    }
    if (l==0) {
      break;
    }
    len2 = l;
  }
  free(p);
  UNPROTECT(nprotect);
  return ans;
}
