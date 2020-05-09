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

#define IIF_LOOP(a, b, n)   OMP_PARALLEL_FOR(nth)                                                  \
                            for (ssize_t i=0; i<len_l; ++i) {                                      \
                              pans[i] = pl[i]==0 ? b : (pl[i]==1 ? a : n);                         \
                            }                                                                      \

#define IIF_LOGIC(x) if(len_a>1) {                                                                 \
                      if(len_b>1) {                                                                \
                        if(len_na>1) {                                                             \
                           IIF_LOOP(pa[i], pb[i], pna[i])                                          \
                        } else {                                                                   \
                          IIF_LOOP(pa[i], pb[i], pna[0])                                           \
                        }                                                                          \
                      } else {                                                                     \
                        if(len_na>1) {                                                             \
                          IIF_LOOP(pa[i], pb[0], pna[i])                                           \
                        } else {                                                                   \
                          IIF_LOOP(pa[i], pb[0], pna[0])                                           \
                        }                                                                          \
                      }                                                                            \
                      } else {                                                                     \
                        if(len_b>1) {                                                              \
                          if(len_na>1) {                                                           \
                            IIF_LOOP(pa[0], pb[i], pna[i])                                         \
                          } else {                                                                 \
                            IIF_LOOP(pa[0], pb[i], pna[0])                                         \
                          }                                                                        \
                        } else {                                                                   \
                          if(len_na>1) {                                                           \
                            IIF_LOOP(pa[0], pb[0], pna[i])                                         \
                          } else {                                                                 \
                            IIF_LOOP(pa[0], pb[0], pna[0])                                         \
                          }                                                                        \
                        }                                                                          \
                      }                                                                            \
                      } else {                                                                     \
                        if(len_a>1) {                                                              \
                          if(len_b>1) {                                                            \
                            IIF_LOOP(pa[i], pb[i], x)                                              \
                          } else {                                                                 \
                            IIF_LOOP(pa[i], pb[0], x)                                              \
                          }                                                                        \
                        } else {                                                                   \
                          if(len_b>1) {                                                            \
                            IIF_LOOP(pa[0], pb[i], x)                                              \
                          } else {                                                                 \
                            IIF_LOOP(pa[0], pb[0], x)                                              \
                          }                                                                        \
                        }                                                                          \

SEXP iifR(SEXP l, SEXP a, SEXP b, SEXP na, SEXP tprom, SEXP nthreads) {
  if (!isLogical(l)) {
    error("Argument 'test' must be logical.");
  }
  if (isS4(a) || isS4(b)) {
    error("S4 class objects are not supported.");
  }
  if (!IS_BOOL(tprom)) {
    error("Argument 'tprom' must be either FALSE or TRUE and length 1.");
  }
  int nth = asInteger(nthreads);
  nth = nth > max_thread ? max_thread : (nth < min_thread ? min_thread : nth); //revisit this
  const R_xlen_t len_l  = xlength(l);
  const R_xlen_t len_a  = xlength(a);
  const R_xlen_t len_b  = xlength(b);
  const R_xlen_t len_na = xlength(na);
  SEXPTYPE ta = UTYPEOF(a);
  SEXPTYPE tb = UTYPEOF(b);
  SEXPTYPE tn = UTYPEOF(na);
  const bool na_non_null = !isNull(na);
  const bool tp = LOGICAL(tprom)[0] == 0;
  int nprotect = 0;
  if (ta != tb) {
    if (tp) {
      error("'yes' is of type %s but 'no' is of type %s. Please make sure that both arguments have the same type.", type2char(ta), type2char(tb));
    } else {
      if(IS_VALID_TYPE(ta) && IS_VALID_TYPE(tb)) {
        if(ta < tb) {
          SEXP tmp = PROTECT(coerceVector(a, tb)); nprotect++;
          a = tmp;
          ta = tb;
          copyMostAttrib(b, a);
        } else {
          SEXP tmp = PROTECT(coerceVector(b, ta)); nprotect++;
          b = tmp;
          tb = ta;
          copyMostAttrib(a, b);
        }
      } else {
        if (!IS_VALID_TYPE(ta)) {
          error("Type %s (argument 'yes') is not supported.", type2char(ta));
        } else {
          error("Type %s (argument 'no') is not supported.", type2char(tb));
        }
      }
    }
  }
  if (len_a!=1 && len_a!=len_l) {
    error("Length of 'yes' is %zu but must be 1 or length of 'test' (%zu).", len_a, len_l);
  }
  if (len_b!=1 && len_b!=len_l) {
    error("Length of 'no' is %zu but must be 1 or length of 'test' (%zu).", len_b, len_l);
  }
  SEXP class_a = PROTECT(getAttrib(a, R_ClassSymbol)); nprotect++;
  SEXP class_b = PROTECT(getAttrib(b, R_ClassSymbol)); nprotect++;
  const bool same_class = !R_compute_identical(class_a, class_b, 0);
  if (tp) {
    if (same_class) {
      error("'yes' has different class than 'no'. Please make sure that both arguments have the same class."); 
    }
  } else {
    if (!same_class) {
      copyMostAttrib(a, b);
    }
  }
  if (na_non_null) {
    if (len_na!=1 && len_na!=len_l) {
      error("Length of 'na' is %zu but must be 1 or length of 'test' (%zu).", len_na, len_l);
    }
    if (tn != ta) {
      if (tp) {
        error("'yes' is of type %s but 'na' is of type %s. Please make sure that both arguments have the same type.", type2char(ta), type2char(tn));
      } else if (tn > ta) {
        error("Type of 'na' (%s) is higher than %s (highest type of 'yes' and 'no'). Please make sure that it is at lower or the same.", type2char(tn), type2char(ta));  
	    } else {
		    SEXP tmp = PROTECT(coerceVector(na, ta)); nprotect++;
        na = tmp;
        tn = ta;
        copyMostAttrib(a, na);
      }
    }
    SEXP class_na = PROTECT(getAttrib(na, R_ClassSymbol)); nprotect++;
    if (tp) {
      if (!R_compute_identical(class_a, class_na, 0)) {
        error("'yes' has different class than 'na'. Please make sure that both arguments have the same class.");  
      }
    } else {
      if (!R_compute_identical(class_a, class_na, 0)) {
        copyMostAttrib(a, na); 
      }
    }
  }
  if (isFactor(a)) {
    SEXP level_a = PROTECT(getAttrib(a, R_LevelsSymbol)); nprotect++;
    SEXP level_b = PROTECT(getAttrib(b, R_LevelsSymbol)); nprotect++;
    if (!R_compute_identical(level_a, level_b, 0)) {
      error("'yes' and 'no' are both type factor but their levels are different.");
    }
    if (na_non_null) {
      SEXP level_na = PROTECT(getAttrib(na, R_LevelsSymbol)); nprotect++;
      if (!R_compute_identical(level_a, level_na, 0)) {
        error("'yes' and 'na' are both type factor but their levels are different.");
      }
    }
  }
  const int *restrict pl = LOGICAL(l);
  SEXP ans = PROTECT(allocVector(ta, len_l)); nprotect++;
  copyMostAttrib(a, ans);
  switch(ta) {
  case LGLSXP: {
    int *restrict pans = LOGICAL(ans);
    const int *restrict pa = LOGICAL(a);
    const int *restrict pb = LOGICAL(b);
    if(na_non_null) {
      const int *restrict pna = LOGICAL(na);
      IIF_LOGIC(NA_LOGICAL)
    }
  } break;
  case INTSXP: {
    int *restrict pans = INTEGER(ans);
    const int *restrict pa = INTEGER(a);
    const int *restrict pb = INTEGER(b);
    if(na_non_null) {
      const int *restrict pna = INTEGER(na);
      IIF_LOGIC(NA_INTEGER)
    }
  } break;
  case REALSXP: {
    double *restrict pans = REAL(ans);
    const double *restrict pa = REAL(a);
    const double *restrict pb = REAL(b);
    if(na_non_null) {
      const double *restrict pna = REAL(na);
      IIF_LOGIC(NA_REAL)
    }
  } break;
  case CPLXSXP : {
    Rcomplex *restrict pans = COMPLEX(ans);
    const Rcomplex *restrict pa = COMPLEX(a);
    const Rcomplex *restrict pb = COMPLEX(b);
    Rcomplex NA_CPLX; NA_CPLX.r = NA_REAL; NA_CPLX.i = NA_REAL; // deal with that across all functions
    if(na_non_null) {
      const Rcomplex *restrict pna = COMPLEX(na);
      IIF_LOGIC(NA_CPLX)
    }
  } break;
  case STRSXP : {
    const ssize_t amask = len_a>1 ? SSIZE_MAX : 0;
    const ssize_t bmask = len_b>1 ? SSIZE_MAX : 0;
    const ssize_t namask = len_na>1 ? SSIZE_MAX : 0;
    const SEXP *restrict pa = STRING_PTR(a);
    const SEXP *restrict pb = STRING_PTR(b);
    const SEXP *restrict pna = na_non_null ? STRING_PTR(na) : NULL;
    for (ssize_t i=0; i<len_l; ++i) {
      switch(pl[i]) {
        case 0 : SET_STRING_ELT(ans, i, pb[i & bmask]); break;
        case 1 : SET_STRING_ELT(ans, i, pa[i & amask]); break;
        default : SET_STRING_ELT(ans, i, na_non_null ? pna[i & namask] : NA_STRING);
      }
    }
  } break;
  case VECSXP : {
    const ssize_t amask = len_a>1 ? SSIZE_MAX : 0;
    const ssize_t bmask = len_b>1 ? SSIZE_MAX : 0;
    const ssize_t namask = len_na>1 ? SSIZE_MAX : 0;
    const SEXP *restrict pa  = SEXPPTR_RO(a);
    const SEXP *restrict pb  = SEXPPTR_RO(b);
    const SEXP *restrict pna = SEXPPTR_RO(na);
    for (ssize_t i=0; i<len_l; ++i) {
      if (pl[i]==NA_LOGICAL) {
        if (na_non_null) {
          SET_VECTOR_ELT(ans, i, pna[i & namask]);
        }
        continue;
      }
      if (pl[i]==0) {
        SET_VECTOR_ELT(ans, i, pb[i & bmask]);
      } else {
        SET_VECTOR_ELT(ans, i, pa[i & amask]);
      }
    }
  } break;
  default:
    error("Type %s is not supported.", type2char(ta));
  }
  SEXP l_names = PROTECT(getAttrib(l, R_NamesSymbol)); nprotect++;
  if (!isNull(l_names)) {
    setAttrib(ans, R_NamesSymbol, l_names);
  }
  UNPROTECT(nprotect);
  return ans;
}
