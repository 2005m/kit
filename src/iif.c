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

#define IIF_LOOP(a, b, n)   OMP_PARALLEL_FOR(nth)                                                  \
for (ssize_t i=0; i<len_l; ++i) {                                                                  \
  pans[i] = pl[i]==0 ? b : (pl[i]==1 ? a : n);                                                     \
}                                                                                                  \

#define IIF_LOGIC(x) if(len_a>1) {                                                                 \
if(len_b>1) {                                                                                      \
  if(len_na>1) {                                                                                   \
    IIF_LOOP(pa[i], pb[i], pna[i])                                                                 \
  } else {                                                                                         \
    IIF_LOOP(pa[i], pb[i], pna[0])                                                                 \
  }                                                                                                \
} else {                                                                                           \
  if(len_na>1) {                                                                                   \
    IIF_LOOP(pa[i], pb[0], pna[i])                                                                 \
  } else {                                                                                         \
    IIF_LOOP(pa[i], pb[0], pna[0])                                                                 \
  }                                                                                                \
}                                                                                                  \
} else {                                                                                           \
  if(len_b>1) {                                                                                    \
    if(len_na>1) {                                                                                 \
      IIF_LOOP(pa[0], pb[i], pna[i])                                                               \
    } else {                                                                                       \
      IIF_LOOP(pa[0], pb[i], pna[0])                                                               \
    }                                                                                              \
  } else {                                                                                         \
    if(len_na>1) {                                                                                 \
      IIF_LOOP(pa[0], pb[0], pna[i])                                                               \
    } else {                                                                                       \
      IIF_LOOP(pa[0], pb[0], pna[0])                                                               \
    }                                                                                              \
  }                                                                                                \
}                                                                                                  \
} else {                                                                                           \
  if(len_a>1) {                                                                                    \
    if(len_b>1) {                                                                                  \
      IIF_LOOP(pa[i], pb[i], x)                                                                    \
    } else {                                                                                       \
      IIF_LOOP(pa[i], pb[0], x)                                                                    \
    }                                                                                              \
  } else {                                                                                         \
    if(len_b>1) {                                                                                  \
      IIF_LOOP(pa[0], pb[i], x)                                                                    \
    } else {                                                                                       \
      IIF_LOOP(pa[0], pb[0], x)                                                                    \
    }                                                                                              \
  }                                                                                                \

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
      SET_STRING_ELT(ans, i, pl[i] == 1 ? pa[i & amask] : (pl[i] == 0 ? pb[i & bmask] :(na_non_null ? pna[i & namask] : NA_STRING)));
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
      SET_VECTOR_ELT(ans, i, pl[i]==0 ? pb[i & bmask] : pa[i & amask]);
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

SEXP nifR(SEXP na, SEXP rho, SEXP args) {
  R_len_t n=length(args);
  if (n % 2) {
    error("Received %d inputs; please supply an even number of arguments in ... "
            "consisting of logical condition, resulting value pairs (in that order). "
            "Note that argument 'default' must be named explicitly (e.g.: default=0)", n);
  }
  SEXP cons0 = PROTECT(eval(PTR_ETL(args,0),rho));
  SEXP out0 = PROTECT(eval(PTR_ETL(args,1),rho));
  if (isS4(out0)) {
    error("S4 class objects are not supported.");
  }
  SEXP out0c = PROTECT(getAttrib(out0, R_ClassSymbol));
  SEXP out0l = PROTECT(getAttrib(out0, R_LevelsSymbol));
  SEXP cons = R_NilValue, outs = R_NilValue;
  PROTECT_INDEX Icons, Iouts;
  PROTECT_WITH_INDEX(cons, &Icons);
  PROTECT_WITH_INDEX(outs, &Iouts);
  const SEXPTYPE type0 = UTYPEOF(out0);
  const R_xlen_t len0 = xlength(cons0);
  const R_xlen_t len_na = xlength(na);
  R_xlen_t len2 = len0;
  const bool nonna = !isNull(na);
  const bool ifact = isFactor(out0);
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
  SEXP ans = PROTECT(allocVector(type0, len0));
  SEXP tracker = PROTECT(allocVector(INTSXP, len0));
  int *restrict p = INTEGER(tracker);
  copyMostAttrib(out0, ans);
  n = n/2;
  const int dn = n-1;
  for (int i=0; i<n; ++i) {
    const bool imask = i==0;
    REPROTECT(cons = eval(PTR_ETL(args, 2*i), rho), Icons);
    REPROTECT(outs = eval(PTR_ETL(args, 2*i+1), rho), Iouts);
    if (isS4(outs)) {
      error("S4 class objects are not supported.");
    }
    if (!isLogical(cons)) {
      error("Argument #%d must be logical.", 2*i+1);
    }
    const int *restrict pcons = LOGICAL(cons);
    R_xlen_t l = 0;
    SEXPTYPE type1 = UTYPEOF(outs);
    if (!imask) {
      if (xlength(cons) != len0) {
        error("Argument #%d has a different length than argument #1. "
                "Please make sure all logical conditions have the same length.",
                i*2+1);
      }
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
    const R_xlen_t len1 = xlength(outs);
    if (len1 != len0 && len1 != 1) {
      error("Length of output value #%d must either be 1 or length of logical condition.", i*2+2);
    }
    const ssize_t amask = len1>1 ? SSIZE_MAX : 0;
    switch(type1) {
    case LGLSXP: {
      const int *restrict pouts = LOGICAL(outs);
      int *restrict pans = LOGICAL(ans);
      if (imask) {
        for (ssize_t j=0; j<len2; ++j) {
          if (pcons[j]==1) {
            pans[j] = pouts[j & amask];
          } else {
            p[l++] = j;
          }
        }
      } else {
        for (ssize_t j=0; j<len2; ++j) {
          if (pcons[p[j]]==1) {
            pans[p[j]] = pouts[p[j] & amask];
          } else {
            p[l++] = p[j];
          }
        }
      }
      if (i==dn) {
        const ssize_t bmask = len_na>1 ? SSIZE_MAX : 0;
        const int *restrict pna = nonna ? LOGICAL(na) : NULL;
        for (ssize_t j=0; j<l; ++j) {
          pans[p[j]] = nonna ? pna[p[j] & bmask] : NA_LOGICAL;
        }
      }
    } break;
    case INTSXP: {
      const int *restrict pouts = INTEGER(outs);
      int *restrict pans = INTEGER(ans);
      if (imask) {
        for (ssize_t j=0; j<len2; ++j) {
          if (pcons[j]==1) {
            pans[j] = pouts[j & amask];
          } else {
            p[l++] = j;
          }
        }
      } else {
        for (ssize_t j=0; j<len2; ++j) {
          if (pcons[p[j]]==1) {
            pans[p[j]] = pouts[p[j] & amask];
          } else {
            p[l++] = p[j];
          }
        }
      }
      if (i==dn) {
        const ssize_t bmask = len_na>1 ? SSIZE_MAX : 0;
        const int *restrict pna = nonna ? INTEGER(na) : NULL;
        for (ssize_t j=0; j<l; ++j) {
          pans[p[j]] = nonna ? pna[p[j] & bmask] : NA_INTEGER;
        }
      }
    } break;
    case REALSXP: {
      const double *restrict pouts = REAL(outs);
      double *restrict pans = REAL(ans);
      if (imask) {
        for (ssize_t j=0; j<len2; ++j) {
          if (pcons[j]==1) {
            pans[j] = pouts[j & amask];
          } else {
            p[l++] = j;
          }
        }
      } else {
        for (ssize_t j=0; j<len2; ++j) {
          if (pcons[p[j]]==1) {
            pans[p[j]] = pouts[p[j] & amask];
          } else {
            p[l++] = p[j];
          }
        }
      }
      if (i==dn) {
        const ssize_t bmask = len_na>1 ? SSIZE_MAX : 0;
        const double *restrict pna = nonna ? REAL(na) : NULL;
        for (ssize_t j=0; j<l; ++j) {
          pans[p[j]] = nonna ? pna[p[j] & bmask] : NA_REAL;
        }
      }
    } break;
    case CPLXSXP: {
      const Rcomplex *restrict pouts = COMPLEX(outs);
      Rcomplex *restrict pans = COMPLEX(ans);
      if (imask) {
        for (ssize_t j=0; j<len2; ++j) {
          if (pcons[j]==1) {
            pans[j] = pouts[j & amask];
          } else {
            p[l++] = j;
          }
        }
      } else {
        for (ssize_t j=0; j<len2; ++j) {
          if (pcons[p[j]]==1) {
            pans[p[j]] = pouts[p[j] & amask];
          } else {
            p[l++] = p[j];
          }
        }
      }
      if (i==dn) {
        const ssize_t bmask = len_na>1 ? SSIZE_MAX : 0;
        Rcomplex NA_CPLX; NA_CPLX.r = NA_REAL; NA_CPLX.i = NA_REAL; // deal with that across all functions
        const Rcomplex *restrict pna = nonna ? COMPLEX(na) : NULL;
        for (ssize_t j=0; j<l; ++j) {
          pans[p[j]] = nonna ? pna[p[j] & bmask] : NA_CPLX;
        }
      }
    } break;
    case STRSXP: {
      const SEXP *restrict pouts = STRING_PTR(outs);
      if (imask) {
        for (ssize_t j=0; j<len2; ++j) {
          if (pcons[j]==1) {
            SET_STRING_ELT(ans, j, pouts[j & amask]);
          } else {
            p[l++] = j;
          }
        }
      } else {
        for (ssize_t j=0; j<len2; ++j) {
          if (pcons[p[j]]==1) {
            SET_STRING_ELT(ans, p[j], pouts[p[j] & amask]);
          } else {
            p[l++] = p[j];
          }
        }
      }
      if (i==dn) {
        const ssize_t bmask = len_na>1 ? SSIZE_MAX : 0;
        const SEXP *restrict pna = nonna ? STRING_PTR(na) : NULL;
        for (ssize_t j=0; j<l; ++j) {
          SET_STRING_ELT(ans, p[j], nonna ? pna[p[j] & bmask] : NA_STRING);
        }
      }
    } break;
    case VECSXP: {
      const SEXP *restrict pouts = SEXPPTR_RO(outs);
      if (imask) {
        for (ssize_t j=0; j<len2; ++j) {
          if (pcons[j]==1) {
            SET_VECTOR_ELT(ans, j, pouts[j & amask]);
          } else {
            p[l++] = j;
          }
        }
      } else {
        for (ssize_t j=0; j<len2; ++j) {
          if (pcons[p[j]]==1) {
            SET_VECTOR_ELT(ans, p[j], pouts[p[j] & amask]);
          } else {
            p[l++] = p[j];
          }
        }
      }
      if (i==dn) {
        const ssize_t bmask = len_na>1 ? SSIZE_MAX : 0;
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
  UNPROTECT(8);
  return ans;
}

SEXP nifInternalR(SEXP na, SEXP rho, SEXP args) { // # nocov start
  R_len_t n=length(args);
  if (n % 2) {
    error("Received %d inputs; please supply an even number of arguments in ... "
            "consisting of logical condition, resulting value pairs (in that order). "
            "Note that argument 'default' must be named explicitly (e.g.: default=0)", n);
  }
  SEXP cons0 = PROTECT(PTR_ETL(args,0));
  SEXP out0 = PROTECT(PTR_ETL(args,1));
  if (isS4(out0)) {
      error("S4 class objects are not supported.");
  }
  SEXP out0c = PROTECT(getAttrib(out0, R_ClassSymbol));
  SEXP out0l = PROTECT(getAttrib(out0, R_LevelsSymbol));
  SEXP cons = R_NilValue, outs = R_NilValue;
  PROTECT_INDEX Icons, Iouts;
  PROTECT_WITH_INDEX(cons, &Icons);
  PROTECT_WITH_INDEX(outs, &Iouts);
  const SEXPTYPE type0 = UTYPEOF(out0);
  const R_xlen_t len0 = xlength(cons0);
  const R_xlen_t len_na = xlength(na);
  R_xlen_t len2 = len0;
  const bool nonna = !isNull(na);
  const bool ifact = isFactor(out0);
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
  SEXP ans = PROTECT(allocVector(type0, len0));
  SEXP tracker = PROTECT(allocVector(INTSXP, len0));
  int *restrict p = INTEGER(tracker);
  copyMostAttrib(out0, ans);
  n = n/2;
  const int dn = n-1;
  for (int i=0; i<n; ++i) {
    const bool imask = i==0;
    REPROTECT(cons = PTR_ETL(args, 2*i), Icons);
    REPROTECT(outs = PTR_ETL(args, 2*i+1), Iouts);
      if (isS4(outs)) {
      error("S4 class objects are not supported.");
    }
    if (!isLogical(cons)) {
      error("Argument #%d must be logical.", 2*i+1);
    }
    const int *restrict pcons = LOGICAL(cons);
    R_xlen_t l = 0;
    SEXPTYPE type1 = UTYPEOF(outs);
    if (!imask) {
      if (xlength(cons) != len0) {
        error("Argument #%d has a different length than argument #1. "
                "Please make sure all logical conditions have the same length.",
                i*2+1);
      }
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
    const R_xlen_t len1 = xlength(outs);
    if (len1 != len0 && len1 != 1) {
      error("Length of output value #%d must either be 1 or length of logical condition.", i*2+2);
    }
    const ssize_t amask = len1>1 ? SSIZE_MAX : 0;
    switch(type1) {
    case LGLSXP: {
      const int *restrict pouts = LOGICAL(outs);
      int *restrict pans = LOGICAL(ans);
      if (imask) {
        for (ssize_t j=0; j<len2; ++j) {
          if (pcons[j]==1) {
            pans[j] = pouts[j & amask];
          } else {
            p[l++] = j;
          }
        }
      } else {
        for (ssize_t j=0; j<len2; ++j) {
          if (pcons[p[j]]==1) {
            pans[p[j]] = pouts[p[j] & amask];
          } else {
            p[l++] = p[j];
          }
        }
      }
      if (i==dn) {
        const ssize_t bmask = len_na>1 ? SSIZE_MAX : 0;
        const int *restrict pna = nonna ? LOGICAL(na) : NULL;
        for (ssize_t j=0; j<l; ++j) {
          pans[p[j]] = nonna ? pna[p[j] & bmask] : NA_LOGICAL;
        }
      }
    } break;
    case INTSXP: {
      const int *restrict pouts = INTEGER(outs);
      int *restrict pans = INTEGER(ans);
      if (imask) {
        for (ssize_t j=0; j<len2; ++j) {
          if (pcons[j]==1) {
            pans[j] = pouts[j & amask];
          } else {
            p[l++] = j;
          }
        }
      } else {
        for (ssize_t j=0; j<len2; ++j) {
          if (pcons[p[j]]==1) {
            pans[p[j]] = pouts[p[j] & amask];
          } else {
            p[l++] = p[j];
          }
        }
      }
      if (i==dn) {
        const ssize_t bmask = len_na>1 ? SSIZE_MAX : 0;
        const int *restrict pna = nonna ? INTEGER(na) : NULL;
        for (ssize_t j=0; j<l; ++j) {
          pans[p[j]] = nonna ? pna[p[j] & bmask] : NA_INTEGER;
        }
      }
    } break;
    case REALSXP: {
      const double *restrict pouts = REAL(outs);
      double *restrict pans = REAL(ans);
      if (imask) {
        for (ssize_t j=0; j<len2; ++j) {
          if (pcons[j]==1) {
            pans[j] = pouts[j & amask];
          } else {
            p[l++] = j;
          }
        }
      } else {
        for (ssize_t j=0; j<len2; ++j) {
          if (pcons[p[j]]==1) {
            pans[p[j]] = pouts[p[j] & amask];
          } else {
            p[l++] = p[j];
          }
        }
      }
      if (i==dn) {
        const ssize_t bmask = len_na>1 ? SSIZE_MAX : 0;
        const double *restrict pna = nonna ? REAL(na) : NULL;
        for (ssize_t j=0; j<l; ++j) {
          pans[p[j]] = nonna ? pna[p[j] & bmask] : NA_REAL;
        }
      }
    } break;
    case CPLXSXP: {
      const Rcomplex *restrict pouts = COMPLEX(outs);
      Rcomplex *restrict pans = COMPLEX(ans);
      if (imask) {
        for (ssize_t j=0; j<len2; ++j) {
          if (pcons[j]==1) {
            pans[j] = pouts[j & amask];
          } else {
            p[l++] = j;
          }
        }
      } else {
        for (ssize_t j=0; j<len2; ++j) {
          if (pcons[p[j]]==1) {
            pans[p[j]] = pouts[p[j] & amask];
          } else {
            p[l++] = p[j];
          }
        }
      }
      if (i==dn) {
        const ssize_t bmask = len_na>1 ? SSIZE_MAX : 0;
        Rcomplex NA_CPLX; NA_CPLX.r = NA_REAL; NA_CPLX.i = NA_REAL; // deal with that across all functions
        const Rcomplex *restrict pna = nonna ? COMPLEX(na) : NULL;
        for (ssize_t j=0; j<l; ++j) {
          pans[p[j]] = nonna ? pna[p[j] & bmask] : NA_CPLX;
        }
      }
    } break;
    case STRSXP: {
      const SEXP *restrict pouts = STRING_PTR(outs);
      if (imask) {
        for (ssize_t j=0; j<len2; ++j) {
          if (pcons[j]==1) {
            SET_STRING_ELT(ans, j, pouts[j & amask]);
          } else {
            p[l++] = j;
          }
        }
      } else {
        for (ssize_t j=0; j<len2; ++j) {
          if (pcons[p[j]]==1) {
            SET_STRING_ELT(ans, p[j], pouts[p[j] & amask]);
          } else {
            p[l++] = p[j];
          }
        }
      }
      if (i==dn) {
        const ssize_t bmask = len_na>1 ? SSIZE_MAX : 0;
        const SEXP *restrict pna = nonna ? STRING_PTR(na) : NULL;
        for (ssize_t j=0; j<l; ++j) {
          SET_STRING_ELT(ans, p[j], nonna ? pna[p[j] & bmask] : NA_STRING);
        }
      }
    } break;
    case VECSXP: {
      const SEXP *restrict pouts = SEXPPTR_RO(outs);
      if (imask) {
        for (ssize_t j=0; j<len2; ++j) {
          if (pcons[j]==1) {
            SET_VECTOR_ELT(ans, j, pouts[j & amask]);
          } else {
            p[l++] = j;
          }
        }
      } else {
        for (ssize_t j=0; j<len2; ++j) {
          if (pcons[p[j]]==1) {
            SET_VECTOR_ELT(ans, p[j], pouts[p[j] & amask]);
          } else {
            p[l++] = p[j];
          }
        }
      }
      if (i==dn) {
        const ssize_t bmask = len_na>1 ? SSIZE_MAX : 0;
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
  UNPROTECT(8);
  return ans;
} // # nocov end
