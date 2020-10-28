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

SEXP vswitchR(SEXP x, SEXP values, SEXP outputs, SEXP na, SEXP nthreads, SEXP chkenc) {
  if (!IS_BOOL(chkenc)) {
    error("Argument 'checkEnc' must be TRUE or FALSE and length 1.");
  }
  if (isS4(x)) {
    error("S4 class objects for argument 'x' are not supported.");
  }
  if (isS4(values)) {
    error("S4 class objects for argument 'values' are not supported.");
  }
  if (isS4(na)) {
    error("S4 class objects for argument 'na' are not supported.");
  }
  int nth = asInteger(nthreads);
  nth = nth > max_thread ? max_thread : (nth < min_thread ? min_thread : nth); //revisit this
  const R_xlen_t len_x = xlength(x);
  const R_xlen_t len_values = xlength(values);
  const R_xlen_t len_outputs = xlength(outputs);
  const R_xlen_t len_na = xlength(na);
  SEXPTYPE type_x = UTYPEOF(x);
  SEXPTYPE type_na = UTYPEOF(na);
  SEXPTYPE type_values = UTYPEOF(values);
  const bool pchkenc = asLogical(chkenc);
  const bool nonna = !isNull(na);
  const bool nalen = (len_na==1);
  if (!len_values || !len_x) {
    error("Argument 'x' and 'values' connot be zero-length vector.");
  }
  if (len_values != len_outputs) {
    error("Length of 'values' and 'outputs' are different. Please make sure they are the same.");
  }
  if (type_x != type_values) {
    error("Type of 'x' and 'values' are different. Please make sure they are the same.");
  }
  if (nonna && len_x != len_na && !nalen) {
    error("Length of 'na'  is different than 1 and length of 'x'. "
            "Please make length of 'na' is 1 or length of 'x'.");
  }
  if (!R_compute_identical(PROTECT(getAttrib(x, R_ClassSymbol)),
                           PROTECT(getAttrib(values, R_ClassSymbol)), 0)) {
    error("Argument 'x' and 'values' must have same class.");
  }
  UNPROTECT(2);
  if (isFactor(x)) {
    if (!R_compute_identical(PROTECT(getAttrib(x, R_LevelsSymbol)),
                             PROTECT(getAttrib(values, R_LevelsSymbol)), 0)) {
      error("Argument 'x' and 'values' are both factor but their levels are different.");
    }
    UNPROTECT(2);
  }
  int nprotect=0;
  SEXP ans = R_NilValue;
  SEXPTYPE type_o = UTYPEOF(outputs);
  SEXP xans = R_NilValue, vans = R_NilValue;
  bool utfcon = false;
  if (pchkenc && type_x == STRSXP) {
    if (isMixEnc(x) || isMixEnc(values) || getCharCE(STRING_PTR(x)[0]) != getCharCE(STRING_PTR(values)[0])) {
      xans = PROTECT(enc2UTF8(x)); nprotect++;
      vans = PROTECT(enc2UTF8(values)); nprotect++;
      utfcon = true;
    }
  }
  if (type_o == VECSXP) {
    const SEXP *restrict po = DATAPTR_RO(outputs);
    SEXPTYPE type_outputs = UTYPEOF(po[0]);
    if (nonna && type_na != type_outputs) {
      error("Type of 'na' and 'outputs' are different. Please make sure they are the same.");
    }
    SEXP po0_class = PROTECT(getAttrib(po[0], R_ClassSymbol)); nprotect++;
    SEXP po0_level = PROTECT(getAttrib(po[0], R_LevelsSymbol)); nprotect++;
    const bool is_po_fact = isFactor(po[0]);
    if (nonna) {
      if (!R_compute_identical(po0_class, PROTECT(getAttrib(na, R_ClassSymbol)), 0)) {
        error("Argument 'na' and items of 'outputs' must have same class.");
      }
      UNPROTECT(1);
      if (is_po_fact) {
        if (!R_compute_identical(po0_level, PROTECT(getAttrib(na, R_LevelsSymbol)), 0)) {
          error("Argument 'na' and items of 'outputs' are both factor but their levels are different.");
        }
        UNPROTECT(1);
      } 
    }
    ssize_t amask[len_values];
    for (ssize_t i=0; i<len_values;++i) {
      if (isS4(po[i])) {
        error("S4 class objects for argument 'outputs' (item %zu) are not supported.", i+1);
      }
      R_xlen_t len_i = xlength(po[i]);
      if (len_i != len_x && len_i != 1) {
        error("Length of item %zu of 'output' is different than 1 and length of 'x'. "
                "Please make sure that all items of 'output' have length 1 or length of 'x'(%zu).",
                i+1, len_x);
      }
      if (!R_compute_identical(po0_class, PROTECT(getAttrib(po[i], R_ClassSymbol)), 0)) {
        error("Items 1 and  %zu of 'outputs' must have same class.", i+1);
      }
      UNPROTECT(1);
      if (is_po_fact) {
        if (!R_compute_identical(po0_level, PROTECT(getAttrib(po[i], R_LevelsSymbol)), 0)) {
          error("Items 1 and  %zu of 'outputs' are both factor but their levels are different.", i+1);
        }
        UNPROTECT(1);
      }
      amask[i] = len_i>1 ? SSIZE_MAX : 0;
    }
    ans = PROTECT(allocVector(type_outputs, len_x)); nprotect++;
    copyMostAttrib(po[0], ans);
    switch(type_outputs) {
    /*
     *  This part is for LOGICAL
     */
    case LGLSXP:{
      int *restrict pans = LOGICAL(ans);
      const int *restrict pna = nonna ? LOGICAL(na) : NULL; 
      ssize_t namask = len_na>1 ? SSIZE_MAX : 0;
      OMP_PARALLEL_FOR(nth)
      for (ssize_t j=0; j<len_x; ++j) {
        pans[j]= nonna ? pna[j & namask] : NA_LOGICAL;
      }
      switch(type_x) {
      case LGLSXP: {
        const int *restrict px = LOGICAL(x);
        const int *restrict pvalues = LOGICAL(values);
        for (ssize_t i=0; i<len_values; ++i) {
          int *ppo = LOGICAL(po[i]);
          OMP_PARALLEL_FOR(nth)
          for (ssize_t j=0; j<len_x; ++j) {
            if (px[j]==pvalues[i]) {
              pans[j]= ppo[j & amask[i]];
            }
          }
        }
      } break;
      case INTSXP: {
        const int *restrict px = INTEGER(x);
        const int *restrict pvalues = INTEGER(values);
        for (ssize_t i=0; i<len_values; ++i) {
          int *ppo = LOGICAL(po[i]);
          OMP_PARALLEL_FOR(nth)
          for (ssize_t j=0; j<len_x; ++j) {
            if (px[j]==pvalues[i]) {
              pans[j]= ppo[j & amask[i]];
            }
          }
        }
      } break;
      case REALSXP: {
        const double *restrict px = REAL(x);
        const double *restrict pvalues = REAL(values);
        for (ssize_t i=0; i<len_values; ++i) {
          int *ppo = LOGICAL(po[i]);
          OMP_PARALLEL_FOR(nth)
          for (ssize_t j=0; j<len_x; ++j) {
            if (px[j]==pvalues[i]) {
              pans[j]= ppo[j & amask[i]];
            }
          }
        }
      } break;
      case CPLXSXP: {
        const Rcomplex *restrict px = COMPLEX(x);
        const Rcomplex *restrict pvalues = COMPLEX(values);
        for (ssize_t i=0; i<len_values; ++i) {
          int *ppo = LOGICAL(po[i]);
          OMP_PARALLEL_FOR(nth)
          for (ssize_t j=0; j<len_x; ++j) {
            if (EQUAL_CPLX(px[j],pvalues[i])) {
              pans[j]= ppo[j & amask[i]];
            }
          }
        }
      } break;
      case STRSXP: {
        const SEXP *restrict px = STRING_PTR(utfcon ? xans : x);
        const SEXP *restrict pvalues = STRING_PTR(utfcon ? vans : values);
        for (ssize_t i=0; i<len_values; ++i) {
          int *ppo = LOGICAL(po[i]);
          OMP_PARALLEL_FOR(nth)
          for (ssize_t j=0; j<len_x; ++j) {
            if (px[j]==pvalues[i]) {
              pans[j]= ppo[j & amask[i]];
            }
          }
        }
      } break;
      case VECSXP: {
        const SEXP *restrict px = SEXPPTR_RO(x);
        const SEXP *restrict pvalues = SEXPPTR_RO(values);
        for (ssize_t i=0; i<len_values; ++i) {
          int *ppo = LOGICAL(po[i]);
          for (ssize_t j=0; j<len_x; ++j) {
            if (R_compute_identical(px[j], pvalues[i], 0)) {
              pans[j]= ppo[j & amask[i]];
            }
          }
        }
      } break;
      default:
        error("Type %s is not supported for argument 'x'.", type2char(type_x));
      }
    } break;
    /*
     *  This part is for INTEGER
     */
    case INTSXP:{
      int *restrict pans = INTEGER(ans);
      const int *restrict pna = nonna ? INTEGER(na) : NULL; 
      ssize_t namask = len_na>1 ? SSIZE_MAX : 0;
      OMP_PARALLEL_FOR(nth)
      for (ssize_t j=0; j<len_x; ++j) {
        pans[j]= nonna ? pna[j & namask] : NA_INTEGER;
      }
      switch(type_x) {
      case LGLSXP: {
        const int *restrict px = LOGICAL(x);
        const int *restrict pvalues = LOGICAL(values);
        for (ssize_t i=0; i<len_values; ++i) {
          int *ppo = INTEGER(po[i]);
          OMP_PARALLEL_FOR(nth)
          for (ssize_t j=0; j<len_x; ++j) {
            if (px[j]==pvalues[i]) {
              pans[j]= ppo[j & amask[i]];
            }
          }
        }
      } break;
      case INTSXP: {
        const int *restrict px = INTEGER(x);
        const int *restrict pvalues = INTEGER(values);
        for (ssize_t i=0; i<len_values; ++i) {
          int *ppo = INTEGER(po[i]);
          OMP_PARALLEL_FOR(nth)
          for (ssize_t j=0; j<len_x; ++j) {
            if (px[j]==pvalues[i]) {
              pans[j]= ppo[j & amask[i]];
            }
          }
        }
      } break;
      case REALSXP: {
        const double *restrict px = REAL(x);
        const double *restrict pvalues = REAL(values);
        for (ssize_t i=0; i<len_values; ++i) {
          int *ppo = INTEGER(po[i]);
          OMP_PARALLEL_FOR(nth)
          for (ssize_t j=0; j<len_x; ++j) {
            if (px[j]==pvalues[i]) {
              pans[j]= ppo[j & amask[i]];
            }
          }
        }
      } break;
      case CPLXSXP: {
        const Rcomplex *restrict px = COMPLEX(x);
        const Rcomplex *restrict pvalues = COMPLEX(values);
        for (ssize_t i=0; i<len_values; ++i) {
          int *ppo = INTEGER(po[i]);
          OMP_PARALLEL_FOR(nth)
          for (ssize_t j=0; j<len_x; ++j) {
            if (EQUAL_CPLX(px[j],pvalues[i])) {
              pans[j]= ppo[j & amask[i]];
            }
          }
        }
      } break;
      case STRSXP: {
        const SEXP *restrict px = STRING_PTR(utfcon ? xans : x);
        const SEXP *restrict pvalues = STRING_PTR(utfcon ? vans : values);
        for (ssize_t i=0; i<len_values; ++i) {
          int *ppo = INTEGER(po[i]);
          for (ssize_t j=0; j<len_x; ++j) {
            if (px[j]==pvalues[i]) {
              pans[j]= ppo[j & amask[i]];
            }
          }
        }
      } break;
      case VECSXP: {
        const SEXP *restrict px = SEXPPTR_RO(x);
        const SEXP *restrict pvalues = SEXPPTR_RO(values);
        for (ssize_t i=0; i<len_values; ++i) {
          int *ppo = INTEGER(po[i]);
          for (ssize_t j=0; j<len_x; ++j) {
            if (R_compute_identical(px[j], pvalues[i], 0)) {
              pans[j]= ppo[j & amask[i]];
            }
          }
        }
      } break;
      default:
        error("Type %s is not supported for argument 'x'.", type2char(type_x));
      }
    } break;
    /*
     *  This part is for REAL
     */
    case REALSXP:{
      double *restrict pans = REAL(ans);
      const double *restrict pna = nonna ? REAL(na) : NULL; 
      ssize_t namask = len_na>1 ? SSIZE_MAX : 0;
      OMP_PARALLEL_FOR(nth)
      for (ssize_t j=0; j<len_x; ++j) {
        pans[j]= nonna ? pna[j & namask] : NA_REAL;
      }
      switch(type_x) {
      case LGLSXP: {
        const int *restrict px = LOGICAL(x);
        const int *restrict pvalues = LOGICAL(values);
        for (ssize_t i=0; i<len_values; ++i) {
          double *ppo = REAL(po[i]);
          OMP_PARALLEL_FOR(nth)
          for (ssize_t j=0; j<len_x; ++j) {
            if (px[j]==pvalues[i]) {
              pans[j]= ppo[j & amask[i]];
            }
          }
        }
      } break;
      case INTSXP: {
        const int *restrict px = INTEGER(x);
        const int *restrict pvalues = INTEGER(values);
        for (ssize_t i=0; i<len_values; ++i) {
          double *ppo = REAL(po[i]);
          OMP_PARALLEL_FOR(nth)
          for (ssize_t j=0; j<len_x; ++j) {
            if (px[j]==pvalues[i]) {
              pans[j]= ppo[j & amask[i]];
            }
          }
        }
      } break;
      case REALSXP: {
        const double *restrict px = REAL(x);
        const double *restrict pvalues = REAL(values);
        for (ssize_t i=0; i<len_values; ++i) {
          double *ppo = REAL(po[i]);
          OMP_PARALLEL_FOR(nth)
          for (ssize_t j=0; j<len_x; ++j) {
            if (px[j]==pvalues[i]) {
              pans[j]= ppo[j & amask[i]];
            }
          }
        }
      } break;
      case CPLXSXP: {
        const Rcomplex *restrict px = COMPLEX(x);
        const Rcomplex *restrict pvalues = COMPLEX(values);
        for (ssize_t i=0; i<len_values; ++i) {
          double *ppo = REAL(po[i]);
          OMP_PARALLEL_FOR(nth)
          for (ssize_t j=0; j<len_x; ++j) {
            if (EQUAL_CPLX(px[j],pvalues[i])) {
              pans[j]= ppo[j & amask[i]];
            }
          }
        }
      } break;
      case STRSXP: {
        const SEXP *restrict px = STRING_PTR(utfcon ? xans : x);
        const SEXP *restrict pvalues = STRING_PTR(utfcon ? vans : values);
        for (ssize_t i=0; i<len_values; ++i) {
          double *ppo = REAL(po[i]);
          for (ssize_t j=0; j<len_x; ++j) {
            if (px[j]==pvalues[i]) {
              pans[j]= ppo[j & amask[i]];
            }
          }
        }
      } break;
      case VECSXP: {
        const SEXP *restrict px = SEXPPTR_RO(x);
        const SEXP *restrict pvalues = SEXPPTR_RO(values);
        for (ssize_t i=0; i<len_values; ++i) {
          double *ppo = REAL(po[i]);
          for (ssize_t j=0; j<len_x; ++j) {
            if (R_compute_identical(px[j], pvalues[i], 0)) {
              pans[j]= ppo[j & amask[i]];
            }
          }
        }
      } break;
      default:
        error("Type %s is not supported for argument 'x'.", type2char(type_x));
      }
    } break;
    /*
     *  This part is for COMPLEX
     */
    case CPLXSXP:{
      Rcomplex *restrict pans = COMPLEX(ans);
      const Rcomplex *restrict pna = nonna ? COMPLEX(na) : NULL; 
      ssize_t namask = len_na>1 ? SSIZE_MAX : 0;
      Rcomplex NA_CPLX; NA_CPLX.r = NA_REAL; NA_CPLX.i = NA_REAL; // deal with that across all functions
      OMP_PARALLEL_FOR(nth)
      for (ssize_t j=0; j<len_x; ++j) {
        pans[j]= nonna ? pna[j & namask] : NA_CPLX;
      }
      switch(type_x) {
      case LGLSXP: {
        const int *restrict px = LOGICAL(x);
        const int *restrict pvalues = LOGICAL(values);
        for (ssize_t i=0; i<len_values; ++i) {
          Rcomplex *ppo = COMPLEX(po[i]);
          OMP_PARALLEL_FOR(nth)
          for (ssize_t j=0; j<len_x; ++j) {
            if (px[j]==pvalues[i]) {
              pans[j]= ppo[j & amask[i]];
            }
          }
        }
      } break;
      case INTSXP: {
        const int *restrict px = INTEGER(x);
        const int *restrict pvalues = INTEGER(values);
        for (ssize_t i=0; i<len_values; ++i) {
          Rcomplex *ppo = COMPLEX(po[i]);
          OMP_PARALLEL_FOR(nth)
          for (ssize_t j=0; j<len_x; ++j) {
            if (px[j]==pvalues[i]) {
              pans[j]= ppo[j & amask[i]];
            }
          }
        }
      } break;
      case REALSXP: {
        const double *restrict px = REAL(x);
        const double *restrict pvalues = REAL(values);
        for (ssize_t i=0; i<len_values; ++i) {
          Rcomplex *ppo = COMPLEX(po[i]);
          OMP_PARALLEL_FOR(nth)
          for (ssize_t j=0; j<len_x; ++j) {
            if (px[j]==pvalues[i]) {
              pans[j]= ppo[j & amask[i]];
            }
          }
        }
      } break;
      case CPLXSXP: {
        const Rcomplex *restrict px = COMPLEX(x);
        const Rcomplex *restrict pvalues = COMPLEX(values);
        for (ssize_t i=0; i<len_values; ++i) {
          Rcomplex *ppo = COMPLEX(po[i]);
          OMP_PARALLEL_FOR(nth)
          for (ssize_t j=0; j<len_x; ++j) {
            if (EQUAL_CPLX(px[j],pvalues[i])) {
              pans[j]= ppo[j & amask[i]];
            }
          }
        }
      } break;
      case STRSXP: {
        const SEXP *restrict px = STRING_PTR(utfcon ? xans : x);
        const SEXP *restrict pvalues = STRING_PTR(utfcon ? vans : values);
        for (ssize_t i=0; i<len_values; ++i) {
          Rcomplex *ppo = COMPLEX(po[i]);
          OMP_PARALLEL_FOR(nth)
          for (ssize_t j=0; j<len_x; ++j) {
            if (px[j]==pvalues[i]) {
              pans[j]= ppo[j & amask[i]];
            }
          }
        }
      } break;
      case VECSXP: {
        const SEXP *restrict px = SEXPPTR_RO(x);
        const SEXP *restrict pvalues = SEXPPTR_RO(values);
        for (ssize_t i=0; i<len_values; ++i) {
          Rcomplex *ppo = COMPLEX(po[i]);
          for (ssize_t j=0; j<len_x; ++j) {
            if (R_compute_identical(px[j], pvalues[i], 0)) {
              pans[j]= ppo[j & amask[i]];
            }
          }
        }
      } break;
      default:
        error("Type %s is not supported for argument 'x'.", type2char(type_x));
      }
    } break;
    /*
     *  This part is for STRING
     */
    case STRSXP:{
      const SEXP *restrict pna = nonna ? STRING_PTR(na) : NULL; 
      ssize_t namask = len_na>1 ? SSIZE_MAX : 0;
      for (ssize_t j=0; j<len_x; ++j) {
        SET_STRING_ELT(ans, j, nonna ? pna[j & namask] : NA_STRING);
      }
      switch(type_x) {
      case LGLSXP: {
        const int *restrict px = LOGICAL(x);
        const int *restrict pvalues = LOGICAL(values);
        for (ssize_t i=0; i<len_values; ++i) {
          SEXP *ppo = STRING_PTR(po[i]);
          for (ssize_t j=0; j<len_x; ++j) {
            if (px[j]==pvalues[i]) {
              SET_STRING_ELT(ans, j, ppo[j & amask[i]]);
            }
          }
        }
      } break;
      case INTSXP: {
        const int *restrict px = INTEGER(x);
        const int *restrict pvalues = INTEGER(values);
        for (ssize_t i=0; i<len_values; ++i) {
          SEXP *ppo = STRING_PTR(po[i]);
          for (ssize_t j=0; j<len_x; ++j) {
            if (px[j]==pvalues[i]) {
              SET_STRING_ELT(ans, j, ppo[j & amask[i]]);
            }
          }
        }
      } break;
      case REALSXP: {
        const double *restrict px = REAL(x);
        const double *restrict pvalues = REAL(values);
        for (ssize_t i=0; i<len_values; ++i) {
          SEXP *ppo = STRING_PTR(po[i]);
          for (ssize_t j=0; j<len_x; ++j) {
            if (px[j]==pvalues[i]) {
              SET_STRING_ELT(ans, j, ppo[j & amask[i]]);
            }
          }
        }
      } break;
      case CPLXSXP: {
        const Rcomplex *restrict px = COMPLEX(x);
        const Rcomplex *restrict pvalues = COMPLEX(values);
        for (ssize_t i=0; i<len_values; ++i) {
          SEXP *ppo = STRING_PTR(po[i]);
          for (ssize_t j=0; j<len_x; ++j) {
            if (EQUAL_CPLX(px[j],pvalues[i])) {
              SET_STRING_ELT(ans, j, ppo[j & amask[i]]);
            }
          }
        }
      } break;
      case STRSXP: {
        const SEXP *restrict px = STRING_PTR(utfcon ? xans : x);
        const SEXP *restrict pvalues = STRING_PTR(utfcon ? vans : values);
        for (ssize_t i=0; i<len_values; ++i) {
          SEXP *ppo = STRING_PTR(po[i]);
          for (ssize_t j=0; j<len_x; ++j) {
            if (px[j]==pvalues[i]) {
              SET_STRING_ELT(ans, j, ppo[j & amask[i]]);
            }
          }
        }
      } break;
      case VECSXP: {
        const SEXP *restrict px = SEXPPTR_RO(x);
        const SEXP *restrict pvalues = SEXPPTR_RO(values);
        for (ssize_t i=0; i<len_values; ++i) {
          SEXP *ppo = STRING_PTR(po[i]);
          for (ssize_t j=0; j<len_x; ++j) {
            if (R_compute_identical(px[j], pvalues[i], 0)) {
              SET_STRING_ELT(ans, j, ppo[j & amask[i]]);
            }
          }
        }
      } break;
      default:
        error("Type %s is not supported for argument 'x'.", type2char(type_x));
      }
    } break;
    /*
     *  This part is for LIST
     */
    case VECSXP:{
      const SEXP *restrict pna = nonna ? SEXPPTR_RO(na) : NULL; 
      ssize_t namask = len_na>1 ? SSIZE_MAX : 0;
      for (ssize_t j=0; j<len_x; ++j) {
        if (nonna) {
          SET_VECTOR_ELT(ans, j, pna[j & namask]); 
        }
      }
      switch(type_x) {
      case LGLSXP: {
        const int *restrict px = LOGICAL(x);
        const int *restrict pvalues = LOGICAL(values);
        for (ssize_t i=0; i<len_values; ++i) {
          const SEXP *ppo = SEXPPTR_RO(po[i]);
          for (ssize_t j=0; j<len_x; ++j) {
            if (px[j]==pvalues[i]) {
              SET_VECTOR_ELT(ans, j, ppo[j & amask[i]]);
            }
          }
        }
      } break;
      case INTSXP: {
        const int *restrict px = INTEGER(x);
        const int *restrict pvalues = INTEGER(values);
        for (ssize_t i=0; i<len_values; ++i) {
          const SEXP *ppo = SEXPPTR_RO(po[i]);
          for (ssize_t j=0; j<len_x; ++j) {
            if (px[j]==pvalues[i]) {
              SET_VECTOR_ELT(ans, j, ppo[j & amask[i]]);
            }
          }
        }
      } break;
      case REALSXP: {
        const double *restrict px = REAL(x);
        const double *restrict pvalues = REAL(values);
        for (ssize_t i=0; i<len_values; ++i) {
          const SEXP *ppo = SEXPPTR_RO(po[i]);
          for (ssize_t j=0; j<len_x; ++j) {
            if (px[j]==pvalues[i]) {
              SET_VECTOR_ELT(ans, j, ppo[j & amask[i]]);
            }
          }
        }
      } break;
      case CPLXSXP: {
        const Rcomplex *restrict px = COMPLEX(x);
        const Rcomplex *restrict pvalues = COMPLEX(values);
        for (ssize_t i=0; i<len_values; ++i) {
          const SEXP *ppo = SEXPPTR_RO(po[i]);
          for (ssize_t j=0; j<len_x; ++j) {
            if (EQUAL_CPLX(px[j],pvalues[i])) {
              SET_VECTOR_ELT(ans, j, ppo[j & amask[i]]);
            }
          }
        }
      } break;
      case STRSXP: {
        const SEXP *restrict px = STRING_PTR(utfcon ? xans : x);
        const SEXP *restrict pvalues = STRING_PTR(utfcon ? vans : values);
        for (ssize_t i=0; i<len_values; ++i) {
          const SEXP *ppo = SEXPPTR_RO(po[i]);
          for (ssize_t j=0; j<len_x; ++j) {
            if (px[j]==pvalues[i]) {
              SET_VECTOR_ELT(ans, j, ppo[j & amask[i]]);
            }
          }
        }
      } break;
      case VECSXP: {
        const SEXP *restrict px = SEXPPTR_RO(x);
        const SEXP *restrict pvalues = SEXPPTR_RO(values);
        for (ssize_t i=0; i<len_values; ++i) {
          const SEXP *ppo = SEXPPTR_RO(po[i]);
          for (ssize_t j=0; j<len_x; ++j) {
            if (R_compute_identical(px[j], pvalues[i], 0)) {
              SET_VECTOR_ELT(ans, j, ppo[j & amask[i]]);
            }
          }
        }
      } break;
      default:
        error("Type %s is not supported for argument 'x'.", type2char(type_x));
      }
    } break;
    default :
      error("Type %s is not supported for argument 'outputs'", type2char(type_outputs));
    }
  } else { // Here it is for vectors
    if (nonna && type_na != type_o) {
      error("Type of 'na' and 'outputs' are different. Please make sure they are the same.");
    }
    if (nonna) {
      if (!R_compute_identical(PROTECT(getAttrib(outputs, R_ClassSymbol)), PROTECT(getAttrib(na, R_ClassSymbol)), 0)) {
        error("Argument 'na' and 'outputs' must have same class.");
      }
      UNPROTECT(2);
      if (isFactor(outputs)) {
        if (!R_compute_identical(PROTECT(getAttrib(outputs, R_LevelsSymbol)), PROTECT(getAttrib(na, R_LevelsSymbol)), 0)) {
          error("Argument 'na' and 'outputs' are both factor but their levels are different.");
        }
        UNPROTECT(2);
      } 
    }
    ans = PROTECT(allocVector(type_o, len_x)); nprotect++;
    copyMostAttrib(outputs, ans);
    switch(type_o) {
    /*
     *  This part is for LOGICAL
     */
    case LGLSXP:{
      int *restrict pans = LOGICAL(ans);
      const int *restrict pna = nonna ? LOGICAL(na) : NULL; 
      ssize_t namask = len_na>1 ? SSIZE_MAX : 0;
      OMP_PARALLEL_FOR(nth)
      for (ssize_t j=0; j<len_x; ++j) {
        pans[j]= nonna ? pna[j & namask] : NA_LOGICAL;
      }
      const int *restrict pto = LOGICAL(outputs);
      switch(type_x) {
      case LGLSXP: {
        const int *restrict px = LOGICAL(x);
        const int *restrict pvalues = LOGICAL(values);
        OMP_PARALLEL_FOR(nth)
        for (ssize_t j=0; j<len_x; ++j) {
          for (ssize_t i=0; i<len_values; ++i) {
            if (px[j]==pvalues[i]) {
              pans[j]=pto[i];
            }
          }
        }
      } break;
      case INTSXP: {
        const int *restrict px = INTEGER(x);
        const int *restrict pvalues = INTEGER(values);
        OMP_PARALLEL_FOR(nth)
        for (ssize_t j=0; j<len_x; ++j) {
          for (ssize_t i=0; i<len_values; ++i) {
            if (px[j]==pvalues[i]) {
              pans[j]=pto[i];
            }
          }
        }
      } break;
      case REALSXP: {
        const double *restrict px = REAL(x);
        const double *restrict pvalues = REAL(values);
        OMP_PARALLEL_FOR(nth)
        for (ssize_t j=0; j<len_x; ++j) {
          for (ssize_t i=0; i<len_values; ++i) {
            if (px[j]==pvalues[i]) {
              pans[j]=pto[i];
            }
          }
        }
      } break;
      case CPLXSXP: {
        const Rcomplex *restrict px = COMPLEX(x);
        const Rcomplex *restrict pvalues = COMPLEX(values);
        OMP_PARALLEL_FOR(nth)
        for (ssize_t j=0; j<len_x; ++j) {
          for (ssize_t i=0; i<len_values; ++i) {
            if (EQUAL_CPLX(px[j],pvalues[i])) {
              pans[j]=pto[i];
            }
          }
        }
      } break;
      case STRSXP: {
        const SEXP *restrict px = STRING_PTR(utfcon ? xans : x);
        const SEXP *restrict pvalues = STRING_PTR(utfcon ? vans : values);
        for (ssize_t j=0; j<len_x; ++j) {
          for (ssize_t i=0; i<len_values; ++i) {
            if (px[j]==pvalues[i]) {
              pans[j]=pto[i];
            }
          }
        }
      } break;
      case VECSXP: {
        const SEXP *restrict px = SEXPPTR_RO(x);
        const SEXP *restrict pvalues = SEXPPTR_RO(values);
        for (ssize_t j=0; j<len_x; ++j) {
          for (ssize_t i=0; i<len_values; ++i) {
            if (R_compute_identical(px[j], pvalues[i], 0)) {
              pans[j]=pto[i];
            }
          }
        }
      } break;
      default:
        error("Type %s is not supported for argument 'x'.", type2char(type_x));
      }
    } break;
    /*
     *  This part is for INTEGER
     */
    case INTSXP:{
      int *restrict pans = INTEGER(ans);
      const int *restrict pna = nonna ? INTEGER(na) : NULL; 
      ssize_t namask = len_na>1 ? SSIZE_MAX : 0;
      OMP_PARALLEL_FOR(nth)
      for (ssize_t j=0; j<len_x; ++j) {
        pans[j]= nonna ? pna[j & namask] : NA_INTEGER;
      }
      const int *restrict pto = INTEGER(outputs);
      switch(type_x) {
      case LGLSXP: {
        const int *restrict px = LOGICAL(x);
        const int *restrict pvalues = LOGICAL(values);
        OMP_PARALLEL_FOR(nth)
        for (ssize_t j=0; j<len_x; ++j) {
          for (ssize_t i=0; i<len_values; ++i) {
            if (px[j]==pvalues[i]) {
              pans[j]=pto[i];
            }
          }
        }
      } break;
      case INTSXP: {
        const int *restrict px = INTEGER(x);
        const int *restrict pvalues = INTEGER(values);
        OMP_PARALLEL_FOR(nth)
        for (ssize_t j=0; j<len_x; ++j) {
          for (ssize_t i=0; i<len_values; ++i) {
            if (px[j]==pvalues[i]) {
              pans[j]=pto[i];
            }
          }
        }
      } break;
      case REALSXP: {
        const double *restrict px = REAL(x);
        const double *restrict pvalues = REAL(values);
        OMP_PARALLEL_FOR(nth)
        for (ssize_t j=0; j<len_x; ++j) {
          for (ssize_t i=0; i<len_values; ++i) {
            if (px[j]==pvalues[i]) {
              pans[j]=pto[i];
            }
          }
        }
      } break;
      case CPLXSXP: {
        const Rcomplex *restrict px = COMPLEX(x);
        const Rcomplex *restrict pvalues = COMPLEX(values);
        OMP_PARALLEL_FOR(nth)
        for (ssize_t j=0; j<len_x; ++j) {
          for (ssize_t i=0; i<len_values; ++i) {
            if (EQUAL_CPLX(px[j],pvalues[i])) {
              pans[j]=pto[i];
            }
          }
        }
      } break;
      case STRSXP: {
        const SEXP *restrict px = STRING_PTR(utfcon ? xans : x);
        const SEXP *restrict pvalues = STRING_PTR(utfcon ? vans : values);
        for (ssize_t j=0; j<len_x; ++j) {
          for (ssize_t i=0; i<len_values; ++i) {
            if (px[j]==pvalues[i]) {
              pans[j]=pto[i];
            }
          }
        }
      } break;
      case VECSXP: {
        const SEXP *restrict px = SEXPPTR_RO(x);
        const SEXP *restrict pvalues = SEXPPTR_RO(values);
        for (ssize_t j=0; j<len_x; ++j) {
          for (ssize_t i=0; i<len_values; ++i) {
            if (R_compute_identical(px[j], pvalues[i], 0)) {
              pans[j]=pto[i];
            }
          }
        }
      } break;
      default:
        error("Type %s is not supported for argument 'x'.", type2char(type_x));
      }
    } break;
    /*
     *  This part is for REAL
     */
    case REALSXP:{
      double *restrict pans = REAL(ans);
      const double *restrict pna = nonna ? REAL(na) : NULL; 
      ssize_t namask = len_na>1 ? SSIZE_MAX : 0;
      OMP_PARALLEL_FOR(nth)
      for (ssize_t j=0; j<len_x; ++j) {
        pans[j]= nonna ? pna[j & namask] : NA_REAL;
      }
      const double *restrict pto = REAL(outputs);
      switch(type_x) {
      case LGLSXP: {
        const int *restrict px = LOGICAL(x);
        const int *restrict pvalues = LOGICAL(values);
        OMP_PARALLEL_FOR(nth)
        for (ssize_t j=0; j<len_x; ++j) {
          for (ssize_t i=0; i<len_values; ++i) {
            if (px[j]==pvalues[i]) {
              pans[j]=pto[i];
            }
          }
        }
      } break;
      case INTSXP: {
        const int *restrict px = INTEGER(x);
        const int *restrict pvalues = INTEGER(values);
        OMP_PARALLEL_FOR(nth)
        for (ssize_t j=0; j<len_x; ++j) {
          for (ssize_t i=0; i<len_values; ++i) {
            if (px[j]==pvalues[i]) {
              pans[j]=pto[i];
            }
          }
        }
      } break;
      case REALSXP: {
        const double *restrict px = REAL(x);
        const double *restrict pvalues = REAL(values);
        OMP_PARALLEL_FOR(nth)
        for (ssize_t j=0; j<len_x; ++j) {
          for (ssize_t i=0; i<len_values; ++i) {
            if (px[j]==pvalues[i]) {
              pans[j]=pto[i];
            }
          }
        }
      } break;
      case CPLXSXP: {
        const Rcomplex *restrict px = COMPLEX(x);
        const Rcomplex *restrict pvalues = COMPLEX(values);
        OMP_PARALLEL_FOR(nth)
        for (ssize_t j=0; j<len_x; ++j) {
          for (ssize_t i=0; i<len_values; ++i) {
            if (EQUAL_CPLX(px[j],pvalues[i])) {
              pans[j]=pto[i];
            }
          }
        }
      } break;
      case STRSXP: {
        const SEXP *restrict px = STRING_PTR(utfcon ? xans : x);
        const SEXP *restrict pvalues = STRING_PTR(utfcon ? vans : values);
        for (ssize_t j=0; j<len_x; ++j) {
          for (ssize_t i=0; i<len_values; ++i) {
            if (px[j]==pvalues[i]) {
              pans[j]=pto[i];
            }
          }
        }
      } break;
      case VECSXP: {
        const SEXP *restrict px = SEXPPTR_RO(x);
        const SEXP *restrict pvalues = SEXPPTR_RO(values);
        for (ssize_t j=0; j<len_x; ++j) {
          for (ssize_t i=0; i<len_values; ++i) {
            if (R_compute_identical(px[j], pvalues[i], 0)) {
              pans[j]=pto[i];
            }
          }
        }
      } break;
      default:
        error("Type %s is not supported for argument 'x'.", type2char(type_x));
      }
    } break;
    /*
     *  This part is for COMPLEX
     */
    case CPLXSXP:{
      Rcomplex *restrict pans = COMPLEX(ans);
      const Rcomplex *restrict pna = nonna ? COMPLEX(na) : NULL; 
      ssize_t namask = len_na>1 ? SSIZE_MAX : 0;
      Rcomplex NA_CPLX; NA_CPLX.r = NA_REAL; NA_CPLX.i = NA_REAL; // deal with that across all functions
      OMP_PARALLEL_FOR(nth)
      for (ssize_t j=0; j<len_x; ++j) {
        pans[j]= nonna ? pna[j & namask] : NA_CPLX;
      }
      const Rcomplex *restrict pto = COMPLEX(outputs);
      switch(type_x) {
      case LGLSXP: {
        const int *restrict px = LOGICAL(x);
        const int *restrict pvalues = LOGICAL(values);
        OMP_PARALLEL_FOR(nth)
        for (ssize_t j=0; j<len_x; ++j) {
          for (ssize_t i=0; i<len_values; ++i) {
            if (px[j]==pvalues[i]) {
              pans[j]=pto[i];
            }
          }
        }
      } break;
      case INTSXP: {
        const int *restrict px = INTEGER(x);
        const int *restrict pvalues = INTEGER(values);
        OMP_PARALLEL_FOR(nth)
        for (ssize_t j=0; j<len_x; ++j) {
          for (ssize_t i=0; i<len_values; ++i) {
            if (px[j]==pvalues[i]) {
              pans[j]=pto[i];
            }
          }
        }
      } break;
      case REALSXP: {
        const double *restrict px = REAL(x);
        const double *restrict pvalues = REAL(values);
        OMP_PARALLEL_FOR(nth)
        for (ssize_t j=0; j<len_x; ++j) {
          for (ssize_t i=0; i<len_values; ++i) {
            if (px[j]==pvalues[i]) {
              pans[j]=pto[i];
            }
          }
        }
      } break;
      case CPLXSXP: {
        const Rcomplex *restrict px = COMPLEX(x);
        const Rcomplex *restrict pvalues = COMPLEX(values);
        OMP_PARALLEL_FOR(nth)
        for (ssize_t j=0; j<len_x; ++j) {
          for (ssize_t i=0; i<len_values; ++i) {
            if (EQUAL_CPLX(px[j],pvalues[i])) {
              pans[j]=pto[i];
            }
          }
        }
      } break;
      case STRSXP: {
        const SEXP *restrict px = STRING_PTR(utfcon ? xans : x);
        const SEXP *restrict pvalues = STRING_PTR(utfcon ? vans : values);
        for (ssize_t j=0; j<len_x; ++j) {
          for (ssize_t i=0; i<len_values; ++i) {
            if (px[j]==pvalues[i]) {
              pans[j]=pto[i];
            }
          }
        }
      } break;
      case VECSXP: {
        const SEXP *restrict px = SEXPPTR_RO(x);
        const SEXP *restrict pvalues = SEXPPTR_RO(values);
        for (ssize_t j=0; j<len_x; ++j) {
          for (ssize_t i=0; i<len_values; ++i) {
            if (R_compute_identical(px[j], pvalues[i], 0)) {
              pans[j]=pto[i];
            }
          }
        }
      } break;
      default:
        error("Type %s is not supported for argument 'x'.", type2char(type_x));
      }
    } break;
    /*
     *  This part is for STRING
     */
    case STRSXP:{
      const SEXP *restrict pna = nonna ? STRING_PTR(na) : NULL; 
      ssize_t namask = len_na>1 ? SSIZE_MAX : 0;
      for (ssize_t j=0; j<len_x; ++j) {
        SET_STRING_ELT(ans, j, nonna ? pna[j & namask] : NA_STRING);
      }
      const SEXP *restrict pto = STRING_PTR(outputs);
      switch(type_x) {
      case LGLSXP: {
        const int *restrict px = LOGICAL(x);
        const int *restrict pvalues = LOGICAL(values);
        for (ssize_t j=0; j<len_x; ++j) {
          for (ssize_t i=0; i<len_values; ++i) {
            if (px[j]==pvalues[i]) {
              SET_STRING_ELT(ans, j, pto[i]);
            }
          }
        }
      } break;
      case INTSXP: {
        const int *restrict px = INTEGER(x);
        const int *restrict pvalues = INTEGER(values);
        for (ssize_t j=0; j<len_x; ++j) {
          for (ssize_t i=0; i<len_values; ++i) {
            if (px[j]==pvalues[i]) {
              SET_STRING_ELT(ans, j, pto[i]);
            }
          }
        }
      } break;
      case REALSXP: {
        const double *restrict px = REAL(x);
        const double *restrict pvalues = REAL(values);
        for (ssize_t j=0; j<len_x; ++j) {
          for (ssize_t i=0; i<len_values; ++i) {
            if (px[j]==pvalues[i]) {
              SET_STRING_ELT(ans, j, pto[i]);
            }
          }
        }
      } break;
      case CPLXSXP: {
        const Rcomplex *restrict px = COMPLEX(x);
        const Rcomplex *restrict pvalues = COMPLEX(values);
        for (ssize_t j=0; j<len_x; ++j) {
          for (ssize_t i=0; i<len_values; ++i) {
            if (EQUAL_CPLX(px[j],pvalues[i])) {
              SET_STRING_ELT(ans, j, pto[i]);
            }
          }
        }
      } break;
      case STRSXP: {
        const SEXP *restrict px = STRING_PTR(utfcon ? xans : x);
        const SEXP *restrict pvalues = STRING_PTR(utfcon ? vans : values);
        for (ssize_t j=0; j<len_x; ++j) {
          for (ssize_t i=0; i<len_values; ++i) {
            if (px[j]==pvalues[i]) {
              SET_STRING_ELT(ans, j, pto[i]);
            }
          }
        }
      } break;
      case VECSXP: {
        const SEXP *restrict px = SEXPPTR_RO(x);
        const SEXP *restrict pvalues = SEXPPTR_RO(values);
        for (ssize_t j=0; j<len_x; ++j) {
          for (ssize_t i=0; i<len_values; ++i) {
            if (R_compute_identical(px[j], pvalues[i], 0)) {
              SET_STRING_ELT(ans, j, pto[i]);
            }
          }
        }
      } break;
      default:
        error("Type %s is not supported for argument 'x'.", type2char(type_x));
      }
    } break;
    default:
      error("Type %s is not supported for argument 'outputs'.", type2char(type_o));
    }
  }
  UNPROTECT(nprotect);
  return ans;
}
