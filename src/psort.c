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

#define HASHSTR(x) (HASH(((intptr_t) (x) & 0xffffffff), K))
#define LOOKUP_VAL(x) (lookupTable[getIndex((x), pvalSorted, lookupTable)]-1)

#define STR_SORT   install("sort")
#define STR_ORDER  install("order")
#define STR_METHOD install("method")
#define STR_NALAST install("na.last")
#define STR_DECREA install("decreasing")
#define STR_FACTOR mkChar("factor")

static int K = 8;
static size_t M = 256;

static void recursiveRadix(SEXP *restrict pans, const size_t k, size_t *restrict pos,
                           size_t *restrict incr, uint8_t *restrict test, SEXP tmp,
                           SEXP *restrict ptmp, size_t start, size_t *restrict newpos) {
  for (uint16_t i = 1; i < 257; ++i){
    if (pos[i] ==  1) {
      start++; continue;
    }
    if (pos[i] > 1) {
      const size_t ct = pos[i];
      SEXP *npans = pans + start;
      memset(incr, 0, 257*sizeof(size_t));
      for(size_t j = 0; j < ct; ++j) {
        test[j] = (uint8_t)(CHAR(npans[j])[k]);
        incr[++test[j]]++;
      }
      if (incr[1] == ct) {
        start += ct;
        continue;
      }
      newpos[0] = incr[0];
      for (uint16_t j = 1;  j < 257; ++j){
        newpos[j] = incr[j] + newpos[j-1];
      }
      for (size_t j = 0; j < ct; ++j){
        SET_STRING_ELT(tmp, --newpos[test[j]], npans[j]);
      }
      memcpy(npans, ptmp, ct*sizeof(SEXP));
      size_t maxLen=0;
      for (uint16_t j = 2; j < 257; ++j){
        if(incr[j] > maxLen) {
          maxLen = incr[j];
        }
      }
      if (maxLen > 1) {
        size_t *restrict secpos = malloc(257*sizeof(size_t));
        recursiveRadix(pans, k+1, incr, newpos, test, tmp, ptmp, start,secpos);
        free(secpos);
      }
      start += ct;
    }
  }
}

static void recursiveRadixrev(SEXP *restrict pans, const size_t k, size_t *restrict pos,
                              size_t *restrict incr, uint8_t *restrict test, SEXP tmp,
                              SEXP *restrict ptmp, size_t start, size_t *restrict newpos) {
  for (uint16_t i = 256; i > 0; --i){
    if (pos[i] ==  1) {
      start++; continue;
    }
    if (pos[i] > 1) {
      const size_t ct = pos[i];
      SEXP *npans = pans + start;
      memset(incr, 0, 257*sizeof(size_t));
      for(size_t j = 0; j < ct; ++j) {
        test[j] = (uint8_t)(CHAR(npans[j])[k]);
        incr[++test[j]]++;
      }
      if (incr[1] == ct) {
        start += ct;
        continue;
      }
      newpos[256] = incr[256];
      for (uint16_t j = 255;  j >= 1; --j){
        newpos[j] = incr[j] + newpos[j+1];
      }
      for (size_t j = 0; j < ct; ++j){
        SET_STRING_ELT(tmp, --newpos[test[j]], npans[j]);
      }
      memcpy(npans, ptmp, ct*sizeof(SEXP));
      size_t maxLen=0;
      for (uint16_t j = 2; j < 257; ++j){
        if(incr[j] > maxLen) {
          maxLen = incr[j];
        }
      }
      if (maxLen > 1) {
        size_t *restrict secpos = malloc(257*sizeof(size_t));
        recursiveRadixrev(pans, k+1, incr, newpos, test, tmp, ptmp, start,secpos);
        free(secpos);
      }
      start += ct;
    }
  }
}

static SEXP rsort (SEXP x) {
  const size_t len = LENGTH(x);
  const SEXP *restrict px = STRING_PTR(x);
  SEXP ans = PROTECT(allocVector(STRSXP, len));
  uint8_t nprotect = 1;
  
  uint8_t *restrict test = malloc(len * sizeof(uint8_t));
  size_t *restrict pos = calloc(257, sizeof(size_t));
  size_t *restrict incr = malloc(257*sizeof(size_t));
  
  for (size_t i = 0; i < len; ++i){
    test[i] = (uint8_t)(CHAR(px[i])[0]);
    pos[++test[i]]++;
  }
  
  size_t maxLen=0;
  for (uint16_t i = 2; i < 257; ++i){ // start at 2, is that correct?
    if(pos[i] > maxLen) {
      maxLen = pos[i];
    }
  }
  
  incr[0] = pos[0];
  for (uint16_t i = 1; i < 257 ; ++i){
    incr[i] = pos[i] + incr[i-1];
  }
  
  for (size_t i = 0; i < len; ++i){
    SET_STRING_ELT(ans, --incr[test[i]], px[i]);
  }
  
  if (maxLen > 1) {
    SEXP tmp = PROTECT(allocVector(STRSXP, maxLen));
    nprotect++;
    size_t start = 0;
    size_t *restrict newpos = malloc(257*sizeof(size_t));
    recursiveRadix(STRING_PTR(ans), 1, pos, incr, test, tmp, STRING_PTR(tmp), start,newpos);
    free(newpos);
  }
  
  free(pos);
  free(test);
  free(incr);
  
  UNPROTECT(nprotect);
  return ans;
}

static SEXP rsortrev (SEXP x) {
  const size_t len = LENGTH(x);
  const SEXP *restrict px = STRING_PTR(x);
  SEXP ans = PROTECT(allocVector(STRSXP, len));
  uint8_t nprotect = 1;
  
  uint8_t *restrict test = malloc(len * sizeof(uint8_t));
  size_t *restrict pos = calloc(257, sizeof(size_t));
  size_t *restrict incr = malloc(257*sizeof(size_t));
  
  for (size_t i = 0; i < len; ++i){
    test[i] = (uint8_t)(CHAR(px[i])[0]);
    pos[++test[i]]++;
  }
  
  size_t maxLen=0;
  for (uint16_t i = 2; i < 257; ++i){ // start at 2, is that correct?
    if(pos[i] > maxLen) {
      maxLen = pos[i];
    }
  }
  
  incr[256] = pos[256];
  for (uint16_t i = 255; i >= 1 ; --i){
    incr[i] = pos[i] + incr[i+1];
  }
  
  for (size_t i = 0; i < len; ++i){
    SET_STRING_ELT(ans, --incr[test[i]], px[i]);
  }
  
  if (maxLen > 1) {
    SEXP tmp = PROTECT(allocVector(STRSXP, maxLen));
    nprotect++;
    size_t start = 0;
    size_t *restrict newpos = malloc(257*sizeof(size_t));
    recursiveRadixrev(STRING_PTR(ans), 1, pos, incr, test, tmp, STRING_PTR(tmp), start,newpos);
    free(newpos);
  }
  
  free(pos);
  free(test);
  free(incr);
  
  UNPROTECT(nprotect);
  return ans;
}

static SEXP dupVecSort(SEXP x) {
  const R_xlen_t n = xlength(x);
  int K =8;
  size_t M = 256;
  const size_t n2 = 2U * (size_t) n;
  while (M < n2) {
    M *= 2;
    K++;
  }
  R_xlen_t count = 0;
  size_t id = 0;
  int *restrict h = (int*)calloc(M, sizeof(int));
  int *restrict pans = (int*)calloc(n, sizeof(int));
  const SEXP *restrict px = STRING_PTR(x);
  for (int i = 0; i < n; ++i) {
    id = HASHSTR(px[i]);
    while (h[id]) {
      if (px[h[id] - 1]==px[i]) {
        goto sbl;
      }
      id++;
      id %= M;
    }
    h[id] = (int) i + 1;
    pans[i]++;
    count++;
    sbl:;
  }
  free(h);
  SEXP indx = PROTECT(allocVector(STRSXP, count));
  R_xlen_t ct = 0;
  for (int i = 0; ct < count; ++i) {
    if (pans[i]) {
      SET_STRING_ELT(indx, ct++, px[i]);
    }
  }
  free(pans);
  UNPROTECT(1);
  return indx;
}

static int *buildTable (SEXP x) {
  const R_xlen_t n = xlength(x);
  K = 8;
  M = 256;
  const size_t n2 = 2U * (size_t) n;
  while (M < n2) { M *= 2; K++;}
  size_t id = 0;
  int *h = (int*)calloc(M, sizeof(int));
  const SEXP *restrict px = STRING_PTR(x);
  for (int i = 0; i < n; ++i) {
    id = HASHSTR(px[i]);
    while (h[id]) {
      if (px[h[id] - 1]==px[i]) {
        goto bl; // # nocov
      }
      id++;
      id %= M;
    }
    h[id] = (int) i + 1;
    bl:;
  }
  return h;
}

static inline int getIndex(SEXP ptr, const SEXP *restrict cmp, int *lkpTbl) {
  size_t id = HASHSTR(ptr);
  while(true) {
    if (cmp[lkpTbl[id] - 1]==ptr) {
      return id;
    }
    id++;
    id %= M;
  }
}

static SEXP callToSort (SEXP x, const char* method, SEXP env) {
  SEXP call = PROTECT(allocVector(LANGSXP, 4));
  SETCAR(call, STR_SORT);
  
  SEXP s = CDR(call);
  SETCAR(s, x);
  SET_TAG(s, install("x"));
  
  s = CDR(s);
  SETCAR(s, PROTECT(mkString(method)));
  SET_TAG(s, STR_METHOD);
  
  s = CDR(s);
  SETCAR(s, ScalarLogical(0));
  SET_TAG(s, STR_NALAST);
  
  SEXP out = PROTECT(eval(call, env));
  UNPROTECT(3);
  return out;
}

static SEXP callToSort2 (SEXP x, const char* method, const int desc, const int na, SEXP env) {
  SEXP call = PROTECT(allocVector(LANGSXP, 5));
  SETCAR(call, STR_SORT);
  
  SEXP s = CDR(call);
  SETCAR(s, x);
  SET_TAG(s, install("x"));
  
  s = CDR(s);
  SETCAR(s, PROTECT(mkString(method)));
  SET_TAG(s, STR_METHOD);
  
  s = CDR(s);
  SETCAR(s, ScalarLogical(na));
  SET_TAG(s, STR_NALAST);
  
  s = CDR(s);
  SETCAR(s, ScalarLogical(desc));
  SET_TAG(s, STR_DECREA);
  
  SEXP out = PROTECT(eval(call, env));
  UNPROTECT(3);
  return out;
}

SEXP callToOrder (SEXP x, const char* method, bool desc, Rboolean na, SEXP env) {
  SEXP call = PROTECT(allocVector(LANGSXP, 5));
  SETCAR(call, STR_ORDER);
  
  SEXP s = CDR(call);
  SETCAR(s, x);
  SET_TAG(s, install("..."));
  
  s = CDR(s);
  SETCAR(s, PROTECT(mkString(method)));
  SET_TAG(s, STR_METHOD);
  
  s = CDR(s);
  SETCAR(s, ScalarLogical(na));
  SET_TAG(s, STR_NALAST);
  
  s = CDR(s);
  SETCAR(s, ScalarLogical(desc));
  SET_TAG(s, STR_DECREA);
  
  SEXP out = PROTECT(eval(call, env));
  UNPROTECT(3);
  return out;
}

/*
 *  Character sorting
 */

SEXP cpsortR (SEXP x, SEXP decreasing, SEXP nthread, SEXP nalast, SEXP env, SEXP index, SEXP clocale) {

  if (!IS_BOOL(decreasing)) {
    error("Argument 'decreasing' must be TRUE or FALSE.");
  }
  /*if (!IS_BOOL(index)) {
    error("Argument 'index.return' must be TRUE or FALSE.");
  }*/
  if (!IS_LOGICAL(nalast)) {
    error("Argument 'na.last' must be TRUE, FALSE or NA.");
  }
  if (TYPEOF(nthread) != INTSXP) {
    error("Argument 'nThread' (%s) must be of type integer.",type2char(TYPEOF(nthread)));
  }
  if (!IS_BOOL(clocale)) {
    error("Argument 'c.locale' must be TRUE or FALSE.");
  }
  
  const int na_pos = asLogical(nalast);
  const int cindex = asLogical(index);
  const int dcr    = asLogical(decreasing);
  const int cl     = asLogical(clocale);
  const int xlen   = LENGTH(x);
  
  SEXP uVals = PROTECT(dupVecSort(x));
  const int n = LENGTH(uVals);
  
  const int early = xlen == n;
  SEXP valSorted = early ? (
    cindex ? PROTECT(callToOrder(uVals, "shell", dcr, na_pos, env)) : 
             (cl ? (dcr ? PROTECT(rsortrev(uVals)) : PROTECT(rsort(uVals))) :
                   PROTECT(callToSort2(uVals, "quick", dcr, na_pos, env)))
  ) : ( cl ? PROTECT(rsort(uVals)) : 
             PROTECT(callToSort(uVals, "quick", env)));

  /*if (early && cindex) {
    UNPROTECT(2);
    return valSorted;
  }*/
  
  SEXP *restrict pvalSorted = STRING_PTR(valSorted);
  const int nlen = LENGTH(valSorted);
  
  int NAidx = -1;
  for (int i = 0; i < nlen; ++i) {
    if (pvalSorted[i] == NA_STRING) {
      NAidx = i;
      break;
    }
  }
  if (cl) {
    if ( ((na_pos != 0 && !dcr) || (na_pos == 0 && dcr && !early) || (na_pos != 0 && dcr && early)) && NAidx != nlen-1 ) {
      if (NAidx >= 0) {
        memmove(pvalSorted+NAidx, pvalSorted+NAidx+1, (nlen - (NAidx + 1))*sizeof(SEXP));
        pvalSorted[nlen-1] = NA_STRING;
      }
    } else if ( (na_pos == 0 && !dcr) || (na_pos != 0 && dcr) || (na_pos == 0 && dcr && early)){
      if (NAidx > 0 ) {
        memmove(pvalSorted+1, pvalSorted, NAidx*sizeof(SEXP));
        pvalSorted[0] = NA_STRING;
      }
    }
  } else {
    if ( ((na_pos != 0 && !dcr) || (na_pos == 0 && dcr)) && NAidx != nlen-1) {
      if (NAidx >= 0) {
        memmove(pvalSorted+NAidx, pvalSorted+NAidx+1, (nlen - (NAidx + 1))*sizeof(SEXP));
        pvalSorted[nlen-1] = NA_STRING;
      }
    } else if ( (na_pos == 0 && !dcr) || (na_pos != 0 && dcr) ){
      if (NAidx > 0 ) {
        memmove(pvalSorted+1, pvalSorted, NAidx*sizeof(SEXP));
        pvalSorted[0] = NA_STRING;
      }
    }
  }
  
  if (early) {
    if (na_pos == NA_LOGICAL && cl) {
      const SEXP *restrict pa = STRING_PTR(valSorted); // already used pvalSorted
      int ct = 0;
      for (int i = nlen-1; i >= 0; --i) {
        if(pa[i] == NA_STRING) {
          ct++;
        } else {
          break;
        }
      }
      if (ct > 0) {
        SETLENGTH(valSorted, nlen-ct);
      }
    }
    UNPROTECT(2);
    return valSorted;
  }
  
  int *restrict lookupTable = buildTable(valSorted);
  const SEXP *restrict px = STRING_PTR(x);
  int nth = asInteger(nthread);
  nth = nth > max_thread ? max_thread : (nth < min_thread ? min_thread : nth); //revisit this
  
  SEXP ans = PROTECT(allocVector(cindex ? INTSXP : STRSXP, xlen));
  if (!cindex) {
    copyMostAttrib(x, ans);
  }
  
  int *restrict pos = (int*)R_alloc(n, sizeof(int));
  memset(pos, 0, n*sizeof(int));
  int *restrict lv = (int*)R_alloc(xlen, sizeof(int));
  
  OMP_PARALLEL_FOR(nth)
  for (int j = 0; j < xlen; ++j) {
    lv[j] = LOOKUP_VAL(px[j]);
  }
  free(lookupTable);
  
  for (int j=0; j<xlen; ++j) {
    pos[lv[j]]++;
  }
  
  int cumul = 0, temp = 0;
  if (dcr) {
    for (int i=n-1; i>=0; --i) {
      temp = pos[i];
      pos[i] = cumul;
      cumul += temp; 
    }
  } else {
    for (int i=0; i<n; ++i) {
      temp = pos[i];
      pos[i] = cumul;
      cumul += temp; 
    }
  }
  
  /*if (cindex) {
    int *restrict pans = INTEGER(ans);
    for (int j=0; j<xlen; ++j) {
      pans[pos[lv[j]]++] = j + 1;
    }
    if (na_pos == NA_LOGICAL) {
      int ct = 0;
      for (int i = xlen-1; i >= 0; --i) {
        if( px[pans[i]-1] == NA_STRING) {
          ct++;
        } else {
          break;
        }
      }
      if (ct > 0) {
        SETLENGTH(ans, xlen-ct);
      }
    }
  } else {*/
    for (int j = 0; j < xlen; ++j) {
      SET_STRING_ELT(ans, pos[lv[j]]++ , px[j]);
    }
    if (na_pos == NA_LOGICAL) {
      const SEXP *restrict pa = STRING_PTR(ans);
      int ct = 0;
      for (int i = xlen-1; i >= 0; --i) {
        if(pa[i] == NA_STRING) {
          ct++;
        } else {
          break;
        }
      }
      if (ct > 0) {
        SETLENGTH(ans, xlen-ct);
      }
    }
  //}
  UNPROTECT(3);
  return ans;
}

/*
 *  Character to factor conversion
 */

SEXP charToFactR (SEXP x, SEXP decreasing, SEXP nthread, SEXP nalast, SEXP env, SEXP addNA) {
  
  if (!IS_BOOL(decreasing)) {
    error("Argument 'decreasing' must be TRUE or FALSE.");
  }
  if (!IS_BOOL(addNA)) {
    error("Argument 'addNA' must be TRUE or FALSE.");
  }
  /*if (!IS_LOGICAL(nalast)) {
    error("Argument 'na.last' must be TRUE, FALSE or NA.");
  }*/
  if (TYPEOF(x) != STRSXP) {
    error("Argument 'x' must be of type character.");
  }
  if (TYPEOF(nthread) != INTSXP) {
    error("Argument 'nThread' (%s) must be of type integer.",type2char(TYPEOF(nthread)));
  }

  const int na_pos = asLogical(nalast);
  const int dcr = asLogical(decreasing);
  const int addNAv = asLogical(addNA);
  const int xlen = LENGTH(x);
  
  SEXP uVals = PROTECT(dupVecSort(x));
  const int n = LENGTH(uVals);
  
  SEXP valSorted = PROTECT(callToSort2(uVals, "quick", dcr, 1, env));
  SEXP *restrict pvalSorted = STRING_PTR(valSorted);
  
  int NAidx = -1;
  for (int i = 0; i < n; ++i) {
    if (pvalSorted[i] == NA_STRING) {
      NAidx = i; break;
    }
  }
  if ( ((na_pos != 0 && !dcr) || (na_pos == 0 && dcr)) && NAidx != n-1) {
    if (NAidx >= 0) {
      memmove(pvalSorted+NAidx, pvalSorted+NAidx+1, (n - ((NAidx + 1)))*sizeof(SEXP));
      pvalSorted[n-1] = NA_STRING;
    }
  } else if ( (na_pos == 0 && !dcr) || (na_pos != 0 && dcr) ){
    if (NAidx > 0 ) {
      memmove(pvalSorted+1, pvalSorted, NAidx*sizeof(SEXP));
      pvalSorted[0] = NA_STRING;
    }
  }
  
  int *restrict lookupTable = buildTable(valSorted);
  const SEXP *restrict px = STRING_PTR(x);
  int nth = asInteger(nthread);
  nth = nth > max_thread ? max_thread : (nth < min_thread ? min_thread : nth); //revisit this
  
  SEXP ans = PROTECT(allocVector(INTSXP, xlen));
  int *restrict pans = INTEGER(ans);
  
  if (addNAv == 0) {
    OMP_PARALLEL_FOR(nth)
    for (int j=0; j<xlen; ++j) {
      pans[j] = px[j] == NA_STRING ? NA_INTEGER : LOOKUP_VAL(px[j]) + 1;
    }
  } else {
    OMP_PARALLEL_FOR(nth)
    for (int j=0; j<xlen; ++j) {
      pans[j] = LOOKUP_VAL(px[j]) + 1;
    }
  }

  if (na_pos == NA_LOGICAL) {
    int ct = 0;
    for (int i = xlen-1; i >= 0; --i) {
      if( px[pans[i]-1] == NA_STRING) {
        ct++;
      } else {
        break;
      }
    }
    if (ct > 0) {
      SETLENGTH(ans, xlen-ct);
    }
  }
  
  free(lookupTable);
  if (addNAv == 0) {
    SETLENGTH(valSorted, LENGTH(valSorted)-1);
  }
  setAttrib(ans, R_LevelsSymbol, valSorted);
  SEXP classV = PROTECT(allocVector(STRSXP,1));
  SET_STRING_ELT(classV, 0, STR_FACTOR);
  classgets(ans, classV);

  UNPROTECT(4);
  return ans;
}
