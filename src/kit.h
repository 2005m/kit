#include <R.h>
#include <R_ext/Rdynload.h>
#include <Rversion.h>
#if !defined(R_VERSION) || R_VERSION < R_Version(3, 5, 0)
  #define USE_RINTERNALS
  #define DATAPTR_RO(x) ((const void *)DATAPTR(x))
#endif
#include <Rinternals.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#ifdef _OPENMP
  #include<omp.h>
  #define omp_enabled true
  #define max_thread omp_get_num_procs()
  #define min_thread 1
  #define OMP_PARALLEL_FOR(nth) _Pragma("omp parallel for num_threads(nth)")
#else
  #define omp_enabled false
  #define max_thread 1
  #define min_thread 1
  #define omp_get_thread_num() 0
  #define OMP_PARALLEL_FOR(n)
#endif

#define UTYPEOF(x) ((unsigned)TYPEOF(x))
#define IS_BOOL(x) (LENGTH(x)==1 && TYPEOF(x)==LGLSXP && LOGICAL(x)[0]!=NA_LOGICAL)
#define IS_VALID_TYPE(x) ((x) == LGLSXP || (x)==INTSXP || (x)==REALSXP || (x)==CPLXSXP || (x)==STRSXP || (x)==VECSXP)
#define PTR_ETL(x, y) (((const SEXP *)DATAPTR_RO(x))[y])
#define SEXPPTR_RO(x) ((const SEXP *)DATAPTR_RO(x))
#define ISNA_COMPLEX(x) (ISNA(x.r) || ISNA(x.i))
#define ISNAN_COMPLEX(x) (ISNAN(x.r) || ISNAN(x.i))
#define EQUAL_CPLX(x, y) (((x.r) == (y.r)) && ((x.i) == (y.i)))
#define RCHAR(x, y) CHAR(STRING_ELT(x, y))
#define SEXP_F ScalarLogical(FALSE)
#define SEXP_T ScalarLogical(TRUE)
#define HASH(key, K)  (3141592653U * (unsigned int)(key) >> (32 - (K)))
#define N_ISNAN(x, y) (!ISNAN(x) && !ISNAN(y))
#define B_IsNA(x, y)  (R_IsNA(x) && R_IsNA(y))
#define B_IsNaN(x, y) (R_IsNaN(x) && R_IsNaN(y))
#define B_ISNAN(x, y) (ISNAN(x) && ISNAN(y))
#define C_IsNA(x)     (R_IsNA(x.r) || R_IsNA(x.i))
#define C_IsNaN(x)    (R_IsNaN(x.r) || R_IsNaN(x.i))
#define C_ISNAN(x, y) (B_ISNAN(x, y) || (N_ISNAN(x, y) && x == y))
#define REQUAL(x, y)  (N_ISNAN(x, y) ? (x == y) : (B_IsNA(x, y) || B_IsNaN(x, y)))
#define CEQUAL(x, y) ((N_ISNAN(x.r, x.i) && N_ISNAN(y.r, y.i)) ? (x.r == y.r && x.i == y.i) : (C_IsNA(x) ? C_IsNA(y) : (C_IsNA(y) ? 0 : (C_ISNAN(x.r, y.r) && C_ISNAN(x.i, y.i)))))
#define STR_DF mkString("data.frame")

extern SEXP addColToDataFrame(SEXP df, SEXP mcol, SEXP coln);
extern SEXP countR(SEXP x, SEXP y);
extern SEXP countNAR(SEXP x);
extern SEXP countOccurR(SEXP x);
extern SEXP countOccurDataFrameR(SEXP x);
extern SEXP dupR(SEXP x, SEXP uniq);
extern SEXP dupVecR(SEXP x, SEXP uniq);
extern SEXP dupVecIndexR(SEXP x);
extern SEXP dupVecIndexOnlyR(SEXP x);
extern SEXP dupVecIndexCountR(SEXP x);
extern SEXP dupDataFrameR(SEXP x, SEXP uniq);
extern SEXP dupDataFrameIndexR(SEXP x);
extern SEXP dupDataFrameIndexCountR(SEXP x);
extern SEXP dupMatrixR(SEXP x, SEXP uniq);
extern SEXP dupMatrixIndexR(SEXP x);
extern SEXP fposR(SEXP needle, SEXP haystack, SEXP all, SEXP overlap);
extern SEXP fposMatR(SEXP needle, SEXP haystack, SEXP all, SEXP overlap);
extern SEXP fposVectR(SEXP ndle, SEXP hsk, SEXP all, SEXP overlap);
extern SEXP groupEltVectR(SEXP x);
extern SEXP groupEltDataFrameR(SEXP x);
extern SEXP iifR(SEXP l, SEXP a, SEXP b, SEXP na, SEXP tprom, SEXP nthreads);
extern SEXP nifR(SEXP na, SEXP rho, SEXP md, SEXP args);
extern SEXP ompEnabledR();
extern SEXP pallR(SEXP na, SEXP args);
extern SEXP panyR(SEXP na, SEXP args);
extern SEXP pcountR(SEXP x, SEXP args);
extern SEXP pmeanR(SEXP na, SEXP args);
extern SEXP pprodR(SEXP na, SEXP args);
extern SEXP psumR(SEXP na, SEXP args);
extern SEXP setlevelsR(SEXP x, SEXP old_lvl, SEXP new_lvl, SEXP skip_absent);
extern SEXP subSetColDataFrame(SEXP df, SEXP str);
extern SEXP subSetColMatrix(SEXP x, R_xlen_t idx);
extern SEXP subSetRowDataFrame(SEXP df, SEXP rws);
extern SEXP subSetRowMatrix(SEXP mat, SEXP rws);
extern SEXP topnR(SEXP vec, SEXP n, SEXP dec, SEXP hasna);
extern SEXP vswitchR(SEXP x, SEXP values, SEXP outputs, SEXP na, SEXP nthreads);

union uno { double d; unsigned int u[2]; };
bool isMixEnc(SEXP x);
SEXP enc2UTF8(SEXP x);
