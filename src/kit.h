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

extern SEXP fposR(SEXP needle, SEXP haystack, SEXP all, SEXP overlap);
extern SEXP iifR(SEXP l, SEXP a, SEXP b, SEXP na, SEXP tprom, SEXP nthreads);
extern SEXP nifR(SEXP na, SEXP rho, SEXP md, SEXP args);
extern SEXP ompEnabledR();
extern SEXP pallR(SEXP na, SEXP args);
extern SEXP panyR(SEXP na, SEXP args);
extern SEXP pmeanR(SEXP na, SEXP args);
extern SEXP pprodR(SEXP na, SEXP args);
extern SEXP psumR(SEXP na, SEXP args);
extern SEXP setlevelsR(SEXP x, SEXP old_lvl, SEXP new_lvl, SEXP skip_absent);
extern SEXP topnR(SEXP vec, SEXP n, SEXP dec);
extern SEXP uniquePR(SEXP x);
extern SEXP vswitchR(SEXP x, SEXP values, SEXP outputs, SEXP na, SEXP nthreads);

Rboolean hasNA(SEXP x);
R_xlen_t countNA(SEXP x);
SEXP removeNA(SEXP x);
