#include "kit.h"

static const R_CallMethodDef CallEntries[] = {
  {"CfposR",       (DL_FUNC) &fposR,       -1},
  {"CiifR",        (DL_FUNC) &iifR,        -1},
  {"CnifR",        (DL_FUNC) &nifR,        -1},
  {"CompEnabledR", (DL_FUNC) &ompEnabledR, -1},
  {"CpanyR",       (DL_FUNC) &panyR,       -1},
  {"CpallR",       (DL_FUNC) &pallR,       -1},
  {"CpprodR",      (DL_FUNC) &pprodR,      -1},
  {"CpsumR",       (DL_FUNC) &psumR,       -1},
  {"CsetlevelsR",  (DL_FUNC) &setlevelsR,  -1},
  {"CtopnR",       (DL_FUNC) &topnR,       -1},
  {"CuniquePR",    (DL_FUNC) &uniquePR,    -1},
  {"CvswitchR",    (DL_FUNC) &vswitchR,    -1},
  {NULL,           NULL,                   -1}
};

void R_init_kit(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_RegisterCCallable("kit", "CfposR",       (DL_FUNC) &fposR);
  R_RegisterCCallable("kit", "CiifR",        (DL_FUNC) &iifR);
  R_RegisterCCallable("kit", "CnifR",        (DL_FUNC) &nifR);
  R_RegisterCCallable("kit", "CpanyR",       (DL_FUNC) &panyR);
  R_RegisterCCallable("kit", "CpallR",       (DL_FUNC) &pallR);
  R_RegisterCCallable("kit", "CpprodR",      (DL_FUNC) &pprodR);
  R_RegisterCCallable("kit", "CpsumR",       (DL_FUNC) &psumR);
  R_RegisterCCallable("kit", "CsetlevelsR",  (DL_FUNC) &setlevelsR);
  R_RegisterCCallable("kit", "CtopnR",       (DL_FUNC) &topnR);
  R_RegisterCCallable("kit", "CuniquePR",    (DL_FUNC) &uniquePR);
  R_RegisterCCallable("kit", "CvswitchR",    (DL_FUNC) &vswitchR);
}
