# Function calls
count       = function(x, value) .Call(CcountR, x, value)
countNA     = function(x) .Call(CcountNAR, x)
countOccur  = function(x) .Call(CcountOccurR, x)
fduplicated = function(x, fromLast = FALSE) .Call(CdupR, x, FALSE, fromLast)
fpos        = function(needle, haystack, all=TRUE, overlap=TRUE) .Call(CfposR, needle, haystack, all, overlap)
funique     = function(x, fromLast = FALSE) .Call(CdupR, x, TRUE, fromLast)
iif         = function(test, yes, no, na=NULL, tprom=FALSE, nThread=getOption("kit.nThread")) .Call(CiifR, test, yes, no, na, tprom, nThread)
nif         = function(..., default=NULL) .Call(CnifR, default, parent.frame(), as.list(substitute(...())))
nswitch     = function(x, ..., default=NULL, nThread=getOption("kit.nThread"), checkEnc = TRUE) .Call(CnswitchR, x, default, nThread, checkEnc, list(...))
pall        = function(..., na.rm=FALSE) .Call(CpallR, na.rm, list(...))
pany        = function(..., na.rm=FALSE) .Call(CpanyR, na.rm, list(...))
pcount      = function(..., value) .Call(CpcountR, value, list(...))
pmean       = function(..., na.rm=FALSE) .Call(CpmeanR, na.rm, list(...))
pprod       = function(..., na.rm=FALSE) .Call(CpprodR, na.rm, list(...))
psum        = function(..., na.rm=FALSE) .Call(CpsumR, na.rm, list(...))
setlevels   = function(x, old = levels(x), new, skip_absent=FALSE) invisible(.Call(CsetlevelsR, x, old, new, skip_absent))
topn        = function(vec, n=6L, decreasing=TRUE, hasna=TRUE) .Call(CtopnR, vec, n, decreasing, hasna)
uniqLen     = function(x) .Call(CdupLenR, x)
vswitch     = function(x, values, outputs, default=NULL, nThread=getOption("kit.nThread"), checkEnc = TRUE) .Call(CvswitchR, x, values, outputs, default, nThread, checkEnc)

.onAttach   = function(libname, pkgname) packageStartupMessage(paste0("Attaching kit 0.0.6 (OPENMP ",if(.Call(CompEnabledR)) "enabled" else "disabled"," using 1 thread)"))
.onLoad     = function(libname, pkgname) options("kit.nThread"=1L)   #nocov
.onUnload   = function(libpath) library.dynam.unload("kit", libpath) #nocov
