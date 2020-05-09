# Function calls

fpos      = function(needle, haystack, all=TRUE, overlap=TRUE) .Call(CfposR, needle, haystack, all, overlap)
iif       = function(test, yes, no, na=NULL, tprom=FALSE, nThread=getOption("kit.nThread")) .Call(CiifR, test, yes, no, na, tprom, nThread)
nif       = function(..., default=NULL) .Call(CnifR, default, parent.frame(), TRUE, as.list(substitute(...())))
pprod     = function(..., na.rm=FALSE) .Call(CpprodR, na.rm, list(...))
psum      = function(..., na.rm=FALSE) .Call(CpsumR, na.rm, list(...))
setlevels = function(x, old = levels(x), new, skip_absent=FALSE) invisible(.Call(CsetlevelsR, x, old, new, skip_absent))
topn      = function(vec, n=6L, decreasing=TRUE) .Call(CtopnR, vec, n, decreasing)
vswitch   = function(x, values, outputs, default=NULL, nThread=getOption("kit.nThread")) .Call(CvswitchR, x, values, outputs, default, nThread)

.onAttach = function(libname, pkgname) packageStartupMessage(paste0("Attaching kit 0.0.1 (OPENMP ",if(.Call(CompEnabledR)) "enabled" else "disabled"," using 1 thread)"))
.onLoad   = function(libname, pkgname) options("kit.nThread"=1L)
.onUnload = function(libpath) library.dynam.unload("kit", libpath)
