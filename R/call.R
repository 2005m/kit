# Function calls
charToFact  = function(x, decreasing=FALSE, addNA=TRUE, nThread=getOption("kit.nThread")) .Call(CcharToFactR, x, decreasing, nThread, NA, parent.frame(), addNA)
count       = function(x, value) .Call(CcountR, x, value)
countNA     = function(x) .Call(CcountNAR, x)
countOccur  = function(x) .Call(CcountOccurR, x)
fduplicated = function(x, fromLast = FALSE) .Call(CdupR, x, FALSE, fromLast)
fpos        = function(needle, haystack, all=TRUE, overlap=TRUE) .Call(CfposR, needle, haystack, all, overlap)
funique     = function(x, fromLast = FALSE) .Call(CdupR, x, TRUE, fromLast)
iif         = function(test, yes, no, na=NULL, tprom=FALSE, nThread=getOption("kit.nThread")) .Call(CiifR, test, yes, no, na, tprom, nThread)
nif         = function(..., default=NULL) .Call(CnifR, default, parent.frame(), as.list(substitute(...())))
nswitch     = function(x, ..., default=NULL, nThread=getOption("kit.nThread"), checkEnc = TRUE) .Call(CnswitchR, x, default, nThread, checkEnc, list(...))
pall        = function(..., na.rm=FALSE) .Call(CpallR, na.rm, if (length(a <- list(...)) == 1 && is.data.frame(a[[1]])) a[[1]] else a)
pany        = function(..., na.rm=FALSE) .Call(CpanyR, na.rm, if (length(a <- list(...)) == 1 && is.data.frame(a[[1]])) a[[1]] else a)
pcount      = function(..., value) .Call(CpcountR, value, list(...))
pmean       = function(..., na.rm=FALSE) .Call(CpmeanR, na.rm, if (length(a <- list(...)) == 1 && is.data.frame(a[[1]])) a[[1]] else a)
pprod       = function(..., na.rm=FALSE) .Call(CpprodR, na.rm, if (length(a <- list(...)) == 1 && is.data.frame(a[[1]])) a[[1]] else a)
psum        = function(..., na.rm=FALSE) .Call(CpsumR,  na.rm, if (length(a <- list(...)) == 1 && is.data.frame(a[[1]])) a[[1]] else a)
setlevels   = function(x, old = levels(x), new, skip_absent=FALSE) invisible(.Call(CsetlevelsR, x, old, new, skip_absent))
topn        = function(vec, n=6L, decreasing=TRUE, hasna=TRUE,index=TRUE) if(index) .Call(CtopnR, vec, n, decreasing, hasna, parent.frame()) else vec[.Call(CtopnR, vec, n, decreasing, hasna, parent.frame())]
uniqLen     = function(x) .Call(CdupLenR, x)
vswitch     = function(x, values, outputs, default=NULL, nThread=getOption("kit.nThread"), checkEnc = TRUE) .Call(CvswitchR, x, values, outputs, default, nThread, checkEnc)

.onAttach   = function(libname, pkgname) packageStartupMessage(paste0("Attaching kit 0.0.9 (OPENMP ",if(.Call(CompEnabledR)) "enabled" else "disabled"," using 1 thread)"))
.onLoad     = function(libname, pkgname) options("kit.nThread"=1L)   #nocov
.onUnload   = function(libpath) library.dynam.unload("kit", libpath) #nocov

psort = function(x, decreasing = FALSE, na.last = NA, nThread=getOption("kit.nThread"), c.locale = TRUE) {
  if (typeof(x) == "character") {
    return(.Call(CcpsortR, x, decreasing, nThread, na.last,parent.frame(), FALSE, c.locale))
  }
  warning("Function 'psort' was only implemented for character vectors. Defaulting to base::sort.")
  sort(x, decreasing = decreasing, na.last = na.last,method = if(c.locale) "radix" else "quick")
}

clearData = function(x, verbose=FALSE) .Call("CclearMappingObjectR", x, verbose)

shareData = function(data, map_name, verbose=FALSE) {
  conn = rawConnection(raw(0L), "w")
  serialize(data, conn)
  seek(conn, 0L)
  if (grepl('SunOS',Sys.info()['sysname'])) map_name = paste0("/",map_name)
  x = .Call(
    "CcreateMappingObjectR", map_name, paste0(map_name,"_key"),
    rawConnectionValue(conn), verbose
  )
  close(conn)
  x
}

getData = function(map_name, verbose=FALSE) {
  if (grepl('SunOS',Sys.info()['sysname'])) map_name = paste0("/",map_name)
  output = .Call("CgetMappingObjectR", map_name, paste0(map_name,"_key"), verbose)
  conn = rawConnection(output,"r")
  obj = unserialize(conn)
  close(conn)
  obj
}
