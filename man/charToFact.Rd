\name{charToFact}
\alias{charToFact}
\title{Convert Character Vector to Factor}
\description{
  Similar to \code{base::as.factor} but much faster and only for converting character vector to factor.
}
\usage{
charToFact(x, decreasing=FALSE, addNA=TRUE,
           nThread=getOption("kit.nThread"))
}
\arguments{
  \item{x}{ A vectorof type character}
  \item{decreasing}{ A boolean. Whether to order levels in decreasing order or not. Default is \code{FALSE}.}
  \item{addNA}{ A boolean. Whether to include \code{NA} in levels of the output or not. Default is \code{TRUE}.}
  \item{nThread}{ Number of thread to use.}
}
\value{
  The character vector input as a factor. Please note that, unlike \code{as.factor}, \code{NA} levels are preserved by default, however this can be changed by setting argument \code{addNA} to \code{FALSE}.
}
\examples{
x = c("b","A","B","a","\xe4","a")
Encoding(x) = "latin1"
identical(charToFact(x), as.factor(x))
identical(charToFact(c("a","b",NA,"a")), addNA(as.factor(c("a","b",NA,"a"))))
identical(charToFact(c("a","b",NA,"a"), addNA=FALSE), as.factor(c("a","b",NA,"a")))

# Benchmarks
# ----------
# x = sample(letters,3e7,TRUE)
# microbenchmark::microbenchmark(
#   kit=kit::charToFact(x,nThread = 1L),
#   base=as.factor(x),
#   times = 5L
# )
# Unit: milliseconds
# expr  min   lq   mean  median   uq  max neval
# kit   188  190    196     194  200  208     5
# base 1402 1403   1455    1414 1420 1637     5
}
