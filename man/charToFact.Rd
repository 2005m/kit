\name{charToFact}
\alias{charToFact}
\title{Convert Character Vector to Factor}
\description{
  Similar to \code{base::as.factor} but much faster and only for converting character vector to factor.
}
\usage{
charToFact(x, nThread=getOption("kit.nThread"))
}
\arguments{
  \item{x}{ A vectorof type character}
  \item{nThread}{ Number of thread to use.}
}
\value{
  The character vector input as a factor. Please note that, unlike \code{as.factor}, \code{NA} levels are preserved.
}
\examples{
x = c("b","A","B","a","\xe4","a")
Encoding(x) = "latin1"
identical(charToFact(x), as.factor(x))
identical(charToFact(c("a","b",NA,"a")), addNA(as.factor(c("a","b",NA,"a"))))

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
