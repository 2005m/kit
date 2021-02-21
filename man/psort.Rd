\name{psort}
\alias{psort}
\title{Parallel Sort}
\description{
  Similar to \code{base::sort} but just for character vector and partially using parallelism.
  It is currently experimental and might change in the future. Use with caution.
}
\usage{
  psort(x, decreasing=FALSE, na.last=NA,
        nThread=getOption("kit.nThread"),c.locale=TRUE)
}
\arguments{
  \item{x}{ A vector of type character. If other, it will default to \code{base::sort}}
  \item{na.last}{ For controlling the treatment of \code{NA}s. If \code{TRUE}, missing values in the data are put last; if \code{FALSE}, they are put first; if \code{NA}, they are removed. }
  \item{decreasing}{ A boolean indicating where to sort the data in decreasing way. Default is \code{FALSE}. }
  \item{nThread}{ Number of thread to use. Default value is \code{1L}.}
  \item{c.locale}{ A boolean, whether to use C Locale or R session locale. Default TRUE.}
}
\value{
  Returns the input \code{x} in sorted order similar to \code{base::sort} but usually faster. If \code{c.locale=FALSE}, \code{psort} will return the same output as \code{base::sort} with \code{method="quick"}, i.e. using R session locale. If \code{c.locale=TRUE}, \code{psort} will return the same output as \code{base::sort} with \code{method="radix"}, i.e. using C locale. See example below.
}
\author{Morgan Jacob}
\examples{
x = c("b","A","B","a","\xe4")
Encoding(x) = "latin1"
identical(psort(x, c.locale=FALSE), sort(x))
identical(psort(x, c.locale=TRUE), sort(x, method="radix"))

# Benchmarks
# ----------
# strings = as.character(as.hexmode(1:1000))
# x = sample(strings, 1e8, replace=TRUE)
# system.time({kit::psort(x, na.last = TRUE, nThread = 1L)})
#   user  system elapsed 
#  2.833   0.434   3.277
# system.time({sort(x,method="radix",na.last = TRUE)})
#   user  system elapsed 
#  5.597   0.559   6.176
# system.time({x[order(x,method="radix",na.last = TRUE)]})
#   user  system elapsed 
#  5.561   0.563   6.143
}
