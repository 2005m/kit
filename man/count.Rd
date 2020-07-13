\name{count}
\alias{count}
\alias{countNA}
\alias{countOccur}
\title{ count, countNA and countOccur}
\description{
Simple functions to count the number of times an element occurs.
}
\usage{
  count(x, value)
  countNA(x)
  countOccur(x)
}
\arguments{
  \item{x}{ A vector or list for \code{countNA}. A vector for \code{count} and a vector or \code{data.frame} for \code{countOccur}.}
  \item{value}{ An element to look for. Must be non \code{NULL}, of length 1 and same type as \code{x}.}
}
\value{
For a vector \code{countNA} will return the total number of \code{NA} value. For a list, \code{countNA} will return a list with the number of \code{NA} in each item of the list.
This is a major difference with \code{sum(is.na(x))} which will return the aggregated number of \code{NA}.
Also, please note that every item of a list can be of different type and \code{countNA} will take them into account whether they are of type logical (\code{NA}), integer (\code{NA_integer_}), double (\code{NA_real_}), complex (\code{NA_complex_}) or character (\code{NA_character_}).
As opposed to \code{countNA}, \code{count} does not support list type and requires \code{x} and \code{value} to be of the same type.
Function \code{countOccur} takes vectors or data.frame as inputs and returns a \code{data.frame} with the number of times each value in the vector occurs or number of times each row in a \code{data.frame} occurs.
}
\seealso{
  \code{\link{pcount}}
}
\author{Morgan Jacob}
\examples{
x = c(1, 3, NA, 5)
count(x, 3)

countNA(x)
countNA(as.list(x))

countOccur(x)

# Benchmarks countNA
# ------------------
# x = sample(c(TRUE,NA,FALSE),1e8,TRUE) # 382 Mb
# microbenchmark::microbenchmark(
#   countNA(x),
#   sum(is.na(x)),
#   times=5L
# )
# Unit: milliseconds
#          expr   min    lq   mean  median    uq   max neval
# countNA(x)     98.7  99.2  101.2   100.1 101.4 106.4     5
# sum(is.na(x)) 405.4 441.3  478.9   461.1 523.9 562.6     5
#
# Benchmarks countOccur
# ---------------------
# x = rnorm(1e6)
# y = data.table::data.table(x)
# microbenchmark::microbenchmark(
#   kit= countOccur(x),
#   data.table = y[, .N, keyby = x],
#   table(x),
#   times = 10L
# )
# Unit: milliseconds
# expr        min         lq     mean    median      uq     max neval
# kit          62.26   63.88    89.29     75.49   95.17  162.40    10
# data.table  189.17  194.08   235.30    227.43  263.74  337.74    10 # setDTthreads(1L)
# data.table  140.15  143.91   190.04    182.85  234.48  261.43    10 # setDTthreads(2L)
# table(x)   3560.77 3705.06  3843.47   3807.12 4048.40 4104.11    10
}
