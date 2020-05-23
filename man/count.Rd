\name{count}
\alias{count}
\alias{countNA}
\title{ count and countNA}
\description{
Simple functions to count the number of times an element occurs.
}
\usage{
  count(x, value)
  countNA(x)
}
\arguments{
  \item{x}{ A vector or list for \code{countNA}. A vector for \code{count}.}
  \item{value}{ An element to look for. Must be non \code{NULL}, of length 1 and same type as \code{x}.}
}
\value{
For a vector \code{countNA} will return the total number of \code{NA} value. For a list, \code{countNA} will return a list with the number of \code{NA} in each item of the list.
This is a major difference with \code{sum(is.na(x))} which will return the aggregated number of \code{NA}.
Also, please note that every item of a list can be of different type and \code{countNA} will take them into account whether they are of type logical (\code{NA}), integer (\code{NA_integer_}), double (\code{NA_real_}), complex (\code{NA_complex_}) or character (\code{NA_character_}).
As opposed to \code{countNA}, \code{count} does not support list type and requires \code{x} and \code{value} to be of the same type.
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

# Benchmarks
# ----------
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
}
