\name{countNA}
\alias{countNA}
\title{ count NA}
\description{
A simple function to count the number of NA.
}
\usage{
  countNA(x)
}
\arguments{
  \item{x}{ A vector or list.}
}
\value{
For a vector it will return the total number of \code{NA} value. For a list, \code{countNA} will return a list with the number of \code{NA} in each item of the list. This is a major difference with \code{sum(is.na(x))} which will return the aggregated number of \code{NA}.
}
\author{Morgan Jacob}
\examples{
x = c(1, 3, NA, 5)
countNA(x)
countNA(as.list(x))

# Benchmarks
# ----------
# x = sample(c(TRUE,NA,FALSE),1e8,TRUE) # 382 Mb
# microbenchmark::microbenchmark(
#   countNA(x),
#   sum(is.na(x)),
#   times =5L
# )
# Unit: milliseconds
#          expr   min    lq   mean  median    uq   max neval
# countNA(x)     98.7  99.2  101.2   100.1 101.4 106.4     5
# sum(is.na(x)) 405.4 441.3  478.9   461.1 523.9 562.6     5
}
