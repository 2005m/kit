\name{topn}
\alias{topn}
\title{ Top N values index}
\description{
  \code{topn} is used to get the indices of the few values of an input. This is an extension of \code{\link{which.max}}/\code{\link{which.min}} which provide \emph{only} the first such index. 
  
  The output is the same as \code{order(vec)[1:n]}, but internally optimized not to sort the irrelevant elements of the input (and therefore much faster, for small \code{n} relative to input size).
}
\usage{
  topn(vec, n=6L, decreasing=TRUE, hasna=TRUE, index=TRUE)
}
\arguments{
  \item{vec}{ A numeric vector of type numeric or integer. Other types are not supported yet. }
  \item{n}{ A positive integer value greater or equal to 1. }
  \item{decreasing}{ A logical value (default \code{TRUE}) to indicate whether to sort \code{vec} in decreasing or increasing order. Equivalent to argument \code{decreasing} in function \code{base::order}. Please note that unlike \code{topn} default value in \code{base::order} is \code{FALSE}.}
  \item{hasna}{ A logical value (default \code{TRUE}) to indicate whether \code{vec} contains \code{NA} values. }
  \item{index}{ A logical value (default \code{TRUE}) to indicate whether indexes or values of \code{vec}. }
}
\value{
  \code{integer} vector of indices of the most extreme (according to \code{decreasing}) \code{n} values in vector \code{vec}. Please note that for large value of \code{n}, i.e. 1500 or 2000 (depending on the value of \code{hasna}), \code{topn} will default to base R function \code{order}.
}
\author{Morgan Jacob}
\examples{
x = rnorm(1e4)

# Example 1: index of top 6 negative values 
topn(x, 6L, decreasing=FALSE)
order(x)[1:6]

# Example 2: index of top 6 positive values
topn(x, 6L, decreasing = TRUE)
order(x, decreasing=TRUE)[1:6]

# Example 3: top 6 negative values
topn(x, 6L, decreasing=FALSE, index=FALSE)
sort(x)[1:6]

# Benchmarks
# ----------
# x = rnorm(1e7) # 76Mb
# microbenchmark::microbenchmark(
#   topn=kit::topn(x, 6L),
#   order=order(x, decreasing=TRUE)[1:6],
#   times=10L
# )
# Unit: milliseconds
#  expr min   lq  mean median   uq  max neval
# topn   11   11    13     11   12   18    10
# order 563  565   587    566  602  661    10
#
# microbenchmark::microbenchmark(
#  topn=kit::topn(x, 6L, decreasing=FALSE, index=FALSE),
#  sort=sort(x, partial=1:6)[1:6],
#  times=10L
# )
# Unit: milliseconds
# expr min  lq  mean median   uq  max neval
# topn  11  11    11     11   12   12    10
# sort 167 175   197    178  205  303    10
}
