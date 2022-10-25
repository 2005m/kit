\name{parallel-funs}
\alias{psum}
\alias{pprod}
\alias{pall}
\alias{pallNA}
\alias{pallv}
\alias{pany}
\alias{panyNA}
\alias{panyv}
\alias{pmean}
\alias{pcount}
\alias{pcountNA}
\alias{pfirst}
\alias{plast}
\title{Parallel Statistical Functions}
\description{
Similar to \code{pmin} and \code{pmax} but for sum, product, mean, first and last non-missing value, and for logical evaluations and counting. %Only works for integer, double and complex types. 
These functions do not recycle vectors. \code{pany} and \code{pall} are derived from base functions \code{all} and \code{any}. Note that \code{pmean} only works with integer and double types.
\code{pcountNA} is equivalent to \code{pcount} with the argument \code{value} set to \code{NA} and they both allow \code{NA} counting in the case of mixed data types. \code{pfirst} and \code{plast} find the first non-missing value or non-empty element of lists. 
}
\usage{
  psum(..., na.rm=FALSE)
  pprod(..., na.rm=FALSE)
  pall(..., na.rm=FALSE)
  pallNA(...)
  pallv(..., value)
  pany(..., na.rm=FALSE)
  panyNA(...)
  panyv(..., value)
  pmean(..., na.rm=FALSE)
  pcount(..., value)
  pcountNA(...)
  pfirst(...)
  plast(...)
}
\arguments{
  \item{...}{ Numeric arguments of type integer, double complex, logical vector for \code{pall} and \code{pany}, or any types for \code{pcountNA}, \code{pfirst}, \code{plast}. Alternatively these functions also accept a single \code{data.frame}. See example 7 below.}
  \item{na.rm}{ A logical indicating whether missing values should be removed. Default value is \code{FALSE}.}
  \item{value}{ A non \code{NULL} value of length 1. \code{pcount} will count how many times it occurs.}
}
\value{
Return the sum, product or mean of all numeric arguments. The value returned will be of the type of the highest argument types (integer < double < complex). For \code{pall} and \code{pany}, a logical vector is returned.
Functions \code{psum}, \code{pprod} and \code{pmean} will result in error if used with factors.
}
\author{Morgan Jacob}
\examples{
x = c(1, 3, NA, 5)
y = c(2, NA, 4, 1)
z = c(3, 4, 4, 1)

# Example 1: psum 
psum(x, y, z, na.rm = FALSE)
psum(x, y, z, na.rm = TRUE)

# Example 2: pprod
pprod(x, y, z, na.rm = FALSE)
pprod(x, y, z, na.rm = TRUE)

# Example 3: pmean
pmean(x, y, z, na.rm = FALSE)
pmean(x, y, z, na.rm = TRUE)

# Adjust x, y, and z to use in pall and pany
x = c(TRUE, FALSE, NA, FALSE)
y = c(TRUE, NA, TRUE, TRUE)
z = c(TRUE, TRUE, FALSE, NA)

# Example 4: pall
pall(x, y, z, na.rm = FALSE)
pall(x, y, z, na.rm = TRUE)

# Example 5: pany
pany(x, y, z, na.rm = FALSE)
pany(x, y, z, na.rm = TRUE)

# Example 6: pcount
pcount(x, y, z, value = TRUE)

# Example 7: data.frameas an input
pprod(iris[,1:2])
psum(iris[,1:2])

# Benchmarks
# ----------
# n = 1e8L
# x = rnorm(n) # 763 Mb
# y = rnorm(n)
# z = rnorm(n)
# 
# microbenchmark::microbenchmark(
#   kit=psum(x, y, z, na.rm = TRUE),
#   base=rowSums(do.call(cbind,list(x, y, z)), na.rm=TRUE),
#   times = 5L, unit = "s"
# )
# Unit: Second
# expr  min   lq mean median   uq  max neval
# kit  0.52 0.52 0.65   0.55 0.83 0.84     5
# base 2.16 2.27 2.34   2.35 2.43 2.49     5
#
# x = sample(c(TRUE, FALSE, NA), n, TRUE) # 382 Mb
# y = sample(c(TRUE, FALSE, NA), n, TRUE)
# z = sample(c(TRUE, FALSE, NA), n, TRUE)
# 
# microbenchmark::microbenchmark(
#   kit=pany(x, y, z, na.rm = TRUE),
#   base=sapply(1:n, function(i) any(x[i],y[i],z[i],na.rm=TRUE)),
#   times = 5L
# )
# Unit: Second
# expr    min     lq   mean   median     uq    max neval
# kit    1.07   1.09   1.15     1.10   1.23   1.23     5
# base 111.31 112.02 112.78   112.97 113.55 114.03     5
}
