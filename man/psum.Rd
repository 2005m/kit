\name{psum/pprod/pmean/pall/pany}
\alias{psum}
\alias{pprod}
\alias{pall}
\alias{pany}
\alias{pmean}
\title{ Sum, Product, Mean and more}
\description{
Similar to \code{pmin} and \code{pmax} but for sum, product and mean. Only works for integer, double and complex types. These functions do not recycle vectors. \code{pany} and \code{pall} are derived from base functions \code{all} and \code{any}. Note that \code{pmean} only works with integer and double types.
}
\usage{
  psum(..., na.rm=FALSE)
  pprod(..., na.rm=FALSE)
  pall(..., na.rm=FALSE)
  pany(..., na.rm=FALSE)
  pmean(..., na.rm=FALSE)
}
\arguments{
  \item{...}{ Numeric arguments of type integer, double complex. Logical vector for \code{pall} and \code{pany}.}
  \item{na.rm}{ A logical indicating whether missing values should be removed. Default value is \code{FALSE}.}
}
\value{
Return the sum, product or mean of all numeric arguments. The value returned will be of the type of the highest argument types (integer < double < complex). For \code{pall} and \code{pany}, a logical vector is returned.
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

# Benchmarks
# ----------
# set.seed(123)
# n = 1e7L
# x = rnorm(n)
# y = rnorm(n)
# z = rnorm(n)
# 
# microbenchmark::microbenchmark(
#   psum=psum(x, y, z, na.rm = TRUE), 
#   sum=sapply(1:n, function(i) sum(x[i],y[i],z[i],na.rm=TRUE)),
#   times = 5L
# )
# 
# microbenchmark::microbenchmark(
#   pprod=pprod(x, y, z, na.rm = TRUE), 
#   prod=sapply(1:n, function(i) prod(x[i],y[i],z[i],na.rm=TRUE)),
#   times = 5L
# )
# 
# microbenchmark::microbenchmark(
#   pmean=pmean(x, y, z, na.rm = TRUE), 
#   mean=sapply(1:n, function(i) mean(c(x[i],y[i],z[i]),na.rm=TRUE)),
#   times = 5L
# )
# 
# x = sample(c(TRUE, FALSE, NA),n,TRUE)
# y = sample(c(TRUE, FALSE, NA),n,TRUE)
# z = sample(c(TRUE, FALSE, NA),n,TRUE)
# 
# microbenchmark::microbenchmark(
#   pany=pany(x, y, z, na.rm = TRUE), 
#   any=sapply(1:n, function(i) any(x[i],y[i],z[i],na.rm=TRUE)),
#   times = 5L
# )
# 
# microbenchmark::microbenchmark(
#   pall=pall(x, y, z, na.rm = TRUE), 
#   all=sapply(1:n, function(i) all(x[i],y[i],z[i],na.rm=TRUE)),
#   times = 5L
# )
}
