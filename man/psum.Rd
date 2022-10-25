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
\title{Parallel (Statistical) Functions}
\description{
Vector-valued (statistical) functions operating in parallel over vectors passed as arguments, or a single list of vectors (such as a data frame). Similar to \code{\link{pmin}} and \code{\link{pmax}}, except that these functions do not recycle vectors. 
}
\usage{
  psum(..., na.rm = FALSE)
  pprod(..., na.rm = FALSE)
  pmean(..., na.rm = FALSE)
  pfirst(...)  # (na.rm = TRUE)
  plast(...)   # (na.rm = TRUE)
  pall(..., na.rm = FALSE)
  pallNA(...)
  pallv(..., value)
  pany(..., na.rm = FALSE)
  panyNA(...)
  panyv(..., value)
  pcount(..., value)
  pcountNA(...)
  pfirst(...)
  plast(...)
}
\arguments{
  \item{...}{ suitable (atomic) vectors of the same length, or a single list of vectors (such as a \code{data.frame}). See Details on the allowed data types for each function, and Examples.}
  \item{na.rm}{ A logical indicating whether missing values should be removed. Default value is \code{FALSE}, except for \code{pfirst} and \code{plast}.}
  \item{value}{ A non \code{NULL} value of length 1. } % \code{pcount} will count how many times it occurs.
}
\details{
Functions \code{psum}, \code{pprod} work for integer, logical, double and complex types. \code{pmean} only supports integer, logical and double types. All 3 functions will error if used with factors.

\code{pfirst}/\code{plast} select the first/last non-missing value (or non-empty or \code{NULL} value for list-vectors). They accept all vector types with defined missing values + lists, but can only jointly handle integer and double types (not numeric and complex or character and factor). If factors are passed, they all need to have identical levels.

\code{pany} and \code{pall} are derived from base functions \code{all} and \code{any} and only allow logical inputs.

\code{pcount} counts the occurrence of \code{value}, and expects arguments of the same data type (except for \code{value = NA}). \code{pcountNA} is equivalent to \code{pcount} with \code{value = NA}, and they both allow \code{NA} counting in mixed-type data. \code{pcountNA} additionally supports list vectors and counts empty or \code{NULL} elements as \code{NA}. 

Functions \code{panyv/pallv} are wrappers around \code{pcount}, and \code{panyNA/pallNA} are wrappers around \code{pcountNA}. They return a logical vector instead of the integer count. 

None of these functions recycle vectors i.e. all input vectors need to have the same length. All functions support long vectors with up to \code{2^64-1} elements.
}
\value{
\code{psum/pprod/pmean} return the sum, product or mean of all arguments. The value returned will be of the highest argument type (integer < double < complex). \code{pall[v/NA]} and \code{pany[v/NA]} return a logical vector. \code{pcount[NA]} returns an integer vector. \code{pfirst/plast} return a vector of the same type as the inputs. 
}
\seealso{
Package 'collapse' provides column-wise and scalar-valued analogues to many of these functions.
}
\author{Morgan Jacob and Sebastian Krantz}
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

# Example 4: pfirst and plast
pfirst(x, y, z)
plast(x, y, z)

# Adjust x, y, and z to use in pall and pany
x = c(TRUE, FALSE, NA, FALSE)
y = c(TRUE, NA, TRUE, TRUE)
z = c(TRUE, TRUE, FALSE, NA)

# Example 5: pall
pall(x, y, z, na.rm = FALSE)
pall(x, y, z, na.rm = TRUE)

# Example 6: pany
pany(x, y, z, na.rm = FALSE)
pany(x, y, z, na.rm = TRUE)

# Example 7: pcount
pcount(x, y, z, value = TRUE)
pcountNA(x, y, z)

# Example 8: list/data.frame as an input
pprod(iris[,1:2])
psum(iris[,1:2])
pmean(iris[,1:2])

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
