\name{psum/pprod}
\alias{psum}
\alias{pprod}
\alias{pall}
\alias{pany}
\title{ Sum, Product and more}
\description{
Similar to \code{pmin} and \code{pmax} but for sum and product. Only works for integer, double and complex types. These functions do not recycle vectors. \code{pany} and \code{pall} are derived from base functions \code{all} and \code{any}.
}
\usage{
  psum(..., na.rm=FALSE)
  pprod(..., na.rm=FALSE)
  pall(..., na.rm=FALSE)
  pany(..., na.rm=FALSE)
}
\arguments{
  \item{...}{ Numeric arguments of type integer, double complex. Logical vector for \code{pall} and \code{pany}.}
  \item{na.rm}{ A logical indicating whether missing values should be removed. Default value is \code{FALSE}.}
}
\value{
Return the sum or product of all numeric argument. The value returned will be of the type of the highest argument types (integer < double < complex). For \code{pall} and \code{pany}, a logical vector is returned.
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

# Adjust x, y, and z to use in pall and pany
x = c(TRUE, FALSE, NA, FALSE)
y = c(TRUE, NA, TRUE, TRUE)
z = c(TRUE, TRUE, FALSE, NA)

# Example 3: pall
pall(x, y, z, na.rm = FALSE)
pall(x, y, z, na.rm = TRUE)

# Example 4: pany
pany(x, y, z, na.rm = FALSE)
pany(x, y, z, na.rm = TRUE)
}
