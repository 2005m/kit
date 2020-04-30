\name{vswitch}
\alias{vswitch}
\title{ Vectorised switch }
\description{
  \code{vswitch} is a vectorised version of \code{base} function \code{switch}. This function can also be seen as a particular case of function \code{nif}, as shown in examples below, and should also be faster.
}
\usage{
  vswitch(x, values, outputs, default=NULL, nThread=getOption("kit.nThread"))
}
\arguments{
  \item{x}{A vector or list.}
  \item{values}{A vector or list with values from \code{x} to match. Note that \code{x} and \code{values} must have the same class and attributes.}
  \item{outputs}{A list returning the outputs to return for every matching values. Each item of the list must be of length 1 or length of \code{x}.}
  \item{default}{Values to return is no match. Must be a vector or list of length 1 or same length as \code{x}. Also, \code{default} must have the same type, class and aatributes as items from \code{outputs}.}
  \item{nThread}{ A integer for the number of threads to use with \emph{openmp}. Default value is \code{getOption("kit.nThread")}.}
}
\value{
  A vector or list of the same length as \code{x} with values from \code{outputs} items and from \code{default} if missing.
}
\seealso{
  \code{\link{iif}}
  \code{\link{nif}}
}
\author{Morgan Jacob}
\examples{
x = sample(c(10L, 20L, 30L, 40L, 50L, 60L), 3e2, replace=TRUE)

# The below example of 'vswitch' is
a = vswitch(
  x = x,
  values = c(10L,20L,30L,40L,50L),
  outputs = list(11L,21L,31L,41L,51L),
  default = NA_integer_
)

# equivalent to the following 'nif' example.
# However for large vectors 'vswitch' should be faster.
b = nif(
  x==10L, 11L,
  x==20L, 21L,
  x==30L, 31L,
  x==40L, 41L,
  x==50L, 51L,
  default = NA_integer_
)

identical(a, b)
}
\keyword{ data }
