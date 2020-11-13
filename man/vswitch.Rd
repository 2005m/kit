\name{vswitch/nswitch}
\alias{vswitch}
\alias{nswitch}
\title{ Vectorised switch }
\description{
  \code{vswitch} is a vectorised version of \code{base} function \code{switch}. This function can also be seen as a particular case of function \code{nif}, as shown in examples below, and should also be faster.
}
\usage{
    vswitch(x, values, outputs, default=NULL,
            nThread=getOption("kit.nThread"),
            checkEnc=TRUE)
    nswitch(x, ..., default=NULL,
            nThread=getOption("kit.nThread"),
            checkEnc=TRUE)
}
\arguments{
  \item{x}{A vector or list.}
  \item{values}{A vector or list with values from \code{x} to match. Note that \code{x} and \code{values} must have the same class and attributes.}
  \item{outputs}{A list or vector with the outputs to return for every matching values. Each item of the list must be of length 1 or length of \code{x}. Note that if all list items are of length 1 then it might be simpler to use a vector.}
  \item{...}{A sequence of values and outputs in the following order \code{value1, output1, value2, output2, ..., valueN, outputN}. Values \code{value1, value2, ..., valueN} must all have length1, same type and attributes. Each \code{output} may either share length with \code{x} or be length 1. Please see Examples section for further details.}
  \item{default}{Values to return is no match. Must be a vector or list of length 1 or same length as \code{x}. Also, \code{default} must have the same type, class and aatributes as items from \code{outputs}.}
  \item{nThread}{ A integer for the number of threads to use with \emph{openmp}. Default value is \code{getOption("kit.nThread")}.}
  \item{checkEnc}{ A logical value whether or not to check if \code{x} and \code{values} have comparable and consistant encoding. Default is \code{TRUE}.}
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
a1 = vswitch(
  x = x,
  values = c(10L,20L,30L,40L,50L),
  outputs = c(11L,21L,31L,41L,51L),
  default = NA_integer_
)

# equivalent to the following 'nif' example.
# However for large vectors 'vswitch' should be faster.
b1 = nif(
  x==10L, 11L,
  x==20L, 21L,
  x==30L, 31L,
  x==40L, 41L,
  x==50L, 51L,
  default = NA_integer_
)
identical(a1, b1)

# nswitch can also be used as follows:
c1 = nswitch(x,
  10L, 11L,
  20L, 21L,
  30L, 31L,
  40L, 41L,
  50L, 51L,
  default = NA_integer_
)
identical(a1, c1)

# Example with list in 'outputs' argument
y = c(1, 0, NA_real_)
a2 = vswitch(
  x = y,
  values = c(1, 0),
  outputs = list(c(2, 3, 4), c(5, 6, 7)),
  default = 8
)

b2 = nif(
  y==1, c(2, 3, 4),
  y==0, c(5, 6, 7),
  default = 8
)

identical(a2, b2)

c2 = nswitch(y,
  1, c(2, 3, 4),
  0, c(5, 6, 7),
  default = 8
)

identical(a2, c2)

# Benchmarks
# ----------
# x = sample(1:100, 3e8, TRUE) # 1.1Gb
# microbenchmark::microbenchmark(
# nif=kit::nif(
#   x==10L,  0L,
#   x==20L, 10L,
#   x==30L, 20L,
#   default= 30L
#  ),
# vswitch=kit::vswitch(
#   x, c( 10L, 20L, 30L), list(0L, 10L, 20L), 30L
# ),
# times=10L
# )
# Unit: seconds
#    expr   min    lq  mean median   uq  max neval
# nif      4.27  4.37  4.43   4.42 4.52 4.53    10
# vswitch  1.08  1.09  1.20   1.10 1.43 1.44    10 # 1 thread
# vswitch  0.46  0.57  0.57   0.58 0.58 0.60    10 # 2 threads
}
