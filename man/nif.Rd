\name{nif}
\alias{nif}
\title{Nested if else}
\description{
\code{nif} is a fast implementation of SQL \code{CASE WHEN} statement for R. Conceptually, \code{nif} is a nested version of \code{\link{iif}} (with smarter implementation than manual nesting). It is not the same but it is comparable to \code{dplyr::case_when} and \code{data.table::fcase}.
}
\usage{
  nif(..., default=NULL)
}
\arguments{
\item{...}{ A sequence consisting of logical condition (\code{when})-resulting value (\code{value}) \emph{pairs} in the following order \code{when1, value1, when2, value2, ..., whenN, valueN}. Logical conditions \code{when1, when2, ..., whenN} must all have the same length, type and attributes. Each \code{value} may either share length with \code{when} or be length 1. Please see Examples section for further details.}
\item{default}{ Default return value, \code{NULL} by default, for when all of the logical conditions \code{when1, when2, ..., whenN} are \code{FALSE} or missing for some entries. Argument \code{default} can be a vector either of length 1 or length of logical conditions \code{when1, when2, ..., whenN}. Note that argument 'default' must be named explicitly.}
}
\value{
  Vector with the same length as the logical conditions (\code{when}) in \code{...}, filled with the corresponding values (\code{value}) from \code{...}, or eventually \code{default}. Attributes of output values \code{value1, value2, ...valueN} in \code{...} are preserved.
}
\details{
  Unlike \code{data.table::fcase}, the \code{default} argument is set to \code{NULL}. In addition, \code{nif} can be called by other packages at C level. Note that at C level, the function has an additional argument \code{SEXP md} which is either \code{TRUE} for lazy evaluation or \code{FALSE} for non lazy evaluation. This argument is not exposed to R users and is more for C users.
}
\seealso{
  \code{\link{iif}}
  \code{\link{vswitch}}
}
\author{Morgan Jacob}
\examples{
x = 1:10
nif(
	x < 5L, 1L,
	x > 5L, 3L
)

nif(
	x < 5L, 1L:10L,
	x > 5L, 3L:12L
)

# Lazy evaluation example
nif(
	x < 5L, 1L,
	x >= 5L, 3L,
	x == 5L, stop("provided value is an unexpected one!")
)

# nif preserves attributes, example with dates
nif(
	x < 5L, as.Date("2019-10-11"),
	x > 5L, as.Date("2019-10-14")
)

# nif example with factor; note the matching levels
nif(
	x < 5L, factor("a", levels=letters[1:3]),
	x > 5L, factor("b", levels=letters[1:3])
)

# Example of using the 'default' argument
nif(
	x < 5L, 1L,
	x > 5L, 3L,
	default = 5L
)

nif(
	x < 5L, 1L,
	x > 5L, 3L,
	default = rep(5L, 10L)
)
}
