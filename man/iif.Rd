\name{iif}
\alias{iif}
\alias{iif}
\title{ Fast if else }
\description{
\code{iif} is a faster and more robust replacement of \code{\link[base]{ifelse}}. It is comparable to \code{dplyr::if_else}, \code{hutils::if_else} and \code{data.table::fifelse}. It returns a value with the same length as \code{test} filled with corresponding values from \code{yes}, \code{no} or eventually \code{na}, depending on \code{test}. It does not support S4 classes.
}
\usage{
  iif(test, yes, no, na=NULL, tprom=FALSE, nThread=getOption("kit.nThread"))
}
\arguments{
  \item{test}{ A logical vector. }
  \item{yes, no}{ Values to return depending on \code{TRUE}/\code{FALSE} element of \code{test}. They must be the same type and be either length \code{1} or the same length of \code{test}. }
  \item{na}{ Value to return if an element of \code{test} is missing. It must be the same type as \code{yes}/\code{no} and be either length \code{1} or the same length of \code{test}. Please note that \code{NA} is treated as logical value of length 1 as per the R documentation. \code{NA_integer_}, \code{NA_real_}, \code{NA_complex_} and \code{NA_character_} are equivalent to \code{NA} but for integer, double, complex and character. Default value for argument \code{na} is \code{NULL} and will automatically default to the equivalent NA type of argument \code{yes}.}
  \item{tprom}{ Argument to indicate whether type promotion of \code{yes} and \code{no} is allowed or not. Either \code{FALSE} or \code{TRUE}, default is \code{FALSE} to not allow type promotion. }
  \item{nThread}{ A integer for the number of threads to use with \emph{openmp}. Default value is \code{getOption("kit.nThread")}.}
}
\details{
In contrast to \code{\link[base]{ifelse}} attributes are copied from \code{yes} to the output. This is useful when returning \code{Date}, \code{factor} or other classes.
Like \code{dplyr::if_else} and \code{hutils::if_else}, the \code{na} argument is by default set to \code{NULL}. This argument is set to \code{NA} in data.table::fifelse.
Similarly to \code{dplyr::if_else} and when \code{tprom=FALSE}, \code{iif} requires same type for arguments \code{yes} and \code{no}. This is not strictly the case for \code{data.table::fifelse} which will coerce integer to double. 
When \code{tprom=TRUE}, \code{iif} behaviour is similar to \code{base::ifelse} in the sense that it will promote or coerce \code{yes} and \code{no}to the "highest" used type. Note, however, that unlike \code{base::ifelse} attributes are still conserved.
}
\value{
A vector of the same length as \code{test} and attributes as \code{yes}. Data values are taken from the values of \code{yes} and \code{no}, eventually \code{na}.
}
\seealso{
  \code{\link{nif}}
  \code{\link{vswitch}}
}
\author{Morgan Jacob}
\examples{
x = c(1:4, 3:2, 1:4)
iif(x > 2L, x, x - 1L)

# unlike ifelse, iif preserves attributes, taken from the 'yes' argument
dates = as.Date(c("2011-01-01","2011-01-02","2011-01-03","2011-01-04","2011-01-05"))
ifelse(dates == "2011-01-01", dates - 1, dates)
iif(dates == "2011-01-01", dates - 1, dates)
yes = factor(c("a","b","c"))
no = yes[1L]
ifelse(c(TRUE,FALSE,TRUE), yes, no)
iif(c(TRUE,FALSE,TRUE), yes, no)

# Example of using the 'na' argument
iif(test = c(-5L:5L < 0L, NA), yes = 1L, no = 0L, na = 2L)

# Example of using the 'tprom' argument
iif(test = c(-5L:5L < 0L, NA), yes = 1L, no = "0", na = 2L, tprom = TRUE)
}
\keyword{ data }
