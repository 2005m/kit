\name{setlevels}
\alias{setlevels}
\title{ Set levels of a factor object }
\description{
  A function to set levels of a factor object.
}
\usage{
setlevels(x, old=levels(x), new, skip_absent=FALSE)
}
\arguments{
  \item{x}{ A factor object. }
  \item{old}{ A character vector containing the factor levels to be changed. Default is levels of \code{x}. }
  \item{new}{ The new character vector containing the factor levels to be added. }
  \item{skip_absent}{ Skip items in \code{old} that are missing (i.e. absent) in `names(x)`. Default \code{FALSE} halts with error if any are missing. }
}
\value{
  Returns an invisible and modified factor object.
}
\author{Morgan Jacob}
\examples{
x = factor(c("A", "A", "B", "B", "B", "C")) # factor vector with levels A B C
setlevels(x, new = c("X", "Y", "Z"))        # set factor levels to: X Y Z
setlevels(x, old = "X", new = "A")          # set factor levels X to A
}
