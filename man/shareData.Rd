\name{shareData/getData/clearData}
\alias{shareData}
\alias{getData}
\alias{clearData}
\title{ Share Data between R Sessions}
\description{
Experimental functions that enable the user to share a R object between 2 \R sessions.
}
\usage{
  shareData(data, map_name, verbose=FALSE)
  getData(map_name, verbose=FALSE)
  clearData(x, verbose=FALSE)
}
\arguments{
  \item{data}{ A \R object like a vector or a \code{data.frame}.}
  \item{map_name}{ A character. A name for the memory map location where to store the data.}
  \item{x}{ An external pointer like the one returned by function \code{shareData}.}
  \item{verbose}{ A logical value \code{TRUE} or \code{FALSE} to provide or not information to the user.}
}
\value{
\code{shareData} returns a external pointer.
\code{getData} returns an \R object stored in the memory location \code{map_name}.
\code{clearData} returns \code{TRUE} or \code{FALSE} depending on whether the data have been cleared in memory.
}
\author{Morgan Jacob}
\examples{
# In R session 1: share data in memory
# > x = shareData(mtcars,"share1")
#
# In R session 2: get data from session 1
# > getData("share1")
#
# In R session 1: clear data in memory
# > clearData(x)
}
