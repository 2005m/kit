\name{fduplicated/funique}
\alias{fduplicated}
\alias{funique}
\alias{uniqLen}
\title{ Fast duplicated and unique}
\description{
Similar to base R functions \code{duplicated} and \code{unique}, \code{fduplicated} and \code{funique} are slightly faster for vectors and much faster for \code{data.frame}. Function \code{uniqLen} is equivalent to base R \code{length(unique)} or \code{data.tbale::uniqueN}.
}
\usage{
  fduplicated(x)
  funique(x)
  uniqLen(x)
}
\arguments{
  \item{x}{ A vector, data.frame or matrix.}
}
\value{
Function \code{fduplicated} returns a logical vector and \code{funique} returns a vector of the same type as \code{x} without the duplicated value. Function \code{uniqLen} returns an integer.
}
\author{Morgan Jacob}
\examples{
# Example 1: fduplicated
fduplicated(iris$Species)

# Example 2: funique
funique(iris$Species)

# Example 3: uniqLen
uniqLen(iris$Species)

# Benchmarks
# ----------
# x = sample(c(1:10,NA_integer_),1e8,TRUE) # 382 Mb
# microbenchmark::microbenchmark(
#   duplicated(x),
#   fduplicated(x),
#   times = 5L
# )
# Unit: seconds
#           expr  min   lq  mean  median   uq   max neval
# duplicated(x)  2.21 2.21  2.48    2.21 2.22  3.55     5
# fduplicated(x) 0.38 0.39  0.45    0.48 0.49  0.50     5
#
# vs data.table
# -------------
# df = iris[,5:1]
# for (i in 1:16) df = rbind(df, df)  # 338 Mb
# dt = data.table::as.data.table(df)
# microbenchmark::microbenchmark(
#   kit = funique(df),
#   data.table = unique(dt),
#   times = 5L
# )
# Unit: seconds
#       expr  min   lq  mean  median    uq  max neval
# kit        1.22 1.27  1.33    1.27  1.36 1.55     5
# data.table 6.20 6.24  6.43    6.33  6.46 6.93     5 # (setDTthreads(1L))
# data.table 4.20 4.25  4.47    4.26  4.32 5.33     5 # (setDTthreads(2L))
#
# microbenchmark::microbenchmark(
#   kit=uniqLen(x),
#   data.table=uniqueN(x),
#   times = 5L, unit = "s"
# )
# Unit: seconds
#       expr  min    lq  mean  median   uq  max neval
# kit        0.17  0.17  0.17   0.17  0.17 0.17     5
# data.table 1.66  1.68  1.70   1.71  1.71 1.72     5 # (setDTthreads(1L))
# data.table 1.13  1.15  1.16   1.16  1.18 1.18     5 # (setDTthreads(2L))
}
