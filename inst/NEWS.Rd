\name{NEWS}
\title{News for \R Package \pkg{kit}}
\encoding{UTF-8}

\newcommand{\CRANpkg}{\href{https://CRAN.R-project.org/package=#1}{\pkg{#1}}}
\newcommand{\sspace}{\ifelse{latex}{\out{~}}{\Sexpr{"\u200B"} \Sexpr{"\u200B"} \Sexpr{"\u200B"} }}
\newcommand{\sp}{{\Sexpr{"\u200B"} }}

\section{version 0.0.1 (2020-05-03)}{
  \subsection{Initial Release}{
    \itemize{
      \item Function \code{\strong{fpos}(needle, haystack, all=TRUE, overlap=TRUE)}, implemented in C, is
      inspired by base function \code{which} when used in the following form 
      \code{which(x == y, arr.ind =TRUE}). Function \code{fpos} returns the index(es) or position(s)
      of a matrix/vector within a larger matrix/vector. Please see \code{?kit::fpos} for more 
      details.
      
      \emph{x = matrix(1:5, nrow=1e4, ncol=5e3) # 191Mb\cr
      microbenchmark::microbenchmark(\cr
      \sspace fpos=kit::fpos(1L, x),\cr
      \sspace which=which(x==1L, arr.ind=TRUE),\cr
      \sspace times=10L\cr
      )\cr
      # Unit: milliseconds\cr
      # \sp\sp expr \sp min \sp\sp\sp lq  mean median   \sp\sp uq  max neval\cr
      # fpos  \sp\sp\sp 202  206  \sp\sp 220  \sp\sp\sp\sp\sp\sp  221  231 \sp 241 \sp\sp\sp 10\cr
      # which  612  637   \sp\sp 667 \sp\sp\sp\sp\sp\sp 653  705  \sp 724 \sp\sp\sp 10\cr
      }
      
      \item Function \code{\strong{iif}(test, yes, no, na=NULL, tprom=FALSE, nThread=getOption("kit.nThread"))}, 
      originally contributed as \code{fifelse} in package \CRANpkg{data.table}, was moved to package kit
      to be developed independently. Unlike the current version of \code{fifelse}, \code{iif} allows
      type promotion like base function \code{ifelse}. For further details about the differences 
      with \code{fifelse}, as well as \code{hutils::if_else} and \code{dplyr::if_else}, please see
      \code{?kit::iif}.
      
      \item Function \code{\strong{nif}(..., default=NULL)}, implemented in C, is inspired by 
      \emph{SQL CASE WHEN}. It is comparable to \CRANpkg{dplyr} function \code{case_when} however it 
      evaluates it arguments in a lazy way (i.e only when needed). Function \code{nif} was 
      originally contributed as function \code{fcase} in the \CRANpkg{data.table} package but then moved
      to package kit so its development may resume independently. Please see \code{?kit::nif} for 
      more details.
      
      \item Function \code{\strong{pprod}(..., na.rm=FALSE)} and \code{\strong{psum}(..., na.rm=FALSE)},
      implemented in C, are inspired by base function \code{pmin} and \code{pmax}. These new 
      functions work only for integer, double and complex types and do not recycle vectors. Please 
      see \code{?kit::psum} for more details.
      
      \item Function \code{\strong{setlevels}(x, old, new, skip_absent=FALSE)}, implemented in C, 
      may be used to set levels of a factor object. Please see \code{?kit::setlevels} for more details. 
      
      \item Function \code{\strong{topn}(vec, n=6L, decreasing=TRUE)}, implemented in C, returns the top 
      largest or smallest \code{n} values for a given numeric vector \code{vec}. It is inspired by 
      \code{dplyr::top_n} and equivalent to base functions order and sort in specific cases as shown
      in the documentation. Please see \code{?kit::topn} for more details.
      
      \emph{ x = rnorm(1e7) # 76Mb\cr
      microbenchmark::microbenchmark(\cr
      \sspace  topn=kit::topn(x, 6L),\cr
      \sspace  order=order(x, decreasing=TRUE)[1:6],\cr
      \sspace  times=10L\cr
      )\cr
      # Unit: milliseconds\cr
      # \sp\sp expr min \sp\sp lq  mean median   \sp\sp uq  max neval\cr
      # topn \sp\sp\sp\sp  11  \sp 11  \sp\sp\sp\sp  13   \sp\sp\sp\sp\sp\sp\sp  11  \sp\sp\sp 12  \sp\sp 18  \sp\sp\sp\sp  10\cr
      # order 563 565 \sp\sp  587  \sp\sp\sp\sp\sp  566 \sp 602 661 \sp\sp\sp 10\cr
      microbenchmark::microbenchmark(\cr
      \sspace  topn=x[kit::topn(x, 6L, decreasing=FALSE)],\cr
      \sspace  sort=sort(x, partial=1:6)[1:6],\cr
      \sspace  times=10L\cr
      )\cr
      # Unit: milliseconds\cr
      # expr min \sp\sp\sp lq  mean median   \sp\sp uq  max neval\cr
      # topn \sp\sp  11  \sp\sp 11  \sp\sp\sp\sp  11   \sp\sp\sp\sp\sp\sp\sp  11  \sp\sp 12  \sp\sp\sp 12  \sp\sp\sp\sp  10\cr
      # sort \sp 167 175 \sp\sp  197  \sp\sp\sp\sp\sp  178  205 \sp 303 \sp\sp\sp\sp 10\cr
      }
      
      \item Function \code{\strong{vswitch}(x, values, outputs, default=NULL, nThread=getOption("kit.nThread"))}
      , implemented in C, is a vectorised version of \code{base} \R function \code{switch}. This 
      function can also be seen as a particular case of function \code{nif}. Please see 
      \code{?kit::switch} for more details.
      
      \emph{x = sample(1:100, 3e8, TRUE) # 1.1Gb\cr
      microbenchmark::microbenchmark(\cr
      \sspace  nif=kit::nif(\cr
      \sspace \sspace x==10L, \sp 0L,\cr
      \sspace \sspace x==20L, 10L,\cr
      \sspace \sspace x==30L, 20L,\cr
      \sspace \sspace default= 30L\cr
      \sspace  ),\cr
      \sspace  vswitch=kit::vswitch(\cr
      \sspace \sspace x, c( 10L, 20L, 30L), list(0L, 10L, 20L), 30L\cr
      \sspace  ),\cr
      \sspace  times=10L\cr
      )\cr
      # Unit: seconds\cr
      # \sp\sp\sp\sp\sp expr \sp min \sp\sp\sp\sp\sp lq  mean median   \sp\sp uq \sp max neval\cr
      # nif \sp\sp\sp\sp\sp\sp\sp\sp  4.27 \sp 4.37 \sp 4.43 \sp\sp\sp\sp 4.42 4.52 4.53  \sp\sp\sp\sp  10\cr
      # vswitch  1.08 \sp 1.09 \sp 1.20 \sp\sp\sp\sp  1.10 1.43 1.44  \sp\sp\sp\sp  10 # 1 thread\cr
      # vswitch  0.46 \sp 0.57 \sp 0.57 \sp\sp\sp\sp  0.58 0.58 0.60  \sp\sp\sp\sp  10 # 2 threads\cr
      }
      
    }
  }
}
