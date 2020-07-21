\name{NEWS}
\title{News for \R Package \pkg{kit}}
\encoding{UTF-8}

\newcommand{\CRANpkg}{\href{https://CRAN.R-project.org/package=#1}{\pkg{#1}}}

\section{version 0.0.4 (2020-07-21)}{
  \subsection{New Features}{
    \itemize{
      \item Function \code{\strong{countOccur}(x)}, implemented in C, is comparable to \code{base}
      \R function \code{table}. It returns a \code{data.frame} and is between 3 to 50 times faster.
      For more details, please see \code{?kit::countOccur}.

      \item Functions \code{funique} and \code{fduplicated} now support matrices. Additionally, these
      two functions should also have better performance compare to previous release.

      \item Functions \code{topn} has an additional argument \code{hasna=TRUE} to indicates whether
      data contains \code{NA} value or not. If the data does not contain \code{NA} values, the function
      should be faster.
    }
  }
  \subsection{C-Level Facilities}{
    \itemize{
      \item A few C functions have been added to subset \code{data.frame} and \code{matrix} as well as
	  do other operations. These functions are not exported or visible to the user but might become
	  available and callable at C level in the future.
    }
  }
  \subsection{Bug Fixes}{
    \itemize{
      \item Function \code{fpos} was not properly handling \code{NaN} and \code{NA} for complex and
      double. This should now be fixed. The function has also been changed in case the 'needle' and
      'haysatck' are vectors so that a vector is returned.

      \item Functions \code{funique} and \code{fduplicated} were not properly handling data containing
      \code{POSIX} data. This has now been fixed.
    }
  }
}

\section{version 0.0.3 (2020-06-21)}{
  \subsection{New Features}{
    \itemize{
      \item Functions \code{\strong{fduplicated}(x)} and \code{\strong{funique}(x)},
      implemented in C, are comparable to \code{base} \R functions \code{duplicated} and \code{unique}.
      For more details, please see \code{?kit::funique}.

      \item Functions \code{psum} and \code{pprod} have now better performance for type double and complex.
    }
  }
  \subsection{Bug Fixes}{
    \itemize{
      \item Function \code{count(x, y)} now checks that \code{x} and \code{y} have the same class and
      levels. So does \code{pcount}.

      \item Function \code{pmean} was not callable at C level because of a typo. This is now fixed.
    }
  }
}

\section{version 0.0.2 (2020-05-22)}{
  \subsection{New Features}{
    \itemize{
      \item Function \code{\strong{count}(x, value)}, implemented in C, to simply count the number of times 
      an element \code{value} occurs in a vector or in a list \code{x}. For more details, please see
      \code{?kit::count}.

      \item Function \code{\strong{pmean}(..., na.rm=FALSE)}, \code{\strong{pall}(..., na.rm=FALSE)}, 
      \code{\strong{pany}(..., na.rm=FALSE)} and \code{\strong{pcount}(..., value)}, implemented in C, 
      are similar to already available function \code{psum} and \code{pprod}. These functions respectively 
      apply base \R functions \code{mean}, \code{all} and \code{any} element-wise. For more details, 
      benchmarks and help, please see \code{?kit::pmean}.
    }
  }
  \subsection{Bug Fixes}{
    \itemize{
      \item Fix Solaris Unicode warnings for NEWS file. Benchmarks have been moved from the NEWS file to 
      each function Rd file.
      
      \item Fix some \code{NA} edge cases for \code{pprod} and \code{psum} so these functions behave more
      like base \R function \code{prod} and \code{sum}.

      \item Fix installation errors for version of R (<3.5.0).
    }
  }
}

\section{version 0.0.1 (2020-05-03)}{
  \subsection{Initial Release}{
    \itemize{
      \item Function \code{\strong{fpos}(needle, haystack, all=TRUE, overlap=TRUE)}, implemented in C, is
      inspired by base function \code{which} when used in the following form 
      \code{which(x == y, arr.ind =TRUE}). Function \code{fpos} returns the index(es) or position(s)
      of a matrix/vector within a larger matrix/vector. Please see \code{?kit::fpos} for more 
      details.
      
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
      
      \item Function \code{\strong{vswitch}(x, values, outputs, default=NULL, nThread=getOption("kit.nThread"))}
      , implemented in C, is a vectorised version of \code{base} \R function \code{switch}. This 
      function can also be seen as a particular case of function \code{nif}. Please see 
      \code{?kit::switch} for more details.
    }
  }
}
