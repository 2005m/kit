\name{NEWS}
\title{News for \R Package \pkg{kit}}
\encoding{UTF-8}

\newcommand{\CRANpkg}{\href{https://CRAN.R-project.org/package=#1}{\pkg{#1}}}

\section{version 0.0.6 (2021-02-21)}{
  \subsection{New Features}{
    \itemize{
      \item Function \code{\strong{funique}} and \code{\strong{fdplicated}} gain an additional argument
      \code{fromLast=FALSE} to indicate whether the search should start from the end or beginning 
      \href{https://github.com/2005m/kit/pull/11}{PR#11}.
    }
  }
  \subsection{Notes}{
    \itemize{
      \item A few OpenMP directives were missing for functions \code{\strong{vswitch}} and
      \code{\strong{nswitch}} for character vectors. These have been added in
      \href{https://github.com/2005m/kit/pull/12}{PR#12}.
      
      \item Function \code{\strong{funique}} was not preserving attributes for character, logical and
      complex vectors/data.frames. Thanks to Sebastian Krantz (@SebKrantz) for bringing that to my
      attention. This has been fixed in \href{https://github.com/2005m/kit/pull/13}{PR#13}.
      
      \item Functions \code{\strong{funique}} and \code{\strong{uniqLen}} should now be faster for 
      \code{factors} \href{https://github.com/2005m/kit/pull/14}{PR#14}.
    }
  }
}

\section{version 0.0.5 (2020-11-21)}{
  \subsection{New Features}{
    \itemize{
      \item Function \code{\strong{uniqLen}(x)} is equivalent to base R \code{length(unique(x))} and
      \code{uniqueN} in package \CRANpkg{data.table}. Function \code{uniqLen}, implemented in C, supports
      vectors, \code{data.frame} and \code{matrix}. It should be faster than these functions. For more
      details and benchmark please see \code{?kit::uniqLen}.
      
      \item Function \code{\strong{vswitch}} now supports mixed encoding and gains an additional argument 
      \code{checkEnc=TRUE}. Thanks to Xianying Tan (@shrektan) for the request and review 
      \href{https://github.com/2005m/kit/pull/7}{PR#7}.
      
      \item Function \code{\strong{nswitch}} is a nested version of function \code{\strong{vswitch}} 
      and also supports mixed encoding. Please see please see \code{?kit::nswitch} for further details. 
      Thanks to Xianying Tan (@shrektan) for the request and review \href{https://github.com/2005m/kit/pull/10}{PR#10}.
    }
  }
  \subsection{Notes}{
    \itemize{
      \item Small algorithmic improvement for functions \code{\strong{fduplicated}}, \code{\strong{funique}}
      and \code{\strong{countOccur}} for \code{vectors}, \code{data.frame} and \code{matrix}.
      
      \item A tests folder has been added to the source package to track coverage and bugs.
    }
  }
  \subsection{C-Level Facilities}{
    \itemize{
      \item Function \code{\strong{nif}} has been split into two distinctive functions at C level,
      one has its arguments evaluated in a lazy way and is for R users and the other one (nifInternalR)
      is not lazy and is intended for usage at C level.
    }
  }
}

\section{version 0.0.4 (2020-07-21)}{
  \subsection{New Features}{
    \itemize{
      \item Function \code{\strong{countOccur}(x)}, implemented in C, is comparable to \code{base}
      \R function \code{table}. It returns a \code{data.frame} and is between 3 to 50 times faster.
      For more details, please see \code{?kit::countOccur}.

      \item Functions \code{\strong{funique}} and \code{\strong{fduplicated}} now support matrices.
      Additionally, these two functions should also have better performance compare to previous release.

      \item Functions \code{\strong{topn}} has an additional argument \code{hasna=TRUE} to indicates whether
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
      \item Function \code{\strong{fpos}} was not properly handling \code{NaN} and \code{NA} for complex 
      and double. This should now be fixed. The function has also been changed in case the 'needle' and
      'haysatck' are vectors so that a vector is returned.

      \item Functions \code{\strong{funique}} and \code{\strong{fduplicated}} were not properly handling
      data containing \code{POSIX} data. This has now been fixed.
    }
  }
}

\section{version 0.0.3 (2020-06-21)}{
  \subsection{New Features}{
    \itemize{
      \item Functions \code{\strong{fduplicated}(x)} and \code{\strong{funique}(x)}, implemented in C, 
      are comparable to \code{base} \R functions \code{duplicated} and \code{unique}. For more details, 
      please see \code{?kit::funique}.

      \item Functions \code{\strong{psum}} and \code{\strong{pprod}} have now better performance for 
      type double and complex.
    }
  }
  \subsection{Bug Fixes}{
    \itemize{
      \item Function \code{\strong{count}(x, y)} now checks that \code{x} and \code{y} have the same class and
      levels. So does \code{pcount}.

      \item Function \code{\strong{pmean}} was not callable at C level because of a typo. This is now fixed.
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
      are similar to already available function \code{\strong{psum}} and \code{\strong{pprod}}. These 
      functions respectively apply base \R functions \code{mean}, \code{all} and \code{any} element-wise. 
      For more details, benchmarks and help, please see \code{?kit::pmean}.
    }
  }
  \subsection{Bug Fixes}{
    \itemize{
      \item Fix Solaris Unicode warnings for NEWS file. Benchmarks have been moved from the NEWS file to 
      each function Rd file.
      
      \item Fix some \code{NA} edge cases for \code{\strong{pprod}} and \code{\strong{psum}} so these 
      functions behave more like base \R function \code{prod} and \code{sum}.

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
