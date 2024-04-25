\name{NEWS}
\title{News for \R Package \pkg{kit}}
\encoding{UTF-8}

\newcommand{\CRANpkg}{\href{https://CRAN.R-project.org/package=#1}{\pkg{#1}}}

\section{version 0.0.17 (2024-04-25)}{
  \subsection{Bug Fixes}{
    \itemize{
      \item Fix \code{nswitch}. Thanks to Sebastian Krantz for raising an issue.
    }
  }
  \subsection{Notes}{
    \itemize{
      \item Update copyright date in c files
    }
  }
}

\section{version 0.0.16 (2024-03-01)}{
  \subsection{Notes}{
    \itemize{
     \item Check if \code{"kit.nThread"} is defined before setting it to \code{1L}
    }
  }
}

\section{version 0.0.15 (2023-10-01)}{
  \subsection{Notes}{
    \itemize{
     \item Correct typo in configure file
    }
  }
}

\section{version 0.0.14 (2023-08-12)}{
  \subsection{Notes}{
    \itemize{
     \item Update configure file to extend support for GCC

     \item Correct warnings in NEWS.Rd (strong)

     \item Correct typo in funique.Rd thanks to @davidbudzynski
    }
  }
}

\section{version 0.0.13 (2023-02-24)}{
  \subsection{Notes}{
    \itemize{
     \item Function \code{pprod} now returns double output even if inputs are integer - in line with \code{base::prod} - to avoid integer overflows.

     \item Update configure file
    }
  }
}

\section{version 0.0.12 (2022-10-26)}{
  \subsection{New Features}{
    \itemize{
      \item Function \code{pcountNA} is equivalent to \code{pcount(..., value = NA)}.
      
      \item Function \code{pcountNA} and \code{pcount(..., value = NA)} allow \code{NA} counting with mixed data type (including \code{data.frame}). \code{pcountNA} also supports list-vectors as inputs and counts empty or \code{NULL} elements as \code{NA}.
      
      \item Functions \code{panyv}, \code{panyNA}, \code{pallv} and \code{pallNA} are added as efficient wrappers around \code{pcount} and \code{pcountNA}. They are parallel equivalents of scalar functions \code{base::anyNA} and \code{anyv}, \code{allv} and \code{allNA} in the 'collapse' R package.
      
      \item Functions \code{pfirst} and \code{plast} are added to efficiently obtain the row-wise first and last non-missing value or non-empty element of lists. They are parallel equivalents to the (column-wise) \code{ffirst} and \code{flast} functions in the 'collapse' R package. Implemented by @SebKrantz.
      
      \item Functions \code{psum/pprod/pmean} also support logical vectors as input. Implemented by @SebKrantz.
    }
  }
  \subsection{Bug Fixes}{
    \itemize{
      \item Function \code{charToFact} was not returning proper results. Thanks to @alex-raw for raising an issue.
    }
  }
  \subsection{Notes}{
    \itemize{
     \item Function \code{pprod} now returns double output even if inputs are integer - in line with \code{base::prod} - to avoid integer overflows.
    
      \item C compiler warnings on CRAN R-devel caused by compilation with -Wstrict-prototypes are now fixed. Declaration of functions without prototypes is depreciated in all versions of C. Thanks to Sebastian Krantz for the PR.
    }
  }
}

\section{version 0.0.11 (2022-03-19)}{
  \subsection{New Features}{
    \itemize{
      \item Function \code{pcount} now supports data.frame.
    }
  }
  \subsection{Bug Fixes}{
    \itemize{
      \item Function \code{pcount} now works with specific NA values, i.e. NA_real_, NA_character_ etc...
    }
  }
}

\section{version 0.0.10 (2021-11-28)}{
  \subsection{New Features}{
    \itemize{
      \item Function \code{psum}, \code{pmean}, \code{pprod}, \code{pany} and \code{pall} now support lists. Thanks to Sebastian Krantz for the request and code suggestion.
    }
  }
  \subsection{Bug Fixes}{
    \itemize{
      \item Function \code{topn} should now work for ALTREP object. Thanks to @ben-schwen for raising an issue.
    }
  }
}

\section{version 0.0.9 (2021-09-12)}{
  \subsection{Notes}{
    \itemize{
      \item Re-organise header to prevent compilation errors with new version of Clang due to conflicts between R C headers and OpenMP.
    }
  }
}

\section{version 0.0.8 (2021-08-21)}{
  \subsection{New Features}{
    \itemize{
      \item Function \code{funique} now preserves the attributes if the input is a 
      \code{data.table}, \code{tibble} or similar objects. Thanks to Sebastian Krantz for the request.
      
      \item Function \code{topn} now defaults to base R \code{order} for large value of \code{n}.
      Please see updated documentation for more information \code{?kit::topn}.
      
      \item Function \code{charToFact} gains a new argument \code{addNA=TRUE} to be used
      to include (or not) \code{NA} in levels of the output.
      
      \item Function \code{shareData}, \code{getData} and \code{clearData} implemented
      to share data objects between \R sessions. These functions are experimental and might change in the future.
      Feedback is welcome. Please see \code{?kit::shareData} for more information.
    }
  }
  \subsection{Notes}{
    \itemize{
      \item Few \code{calloc} functions at C level have been replaced by R C API function 
      \code{Calloc} to avoid valgrind errors/warnings in Travis CI.
      
      \item Errors reported by \code{rchk} on CRAN have been fixed.
    }
  }
}

\section{version 0.0.7 (2021-03-07)}{
  \subsection{New Features}{
    \itemize{
      \item Function \code{charToFact} gains a new argument \code{decreasing=FALSE} to be used
      to order levels of the output in decreasing or increasing order.
      
      \item Function \code{topn} gains a new argument \code{index=TRUE} to be used return
      index (\code{TRUE}) or values (\code{FALSE}) of input vector.
    }
  }
  \subsection{Bug Fixes}{
    \itemize{
      \item Some tests of memory access errors using valgrind and AddressSanitizer were reported by CRAN.
      An attempt to fix these errors has been submitted as part of this package version. It also seems that
      these same errors were causing some tests to fail for \code{funique} and \code{psort} on some platforms.
    }
  }
  \subsection{Notes}{
    \itemize{
      \item Functions \code{pmean}, \code{pprod} and \code{psum} will result
      in error if used with factors. Documentation has been updated.
    }
  }
}

\section{version 0.0.6 (2021-02-21)}{
  \subsection{New Features}{
    \itemize{
      \item Function \code{funique} and \code{fduplicated} gain an additional argument
      \code{fromLast=FALSE} to indicate whether the search should start from the end or beginning 
      \href{https://github.com/2005m/kit/pull/11}{PR#11}.
      
      \item Functions \code{pall}, \code{pany}, \code{pmean},
      \code{pprod} and \code{psum} accept \code{data.frame} as input
      \href{https://github.com/2005m/kit/pull/15}{PR#15}. Please see documentation for more
      information.
      
      \item Function \code{charToFact} is equivalent to to base R \code{as.factor} but is much
      quicker and only converts character vector to factor. Note that it is parallelised. For more
      details and benchmark please see \code{?kit::charToFact}.
      
      \item Function \code{psort} is experimental and equivalent to to base R \code{sort} 
      but is only for character vector. It can sort by "C locale" or by "R session locale".
      For more details and benchmark please see \code{?kit::psort}.
    }
  }
  \subsection{Notes}{
    \itemize{
      \item A few OpenMP directives were missing for functions \code{vswitch} and
      \code{nswitch} for character vectors. These have been added in
      \href{https://github.com/2005m/kit/pull/12}{PR#12}.
      
      \item Function \code{funique} was not preserving attributes for character, logical and
      complex vectors/data.frames. Thanks to Sebastian Krantz (@SebKrantz) for bringing that to my
      attention. This has been fixed in \href{https://github.com/2005m/kit/pull/13}{PR#13}.
      
      \item Functions \code{funique} and \code{uniqLen} should now be faster for 
      \code{factor} and \code{logical} vectors \href{https://github.com/2005m/kit/pull/14}{PR#14}.
    }
  }
}

\section{version 0.0.5 (2020-11-21)}{
  \subsection{New Features}{
    \itemize{
      \item Function \code{uniqLen(x)} is equivalent to base R \code{length(unique(x))} and
      \code{uniqueN} in package \CRANpkg{data.table}. Function \code{uniqLen}, implemented in C, supports
      vectors, \code{data.frame} and \code{matrix}. It should be faster than these functions. For more
      details and benchmark please see \code{?kit::uniqLen}.
      
      \item Function \code{vswitch} now supports mixed encoding and gains an additional argument 
      \code{checkEnc=TRUE}. Thanks to Xianying Tan (@shrektan) for the request and review 
      \href{https://github.com/2005m/kit/pull/7}{PR#7}.
      
      \item Function \code{nswitch} is a nested version of function \code{vswitch} 
      and also supports mixed encoding. Please see please see \code{?kit::nswitch} for further details. 
      Thanks to Xianying Tan (@shrektan) for the request and review \href{https://github.com/2005m/kit/pull/10}{PR#10}.
    }
  }
  \subsection{Notes}{
    \itemize{
      \item Small algorithmic improvement for functions \code{fduplicated}, \code{funique}
      and \code{countOccur} for \code{vectors}, \code{data.frame} and \code{matrix}.
      
      \item A tests folder has been added to the source package to track coverage and bugs.
    }
  }
  \subsection{C-Level Facilities}{
    \itemize{
      \item Function \code{nif} has been split into two distinctive functions at C level,
      one has its arguments evaluated in a lazy way and is for R users and the other one (nifInternalR)
      is not lazy and is intended for usage at C level.
    }
  }
}

\section{version 0.0.4 (2020-07-21)}{
  \subsection{New Features}{
    \itemize{
      \item Function \code{countOccur(x)}, implemented in C, is comparable to \code{base}
      \R function \code{table}. It returns a \code{data.frame} and is between 3 to 50 times faster.
      For more details, please see \code{?kit::countOccur}.

      \item Functions \code{funique} and \code{fduplicated} now support matrices.
      Additionally, these two functions should also have better performance compare to previous release.

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
      \item Function \code{fpos} was not properly handling \code{NaN} and \code{NA} for complex 
      and double. This should now be fixed. The function has also been changed in case the 'needle' and
      'haysatck' are vectors so that a vector is returned.

      \item Functions \code{funique} and \code{fduplicated} were not properly handling
      data containing \code{POSIX} data. This has now been fixed.
    }
  }
}

\section{version 0.0.3 (2020-06-21)}{
  \subsection{New Features}{
    \itemize{
      \item Functions \code{fduplicated(x)} and \code{funique(x)}, implemented in C, 
      are comparable to \code{base} \R functions \code{duplicated} and \code{unique}. For more details, 
      please see \code{?kit::funique}.

      \item Functions \code{psum} and \code{pprod} have now better performance for 
      type double and complex.
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
      \item Function \code{count(x, value)}, implemented in C, to simply count the number of times 
      an element \code{value} occurs in a vector or in a list \code{x}. For more details, please see
      \code{?kit::count}.

      \item Function \code{pmean(..., na.rm=FALSE)}, \code{pall(..., na.rm=FALSE)}, 
      \code{pany(..., na.rm=FALSE)} and \code{pcount(..., value)}, implemented in C, 
      are similar to already available function \code{psum} and \code{pprod}. These 
      functions respectively apply base \R functions \code{mean}, \code{all} and \code{any} element-wise. 
      For more details, benchmarks and help, please see \code{?kit::pmean}.
    }
  }
  \subsection{Bug Fixes}{
    \itemize{
      \item Fix Solaris Unicode warnings for NEWS file. Benchmarks have been moved from the NEWS file to 
      each function Rd file.
      
      \item Fix some \code{NA} edge cases for \code{pprod} and \code{psum} so these 
      functions behave more like base \R function \code{prod} and \code{sum}.

      \item Fix installation errors for version of R (<3.5.0).
    }
  }
}

\section{version 0.0.1 (2020-05-03)}{
  \subsection{Initial Release}{
    \itemize{
      \item Function \code{fpos(needle, haystack, all=TRUE, overlap=TRUE)}, implemented in C, is
      inspired by base function \code{which} when used in the following form 
      \code{which(x == y, arr.ind =TRUE}). Function \code{fpos} returns the index(es) or position(s)
      of a matrix/vector within a larger matrix/vector. Please see \code{?kit::fpos} for more 
      details.
      
      \item Function \code{iif(test, yes, no, na=NULL, tprom=FALSE, nThread=getOption("kit.nThread"))}, 
      originally contributed as \code{fifelse} in package \CRANpkg{data.table}, was moved to package kit
      to be developed independently. Unlike the current version of \code{fifelse}, \code{iif} allows
      type promotion like base function \code{ifelse}. For further details about the differences 
      with \code{fifelse}, as well as \code{hutils::if_else} and \code{dplyr::if_else}, please see
      \code{?kit::iif}.
      
      \item Function \code{nif(..., default=NULL)}, implemented in C, is inspired by 
      \emph{SQL CASE WHEN}. It is comparable to \CRANpkg{dplyr} function \code{case_when} however it 
      evaluates it arguments in a lazy way (i.e only when needed). Function \code{nif} was 
      originally contributed as function \code{fcase} in the \CRANpkg{data.table} package but then moved
      to package kit so its development may resume independently. Please see \code{?kit::nif} for 
      more details.
      
      \item Function \code{pprod(..., na.rm=FALSE)} and \code{psum(..., na.rm=FALSE)},
      implemented in C, are inspired by base function \code{pmin} and \code{pmax}. These new 
      functions work only for integer, double and complex types and do not recycle vectors. Please 
      see \code{?kit::psum} for more details.
      
      \item Function \code{setlevels(x, old, new, skip_absent=FALSE)}, implemented in C, 
      may be used to set levels of a factor object. Please see \code{?kit::setlevels} for more details. 
      
      \item Function \code{topn(vec, n=6L, decreasing=TRUE)}, implemented in C, returns the top 
      largest or smallest \code{n} values for a given numeric vector \code{vec}. It is inspired by 
      \code{dplyr::top_n} and equivalent to base functions order and sort in specific cases as shown
      in the documentation. Please see \code{?kit::topn} for more details.
      
      \item Function \code{vswitch(x, values, outputs, default=NULL, nThread=getOption("kit.nThread"))}
      , implemented in C, is a vectorised version of \code{base} \R function \code{switch}. This 
      function can also be seen as a particular case of function \code{nif}. Please see 
      \code{?kit::switch} for more details.
    }
  }
}
