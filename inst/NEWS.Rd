\name{NEWS}
\title{News for Package 'rawrr'}

\newcommand{\ghit}{\href{https://github.com/fgcz/rawrr/issues/#1}{##1}}

\section{Changes in version 1.7.4 (2023-03-01)}{
  \itemize{
	  \item Add \code{rawrr::readTrailer}.
  }
}

\section{Changes in version 1.5 (2022-08-22)}{
  \itemize{
	  \item Add \code{centroid.PreferredNoises}.
  }
}

\section{Changes in version 1.3 (2022-03-19)}{
  \itemize{
	  \item Add barebone \code{mode} para in \code{readSpectrum} \ghit{43}.
	  \item Add \code{rawrr} namespace in help pages.
	  \item Add 'Monoisotopic M/Z:' from TrailerExtraHeaderInformation as column to \code{rawrr::readIndex} function.
  }
}

\section{Changes in version 1.1 (2021-05-31)}{
  \itemize{
	  \item Improve error handling of system2 call in \code{.rawrrSystem2Source}
	  by logging stdout and stderr and make them available from the R console.
	  \item Add helper function \code{.checkReaderFunctions}.
	  \item Use pipe \code{|>} in vignette.
  }
}
