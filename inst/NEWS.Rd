\name{NEWS}
\title{News for Package 'rawrr'}

\newcommand{\ghit}{\href{https://github.com/fgcz/rawrr/issues/#1}{##1}}


\section{Changes in version 1.3 (2021-12-01)}{
  \itemize{
	  \item Add barebone \code{mode} para in \code{readSpectrum} \ghit{43}.
	  \item Add \code{rawrr} namespace in help pages.
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
