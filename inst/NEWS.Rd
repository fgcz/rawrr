\name{NEWS}
\title{News for Package 'rawrr'}

\newcommand{\ghit}{\href{https://github.com/fgcz/rawrr/issues/#1}{##1}}

\section{Changes in version 1.10.2 (Unreleased)}{
  \itemize{
    \item Consistently normalize paths \ghit{76}.
  }
}

\section{Changes in version 1.10.1 (2023-11-02)}{
  \itemize{
	  \item Fix index error \ghit{67}. 
  }
}


\section{Changes in version 1.9.2 (2023-10-24)}{
  \itemize{
	  \item Download RawFileReader DLL’s from  \url{https://github.com/thermofisherlsms/RawFileReader} \ghit{66}. 
  }
}

\section{Changes in version 1.7.12 (2023-03-17)}{
  \itemize{
	  \item Replace rtinseconds by StartTime [min] as provided by the TFS assembly \ghit{60}. 
  }
}

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
