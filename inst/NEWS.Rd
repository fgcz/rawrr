\name{NEWS}
\title{News for Package 'rawrr'}

\newcommand{\ghit}{\href{https://github.com/fgcz/rawrr/issues/#1}{##1}}

\section{Changes in version 1.21.1 (Unreleased)}{
  \itemize{
    \item Provide native \code{rawrr} executables for \code{linux-arm64},
    \code{osx-arm64}, and \code{win-arm64} in addition to the x64 runtimes;
    the runtime is derived from the architecture of the running R session,
    so Apple Silicon no longer requires Rosetta \ghit{96} \ghit{97}.
    \item Use Thermo Fisher RawFileReader 8.0.42 (.NET 8) \ghit{97}.
    \item \code{buildRawrrExe()} gains the arguments \code{runtime}, allowing
    cross-builds for all six runtimes, and \code{sourceUrl}; it mirrors
    \code{inst/rawrrassembly/build.sh}: the RawFileReader NuGet packages are
    downloaded once into a local feed and passed via
    \code{dotnet publish --source}, leaving the user's global NuGet
    configuration untouched \ghit{97}.
    \item Remove the internal helpers \code{.addPackages()} and
    \code{.addNupkgSource()}.
    \item \code{.isAssemblyWorking()} reports the exit status and hints
    (e.g., macOS quarantine attribute) instead of failing with
    'argument is of length zero' if the executable cannot be started
    \ghit{96}.
    \item \code{installRawrrExe()} downloads from the 1.21.1 binary tree.
    \item Add tests comparing reader output of \code{sample.raw} against
    reference values (scan header, trailer, file header) and tests for the
    runtime detection.
    \item Update \code{INSTALL}.
  }
}

\section{Changes in version 1.15.x (Unreleased)}{
  \itemize{
    \item Add Leo as author.
    \item Replace `mono` with `dotnet`; Introduced a statically linked runtime `rawrr` assembly for Windows, Linux, and macOS on x64 architectures, streamlining installation and usage \ghit{75}.
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
