\name{NEWS}
\title{News for Package 'rawrr'}

\newcommand{\ghit}{\href{https://github.com/fgcz/rawrr/issues/#1}{##1}}

\section{Changes in version 1.23.2 (Unreleased)}{
  \itemize{
    \item Add access to the instruments (devices) and status logs of a raw
    file, e.g., LC pump pressure: \code{readDevices()} lists all devices
    (MS, LC pump, autosampler, analog channels, ...),
    \code{readStatusLog()} returns the plottable status log labels of a
    device or the time series of one label, and
    \code{readPressureProfile()} extracts all status log labels matching
    \code{"[Pp]ressure"} of the non-MS devices as a long
    \code{data.frame}.
    \item Status log values are read unformatted, i.e., with full precision
    instead of the display precision of the device (e.g., an ion gauge
    pressure of 1.36e-05 Torr was returned as 0).
    \item The man pages of \code{readPressureProfile()},
    \code{readStatusLog()}, and \code{readIndex()} document which LC
    systems log pressure (e.g., Vanquish Neo
    \code{AnalyticalPumpPressureInBar}; empty status log for Waters
    nanoACQUITY) and that status log times and \code{readIndex()$StartTime}
    share the same time axis (minutes since the start of the run).
    \item \code{?readPressureProfile} shows how to extract MS vacuum and gas
    pressure profiles from the \code{tartare} ExperimentHub files
    \code{EH4547}, \code{EH3220}, and \code{EH3222}, which contain no LC
    pressure.
    \item The \code{rawrr} executable gains the options \code{devices} and
    \code{statusLog}.
    \item Refactor the option dispatch of \code{rawrr.cs} into a command
    table; the command line interface and all data outputs are unchanged.
    Errors (raw file not accessible, missing arguments, invalid filter,
    \code{xic} failures) are written to stderr with exit status 1; the
    unused \code{GetFilters()} call executed for every MS option and the
    'Memory Usage' output on stdout are removed.
    \item The command line help is generated from the command table:
    \code{rawrr help <option>} prints the arguments and output of an
    option, \code{rawrr help --markdown} the option table, which is
    embedded in \code{inst/rawrrassembly/README.md} at render time.
    \item \code{inst/rawrrassembly/README.md} documents \code{devices},
    \code{statusLog}, and the new error behavior; it uses the installed
    assembly and \code{sampleFilePath()} instead of fixed paths.
    \item Add tests for the status log functions and a test ensuring that
    every option of the \code{rawrr} executable is documented in
    \code{inst/rawrrassembly/README.md}.
  }
}

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
