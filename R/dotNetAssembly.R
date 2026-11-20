#R

#' Test if \code{rawrr.exe} .NET assembly is working
#' The default when the assembly is not working is an 
#' error message.
#' @param FUN function triggered. default is using the \code{stop} method
#' @param exe path to the assembly. default is \code{rawrr:::.rawrrAssembly()}.
#' @return TRUE if assembly is working
#' @export
.isAssemblyWorking <-
  function(FUN = stop, exe = .rawrrAssembly()){

    if (isFALSE(file.exists(exe))){
      msg <- c("'rawrr.exe' not found.\n",
               "Run 'rawrr::installRawrrExe()'.",
               " For more information, type '?rawrr.exe'.")
      FUN(msg)
    }

    # execute rawrr.exe assembly and keep output string
    rvs <-  "?"
    if (file.exists(exe)){
      rvs <- system2(exe, stdout = TRUE)
    }

    # expect that output string
    if (rvs != "No RAW file specified!"){
      msg <- ("The 'rawrr.exe' dot Net assembly is not working!")
      FUN(msg)
    }

    if(interactive()){ stopifnot(.isRawFileReaderLicenseAccepted()) }
    TRUE
  }


## TODO: refactor
.rawfileReaderDLLs <- function(){
  # 'ThermoFisher.CommonCore.BackgroundSubtraction.dll',
  c(
    'ThermoFisher.CommonCore.BackgroundSubtraction.dll',
    'ThermoFisher.CommonCore.Data.dll',
    'ThermoFisher.CommonCore.MassPrecisionEstimator.dll',
    'ThermoFisher.CommonCore.RawFileReader.dll')
}

#' Derives the path where all .NET assemblies are stored.
#'
#' @return path
#' @seealso \code{installRawrrExe} and  \code{buildRawrrExe}
#'
#' @examples
#' rawrrAssemblyPath()
#' @export
rawrrAssemblyPath <- function(){
  dirname(.rawrrAssembly())
}


#' .NET runtime identifier (RID) of the running R session
#'
#' @param sysname operating system name, default \code{Sys.info()[["sysname"]]}.
#' @param arch architecture of the R build, default \code{R.version$arch}.
#'
#' @return one of \code{linux-x64}, \code{osx-x64}, \code{win-x64},
#' \code{linux-arm64}, \code{osx-arm64}, or \code{win-arm64}.
#' @noRd
.rawrrRuntime <- function(sysname = Sys.info()[["sysname"]],
                          arch = R.version$arch){
  os <- if (sysname == "Darwin") {
    "osx"
  } else if (sysname == "Linux") {
    "linux"
  } else {
    "win"
  }
  cpu <- if (tolower(arch) %in% c("aarch64", "arm64")) "arm64" else "x64"
  paste(os, cpu, sep = "-")
}

#' Helper function providing the pass to the rawrr assembly.
#' @importFrom tools R_user_dir
#' @export
.rawrrAssembly <- function(){
  libdir <- tools::R_user_dir("rawrr", which='cache')
  d <- file.path(libdir, 'rawrrassembly')

  rid <- .rawrrRuntime()
  exe <- if (startsWith(rid, "win-")) "rawrr.exe" else "rawrr"
  file.path(d, rid, exe)
}


#' URL for Thermo Fisher .NET assemblies
#'
#' @return an URL
.thermofisherlsmsUrl <- function(){
 #  "https://github.com/thermofisherlsms/RawFileReader/tree/main/Libs/NetCore/Net8/"
    "https://github.com/thermofisherlsms/RawFileReader/raw/refs/heads/main/Libs/NetCore/Net8/"
}


#' Local NuGet feed directory holding the Thermo Fisher nupkgs
#'
#' Shared by all runtimes; passed to \code{dotnet publish --source}.
#' @noRd
.nupkgFeedPath <- function(){
  file.path(tools::R_user_dir("rawrr", which = "cache"), "rawrrassembly",
            "nupkgs")
}

#' Download and install the Thermo Fisher Scientific .NET 8.0 nupkgs
#'
#' @param sourceUrl url of nupkgs.
#' @param feed local directory the nupkgs are downloaded to.
#' @param force if \code{TRUE} it will overwrite already downloaded pkgs.
#'
#' @aliases Thermo
#' @aliases ThermoFisher
#' @aliases ThermoFisherScientific
#'
#' @author Christian Panse <cp@fgcz.ethz.ch>, 2021, 2024
#'
#' @return An (invisible) vector of integer code, 0 for success and non-zero for
#' failure. For the "wget" and "curl" methods this is the status code returned
#' by the external program.
#'
#' @importFrom utils download.file
.downloadNupkgs <- function(sourceUrl = .thermofisherlsmsUrl(),
                            feed = .nupkgFeedPath(), force = FALSE){
  dir.create(feed, recursive = TRUE, showWarnings = FALSE)

  nupkgs <- sprintf("thermofisher.commoncore.%s.8.0.42.nupkg",
                    c("backgroundsubtraction", "randomaccessreaderplugin",
                      "data", "rawfilereader", "massprecisionestimator"))

  vapply(nupkgs, FUN = function(nupkg){
    destfile <- file.path(feed, nupkg)
    if (file.exists(destfile) && isFALSE(force)){
      return(0L)
    }
    download.file(file.path(sourceUrl, nupkg), destfile = destfile,
                  mode = "wb")
  }, FUN.VALUE = 0L)
}

.copySourceCode <- function(dir) {
  dst <- dir
  sourceCodeFiles <- c("rawrrassembly/rawrr.cs", "rawrrassembly/rawrr.csproj")
  packagedir <- system.file(package = 'rawrr')


  sourceCodeFiles |>
    lapply(function(f){
      src <- file.path(packagedir, f)
      message("Copying ", basename(src), " to ", dst)
      stopifnot(file.copy(src, dst, overwrite = TRUE))
  })
}

.clean <- function(){
  message("Removing ", .rawrrAssembly())
  file.remove(.rawrrAssembly())
}

.build <- function(dir, runtime = .rawrrRuntime(), feed = .nupkgFeedPath()){
  exe <- if (startsWith(runtime, "win-")) "rawrr.exe" else "rawrr"
  outDir <- file.path(dirname(rawrrAssemblyPath()), runtime)

  ## logs go to tempdir() so they survive removal of the build dir
  tempOut <- tempfile(pattern = "rawrr.build.stdout.", fileext = ".txt")
  tempErr <- tempfile(pattern = "rawrr.build.stderr.", fileext = ".txt")

  message("Running dotnet publish for runtime '", runtime, "' ...")
  message("Write stdout to ", tempOut)
  message("Write stderr to ", tempErr)

  ## --source only applies to this invocation, unlike 'dotnet nuget add
  ## source', which writes to the user's global nuget.config (see build.sh).
  rv <- system2("dotnet",
    args = c("publish", shQuote(file.path(dir, "rawrr.csproj")),
             "--runtime", runtime, "-c", "Release",
             "--source", "https://api.nuget.org/v3/index.json",
             "--source", shQuote(feed),
             "-o", shQuote(outDir)),
    stdout = tempOut, stderr = tempErr)

  if (rv != 0){
    if (interactive()){
      file.show(tempOut)
      file.show(tempErr)
    }
    stop("dotnet publish failed (exit code ", rv, "); see ", tempOut,
         " and ", tempErr)
  }

  message("Build successfully done: ", file.path(outDir, exe))
  invisible(file.path(outDir, exe))
}


#' installRawFileReaderDLLs (deprecated)
#'
#' Download and install the New RawFileReader from Thermo Fisher
#' Scientific .Net assemblies in the directory provided by
#' \code{rawrrAssemblyPath()}.
#'
#' @export
installRawFileReaderDLLs <- function(){
  .Deprecated("installRawrrExe", msg = "Deprecated since rawrr version 1.15.3.")
}

#' Download \code{rawrr} assembly
#'
#' @description downloads and installs the \code{rawrr.exe} .NET assembly in
#' the directory provided by \code{rawrrAssemblyPath()}.
#'
#' @details The console application \code{rawrr} is used by the package's
#' reader functions through a \link{system2} call.
#'
#' @param sourceUrl url of \code{rawrr.exe} assembly.
#' @param force if \code{TRUE} it will overwrite the assembly
#' @param ... other parameter for \code{download.file}.
#'
#' @return An integer code, 0 for success and non-zero for
#' failure. For the "wget" and "curl" methods this is the status code returned
#' by the external program.
#' @seealso \link{buildRawrrExe}
#' @aliases rawrr.exe
#' @export
installRawrrExe <-
  function (sourceUrl = "https://fgcz-ms.uzh.ch/~cpanse/rawrr/dotnet/1.21.1/",
            force = FALSE,
            ...) {
  rawrrAssembly <- .rawrrAssembly()

  if (isTRUE(file.exists(rawrrAssembly)) && isFALSE(force)){
     if (interactive()){
	message("The rawrr assembly exists. Have a lot of fun!")
     }
     return()
  }

  if (isTRUE(file.exists(rawrrAssembly)) && isTRUE(force)){
	## TODO: if interactive ask to override
        if (interactive()){
            response <- readline(prompt = sprintf("Assembly exists. Do you want to overwrite it? [Y/n]: "))
            if (tolower(response) == "y"){

            }else{
                return()
            }
	}
  }

  if (isFALSE(dir.exists(rawrrAssemblyPath()))) {
        dir.create(rawrrAssemblyPath(), recursive = TRUE)
  }

  sourceUrl <- file.path(sourceUrl, .rawrrRuntime(), basename(rawrrAssembly))
  message("Overwrite sourceUrl to ", sourceUrl)


  dir.create(dirname(rawrrAssembly), recursive = TRUE, showWarnings = FALSE)
  rv = download.file(sourceUrl, destfile = rawrrAssembly, mode = "wb",
        ...)
  Sys.chmod(rawrrAssembly, mode = "0777", use_umask = TRUE)

  message(sprintf("MD5 %s %s", tools::md5sum(rawrrAssembly), rawrrAssembly))

  rv
}

.buildOnLoad <- function(){
  # nothing to do
  if (file.exists(.rawrrAssembly())){
    return()
  }

  if (Sys.which("dotnet") == "")
  {
    msg <- c("Could not find 'dotnet' in the path. Therefore, ",
         "it is not possible to build the 'rawrr.exe' assembly from",
         " source code.\nTry to run rawrr::installRawrrExe().")
    warning(msg)
    return()
  }

  buildRawrrExe()
}


#' Build \code{rawrr.exe} console application.
#'
#' @description builds \code{rawrr.exe} file from C# source code requiring
#' .NET SDK. The console application \code{rawrr.exe}
#' is used by the package's reader functions through a \link{system2} call
#' or a \link{textConnection}.
#'
#' The Thermo Fisher RawFileReader NuGet packages are downloaded (once) into a
#' local feed directory in the rawrr cache and passed to
#' \code{dotnet publish --source}; the user's global NuGet configuration is
#' not modified.
#'
#' @param runtime .NET runtime identifier, one of \code{linux-x64},
#' \code{osx-x64}, \code{win-x64}, \code{linux-arm64}, \code{osx-arm64}, or
#' \code{win-arm64}. Defaults to the platform of the running R session.
#' @param sourceUrl url of the Thermo Fisher RawFileReader nupkgs.
#'
#' @author Christian Panse <cp@fgcz.ethz.ch>, 2021, 2024
#'
#' @seealso \link{installRawrrExe}
#'
#' @references \itemize{
#'   \item{\url{https://www.mono-project.com/docs/advanced/assemblies-and-the-gac/}, 2020}
#'   \item{\url{https://planetorbitrap.com/rawfilereader}, 2020}
#'   \item{\url{https://github.com/thermofisherlsms/RawFileReader/}, 2024}
#'   \item{\url{https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/compiler-options/advanced}}
#'   \item{\doi{10.1021/acs.jproteome.0c00866}}
#' }
#'
#' @return for the host runtime, the result of the assembly sanity check;
#' otherwise (invisibly) the path of the built executable.
#' @export
buildRawrrExe <- function(runtime = .rawrrRuntime(),
                          sourceUrl = .thermofisherlsmsUrl()){
  runtime <- match.arg(runtime, c("linux-x64", "osx-x64", "win-x64",
                                  "linux-arm64", "osx-arm64", "win-arm64"))

  if (Sys.which("dotnet") == ""){
    stop("Could not find 'dotnet' in the path. Therefore, it is not ",
         "possible to build the 'rawrr' assembly from source code.\n",
         "Try to run rawrr::installRawrrExe().")
  }

  ## dedicated build directory, removed on exit
  buildDir <- tempfile(pattern = "rawrr.build.")
  dir.create(buildDir)
  on.exit(unlink(buildDir, recursive = TRUE), add = TRUE)

  message("Building rawrr assembly for '", runtime, "' using .NET 8.0 ...")

  .copySourceCode(dir = buildDir)
  rv <- .downloadNupkgs(sourceUrl = sourceUrl)
  if (any(rv != 0)){
    stop("Downloading the RawFileReader nupkgs failed: ",
         paste(names(rv)[rv != 0], collapse = ", "))
  }

  exe <- .build(dir = buildDir, runtime = runtime)

  ## only the host runtime can be executed for the sanity check
  if (runtime == .rawrrRuntime()){
    .isAssemblyWorking()
  } else {
    invisible(exe)
  }
}

.eulaPath <- function(){
  file.path(rawrrAssemblyPath(), "eula.txt")
}

.isRawFileReaderLicenseAccepted <- function(){
  licenseFile <- file.path(system.file(package = 'rawrr'), 'rawrrassembly',
                           'RawFileReaderLicense.txt')
  stopifnot(file.exists(licenseFile))

  eulaFile <- .eulaPath()

  msg <- c("# By changing the setting below to TRUE you are accepting ",
           "the Thermo License agreement.")

  if (!file.exists(eulaFile)){
    file.show(licenseFile)
    fmt <- "Do you accept the Thermo License agreement '%s'? [Y/n]: "
    prompt <- sprintf(fmt, licenseFile)
    response <- readline(prompt = prompt)
    if (tolower(response) == "y"){
      if (isFALSE(dir.exists(dirname(eulaFile)))) {
        dir.create(dirname(eulaFile), recursive = TRUE)
      }
      fileConn <- file(eulaFile)
      writeLines(paste(msg, paste0("# ", date()), "eula=true", sep="\n"),
                 fileConn)
      close(fileConn)

      return(TRUE %in% grepl("eula=true", tolower(readLines(eulaFile))))
    }
  }else{
    return(TRUE %in% grepl("eula=true", tolower(readLines(eulaFile))))
  }

  msg <- ("You have to accept the Thermo Fisher Scientific License agreement!")
  stop(msg)
}

