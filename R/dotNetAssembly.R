.rawfileReaderDLLs <- function(){
  # 'ThermoFisher.CommonCore.BackgroundSubtraction.dll',
  c(
    'ThermoFisher.CommonCore.Data.dll',
    'ThermoFisher.CommonCore.MassPrecisionEstimator.dll',
    'ThermoFisher.CommonCore.RawFileReader.dll')
}

.userRawfileReaderDLLsPath <- function(){
  libdir <- tools::R_user_dir("rawrr", which='data')
  f <- file.path(libdir, 'rawrrassembly')
  return(f)
}

.checkRawfileReaderDLLs <- function(){
  monoPath <- Sys.getenv("MONO_PATH", names=TRUE)

  rv <- vapply(.rawfileReaderDLLs(), function(dll){
    ff <- file.path(.userRawfileReaderDLLsPath(), dll)
    if (monoPath != ""){
      ff <- file.path(monoPath, dll)
      if (isTRUE(ff)) return (TRUE)
    }
    file.exists(ff)
  }, FALSE)
  if (isFALSE(all(rv))){
    warning("'ThermoFisher.CommonCore.*.dll' files are not complete.\n",
            "Run 'rawrr::installRawfileReaderDLLs()' or setenv MONO_PATH to ",
            "the location, where the assemblies are located.\n",
            "For more information, type '?ThermoFisher'.")
  }
  all(rv)
}


.rawrrAssembly <- function(){
  f <- file.path(.userRawfileReaderDLLsPath(), 'rawrr.exe')
  return(f)
}


#' Install the New RawFileReader from Thermo Fisher Scientific
#'
#' @param ... other parameter for \code{download.file}
#' @param sourceUrl url of New RawFileReader from Thermo Fisher Scientific
#' assemblies.
#'
#' @aliases Thermo
#' @aliases ThermoFisher
#' @aliases ThermoFisherScientific
#'
#' @seealso \url{https://planetorbitrap.com/rawfilereader}
#'
#' @return An (invisible) vector of integer code, 0 for success and non-zero for
#' failure. For the "wget" and "curl" methods this is the status code returned
#' by the external program.
#'
#' @export installRawfileReaderDLLs
#' @importFrom utils download.file
#'
installRawfileReaderDLLs <-
  function(sourceUrl = "https://github.com/compomics/ThermoRawFileParser/raw/master/packages/mzLib.1.0.450/lib/netstandard2.0/", ...){

  rawfileReaderDLLsPath <- .userRawfileReaderDLLsPath()
  message(sprintf("Installiung Thermo Fisher Rawfile Reader assemblies in %s ...",
          rawfileReaderDLLsPath))
  if (isFALSE(dir.exists(rawfileReaderDLLsPath))){
    dir.create(rawfileReaderDLLsPath, recursive = TRUE)
  }

  vapply(.rawfileReaderDLLs(), function(dll){
    destfile <- file.path(rawfileReaderDLLsPath, dll)
    rv <- download.file(file.path(sourceUrl, dll),
                  destfile=destfile, mode='wb', ...)
    message(sprintf("MD5 %s %s", tools::md5sum(destfile), destfile))
    rv},
    0)
}

#' Installing \code{rawrr.exe} assembly
#'
#' @param sourceUrl url of r\code{rawrr.exe} assembly.
#' @param ... other parameter for \code{download.file}.
#'
#' @return An integer code, 0 for success and non-zero for
#' failure. For the "wget" and "curl" methods this is the status code returned
#' by the external program.
#'
#' @export installRawrrExe
installRawrrExe <-
  function (sourceUrl = "http://fgcz-ms.uzh.ch/~cpanse/rawrr/rawrr.exe",
            ...)
  {
    rawrrAssembly <- .rawrrAssembly()
    rv = download.file(sourceUrl, destfile = rawrrAssembly, mode='wb', ...)
    message(sprintf("MD5 %s %s", tools::md5sum(rawrrAssembly), rawrrAssembly))
    rv
  }

.buildRawrrExe <- function(){
  packagedir <- system.file(package = 'rawrr')

  if (isFALSE(.checkRawfileReaderDLLs())){
    return()
  }

  if (Sys.which("msbuild") == "" && Sys.which("xbuild") == "")
  {
    warning ("could not find msbuild or xbuild in path; will not be able to use rDotNet unless corrected and rebuilt")
    return()
  }

  cwd <- getwd()
  setwd(file.path(packagedir, 'rawrrassembly'))

  cmd <- ifelse(Sys.which("msbuild") != "", "msbuild", "xbuild")
  cmdArgs <- sprintf("/p:OutputPath='%s/'", dirname(.rawrrAssembly()))
  if (Sys.getenv("MONO_PATH") == ""){
    Sys.setenv(MONO_PATH = dirname(.rawrrAssembly()))
  }

  rv <- system2 (cmd, cmdArgs, wait=TRUE, stderr=TRUE, stdout=TRUE)

  if (rv <- any(grepl("Build succeeded.", rv)) && file.exists(.rawrrAssembly())){
    message(sprintf("rawrr.exe successfully built\n'%s'.",
                    dirname(.rawrrAssembly())))
    message(rv)
  }else{
    warning("rawrr.exe build failed. Try to download and install by calling ",
            "the 'rawrr::installRawrrExe()' method.")
    warning(rv)
  }
  setwd(cwd)
  rv
}


.isRawFileReaderLicenseAccepted <- function(){
  licenseFile <- file.path(system.file(package = 'rawrr'), 'rawrrassembly', 'RawFileReaderLicense.txt')
  stopifnot(file.exists(licenseFile))

  eulaFile <- file.path(cachedir <- tools::R_user_dir("rawrr", which='cache'), "eula.txt")
  msg <- "# By changing the setting below to TRUE you are accepting the Thermo License agreement."

  if (!file.exists(eulaFile)){
    file.show(licenseFile)
    fmt <- "Do you accept the Thermo License agreement '%s'? [Y/n]: "
    prompt <- sprintf(fmt, licenseFile)
    response <- readline(prompt = prompt)
    if (tolower(response) == "y"){
      if (!dir.exists(cachedir)) { dir.create(cachedir, recursive = TRUE) }
      fileConn <- file(eulaFile)
      writeLines(paste(msg, paste0("# ", date()), "eula=true", sep="\n"), fileConn)
      close(fileConn)

      return(TRUE %in% grepl("eula=true", tolower(readLines(eulaFile))))
    }
  }else{
    return(TRUE %in% grepl("eula=true", tolower(readLines(eulaFile))))
  }

  stop("You have to accept the Thermo License agreement!")

  FALSE
}

