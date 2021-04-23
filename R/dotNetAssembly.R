#R

.isAssemblyWorking <-
  function(FUN = stop, exe = .rawrrAssembly()){
    if(Sys.info()['sysname'] %in% c("Darwin", "Linux")){
      if (Sys.which('mono') == ""){
        # TODO(cp) mono runtime or dotNet runtime
        msg <- c("Cannot find the Mono runtime.",
                 " Check the rawrr package system requirements.")
        FUN(msg)
      }
    }
    
    .checkRawfileReaderDLLs(FUN)
    
    if (isFALSE(file.exists(exe))){
      msg <- c("'rawrr.exe' not found.\n",
               "Run 'rawrr::installRawrrExe()'.",
               "For more information, type '?rawrr.exe'.")
      FUN(msg)
    }
    
    if (Sys.info()['sysname'] %in% c("Darwin", "Linux")){
      if (Sys.which('mono') == ""){
        msg <- c("The dot Net runtime system (mono) is not available.\n", 
                 "Consider to install 'apt-get install mono-runtime' on Linux",
                 " or download/install from https://www.mono-project.com/.")
        FUN(msg)
      }
    }
    
    # execute rawrr.exe assembly and keep output string
    rvs <-  "?"
    if (Sys.info()['sysname'] %in% c("Darwin", "Linux")){
      if (file.exists(exe) && Sys.which('mono') != ""){
        rvs <- system2(Sys.which('mono'), args = c(shQuote(exe)),
                       stdout = TRUE)
      }
    }else{
      if (file.exists(exe)){
        rvs <- system2(exe, stdout = TRUE)
      }
      
    }
    
    # expect that output string
    if (rvs != "No RAW file specified!"){
      msg <- ("The 'rawrr.exe' dot Net assembly is not working!")
      FUN(msg)
    }
    
    if(interactive()){ stopifnot(.isRawFileReaderLicenseAccepted()) }
    TRUE
  }


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

.checkDllInMonoPath <- function(dll="ThermoFisher.CommonCore.Data.dll"){
  monoPath <- Sys.getenv("MONO_PATH", names=TRUE)
  monoPath <- strsplit(monoPath, .Platform$path.sep)[[1]]
  any(vapply(monoPath, function(d){
    file.exists(file.path(d, dll))
  }, FALSE))  
}

.checkRawfileReaderDLLs <- function(FUN=stop){
  rv <- vapply(.rawfileReaderDLLs(), function(dll){
    userFileDllPath <- file.path(.userRawfileReaderDLLsPath(), dll)
    dllExists <- file.exists(userFileDllPath) || .checkDllInMonoPath(dll)
    if (isFALSE(dllExists)){
      message(sprintf("'%s' is missing.", dll))
    }
    return(dllExists)
  }, FALSE)
  
  if (isFALSE(all(rv)) && TRUE){
    FUN("'ThermoFisher.CommonCore.*.dll' files are not complete or missing.\n",
         "Run 'rawrr::installRawFileReaderDLLs()' or setenv MONO_PATH to ",
         "the location where the assemblies are located.\n",
         "For more information, type '?ThermoFisher'.")
  }
  all(rv)
}


.rawrrAssembly <- function(){
  f <- file.path(.userRawfileReaderDLLsPath(), 'rawrr.exe')
  return(f)
}


#' Download and install the New RawFileReader from Thermo Fisher Scientific .Net assemblies
#'
#' @param ... other parameter for \code{download.file}
#' @param sourceUrl url of New RawFileReader from Thermo Fisher Scientific
#' assemblies.
#'
#' @aliases Thermo
#' @aliases ThermoFisher
#' @aliases ThermoFisherScientific
#' 
#' @details 
#' The console application assembly \code{rawrr.exe} requires three Thermo assemplies \code{ThermoFisher.CommonCore.Data.dll},
#' \code{ThermoFisher.CommonCore.MassPrecisionEstimator.dll},
#' and \code{ThermoFisher.CommonCore.RawFileReader.dll}.
#' 
#' The \code{rawrr.exe} assembly can be built from C# source code by using the \code{msbuild} tool shipped by the \url{https://www.mono-project.com} or by Microsoft's .NET SDK \url{https://dotnet.microsoft.com} on Linux, Microsoft, and macOS.
#'
#' If no build tool and C# compiler (csc, msc) are available or the build process fails, you can download \code{rawrr.exe} assembly from the authors' site.
#' 
#' @seealso \link{buildRawrrExe} ande \link{installRawrrExe}
#' 
#' @references \itemize{
#'   \item{\url{https://www.mono-project.com/docs/advanced/assemblies-and-the-gac/}}
#'   \item{\url{https://planetorbitrap.com/rawfilereader}}
#' }
#' 
#' @author Christian Panse <cp@fgcz.ethz.ch>, 2021
#' 
#' @return An (invisible) vector of integer code, 0 for success and non-zero for
#' failure. For the "wget" and "curl" methods this is the status code returned
#' by the external program.
#'
#' @export installRawFileReaderDLLs
#' @importFrom utils download.file
#' 
#' @examples 
#' # to install all assemblies
#' \dontrun{
#' rawrr::installRawFileReaderDLLs() 
#' rawrr:::buildRawrrExe() || rawrr::installRawrrExe()
#' }
# TODO(cp): rename installThermoFisherScientificRawFileReaderAssemblyDLLs()
installRawFileReaderDLLs <-
  function(sourceUrl = paste0("https://github.com/",
                              "thermofisherlsms/ThermoRawFileParser/",
                              "raw/master/packages/",
                              "ThermoFisher.CommonCore.RawFileReader.4.0.26/lib/"),
           ...){
    if(interactive()){ stopifnot(.isRawFileReaderLicenseAccepted()) }
    
    rawfileReaderDLLsPath <- .userRawfileReaderDLLsPath()
    msg <- sprintf(c("Installiung Thermo Fisher Rawfile Reader assemblies",
                     " will be copied to '%s' ..."), rawfileReaderDLLsPath) 
    message(msg)
    
    if (isFALSE(dir.exists(rawfileReaderDLLsPath))){
      dir.create(rawfileReaderDLLsPath, recursive = TRUE)
    }
    
    vapply(.rawfileReaderDLLs(), function(dll){
      destfile <- file.path(rawfileReaderDLLsPath, dll)
      
      if (file.exists(destfile)){
        if(interactive() ){ 
          fmt <- "Overwrite '%s' ? [Y/n]: "
          prompt <- sprintf(fmt, destfile)
          response <- readline(prompt = prompt)
          if (tolower(response) != "y"){
            message("Aborting ...")
            return(1)
          }
        }else{
          fmt <- "Overwriting '%s' ..."
          warning(sprintf(fmt, destfile))
        }
      }
      
      rv <- download.file(file.path(sourceUrl, dll),
                          destfile=destfile, mode='wb', ...)
      message(sprintf("MD5 %s %s", tools::md5sum(destfile), destfile))
      rv},
      0)
    
    
    if (isFALSE(file.exists(.rawrrAssembly())) && interactive()){
      msg <- c("'rawrr.exe' is not available.", 
               "\nRun 'rawrr:::buildRawrrExe()' or 'rawrr::installRawrrExe()'.")
      warning(msg)
    }
  }

#' Installing \code{rawrr.exe} console application
#' 
#' @description downloads and install the \code{rawrr.exe} .Net assembly in 
#' the \code{tools::R_user_dir("rawrr", which='data')} path.
#' 
#' @details The console application \code{rawrr.exe} is used by the package's reader functions through a \link{system2} call.
#' 
#' @param sourceUrl url of \code{rawrr.exe} assembly.
#' @param ... other parameter for \code{download.file}.
#'
#' @return An integer code, 0 for success and non-zero for
#' failure. For the "wget" and "curl" methods this is the status code returned
#' by the external program.
#' @seealso \link{buildRawrrExe}
#'
#' @aliases rawrr.exe
#' @export installRawrrExe
installRawrrExe <-
  function (sourceUrl = "http://fgcz-ms.uzh.ch/~cpanse/rawrr/rawrr.exe",
            ...)
  {
    rawrrAssembly <- .rawrrAssembly()
  rawfileReaderDLLsPath <- .userRawfileReaderDLLsPath()

  if (isFALSE(dir.exists(rawfileReaderDLLsPath))){
    dir.create(rawfileReaderDLLsPath, recursive = TRUE)
  }
  
  
  if (file.exists(rawrrAssembly)){
    if(interactive() ){ 
      fmt <- "Overwrite '%s' ? [Y/n]: "
      prompt <- sprintf(fmt, rawrrAssembly)
      response <- readline(prompt = prompt)
      if (tolower(response) != "y"){
        message("Aborting ...")
        return()
        }
    }else{
      fmt <- "Overwriting '%s' ..."
      warning(sprintf(fmt, rawrrAssembly))
    }
  }

    rv = download.file(sourceUrl, destfile = rawrrAssembly, mode='wb', ...)
    message(sprintf("MD5 %s %s", tools::md5sum(rawrrAssembly), rawrrAssembly))
    rv
  }

.buildOnLoad <- function(){
  
  # nothing to do
  if (file.exists(.rawrrAssembly())){
    return()
  }
  
  # check Thermo DLLs
  if(isFALSE(.checkRawfileReaderDLLs(message))){
    return()
  }
  
  
  buildRawrrExe()
}


.determineAdditionalLibPath <- function(){
  monoPaths <- strsplit(Sys.getenv("MONO_PATH"), .Platform$path.sep)[[1]]
  pgkPath <- dirname(rawrr:::.rawrrAssembly())
  
  dlls <- rawrr:::.rawfileReaderDLLs()
  
  rv <- sapply(c(pgkPath, monoPaths, '/usr'), function(dir){
    if(all(sapply(dlls, function(x){file.exists(file.path(dir, x))}))){
      return(dir)
    }
  })
  
  rv <- rv[!vapply(rv, is.null, TRUE)]
  
  if(length(rv)>0)
    return (rv[1])
  
  NULL
}

#' Build \code{rawrr.exe} console application \code{rawrr.exe}
#' 
#' @description builds \code{rawrr.exe} file from C# source code requiring 
#' xbuild or msbuild tools.
#' 
#' @details The console application \code{rawrr.exe} is used by the package's reader functions through a \link{system2} call.
#' 
#' @author Christian Panse <cp@fgcz.ethz.ch>, 2021
#' 
#' @seealso \link{installRawrrExe} and \link{installRawFileReaderDLLs}
#' 
#' @references \itemize{
#'   \item{\url{https://www.mono-project.com/docs/advanced/assemblies-and-the-gac/}}
#'   \item{\url{https://planetorbitrap.com/rawfilereader}}
#'   \item{\url{https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/compiler-options/advanced}}
#' }
#' 
#' @return the return value of the system2 command.
#' @export buildRawrrExe
buildRawrrExe <- function(){
  packagedir <- system.file(package = 'rawrr')
 
  if (isFALSE(.checkRawfileReaderDLLs())){
    return()
  }
  
  if (Sys.which("msbuild") == "" && Sys.which("xbuild") == "")
  {
    msg <- c("Could not find 'msbuild' or 'xbuild' in the path. Therefore, ",
         "it is not possible to build the 'rawrr.exe' assembly from",
         " source code.\nTry to run rawrr::installRawrrExe().")
    stop(msg)
  }
  
  cwd <- getwd()
  setwd(file.path(packagedir, 'rawrrassembly'))
  
  cmd <- ifelse(Sys.which("msbuild") != "", "msbuild", "xbuild")

  # https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/compiler-options/advanced#additionallibpaths
  additionalLibPath <- .determineAdditionalLibPath()
  cmdArgs <- sprintf("/p:OutputPath=%s/ /p:AdditionalLibPaths=%s /v:diagnostic rawrr.csproj",
                     shQuote(dirname(.rawrrAssembly())),
                     shQuote(additionalLibPath))

  message("Attempting to build 'rawrr.exe', one time setup ...")
  rv <- system2 (cmd, cmdArgs, wait=TRUE, stderr=TRUE, stdout=TRUE)
  
  if (rv <- any(grepl("Build succeeded.", rv))
      && file.exists(.rawrrAssembly())){
    message(sprintf("rawrr.exe successfully built in \n\t'%s'.",
                    dirname(.rawrrAssembly())))
    message(rv)
  }else{
    message("build error report:")
    message(cmdArgs)
    message(rv)
    msg <- c("'rawrr.exe' build failed. Try to download and install", 
      " by calling ",
      "the 'rawrr::installRawrrExe()' method.")
    warning(msg)
  }
  setwd(cwd)
  rv
}

.isRawFileReaderLicenseAccepted <- function(){
  licenseFile <- file.path(system.file(package = 'rawrr'), 'rawrrassembly',
                           'RawFileReaderLicense.txt')
  stopifnot(file.exists(licenseFile))
  
  eulaFile <- file.path(cachedir <- tools::R_user_dir("rawrr", which='cache'),
                        "eula.txt")
  msg <- c("# By changing the setting below to TRUE you are accepting ",
    "the Thermo License agreement.")
  
  if (!file.exists(eulaFile)){
    file.show(licenseFile)
    fmt <- "Do you accept the Thermo License agreement '%s'? [Y/n]: "
    prompt <- sprintf(fmt, licenseFile)
    response <- readline(prompt = prompt)
    if (tolower(response) == "y"){
      if (!dir.exists(cachedir)) { dir.create(cachedir, recursive = TRUE) }
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

