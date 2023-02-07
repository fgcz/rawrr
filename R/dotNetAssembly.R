#R

# Test if \code{rawrr.exe} .NET assembly is working
.isAssemblyWorking <-
  function(FUN = stop, exe = .rawrrAssembly()){
    if (Sys.info()['sysname'] %in% c("Darwin", "Linux")){
      if (Sys.which('mono') == ""){
        msg <- c("The cross platform, open source .NET framework (mono) is not available.\n", 
                 "Consider to install 'apt-get install mono-runtime' on Linux\n",
                 "or download/install from https://www.mono-project.com/.")
        FUN(msg)
      }
    }
    
    .checkRawFileReaderDLLs(FUN)
    
    if (isFALSE(file.exists(exe))){
      msg <- c("'rawrr.exe' not found.\n",
               "Run 'rawrr::installRawrrExe()'.",
               " For more information, type '?rawrr.exe'.")
      FUN(msg)
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
    
    if(interactive() && isFALSE(.checkDllInMonoPath())){ stopifnot(.isRawFileReaderLicenseAccepted()) }
    TRUE
  }


.rawfileReaderDLLs <- function(){
  # 'ThermoFisher.CommonCore.BackgroundSubtraction.dll',
  c(
    'ThermoFisher.CommonCore.Data.dll',
    'ThermoFisher.CommonCore.MassPrecisionEstimator.dll',
    'ThermoFisher.CommonCore.RawFileReader.dll')
}

#' Derives the path where all .NET assemblies are stored.
#'
#' @return path
#' @export rawrrAssemblyPath
#' @seealso \code{installRawFileReaderDLLs} and \code{installRawrrExe}
#'
#' @examples
#' rawrrAssemblyPath()
rawrrAssemblyPath <- function(){
  libdir <- tools::R_user_dir("rawrr", which='cache')
  d <- file.path(libdir, 'rawrrassembly')
  
  if (interactive()){
    if (isFALSE(dir.exists(d))){
      #msg <- sprintf("rawrr .NET assemply path '%s' is not existing!", d)
      #warning(msg)
    }
  }
  return(d)
}

#' Check if a file is contained in the environment variable \code{MONO_PATH}.
#'
#' @param dll a file name.
#'
#' @return a boolean
#' @export
.checkDllInMonoPath <- function(dll="ThermoFisher.CommonCore.Data.dll"){
  monoPath <- Sys.getenv("MONO_PATH", names=TRUE)
  monoPath <- strsplit(monoPath, .Platform$path.sep)[[1]]
  any(vapply(monoPath, function(d){
    file.exists(file.path(d, dll))
  }, FALSE))  
}


.checkRawFileReaderDLLs <- function(FUN=stop){
  rv <- vapply(.rawfileReaderDLLs(), function(dll){
    userFileDllPath <- file.path(rawrrAssemblyPath(), dll)
    dllExists <- file.exists(userFileDllPath) || .checkDllInMonoPath(dll)
    if (isFALSE(dllExists)){
      message(sprintf("'%s' is missing.", dll))
    }
    return(dllExists)
  }, FALSE)
  
  if (isFALSE(all(rv)) && TRUE){
    FUN("'ThermoFisher.CommonCore.*.dll' files are not available on the system.\n",
         "Run 'rawrr::installRawFileReaderDLLs()' or setenv MONO_PATH to ",
         "the location where the assemblies are located.\n",
         "For more information, type '?ThermoFisher'.")
  }
  all(rv)
}


.rawrrAssembly <- function(){
  f <- file.path(rawrrAssemblyPath(), 'rawrr.exe')
  return(f)
}


#' URL for Thermo Fisher .NET assemblies
#'
#' @return an URL
#' @export
.thermofisherlsmsUrl <- function(){
  "https://github.com/thermofisherlsms/ThermoRawFileParser/raw/master/packages/ThermoFisher.CommonCore.RawFileReader.4.0.26/lib/"
}


#' Download and install the New RawFileReader from Thermo Fisher Scientific .Net
#' assemblies i
#' 
#' @description 
#' Download and install the New RawFileReader from Thermo Fisher Scientific .Net
#' assemblies in 
#' the directory provided by \code{rawrrAssemblyPath()}.
#' 
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
#' The console application assembly \code{rawrr.exe} requires three
#' assemplies:
#' \itemize{
#' \item {\code{ThermoFisher.CommonCore.Data.dll}, }
#' \item{\code{ThermoFisher.CommonCore.MassPrecisionEstimator.dll}, and}
#' \item{ThermoFisher.CommonCore.RawFileReader.dll}
#' }.
#' 
#' The \code{rawrr.exe} assembly can be built from C# source code by using the
#' \code{msbuild} tool shipped by the \url{https://www.mono-project.com} or by
#' Microsoft's .NET SDK \url{https://dotnet.microsoft.com} on Linux, Microsoft,
#' and macOS.
#'
#' If no build tool and C# compiler (\code{csc} or \code{msc}) are available or
#' the build process fails, you can download \code{rawrr.exe} assembly from the
#' authors' site.
#' 
#' @seealso \link{buildRawrrExe} and \link{installRawrrExe}
#' 
#' @references \itemize{
#'   \item{\url{https://www.mono-project.com/docs/advanced/assemblies-and-the-gac/}}
#'   \item{\url{https://planetorbitrap.com/rawfilereader}}
#'   \item{\doi{10.1021/acs.jproteome.0c00866}}
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
#' \donttest{
#' rawrr::installRawFileReaderDLLs() 
#' rawrr::buildRawrrExe() || rawrr::installRawrrExe()
#' }
# TODO(cp): rename installThermoFisherScientificRawFileReaderAssemblyDLLs()
installRawFileReaderDLLs <-
  function(sourceUrl = .thermofisherlsmsUrl(), ...){
    
    rawfileReaderDLLsPath <- rawrrAssemblyPath()
    
    if (isTRUE(dir.exists(rawfileReaderDLLsPath))){
      msg <- sprintf("removing files in directory '%s'", rawfileReaderDLLsPath)
      message(msg)
      
      file.remove(file.path(rawrrAssemblyPath(),
                            list.files(rawrrAssemblyPath())))
    }
    
    if (isFALSE(dir.exists(rawfileReaderDLLsPath))){
      dir.create(rawfileReaderDLLsPath, recursive = TRUE)
    }
    
    if(interactive()){ stopifnot(.isRawFileReaderLicenseAccepted()) }
    
    rv <- vapply(.rawfileReaderDLLs(), function(dll){
      destfile <- file.path(rawfileReaderDLLsPath, dll)
      download.file(file.path(sourceUrl, dll),
                    destfile=destfile, mode='wb', ...)
    }, 0) 
    rv
  }



#' Download and install the \code{rawrr.exe} console application
#' 
#' @description downloads and installs the \code{rawrr.exe} .Net assembly in 
#' the directory provided by \code{rawrrAssemblyPath()}.
#' 
#' @details The console application \code{rawrr.exe} is used by the package's
#' reader functions through a \link{system2} call.
#' 
#' @param sourceUrl url of \code{rawrr.exe} assembly.
#' @param ... other parameter for \code{download.file}.
#'
#' @return An integer code, 0 for success and non-zero for
#' failure. For the "wget" and "curl" methods this is the status code returned
#' by the external program.
#' @seealso \link{buildRawrrExe}
#' @references \doi{10.1021/acs.jproteome.0c00866}
#' @aliases rawrr.exe
#' @export installRawrrExe
installRawrrExe <-
  function (sourceUrl = "https://github.com/fgcz/rawrr/releases/download/1.5.3/rawrr.1.5.3.exe",
            ...)
  {
   
    
    if (isFALSE(dir.exists(rawrrAssemblyPath()))){
      dir.create(rawrrAssemblyPath(), recursive = TRUE)
    }
    
    rawrrAssembly <- .rawrrAssembly()
    
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
  if(isFALSE(.checkRawFileReaderDLLs(message))){
    return()
  }

  if (Sys.which("msbuild") == "" && Sys.which("xbuild") == "")
  {
    msg <- c("Could not find 'msbuild' or 'xbuild' in the path. Therefore, ",
         "it is not possible to build the 'rawrr.exe' assembly from",
         " source code.\nTry to run rawrr::installRawrrExe().")
    message(msg)
    return()
  }

  buildRawrrExe()
}


.determineAdditionalLibPath <- function(){
  monoPaths <- strsplit(Sys.getenv("MONO_PATH"), .Platform$path.sep)[[1]]
  pgkPath <- rawrrAssemblyPath()
  
  dlls <- .rawfileReaderDLLs()
  
  rv <- lapply(c(pgkPath, monoPaths, '/usr/local/lib'), function(d){
    if(all(vapply(dlls, function(x){file.exists(file.path(d, x))}, TRUE))){
      return(d)
    }else{NULL}
  })
  
  rv <- rv[!vapply(rv, is.null, TRUE)]
  
  if(length(rv) > 0)
    return (rv[[1]])
  
  NULL
}

#' Build \code{rawrr.exe} console application.
#' 
#' @description builds \code{rawrr.exe} file from C# source code requiring 
#' xbuild or msbuild tools. The console application \code{rawrr.exe}
#' is used by the package's reader functions through a \link{system2} call.
#' 
#' @details The rawrr package implementation consists of two language layers,
#' the top R layer and the hidden C# layer. Specifically, R functions requesting
#' access to data stored in binary raw files invoke compiled C# wrapper methods
#' using a \link{system2} call. Calling a wrapper method typically results in the
#' execution of methods defined in the RawFileReader dynamic link library
#' provided by Thermo Fisher Scientific. Our precompiled wrapper methods are
#' bundled in the \code{rawrr.exe} executable file (.NET assembly) and shipped
#' with the released R package. Running \code{rawrr.exe} requires the
#' \url{https://www.mono-project.com/} environment on non-Microsoft
#' operating systems. Mono is a cross platform, open source .NET framework.
#' On Microsoft Windows the Microsoft .NET framework is typically already
#' installed and sufficient. Our package also contains the C# source code
#' \code{rawrr.cs}.
#' In order to return extracted data back to the R layer we use file I/O.
#' More specifically, the extracted information is written to a temporary
#' location on the harddrive, read back into memory and parsed into R  objects.
#' 
#' @author Tobias Kockmann, Christian Panse <cp@fgcz.ethz.ch>, 2021
#' 
#' @seealso \link{installRawrrExe} and \link{installRawFileReaderDLLs}
#' 
#' @references \itemize{
#'   \item{\url{https://www.mono-project.com/docs/advanced/assemblies-and-the-gac/}}
#'   \item{\url{https://planetorbitrap.com/rawfilereader}}
#'   \item{\url{https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/compiler-options/advanced}}
#'   \item{\doi{10.1021/acs.jproteome.0c00866}}
#' }
#' 
#' @return the return value of the system2 command.
#' @export buildRawrrExe
buildRawrrExe <- function(){
  packagedir <- system.file(package = 'rawrr')
 
  if (isFALSE(dir.exists(rawrrAssemblyPath()))){
    dir.create(rawrrAssemblyPath(), recursive = TRUE)
  }
  
  if (isFALSE(.checkRawFileReaderDLLs())){
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
  
  buildLog <- tempfile("rawrr_build.log.",
                       tmpdir = rawrrAssemblyPath())
  
  cmdArgs <- sprintf("/p:OutputPath=%s/ /p:AdditionalLibPaths=%s /v:diagnostic /flp:LogFile=%s rawrr.csproj",
                     shQuote(rawrrAssemblyPath()),
                     shQuote(additionalLibPath),
                     shQuote(buildLog))

  message("Attempting to build 'rawrr.exe', one time setup ...")
  rv <- system2 (cmd, cmdArgs, wait=TRUE, stderr=TRUE, stdout=TRUE)
  
  if (rv <- any(grepl("Build succeeded.", rv))
      && file.exists(.rawrrAssembly())){
    msg <- sprintf("'rawrr.exe' successfully built in \n'%s'.
The build report should have been saved in\n'%s'.", .rawrrAssembly(), buildLog)
    message(msg)
  }else{
    err <- sprintf("Building 'rawrr.exe' failed. For details see the build report, supposed to be saved in:
'%s'
Call 'rawrr::installRawrrExe()' to download and install a precompiled version
from a remote location. Note this requires internet connection.",
                   buildLog)
    setwd(cwd)
    stop(err)
  }
  setwd(cwd)
  rv
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

