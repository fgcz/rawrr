#R

# Test if \code{rawrr.exe} .NET assembly is working
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


#' @importFrom tools R_user_dir
.rawrrAssembly <- function(){
  libdir <- tools::R_user_dir("rawrr", which='cache')
  d <- file.path(libdir, 'rawrrassembly')

  if (Sys.info()['sysname'] == "Darwin"){
    file.path(d, 'osx-x64', 'rawrr') -> f
  } else if (Sys.info()['sysname'] == "Linux"){
    file.path(d, 'linux-x64', 'rawrr') -> f
  } else {
    file.path(d, 'win-x64', 'rawrr.exe') -> f
  }
    return(f)
}


#' URL for Thermo Fisher .NET assemblies
#'
#' @return an URL
.thermofisherlsmsUrl <- function(){
 #  "https://github.com/thermofisherlsms/RawFileReader/tree/main/Libs/NetCore/Net8/"
    "https://github.com/thermofisherlsms/RawFileReader/raw/refs/heads/main/Libs/NetCore/Net8/"
}


#' Download and install the Thermo Fisher Scientific .NET 8.0 nupkgs
#'
#' @param sourceUrl url of nupkgs.
#' @param force if \code{TRUE} it will overwrite the pkgs.
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
.downloadNupkgs <- function(sourceUrl = .thermofisherlsmsUrl(), force = TRUE){

    rawfileReaderDLLsPath <- rawrrAssemblyPath()

    if (isFALSE(dir.exists(rawfileReaderDLLsPath))){
      dir.create(rawfileReaderDLLsPath, recursive = TRUE)
    }

    if (isTRUE(dir.exists(rawfileReaderDLLsPath))){
      msg <- sprintf("removing nupkgs files in directory '%s'", rawfileReaderDLLsPath)
      message(msg)

      file.remove(file.path(rawrrAssemblyPath(),
                            list.files(rawrrAssemblyPath(), pattern="\\.nupkg$")))
    }

    c('ThermoFisher.CommonCore.BackgroundSubtraction.8.0.6.nupkg',
      'ThermoFisher.CommonCore.RandomAccessReaderPlugin.8.0.6.nupkg',
      'ThermoFisher.CommonCore.Data.8.0.6.nupkg',
      'ThermoFisher.CommonCore.RawfileReader.8.0.6.nupkg',
      'ThermoFisher.CommonCore.MassPrecisionEstimator.8.0.6.nupkg') |> vapply(FUN = function(nupkg){
      destfile <- file.path(rawfileReaderDLLsPath, nupkg)
      download.file(file.path(sourceUrl, nupkg),
                    destfile = destfile, mode='wb')
    }, FUN.VALUE = 0) -> rv

    rv
  }

# dotnet nuget add source /Users/cp/Library/Caches/org.R-project.R/R/rawrr/rawrrassembly/
# dotnet nuget remove source "Package source 1"
# dotnet nuget list source
.addNupkgSource <- function(){
  system2('dotnet', args = c('nuget', 'add', 'source', rawrrAssemblyPath()))
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

# dotnet add package ThermoFisher.CommonCore.MassPrecisionEstimator
.addPackages <- function(dir, version = "8.0.6"){
  tempOut <- tempfile(pattern = "rawrr.add.packages.stdout.", tmpdir = dir, fileext = ".txt")
  tempErr <- tempfile(pattern = "rawrr.add.packages.stderr.", tmpdir = dir, fileext = ".txt")
   setwd(dir)
    c('ThermoFisher.CommonCore.BackgroundSubtraction',
      'ThermoFisher.CommonCore.RandomAccessReaderPlugin',
      'ThermoFisher.CommonCore.Data',
      'ThermoFisher.CommonCore.RawfileReader',
      'ThermoFisher.CommonCore.MassPrecisionEstimator') |>
   vapply(FUN = function(nupkg){
     system2('dotnet', args = c('add', 'package', nupkg, '-v', version),
       stdout = tempOut,
       stderr = tempErr) -> rv
     if (interactive()){
	if (rv != 0){
	  file.show(tempOut)
	  file.show(tempErr)
	}
     }
     rv
   }, FUN.VALUE = 0)
}

.clean <- function(){
  message("Removing ", .rawrrAssembly())
  file.remove(.rawrrAssembly())
}

.build <- function(dir){
################################################################################
  setwd(dir)

  tempOut <- tempfile(pattern = "rawrr.build.stdout.", tmpdir = dir, fileext = ".txt")
  tempErr <- tempfile(pattern = "rawrr.build.stderr.", tmpdir = dir, fileext = ".txt")

  message("Running build ...")
  message("Write stdout to", tempOut)
  message("Write stderr to", tempErr)

  system2('dotnet', args = c('publish', '-c', 'Release', '-a', 'x64', '-p',
    'PublishReadyToRun=true', '-o', dirname(.rawrrAssembly())),
    stdout = tempOut,
    stderr = tempErr) -> rv


  if (rv == 0){
    message("Build succesfully done.")
  }else{
    message("Build error.")
    if (interactive()){
	file.show(tempOut)
	file.show(tempErr)
    }
  }
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
  function (sourceUrl = "https://fgcz-ms.uzh.ch/~cpanse/rawrr/dotnet/1.17.2/",
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

  if (Sys.info()["sysname"] == "Darwin") {
            sourceUrl <- file.path(sourceUrl, "osx-x64", "rawrr")
  }
  else if (Sys.info()["sysname"] == "Linux") {
            sourceUrl <- file.path(sourceUrl, "linux-x64", "rawrr")
  }
  else {
            sourceUrl <- file.path(sourceUrl, "win-x64", "rawrr.exe")
  }
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
#' To use this function, ensure that the local RawFileReader NuGet packages 
#' are added to the NuGet source list. You can accomplish this by 
#' downloading the necessary packages with 
#' \code{rawrr:::.downloadNupkgs()} and subsequently running 
#' \code{rawrr:::.addNupkgSource()}.
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
#' @return the return value of the system2 command.
#' @export
buildRawrrExe <- function(){
  packagedir <- system.file(package = 'rawrr')
  buildDir <- tempdir()

  if (Sys.which("dotnet") == "")
  {
    msg <- c("Could not find 'dotnet' in the path. Therefore, ",
         "it is not possible to build the 'rawrr.exe' assembly from",
         " source code.\nTry to run rawrr::installRawrrExe().")
    stop(msg)
  }

  message("Building rawrr assembly using .NET 8.0 ...")

  if (isFALSE(dir.exists( rawrrAssemblyPath() ))){
    dir.create(rawrrAssemblyPath(), recursive = TRUE)
  }

  .copySourceCode(dir = buildDir)

  (system2("dotnet", args =c('nuget', 'list', 'source'),
    stdout = TRUE) |>
    grepl(pattern = "rawrrassembly") |>
    sum()  >= 1) -> nugetPkgsPresent

  if(nugetPkgsPresent){
    warning("Have you downloaded the 'thermofisherlsms/RawFileReader' NuGet packages? \n",
    "If not, please consider executing the methods:\n",
    "  -> rawrr:::.downloadNupkgs()\n",
    "  -> rawrr:::.addNupkgSource()\n",
    "These steps should be performed once to ensure proper setup.")
    # .downloadNupkgs()
    # .addNupkgSource()
  }

  .addPackages(dir = buildDir)
  .build(dir = buildDir)
  .isAssemblyWorking()
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

