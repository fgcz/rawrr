#R

#' @importFrom utils packageVersion
.onAttach <- function(lib, pkg){
    if(interactive()){
        packagedir <- system.file(package = 'rawrr')
        version <- packageVersion('rawrr')
        thermocopyright <- "RawFileReader reading tool. Copyright \u00A9 2016 by Thermo Fisher Scientific, Inc. All rights reserved."
        packageStartupMessage("Package 'rawrr' version ", version, " using\n", thermocopyright)
        invisible()
    }
}
    
.onLoad <- function(lib, pkg){
  if (Sys.info()['sysname'] %in% c("Darwin", "Linux")){
    mp <- Sys.which('mono')
    if (!nzchar(mp)){
      msg <- c("The cross platform, open source .NET framework (mono) is not available.\n", 
               "Consider to install 'apt-get install mono-runtime' on Linux\n",
               "or download/install from https://www.mono-project.com/.")
      stop(msg)
    }
  }

  if(interactive()){
    .buildOnLoad()
   }
}
