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
    if(interactive()){
        .buildOnLoad()
    }
}
