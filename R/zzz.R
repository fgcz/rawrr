#R

#' @importFrom utils packageVersion
.onAttach <- function(lib, pkg){
    packagedir <- system.file(package = 'rawrr')
    rawrrAssembly <- .rawrrAssembly()
    
    if(interactive()){
        version <- packageVersion('rawrr')
        thermocopyright <- "RawFileReader reading tool. Copyright \u00A9 2016 by Thermo Fisher Scientific, Inc. All rights reserved."
        packageStartupMessage("Package 'rawrr' version ", version, " using\n", thermocopyright)
        invisible()
    }
    
    if(isFALSE(.checkRawfileReaderDLLs())){return()}

    if (file.exists(rawrrAssembly) && .isMonoAssemblyWorking())
        return()
    
    packageStartupMessage ("Attempting to build 'rawrr.exe', one time setup")
    .buildRawrrExe()
    
    .isMonoAssemblyWorking()
}

