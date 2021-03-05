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
    
    if (file.exists(rawrrAssembly) && .isMonoAssemblyWorking())
        return()
    
    packageStartupMessage ("attempting to build 'rawrr.exe', one time setup")
    
    if (Sys.which("msbuild") == "" && Sys.which("xbuild") == "")
    {
        warning ("could not find msbuild or xbuild in path; will not be able to use rDotNet unless corrected and rebuilt")
        return()
    }
    
    cwd <- getwd()
    # setwd(sprintf("%s/rawrrassembly", packagedir))
    setwd(file.path(packagedir, 'rawrrassembly'))
    
    packageStartupMessage ("building project")
    system2 (ifelse(Sys.which("msbuild") != "", "msbuild", "xbuild"),
             wait=TRUE, stderr=TRUE, stdout=TRUE)
    setwd(cwd)
    
    .isMonoAssemblyWorking()
}

