#R

#' @importFrom utils packageVersion
.onAttach <- function(lib, pkg){
   packagedir <- system.file(package = 'rawrr')
   server <- sprintf("%s/rawrrassembly/bin/rawrr.exe", packagedir)



	if(interactive()){
		version <- packageVersion('rawrr')
		packageStartupMessage("Package 'rawrr' version ", version)
	  invisible()
        }



    if (file.exists(server))
        return()


    packageStartupMessage ("attempting to build rawrr.exe, one time setup")
    if (Sys.which("msbuild") == "" && Sys.which("xbuild") == "")
    {
        warning ("could not find msbuild or xbuild in path; will not be able to use rDotNet unless corrected and rebuilt")
        return()
    }

    cwd <- getwd()
    setwd(sprintf("%s/rawrrassembly", packagedir))


    packageStartupMessage ("building project")
    system2 (ifelse(Sys.which("msbuild") != "", "msbuild", "xbuild"), wait=TRUE, stderr=TRUE, stdout=TRUE)
    setwd(cwd)


    .isMonoAssemblyWorking()
}

