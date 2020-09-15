#R

#' @importFrom utils packageVersion
.onAttach <- function(lib, pkg){
	if(interactive()){
		version <- packageVersion('rawR')
		packageStartupMessage("Package 'rawR' version ", version)
	  invisible()
	}
    .isMonoAssemblyWorking()
}

