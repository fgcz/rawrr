#' Read a Set of Spectra
#'
#' @param rawfile the name of the Thermo Fisher Scietific raw file.
#' @param scans a vector of requested scan numbers.
#' @param tmpdir a non-empty character vector giving the directory name; default
#' uses \code{tempdir()}.
#' @author Tobias Kockmann and Christian Panse <cp@fgz.ethz.ch> 2018, 2019, 2020
#' 
#' @description the function reads scan information, e.g., charge, mZ,
#' or intensity of a given set of scan numbers using a dot net interface and
#' the ThermoFisher NewRawFileReader libraries.
#'  
#' @seealso Thermo Fisher NewRawfileReader C# code snippets
#' \url{https://planetorbitrap.com/rawfilereader}.
#' 
#' @references \itemize{
#' \item{\url{https://doi.org/10.5281/zenodo.2640013}}
#' \item{the R function 1st appeared in
#' \url{https://doi.org/10.1021/acs.jproteome.8b00173}}
#' }
#' 
#' @aliases readSpectrum
#' 
#' @export readSpectrum
#' 
#' @return  peaklistSet, a nested list of \CRANpkg{protViz} peaklist objects.
#' 
#' @examples
#' (rawfile <- file.path(path.package(package = 'rawR'), 'extdata',
#'   'sample.raw'))
#' 
#' S <- readSpectrum(rawfile, scans = 1:9)
#' 
#' S[[1]]
#' 
#' names(S[[1]])
#' 
#' .plot.peaklist <- function(x, ...){
#'   plot(x$mZ, x$intensity, type='h')
#'   labels <- na.omit(lapply(x, function(y){if (length(y)==1){y}else{NA}}))
#'   legend("topright", paste(names(labels), labels, sep=": "), ...)
#'  }
#'  
readSpectrum <- function(rawfile, scans, tmpdir=tempdir()){
    mono <- if(Sys.info()['sysname'] %in% c("Darwin", "Linux")) TRUE else FALSE
    exe <- file.path(path.package(package = "rawR"), "exec", "rawR.exe")
    
    
    if (!file.exists(rawfile)){
        warning('file not available. return.')
        return
    }
    
    tfi <- tempfile(tmpdir=tmpdir)
    tfo <- tempfile(tmpdir=tmpdir)
    tfstdout <- tempfile(tmpdir=tmpdir)
    
    cat(scans, file = tfi, sep="\n")
    
    cmd <- exe
    
    if (mono){
        rvs <- system2("mono", args = c(shQuote(exe), shQuote(rawfile),
                                        "scans", shQuote(tfi), shQuote(tfo)))
    }else{
        rvs <- system2(exe, args = c( shQuote(rawfile), "scans", shQuote(tfi),
                                      shQuote(tfo)))
    }
    
    e <- new.env(); e$PeakList <- list()
    source(tfo, local=TRUE)
    unlink(c(tfi, tfo, tfstdout))
    
    return(lapply(e$PeakList,
                  function(x){class(x) <- c(class(x), 'peaklist'); x}))
}


#' read file header Information
#'
#' @param rawfile the name of the Thermo Fisher Scietific raw file
#' @param mono enviroment
#' @param exe path of the executable.
#' @param mono_path default.
#' @param argv arguments, default.
#' @param system2_call system2 call, default.
#' @param method instrument vendor
#' @description The function extracts some meta information from a given rawfile.
#' The R code output is parsed by the function and a list object is returned.
#' @author Tobias Kockmann and Christian Panse 2018, 2019, 2020
#' @seealso Thermo Fisher NewRawfileReader C# code snippets
#' \url{https://planetorbitrap.com/rawfilereader}.
#' 
#' @return a list object
#' @export readFileHeader
#'
#' @examples
#' (rawfile <- file.path(path.package(package = 'rawR'), 'extdata',
#'   'sample.raw'))
#' 
#' M <- readFileHeader(rawfile)
readFileHeader <- function(rawfile,
    mono = if(Sys.info()['sysname'] %in% c("Darwin", "Linux")) TRUE else FALSE,
    exe = file.path(path.package(package = "rawR"), "exec", "rawR.exe"),
    mono_path = "",
    argv = "infoR",
    system2_call = TRUE,
    method = "thermo"){
    
    if (!file.exists(rawfile)){
        warning('file not available. return.')
        return
    }
    if(system2_call && method == 'thermo'){
        
        tf <- tempfile(fileext = '.R')
        tf.err <- tempfile(fileext = '.err')
        
        # message(paste("system2 is writting to tempfile ", tf, "..."))
        
        if (mono){
            rvs <- system2("mono", args = c(exe, shQuote(rawfile
            ), argv),
            stdout = tf)
        }else{
            rvs <- system2(exe, args = c(shQuote(rawfile
            ), argv),
            stderr = tf.err,
            stdout = tf)
        }
        
        if (rvs == 0){
            
            try({
                source(tf, local=TRUE)
                
                #message(paste("unlinking", tf, "..."))
                #unlink(tf)
                return(e$info)
            }, NULL)
            
            # unlink(tfstdout)
            return(rv)
        }
    }
    NULL
}