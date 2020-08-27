#' readSpectrum of a raw file
#'
#' @param rawfile the name of the Thermo Fisher Scietific raw file which
#' the data are to be read from.  
#' @param scans a vector of requested scan numbers.
#' @param tmpdir a non-empty character vector giving the directory name; default
#' uses \code{tempdir()}.
#' @author Christian Panse <cp@fgz.ethz.ch> 2018, 2019, 2020
#' 
#' @description the function reads scan information of a given set of scan
#' number using a dot net interface and the ThermoFisher NewRawFileReader 
#' libraries. 
#'  
#' @references \url{https://doi.org/10.5281/zenodo.2640013}
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
readSpectrum <- function(rawfile, scans, tmpdir=tempdir()){
    mono <- if(Sys.info()['sysname'] %in% c("Darwin", "Linux")) TRUE else FALSE
    exe <- file.path(path.package(package = "rawR"), "exec", "rawR.exe")
    
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
