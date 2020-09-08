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
            
            rv <- try({
                e <- new.env();
                e$info <- list()
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

#' Extracts Chromatogram (XIC)
#'
#' @param rawfile the file name. 
#' @param mass a vector of mass values. 
#' @param tol tolerance in ppm.
#' @param mono if the mono enviroment should be used. 
#' @param exe the exe file user by mono.
#' @seealso Thermo Fisher NewRawfileReader C# code snippets
#' \url{https://planetorbitrap.com/rawfilereader}.
#' 
#' @return list of chromatogram  objects
#' @references \itemize{
#' \item{\url{https://doi.org/10.5281/zenodo.2640013}}
#' \item{the R function 1st appeared in
#' \url{https://doi.org/10.1021/acs.jproteome.8b00173}}
#' }
#' @author Christian Trachsel, Tobias Kockmann and
#' Christian Panse <cp@fgz.ethz.ch> 2018, 2019, 2020
#' @export readChromatogram 
#' @examples
#' 
#' # Example 1: not meaning full but proof-of-concept
#' (rawfile <- file.path(path.package(package = 'rawR'), 'extdata', 'sample.raw'))
#' 
#' X <- readChromatogram(rawfile, mass=c(669.8381, 726.8357), tol=1000)
#' 
#' # Example 2: extract iRT peptides
#' iRTpeptide <- c("LGGNEQVTR", "YILAGVENSK", "GTFIIDPGGVIR", "GTFIIDPAAVIR",
#' "GAGSSEPVTGLDAK", "TPVISGGPYEYR", "VEATFGVDESNAK",
#' "TPVITGAPYEYR", "DGLDAASYYAPVR", "ADVTPADFSEWSK",
#' "LFLQFGAQGSPFLK")
#' 
#'  # [2H+] 
#' if (require(protViz)){
#'     (mZ <- (parentIonMass(iRTpeptide) + 1.008) / 2)
#'   }else{
#'      message("consider installing  https://CRAN.R-project.org/package=protViz")
#'  }
#'
#' \dontrun{
#' rawfile <- "/home/cp/Downloads/20180220_14_autoQC01.raw"
#'  X <- readChromatogram(rawfile, masses=mZ)
#' }
#' 
readChromatogram <- function(rawfile,
                     mass,
                     tol = 10,
                     mono = if(Sys.info()['sysname'] %in% c("Darwin", "Linux")) TRUE else FALSE,
                     exe = file.path(path.package(package = "rawR"), "exec", "rawR.exe")){
    
    if (!file.exists(rawfile)){
        warning('file not available. return.')
        return
    }
    
    tfi <- tempfile()
    tfo <- tempfile()
    tfstdout <- tempfile()
    
    cat(mass, file=tfi, sep="\n")
    
    cmd <- exe
    
    if (mono){
        rvs <- system2("mono", args = c(shQuote(exe), shQuote(rawfile), "xic", shQuote(tfi), tol, shQuote(tfo)))
    }else{
        rvs <- system2(exe, args = c( shQuote(rawfile), "xic", shQuote(tfi), tol, shQuote(tfo)))
    }
    
    
    rv <- try({
        e <- new.env();
        e$chromatogram <- list()
        source(tfo, local = TRUE)
        
        e$chromatogram
    }, NULL)
    unlink(c(tfi, tfo, tfstdout))
    
    #message(paste(c(tfi, tfo, tfstdout), collapse = ", "))
    #message(length(rv))
   rv <- lapply(rv,
                function(x){
                     x$filename <- rawfile
                     class(x) <- c(class(x), 'chromatogram');
                    x})
    
    class(rv) <- 'chromatogramSet'
    rv
}
