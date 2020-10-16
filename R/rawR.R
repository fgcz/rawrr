.writeRData <- function(rawfile, outputfile=paste0(rawfile, ".RData"), tmpdir=tempdir()){
    
    scanRange <- readFileHeader(rawfile)$`Scan range`
    scanIdx <- seq(scanRange[1], scanRange[2], by=1)
    
    res <- lapply(scanIdx, function(x){
        rv <- readSpectrum(rawfile, scan=x, tmpdir=tmpdir)[[1]]
        list(scanType=rv$scanType, mZ=rv$mZ, intensity=rv$intensity, charge=rv$charge, rtinseconds=rv$rtinseconds)
    })
    e <- new.env()
    objName <- paste("S",basename(rawfile), sep='')
    assign(objName, res, envir = e)
    
    save(objName, file=outputfile, envir = e)

} 

.isMonoAssemblyWorking <- 
    function(exe = file.path(path.package(package = "rawR"), "exec", "rawR.exe")){
        if(Sys.info()['sysname'] %in% c("Darwin", "Linux")){
            if (Sys.which('mono') == ""){
                warning("Can not find Mono JIT compiler. check SystemRequirements.")
                return()
            }
        }
        
        if (!file.exists(exe)){
            warning("rawR.exe is not availble.")
            return (FALSE)
        }
        
        # execute Assembly
        rvs <-  "?"
        if (Sys.info()['sysname'] %in% c("Darwin", "Linux")){
            rvs <- system2(Sys.which('mono'), args = c(shQuote(exe)), stdout = TRUE)
        }else{
            rvs <- system2(exe, stdout = TRUE)
        }
        
        # expect that string
        if (rvs != "No RAW file specified!"){
            warning("Mono JIT compiler and rawR.exe assembly are not working.")
            return (FALSE)
        }
        
        TRUE
    }

#' Test if object is instance of class \code{rawRspectrum}
#'
#' @param x object to be tested
#'
#' @return TRUE or FALSE
#' @export is.rawRspectrum
#'
#' @examples
#' 
#' pathToRawFile <- file.path(path.package(package = 'rawR'), 'extdata', 'sample.raw')
#' S <- readSpectrum(pathToRawFile, scan = 1:10)
#' is.rawRspectrum(S[[1]])
is.rawRspectrum <- function(x){
    class(x) == "rawRspectrum"
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
#' @references Thermo Fisher NewRawfileReader C# code snippets
#' \url{https://planetorbitrap.com/rawfilereader}.
#' 
#' @seealso \link[rawDiag]{read.raw.info}
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
        stop(paste0('file ', rawfile, ' is not available. return.'))
    }
    if (!.isMonoAssemblyWorking()){
        stop('the mono assembly are not available.')
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

#' Read scan index
#'
#' @param rawfile the name of the Thermo Fisher Scietific raw file.
#' @param tmpdir a non-empty character vector giving the directory name; default
#' uses \code{tempdir()}.
#'
#' @return returns a \code{data.frame} with the column names
#' scanType, rtinseconds, precursorMass, and charge of all spectra.
#' @export readIndex
#' @author Tobias Kockmann and Christian Panse <cp@fgz.ethz.ch>, 2020
#' @seealso \link[rawDiag]{read.raw}
#'
#' @examples
#' (rawfile <- file.path(path.package(package = 'rawR'), 'extdata',
#'   'sample.raw'))
#'   
#' Idx <- readIndex(rawfile)
#' table(Idx$scanType)
#' plot(Idx$rtinseconds, Idx$precursorMass, col=as.factor(Idx$charge), pch=16)
readIndex <- function(rawfile, tmpdir=tempdir()){
    mono <- if(Sys.info()['sysname'] %in% c("Darwin", "Linux")) TRUE else FALSE
    exe <- file.path(path.package(package = "rawR"), "exec", "rawR.exe")
    
    if (!file.exists(rawfile)){
        stop(paste0('file ', rawfile, ' is not available. return.'))
    }

    if (!.isMonoAssemblyWorking()){
        stop('the mono assembly are not available.')
    }
    
    tfi <- tempfile(tmpdir=tmpdir)
    tfo <- tempfile(tmpdir=tmpdir)
    tfstdout <- tempfile(tmpdir=tmpdir)
    
    cat(NULL, file = tfi, sep="\n")
    
    cmd <- exe
    
    if (mono){
        rvs <- system2(Sys.which("mono"), args = c(shQuote(exe), shQuote(rawfile),
                                                   "scans", shQuote(tfi), shQuote(tfo)))
    }else{
        rvs <- system2(exe, args = c( shQuote(rawfile), "scans", shQuote(tfi),
                                      shQuote(tfo)))
    }
    e <- new.env()
    
    source(tfo, local=TRUE)
    unlink(c(tfi, tfo, tfstdout))
    
    do.call('rbind', lapply(e$Spectrum, as.data.frame))
}

#' Read a Set of Spectra
#'
#' @param rawfile the name of the Thermo Fisher Scietific raw file.
#' @param scan a vector of requested scan numbers. 
#' @param tmpdir a non-empty character vector giving the directory name; default
#' uses \code{tempdir()}.
#' @param validate boolean default is \code{FALSE}.
#' @author Tobias Kockmann and Christian Panse <cp@fgz.ethz.ch> 2018, 2019, 2020
#' 
#' @description the function reads scan information, e.g., charge, mZ,
#' or intensity of a given set of scan numbers using a dot net interface and
#' the ThermoFisher NewRawFileReader libraries.
#'  
#' @references \itemize{
#' \item{Thermo Fisher NewRawfileReader C# code snippets
#' \url{https://planetorbitrap.com/rawfilereader}}.
#' \item{\url{https://doi.org/10.5281/zenodo.2640013}}
#' \item{the R function 1st appeared in 
#' \url{https://doi.org/10.1021/acs.jproteome.8b00173}.}
#' }
#' 
#' @aliases readSpectrum plot.rawRSpectrum rawRspectrum
#' 
#' @export readSpectrum
#' @exportClass rawRspectrum
#' @exportS3Method plot rawRspectrum
#' @exportS3Method print rawRspectrum
#' 
#' @return  a list of \code{spectrum} objects.
#' @seealso \link[rawDiag]{readScans}
#' 
#' @examples
#' (rawfile <- file.path(path.package(package = 'rawR'), 'extdata',
#'   'sample.raw'))
#' 
#' S <- readSpectrum(rawfile, scan = 1:9)
#' 
#' S[[1]]
#' 
#' names(S[[1]])
#' 
#' plot(S[[1]])
#' 
#'  
#' \dontrun{
#' # INPUT:
#' GAG <- "GAGSSEPVTGLDAK"
#' rawfile <- file.path(Sys.getenv("HOME"),
#'   "Downloads/20180220_14_autoQC01.raw")
#' 
#' # list spectra metainformation
#' S <- readIndex(rawfile)
#' 
#' # determine precursor matches
#' SS <- readSpectrum(rawfile,
#'   which(abs((1.008 + (protViz::parentIonMass(GAG) - 1.008) / 2) - S$precursorMass) < 0.001))
#' 
#' # query spectra with precursor matches
#' rv <-lapply(SS, function(x){protViz::psm(GAG, x, plot=FALSE)})
#' 
#' # determine spectra indices having the  max number of hits hits
#' hit.max <- max(hits <- sapply(rv, function(x){sum(abs(x$mZ.Da.error) < 0.01)}))
#' 
#' # take the 1st one
#' idx <- which(hits == hit.max)[1]
#' 
#' # OUTPUT
#' rv <- protViz::peakplot(GAG,  (SS[[idx]]), FUN=function(b,y){cbind(b=b, y=y)})
#' # https://www.proteomicsdb.org/use/
#' cat(paste(SS[[idx]]$mZ[rv$idx], "\t", SS[[idx]]$intensity[rv$idx]), sep = "\n")
#' }
readSpectrum <- function(rawfile, scan = NULL, tmpdir=tempdir(), validate=FALSE){
    mono <- if(Sys.info()['sysname'] %in% c("Darwin", "Linux")) TRUE else FALSE
    exe <- file.path(path.package(package = "rawR"), "exec", "rawR.exe")
    
    
    if (!file.exists(rawfile)){
        stop(paste0('file ', rawfile, ' is not available. return.'))
    }
    if (is.null(scan)){
        stop('no scan vector is proived.')
    }
    if (!.isMonoAssemblyWorking()){
        stop('the mono assembly are not available.')
    }
    
    tfi <- tempfile(tmpdir=tmpdir)
    tfo <- tempfile(tmpdir=tmpdir)
    tfstdout <- tempfile(tmpdir=tmpdir)
    
    cat(scan, file = tfi, sep="\n")
    
    cmd <- exe
    
    if (mono){
        rvs <- system2(Sys.which("mono"), args = c(shQuote(exe), shQuote(rawfile),
                                                   "scans", shQuote(tfi), shQuote(tfo)))
    }else{
        rvs <- system2(exe, args = c( shQuote(rawfile), "scans", shQuote(tfi),
                                      shQuote(tfo)))
    }
    
    e <- new.env()
    
    source(tfo, local=TRUE)
    unlink(c(tfi, tfo, tfstdout))
    
    
    rv <- lapply(e$Spectrum,
                 function(x){class(x) <- c('rawRspectrum'); x})
    if(validate){
        rv <- lapply(rv, validate_rawRspectrum)
    }
    
    rv
}

## test


#' Extracts Chromatogram (XIC)
#'
#' @param rawfile the file name. 
#' @param mass a vector of mass values. 
#' @param tol tolerance in ppm.
#' @param filter defines the scan filter, default is \code{filter="ms"} if a
#' wrong filter is set the function will return \code{NULL} and gives a warning.
#' @param mono if the mono enviroment should be used. 
#' @param exe the exe file user by mono.
#' 
#' @seealso Thermo Fisher NewRawfileReader C# code snippets
#' \url{https://planetorbitrap.com/rawfilereader}.
#' 
#' @return nested list of chromatogram objects.
#' 
#' @references \itemize{
#' \item{\url{https://doi.org/10.5281/zenodo.2640013}}
#' \item{the R function 1st appeared in
#' \url{https://doi.org/10.1021/acs.jproteome.8b00173}}
#' }
#' @author Christian Trachsel, Tobias Kockmann and
#' Christian Panse <cp@fgz.ethz.ch> 2018, 2019, 2020
#' @seealso \link[rawDiag]{readXIC}
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
#'   "GAGSSEPVTGLDAK", "TPVISGGPYEYR", "VEATFGVDESNAK",
#'   "TPVITGAPYEYR", "DGLDAASYYAPVR", "ADVTPADFSEWSK",
#'   "LFLQFGAQGSPFLK")
#' 
#' # [2H+] 
#' if (require(protViz)){
#'      (mZ <- (parentIonMass(iRTpeptide) + 1.008) / 2)
#'   }else{
#'      message("consider installing  https://CRAN.R-project.org/package=protViz")
#' }
#'
#' \dontrun{
#' # https://fgcz-ms.uzh.ch/p2692/Proteomics/QEXACTIVEHFX_1/tobiasko_20180220_scanSpeed/20180220_14_autoQC01.raw
#' # md5 = 00ffee77b82202200e5aec0522729f51
#' 
#' rawfile <- file.path(Sys.getenv('HOME'), "Downloads", "20180220_14_autoQC01.raw")
#' X <- readChromatogram(rawfile, mZ)
#' }
#' 
readChromatogram <- function(rawfile,
     mass = NULL,
     tol = 10,
     filter = "ms",
     mono = if(Sys.info()['sysname'] %in% c("Darwin", "Linux")) TRUE else FALSE,
     exe = file.path(path.package(package = "rawR"), "exec", "rawR.exe")){

    if (!file.exists(rawfile)){
        stop(paste0('file ', rawfile, ' is not available. return.'))
    }
    if (is.null(mass)){
        stop('no mass vector is proived.')
    }
    if (!.isMonoAssemblyWorking()){
        stop('the mono assembly are not available.')
    }

    tfi <- tempfile()
    tfo <- tempfile()
    tfstdout <- tempfile()
    
    cat(mass, file=tfi, sep="\n")
    
    cmd <- exe
    
    if (mono){
        rvs <- system2("mono", args = c(shQuote(exe), shQuote(rawfile), "xic",
                shQuote(tfi), tol, shQuote(tfo), shQuote(filter)))
    }else{
        rvs <- system2(exe, args = c(shQuote(rawfile), "xic", shQuote(tfi), tol,
                                     shQuote(tfo), shQuote(filter)))
    }
    
    
    rv <- try({
        e <- new.env();
        e$chromatogram <- list()
        source(tfo, local = TRUE)
        
        
        if ('warning' %in% names(e)){
            warning(e$warning)
        }
        
        if ('message' %in% names(e)){
            message(e$message)
        }
        
        if ('error' %in% names(e)){
            warning(e$error)
            return(NULL)
        } 
        
        e$chromatogram
    }, NULL)
    unlink(c(tfi, tfo, tfstdout))
    
    #message(paste(c(tfi, tfo, tfstdout), collapse = ",\n"))
    #message(length(rv))
    rv <- lapply(rv,
                 function(x){
                     x$filename <- rawfile
                     x$tol <- tol
                     x$filter <- filter
                     class(x) <- c(class(x), 'rawRchromatogram');
                     x})
    
    class(rv) <- 'rawRchromatogramSet'
    rv
}


#' Validate instance of class rawRSpectrum 
#'
#' @param x object to be tested
#'
#' @return \code{rawRspectrum} object
#' @export validate_rawRspectrum
validate_rawRspectrum <- function(x){
    values <- unclass(x)
    
    if (length(values$mZ) != length(values$intensity)){
        stop(
            "mZ should have same length as intensities.",
            call. = FALSE
        )
    }
    
    x
}

#' Basic plotting function for instances of \code{rawRspectrum}
#'
#' \code{plot.rawRspectrum} is a low level function that calls
#' \code{base::plot} for plotting \code{rawRspectrum} objects. It passes all
#' additional arguments to \code{plot()}
#'
#' @description Plot method for objects of class \code{rawRspectrum}.
#' @details Is usually called by method dispatch.
#'
#' @param x an object of class \code{rawRspectrum}.
#'
#' @param relative If set to \code{TRUE} enforces plotting of relative
#' intensities rather than absolute.
#' @param ... function passes arbitrary additional arguments.
#' @author Tobias Kockmann, 2020
#' @importFrom graphics legend
plot.rawRspectrum <- function(x, relative = FALSE, ...){
    stopifnot(is.rawRspectrum(x))
    plot(x = x$mZ, y = x$intensity, type = "h",
         xlab = "m/z",
         ylab = "Intensity",
         frame.plot = FALSE, ...)
    
    basePeak <- paste("(", paste(x$basePeak, collapse = ", "), ")", sep='')
    legend("topright",
           paste(c("Scan:", "Scan Type: ", "RT [s]:", "base peak:", "TIC:"),
                 c(x$scan, x$scanType, x$rtinseconds, basePeak, x$TIC)),
           bty = "n",
           cex=0.75)
}

#' Basic print function faking freestyle output
#' @author Tobias Kockmann, 2020
print.rawRspectrum <- function(x, ...){
    cat("Total Ion Current:\t", x$TIC, fill = TRUE)
    cat("Scan Low Mass:\t", x$massRange[1], fill = TRUE)
    cat("Scan High Mass:\t", x$massRange[2], fill = TRUE)
    cat("Scan Start Time (Min):\t", round(x$rtinseconds/60,2), fill = TRUE)
    cat("Scan Number:\t", x$scan, fill=TRUE)
    cat("Base Peak Intensity:\t", x$basePeak[2], fill = TRUE)	
    cat("Base Peak Mass:\t", x$basePeak[1], fill = TRUE)
    cat("Scan Mode:\t", x$scanType, fill = TRUE)	
}

