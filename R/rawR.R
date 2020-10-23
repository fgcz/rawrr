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


#' Extracts Chromatogram (XIC)
#'
#' @param rawfile the file name. 
#' @param mass a vector of mass values. 
#' @param tol tolerance in ppm.
#' @param filter defines the scan filter, default is \code{filter="ms"} if a
#' wrong filter is set the function will return \code{NULL} and gives a warning.
#' @param type xic for extracted ion chromatogram otherwise the function returns
#' a \code{data.frame} with total ion chromatogram and a base peak chromatogram.
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
#' @exportClass rawRchromatogram
#' @exportClass rawRchromatogramSet
#' @exportS3Method plot rawRchromatogram
#' @exportS3Method plot rawRchromatogramSet
#' @examples
#' 
#' # Example 1: not meaning full but proof-of-concept
#' (rawfile <- file.path(path.package(package = 'rawR'), 'extdata', 'sample.raw'))
#' 
#' C <- readChromatogram(rawfile, mass=c(669.8381, 726.8357), tol=1000)
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
                             type = 'xic',
                             mono = if(Sys.info()['sysname'] %in% c("Darwin", "Linux")) TRUE else FALSE,
                             exe = file.path(path.package(package = "rawR"), "exec", "rawR.exe")){
    
    if (!file.exists(rawfile)){
        stop(paste0('file ', rawfile, ' is not available. return.'))
    }
   
    if (!.isMonoAssemblyWorking()){
        stop('the mono assembly are not available.')
    }

    tfstdout <- tempfile()
    tfi <- tempfile()
    tfo <- tempfile()
    if(type == 'xic'){
        if (is.null(mass)){
            stop('no mass vector is proived.')
        }
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
        
        
        #message(paste(c(tfi, tfo, tfstdout), collapse = ",\n"))
        #message(length(rv))
        rv <- lapply(rv,
                     function(x){
                         x$type <- type
                         x$filename <- rawfile
                         x$tol <- tol
                         x$filter <- filter
                         # x$times.max <- x$times[which.max(x$intensities)][1]
                         class(x) <- c(class(x), 'rawRchromatogram');
                         x})
       
    }else{
        if (mono){
            rvs <- system2("mono", args = c(shQuote(exe), shQuote(rawfile), "chromatogram", shQuote(filter)), stdout=tfstdout)
        }else{
            rvs <- system2(exe, args = c(shQuote(rawfile), "chromatogram",  shQuote(filter)),stdout=tfstdout)
        }
        rv <- read.csv(tfstdout, header = TRUE, comment.char = "#")
    }
    unlink(c(tfi, tfo, tfstdout))
    class(rv) <- 'rawRchromatogramSet'
    rv
}


#' Create instances of class \code{rawRspectrum}
#' 
#' Developer function.
#'
#' @param scan scan number
#' @param massRange Mass range covered by spectrum
#' @param scanType Character string describing the scan type.
#' @param rtinseconds Retention time in seconds
#' @param centroidStream Logical indicating if centroided data is available
#' @param mZ m/z values
#' @param intensity Intensity values
#'
#' @return Object of class \code{rawRspectrum}
#' @export new_rawRspectrum
#'
#' @examples
new_rawRspectrum <- function(scan = numeric(), massRange = numeric(),
                             scanType = character(), rtinseconds = numeric(),
                             centroidStream = logical(),
                             mZ = numeric(), intensity = numeric()){
    
    stopifnot(is.numeric(scan), is.numeric(massRange), is.character(scanType),
              is.numeric(rtinseconds), is.logical(centroidStream),
              is.numeric(mZ), is.numeric(intensity)
    )
    
    structure(list(scan = scan,
                   basePeak = c(mZ[which.max(intensity)], intensity[which.max(intensity)]),
                   TIC = sum(intensity), massRange = massRange,
                   scanType = scanType, rtinseconds = rtinseconds,
                   centroidStream = centroidStream,
                   mZ = mZ, intensity = intensity),
              class = "rawRspectrum")

}

#' Create \code{rawRspectrum} objects
#' 
#' @description High-level constructor for instances of class
#' \code{rawRspectrum}, also named helper function. Currently, mainly to support
#' testing and for demonstration.
#' 
#' @usage \code{rawRspectrum(sim = "example_1")}
#'
#' @param sim Either \code{example_1} or \code{TESTPEPTIDE}
#'
#' @return Function returns a validated \code{rawRspectrum} object
#' @export rawRspectrum
#'
#' @examples \code{plot(rawRspectrum(sim = "TESTPEPTIDE"))}
#' \code{rawRspectrum(sim = "example_1")}
#' 
#' @author Tobias Kockmann
rawRspectrum <- function(sim = character()) {
    
    stopifnot(is.character(sim))
    
    if (sim == "example_1") {
        S <- new_rawRspectrum(scan = 1,
                              massRange = c(90, 1510),
                              rtinseconds = 1,
                              scanType = "simulated",
                              centroidStream = FALSE,
                              mZ = 1:15*100,
                              intensity = rep(100, times = 15)
                              )
    }
    
    if (sim == "TESTPEPTIDE") {
        S <- new_rawRspectrum(scan = 1,
                              massRange = c(90, 1510),
                              rtinseconds = 1,
                              scanType = "simulated",
                              centroidStream = FALSE,
                              mZ = c(148.0604, 263.0874, 376.1714, 477.2191,
                                     574.2719, 703.3145, 800.3672, 901.4149,
                                     988.4469, 1117.4895),
                              intensity = rep(100, times = 10)
        )
        
    }
    
    ## more examples?
    
    validate_rawRspectrum(S)
    
}

#' Validate instance of class rawRSpectrum 
#'
#' @description Checks the validity of \code{rawRspectrum} object attributes. 
#'
#' @param x object to be validated.
#'
#' @usage validate_rawRspectrum(x)
#' 
#' @return Validated \code{rawRspectrum} object
#' @export validate_rawRspectrum
validate_rawRspectrum <- function(x){
    values <- unclass(x)
    
    if (values$scan < 1) {
        stop("Scan values just be >= 1", call. = FALSE)
    }
    
    if (length(values$mZ) != length(values$intensity)){
        stop(
            "mZ should have same length as intensities.",
            call. = FALSE
        )
    }
    
    if (any(values$mZ < 0)) {
        stop("All mZ values just be greater than zero", call. = FALSE)
    }
    
    if (any(values$intensity < 0)) {
        stop("All intensity values just be greater than zero", call. = FALSE)
    }
    
    if (any(values$massRange < 0)) {
        stop("All massRange values just be greater than zero", call. = FALSE)
    }
    
    if (values$massRange[1] > values$massRange[2]) {
        stop("massRange[1] must be smaller than massRange[2].", call. = FALSE)
    }
    
    if (any(values$basePeak < 0)) {
        stop("All basePeak values must be greater than zero.", call. = FALSE)
    }
    
    ## still problems here: fails with sample data
    
    if (!values$basePeak[1] %in% values$mZ) {
        stop("basePeak[1] (position) must be found in mZ", call. = FALSE)
    }
    
    if (values$basePeak[2] != max(values$intensity)) {
        stop("basePeak intensity is unequal max. intensity", call. = FALSE)
    }
    
    ##
    
    if (values$rtinseconds < 0) {
        stop("rtinseconds must be greater than zero", call. = FALSE)
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
#' 
#' @param centroid Should centroided data be used for plotting?
#' 
#' @param SN Should Signal/Noise be used for plotting?
#' 
#' @param legend Should legend be printed?
#' 
#' @param ... function passes arbitrary additional arguments.
#' @author Tobias Kockmann, 2020
#' @importFrom graphics legend
plot.rawRspectrum <- function(x, relative = TRUE, centroid = FALSE, SN = FALSE,
                              legend = TRUE, diagnostic = FALSE, ...){
    
    stopifnot(is.rawRspectrum(x))
    
    if (centroid) {
        
        stopifnot(x$centroidStream)
        
        if (SN) {
            
            plot(x = x$centroid.mZ, y = x$centroid.intensity/x$noise,
                 type = "h",
                 xlim = x$massRange,
                 xlab = "Centroid m/z",
                 ylab = "Centroid Signal/Noise",
                 frame.plot = FALSE, ...
            )
            
        } else {
            
            plot(x = x$centroid.mZ, y = x$centroid.intensity,
                 type = "h",
                 xlim = x$massRange,
                 xlab = "Centroid m/z",
                 ylab = "Centroid Intensity",
                 frame.plot = FALSE, ...
            )
            
            i <- which.max(x$centroid.intensity)
            text(x = x$centroid.mZ[i], y = x$centroid.intensity[i], pos = 4,
                 labels = paste(format(x$centroid.mZ[i], nsmall = 4),
                                "\nz = ", x$charges[i], "\nR = "), cex = 0.75)
        
        }
        
        
    } else {
        
        if (relative) {
            
            plot(x = x$mZ, y = x$intensity/x$basePeak[2], type = "h",
                 xlim = x$massRange,
                 xlab = "m/z",
                 ylab = "Relative Intensity",
                 frame.plot = FALSE, ...)
            
        } else {
            
            plot(x = x$mZ, y = x$intensity, type = "h",
                 xlim = x$massRange,
                 xlab = "m/z",
                 ylab = "Intensity",
                 frame.plot = FALSE, ...)
            
        }
        
    }
    
    
    if (legend) {
        
        #basePeak <- paste("(", paste(format(x$basePeak, nsmall = 4),
        #                             collapse = ", "), ")", sep='')
        legend("topleft",
               paste(c("Scan#: ",
                       "Scan Type: ",
                       "RT [s]: ",
                       "Base peak mass [m/z]: ",
                       "Base peak intensity: ",
                       "TIC: "),
                     c(x$scan,
                       x$scanType,
                       x$rtinseconds,
                       format(x$basePeak[1], nnsmall = 4),
                       format(x$basePeak[2], scientific = TRUE),
                       format(x$TIC, scientific = TRUE))
                     ), bty = "n", cex=0.75)
        
    }
    
    if (diagnostic) {
        legend("left", legend = paste(c("Injection time [ms]: ",
                                          "Max. Injection time [ms]: ",
                                          "AGC target: ",
                                          "Resolution: "),
                                        c(x$`Ion Injection Time (ms)`,
                                          x$`Max. Ion Time (ms)`,
                                          x$`AGC Target`,
                                          x$`FT Resolution`)),
               bty = "n", cex = 0.75, text.col = "grey")
    }

}

#' Basic print function faking the look and feel of freestyle's output 
#' @author Christian Panse & Tobias Kockmann, 2020
print.rawRspectrum <- function(x, ...){
    cat("Total Ion Current:\t", x$TIC, fill = TRUE)
    cat("Scan Low Mass:\t", x$massRange[1], fill = TRUE)
    cat("Scan High Mass:\t", x$massRange[2], fill = TRUE)
    cat("Scan Start Time (Min):\t", round(x$rtinseconds/60,2), fill = TRUE)
    cat("Scan Number:\t", x$scan, fill=TRUE)
    cat("Base Peak Intensity:\t", x$basePeak[2], fill = TRUE)	
    cat("Base Peak Mass:\t", x$basePeak[1], fill = TRUE)
    cat("Scan Mode:\t", x$scanType, fill = TRUE)	

    keys <- c("======= Instrument data =====   :",
              "Multiple Injection:",
              "Multi Inject Info:",
              "AGC:",
              "Micro Scan Count:",
              "Scan Segment:",
              "Scan Event:",
              "Master Index:",
              "Charge State:",
              "Monoisotopic M/Z:",
              "Ion Injection Time (ms):",
              "Max. Ion Time (ms):",
              "FT Resolution:",
              "MS2 Isolation Width:",
              "MS2 Isolation Offset:",
              "AGC Target:",
              "HCD Energy:",
              "Analyzer Temperature:",
              "=== Mass Calibration:",
              "Conversion Parameter B:",
              "Conversion Parameter C:",
              "Temperature Comp. (ppm):",
              "RF Comp. (ppm):",
              "Space Charge Comp. (ppm):",
              "Resolution Comp. (ppm):",
              "Number of Lock Masses:",
              "Lock Mass #1 (m/z):",
              "Lock Mass #2 (m/z):",
              "Lock Mass #3 (m/z):",
              "LM Search Window (ppm):",
              "LM Search Window (mmu):",
              "Number of LM Found:",
              "Last Locking (sec):",
              "LM m/z-Correction (ppm):",
              "=== Ion Optics Settings:",
              "S-Lens RF Level:",
              "S-Lens Voltage (V):",
              "Skimmer Voltage (V):",
              "Inject Flatapole Offset (V):",
              "Bent Flatapole DC (V):",
              "MP2 and MP3 RF (V):",
              "Gate Lens Voltage (V):",
              "C-Trap RF (V):",
              "====  Diagnostic Data:",
              "Dynamic RT Shift (min):",
              "Intens Comp Factor:",
              "Res. Dep. Intens:",
              "CTCD NumF:",
              "CTCD Comp:",
              "CTCD ScScr:",
              "RawOvFtT:",
              "LC FWHM parameter:",
              "Rod:",
              "PS Inj. Time (ms):",
              "AGC PS Mode:",
              "AGC PS Diag:",
              "HCD Energy eV:",
              "AGC Fill:",
              "Injection t0:",
              "t0 FLP:",
              "Access Id:",
              "Analog Input 1 (V):",
              "Analog Input 2 (V):"
    )
    for (i in keys){
        value <- x[i]
       
        if (value == "NULL"){
            cat(i, "\t\n", fill = TRUE)
        }else{
            cat(paste(i, x[i],sep='\t'), fill = TRUE) 
        }
    }
}

#' Check if object is instance of class \code{rawRchromatogram}
#'
#' @param x The object to be tested.
#'
#' @usage is.rawRchromatogram(x)
#'
#' @return Boolean
#' @export is.rawRchromatogram
#'
#' @examples pathToRawFile <- file.path(path.package(package = 'rawR'), 'extdata', 'sample.raw')
#' C <- readChromatogram(pathToRawFile, mass = 445.1181, tol = 10)
#' is.rawRchromatogram(C[[1]])
is.rawRchromatogram <- function(x){
    "rawRchromatogram" %in% class(x)
}

#' Plot \code{rawRchromatogram} objects
#'
#' @param x A \code{rawRchromatogram} object to be plotted.
#' @param legend Should legend be printed?
#' @param ... Passes additional arguments.
#'
#' @return
#' @export plot.rawRchromatogram
#'
#' @examples pathToRawFile <- file.path(path.package(package = 'rawR'), 'extdata', 'sample.raw')
#' C <- readChromatogram(pathToRawFile, mass = 445.1181, tol = 10)
#' plot.rawR(C[[1]])
plot.rawRchromatogram <- function(x, legend = TRUE, ...){
    stopifnot(is.rawRchromatogram(x))
    
    plot(x = x$times, y = x$intensities,
         xlab = "RT",
         ylab = "Intensity",
         type = "l",
         frame.plot = FALSE)
    
    if (legend) {
        legend("topright",
               legend = paste(c("File: ", "Filter: ", "Mass: ", "Tolerance: "),
                              c(basename(x$filename), x$filter, format(x$mass),
                                x$ppm)
                        ),
               bty = "n", cex = 0.75)
    }
    
}

#' Plot \code{rawRchromatogramSet} objects
#'
#' @param x A \code{rawRchromatogramSet} object to be plotted.
#' @param ... Passes additional arguments.
#' 
#' @export plot.rawRchromatogramSet
plot.rawRchromatogramSet <- function(x, ...){
    plot(0, 0, type='n',
         xlim=range(unlist(lapply(x, function(o){o$times}))),
         ylim=range(unlist(lapply(x, function(o){o$intensities}))),
         frame.plot = FALSE,
         xlab='retention time [in min]',
         ylab='intensities', ...
    )
    
    cm <- hcl.colors(length(x), "Set 2")
    mapply(function(o, co){lines(o$times, o$intensities, col=co)}, x, cm)
    legend("topleft",
           as.character(sapply(x, function(o){o$mass})),
           col=cm,
           pch=16, 
           title='target mass [m/z]',
           bty='n',cex = 0.75)
}
