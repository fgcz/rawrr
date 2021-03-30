.monoInfo <-function(){
    # system2("mcs", "--version", stdout = TRUE)
    system2("mono", "-V", stdout = TRUE)
}


.writeRData <-
  function(rawfile, outputfile=paste0(rawfile, ".RData"), tmpdir=tempdir()){

    scanRange <- readFileHeader(rawfile)$`Scan range`
    scanIdx <- seq(scanRange[1], scanRange[2], by=1)

    res <- lapply(scanIdx, function(x){
        rv <- readSpectrum(rawfile, scan=x, tmpdir=tmpdir)[[1]]
        list(scanType=rv$scanType, mZ=rv$mZ, intensity=rv$intensity,
             charge=rv$charge, rtinseconds=rv$rtinseconds)
    })
    e <- new.env()
    objName <- paste("S",basename(rawfile), sep='')
    assign(objName, res, envir = e)

    save(objName, file=outputfile, envir = e)

}

.isMonoAssemblyWorking <-
  function(exe = .rawrrAssembly()){
    if(Sys.info()['sysname'] %in% c("Darwin", "Linux")){
      if (Sys.which('mono') == ""){
        warning("Cannot find the Mono JIT compiler. Check system requirements.")
        return(FALSE)
      }
    }

    if (!file.exists(exe)){
      warning("rawrr.exe is not available.")
      return (FALSE)
    }


    if (!file.exists(.rawrrAssembly())){
      warning("'rawrr.exe' not found.\n",
       "Run 'installRawrrExecuatable()'.",
       "For more information, type '?ThermoFisher'.")
      return (FALSE)
    }

    # execute Assembly
    rvs <-  "?"
    if (Sys.info()['sysname'] %in% c("Darwin", "Linux")){
      rvs <- system2(Sys.which('mono'), args = c(shQuote(exe)),
                     stdout = TRUE)
    }else{
      rvs <- system2(exe, stdout = TRUE)
    }

    # expect that string
    if (rvs != "No RAW file specified!"){
      warning("Mono JIT compiler and rawrr.exe assembly are not working.")
      return (FALSE)
    }
    TRUE
  }

#' Check if object is instance of class \code{rawrrSpectrum}
#'
#' @param x object to be tested.
#'
#' @return \code{TRUE} or \code{FALSE}
#' @export
#'
#' @examples
#' rawfile <- sampleFilePath()
#' S <- readSpectrum(rawfile, scan = 1:10)
#' is.rawrrSpectrum(S[[1]])
is.rawrrSpectrum <- function(x){
    class(x) == "rawrrSpectrum"
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
#' @description The function extracts meta information from a given rawfile.
#' @author Tobias Kockmann and Christian Panse 2018, 2019, 2020.
#' @references Thermo Fisher Scientific's NewRawfileReader C# code snippets
#' \url{https://planetorbitrap.com/rawfilereader}.
#'
#'
#' @return a list object containing the following entries: RAW file version,
#' Creation date, Operator, Number of instruments, Description,
#' Instrument model, Instrument name, Serial number, Software version,
#' Firmware version, Units, Mass resolution, Number of scans,
#' Number of ms2 scans, Scan range, Time range, Mass range,
#' Scan filter (first scan), Scan filter (last scan), Total number of filters,
#' Sample name, Sample id, Sample type, Sample comment, Sample vial,
#' Sample volume, Sample injection volume, Sample row number,
#' Sample dilution factor, or Sample barcode.
#'
#' @export
#'
#' @examples
#' (rawfile <- file.path(path.package(package = 'rawrr'), 'extdata',
#'   'sample.raw'))
#'
#' M <- readFileHeader(rawfile)
readFileHeader <- function(rawfile,
   mono = if(Sys.info()['sysname'] %in% c("Darwin", "Linux")) TRUE else FALSE,
   exe = .rawrrAssembly(),
   mono_path = "",
   argv = "infoR",
   system2_call = TRUE,
                           method = "thermo"){
    if(isFALSE(.checkRawfileReaderDLLs())){return(FALSE)}
    if(interactive()){ stopifnot(.isRawFileReaderLicenseAccepted()) }
    rawfile <- normalizePath(rawfile)

    if (!file.exists(rawfile)){
        stop(paste0('File ', rawfile, ' is not available. return.'))
    }
    if (!.isMonoAssemblyWorking()){
        stop('The mono assemblies are not available.')
    }
    if(system2_call && method == 'thermo'){

        tf <- tempfile(fileext = '.R')
        tf.err <- tempfile(fileext = '.err')

        # message(paste("system2 is writting to tempfile ", tf, "..."))

        if (mono){
            rvs <- system2(Sys.which("mono"), args = c(shQuote(exe), shQuote(rawfile), shQuote(argv)),
            stdout = tf)
        }else{
            rvs <- system2(shQuote(exe), args = c(shQuote(rawfile), shQuote(argv)),
            stderr = tf.err,
            stdout = tf)
        }

        if (rvs == 0){

            ## Replace backslashes in Instrument method file path to ensure
            ## the R file can be parsed
            r_file <- readLines(tf)
            r_file[12] <- gsub('\\\\','/',r_file[12])
            writeLines(r_file, tf)

            rv <- try({
                e <- new.env();
                e$info <- list()
                source(tf, local=TRUE)

                ## Keep only the file name for the Instrument method
                e$info$`Instrument method` <- basename(e$info$`Instrument method`)

                #message(paste("unlinking", tf, "..."))
                unlink(tf)
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
#' @importFrom utils read.table
#' @author Tobias Kockmann and Christian Panse <cp@fgz.ethz.ch>, 2020
#'
#' @examples
#' rawfile <- sampleFilePath()
#'
#' Idx <- readIndex(rawfile)
#' table(Idx$scanType)
#' plot(Idx$rtinseconds, Idx$precursorMass, col=as.factor(Idx$charge), pch=16)
#'
#' table(Idx$MSOrder)
#'
#'
#' # given you have a raw file with depende
#'
readIndex <- function(rawfile, tmpdir=tempdir()){
    if(interactive()){ stopifnot(.isRawFileReaderLicenseAccepted()) }

    mono <- if(Sys.info()['sysname'] %in% c("Darwin", "Linux")) TRUE else FALSE
    exe <- .rawrrAssembly()
    if(isFALSE(.checkRawfileReaderDLLs())){return(FALSE)}
    if(interactive()){ stopifnot(.isRawFileReaderLicenseAccepted()) }

    rawfile <- normalizePath(rawfile)

    if (!file.exists(rawfile)){
        stop(paste0('File ', rawfile, ' is not available.'))
    }

    if (!.isMonoAssemblyWorking()){
        stop('The mono assembly are not available.')
    }

    tfi <- tempfile(tmpdir=tmpdir)
    tfo <- tempfile(tmpdir=tmpdir)
    tfstdout <- tempfile(tmpdir=tmpdir)

    cmd <- exe

    if (mono){
        rvs <- system2(Sys.which("mono"),
                       args = c(shQuote(exe), shQuote(rawfile),
                                "index", shQuote(tfstdout)),
                       stdout=tfstdout)
    }else{
        rvs <- system2(exe,
                       args = c( shQuote(rawfile), "index", shQuote(tfstdout)),
                       stdout=tfstdout)
    }

    DF <- read.table(tfstdout,
      header = TRUE,
      comment.char = "#",
      sep = ';',
      na.strings = "-1",
      colClasses = c('integer', 'character', 'numeric', 'numeric', 'character',
                     'integer', 'integer', 'integer'))

    DF$dependencyType <- as.logical(DF$dependencyType)

    unlink(c(tfi, tfo, tfstdout))
    DF
}

#' Validate output of the readIndex function
#'
#' @description Checks the validity of an \code{readIndex} returned object.
#'
#' @param x object to be validated.
#'
#' @usage validate_rawrrIndex(x)
#' @author Tobias Kockmann and Christian Panse, 2020-12-09.
#' @return Validated \code{data.frame} of \code{readIndex} object
#' @export
#' @examples
#' Idx <- readIndex(sampleFilePath())
#' validate_rawrrIndex(Idx)
#' @importFrom stats na.omit
validate_rawrrIndex <- function(x){
    valideIndex <- TRUE

    if (!is.data.frame(x)){
        message("Object is not a 'data.frame'.")
        valideIndex <- FALSE
    }

    if(ncol(x) < 8){
        message("Object has incorrect number of columns.")
        valideIndex <- FALSE
    }

    IndexColNames <- c("scan", "scanType", "rtinseconds", "precursorMass",
                       "MSOrder", "charge", "masterScan", "dependencyType")

    for (i in IndexColNames){
        if (!(i %in% colnames(x))){
            message(paste0("Missing column ", i, "."))
            valideIndex <- FALSE
        }
    }

    if (!is.integer(x$scan)){
        message("Column 'scan' is not an integer.")
        valideIndex <- FALSE
    }


    if (!all(na.omit(x$masterScan) %in% x$scan)){
        message("Master scan not in scan index.")
        valideIndex <- FALSE
    }

    if (!is.logical(x$dependencyType))
    {
        message("'dependencyType' is not logical.")
        valideIndex <- FALSE
    }

    if(!all(x$dependencyType[!is.na(x$masterScan)])){
        message("'dependencyType' violates master scan logic.")
        valideIndex <- FALSE
    }

    if (!all(1:nrow(x), x$scan)){
        warning("Index does not corresbind to scan number. Missing scans?")
    }

    stopifnot(valideIndex)

    return(x)
}

#' {\code{sample.raw}}
#'
#' @description
#' The binary example file sample.raw, shipped with the package, contains
#' 574 Fourier-transformed Orbitrap
#' spectra (FTMS) recorded on a Thermo Fisher Scientific Q Exactive HF-X. The
#' mass spectrometer was operated in line with a nano electrospray source (NSI)
#' in positive mode (+). All spectra were written to disk after applying
#' centroiding (c) and lock mass correction.
#'
#' @details Thermo Fisher Scientific Q Exactive HF-X raw file
#' of size 1.5M bytes and checksum
#' \code{MD5 (sample.raw) = fe67058456c79af7442316c474d20e96}.
#' Additional raw data for
#' demonstration and extended testing is available through
#' \href{https://massive.ucsd.edu/ProteoSAFe/dataset.jsp?accession=MSV000086542}{MSV000086542}
#' and the
#' \href{https://bioconductor.org/packages/tartare/}{tartare} package.
#' \strong{Lions love raw meat!}
#'
#' @references
#' \itemize{
#' \item{Bioconductor
#' \href{https://bioconductor.org/packages/tartare/}{tartare} package.}
#' \item{Automated quality control sample 1 (autoQC01) analyzed across different
#' Thermo Fisher Scientific mass spectrometers,
#' \href{https://massive.ucsd.edu/ProteoSAFe/dataset.jsp?accession=MSV000086542}{MSV000086542}.}
#' }
#'
#'
#' @return file path of the sample.raw location.
#' @export
#' @aliases sample.raw
#' @author Tobias Kockmann, 2018, 2019.
#' @examples
#' sampleFilePath()
sampleFilePath <- function(){
    # path.package(package = 'rawrr')
    f <- file.path(system.file(package = 'rawrr'), 'extdata', 'sample.raw')
    stopifnot(file.exists(f))
    f
}

#' Read a Set of Spectra
#'
#' @param rawfile the name of the Thermo Fisher Scietific raw file.
#' @param scan a vector of requested scan numbers.
#' @param tmpdir a non-empty character vector giving the directory name;
#' default uses \code{tempdir()}.
#' @param validate boolean default is \code{FALSE}.
#' @author Tobias Kockmann and Christian Panse <cp@fgz.ethz.ch> 2018, 2019, 2020
#'
#' @description the function derives spectra of a given rawfile and a given
#' vector of scan numbers.
#'
#' @aliases readSpectrum rawrr
#'
#' @export
## #' @export readSpectrum
## #' @exportClass rawrrSpectrum
## #' @exportS3Method plot rawrrSpectrum
## #' @exportS3Method print rawrrSpectrum
## #' @exportS3Method summary rawrrSpectrum
#'
#' @return a nested list of \code{rawrrSpectrum} objects containing more than 50
#' values of scan information, e.g., the charge state, two vectors containing
#' the mZ and its corresponding intensity values or the AGC information,
#' mass calibration, ion optics \ldots
#'
#' @examples
#' (rawfile <- sampleFilePath())
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
#' IDX <- readIndex(rawfile)
#'
#' # determine precursor matches
#' S <- readSpectrum(rawfile,
#'   which(abs((1.008 + (protViz::parentIonMass(GAG) - 1.008) / 2) - IDX$precursorMass) < 0.001))
#'
#' # query spectra with precursor matches
#' rv <-lapply(S, function(x){protViz::psm(GAG, x, plot=FALSE)})
#'
#' # determine spectra indices having the  max number of hits hits
#' hit.max <- max(hits <- sapply(rv, function(x){sum(abs(x$mZ.Da.error) < 0.01)}))
#'
#' # take the 1st one
#' idx <- which(hits == hit.max)[1]
#'
#' # OUTPUT
#' rv <- protViz::peakplot(GAG,  (S[[idx]]), FUN=function(b,y){cbind(b=b, y=y)})
#' # https://www.proteomicsdb.org/use/
#' cat(paste(S[[idx]]$mZ[rv$idx], "\t", S[[idx]]$intensity[rv$idx]), sep = "\n")
#' }
#' @references \itemize{
#'   \item{Thermo Fisher NewRawfileReader C# code snippets
#'     \url{https://planetorbitrap.com/rawfilereader}}.
#'   \item{\url{https://doi.org/10.5281/zenodo.2640013}}
#'   \item{the R function 1st appeared in
#'     \url{https://doi.org/10.1021/acs.jproteome.8b00173}.
#'   }
#' }
readSpectrum <- function(rawfile, scan = NULL, tmpdir=tempdir(), validate=FALSE){
    if(interactive()){ stopifnot(.isRawFileReaderLicenseAccepted()) }
    mono <- if(Sys.info()['sysname'] %in% c("Darwin", "Linux")) TRUE else FALSE
    exe <- .rawrrAssembly()
    if(isFALSE(.checkRawfileReaderDLLs())){return(FALSE)}
    if(interactive()){ stopifnot(.isRawFileReaderLicenseAccepted()) }

    if (!file.exists(rawfile)){
        stop(paste0('File ', rawfile, ' is not available.'))
    }
    if (is.null(scan)){
        stop('No scan vector is provided.')
    }
    if (!.isMonoAssemblyWorking()){
        stop('The mono assemblies are not available.')
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
                 function(x){class(x) <- c('rawrrSpectrum'); x})
    if(validate){
        rv <- lapply(rv, validate_rawrrSpectrum)
    }

    class(rv) <- 'rawrrSpectrumSet'
    rv
}


#' Extracts Chromatograms
#'
#' @param rawfile the file name.
#' @param mass a vector of mass values iff \code{type = 'xic'}.
#' @param tol mass tolerance in ppm iff \code{type = 'xic'}.
#' @param filter defines the scan filter, default is \code{filter="ms"} if a
#' wrong filter is set the function will return \code{NULL} and draws a warning.
#' @param type \code{c(xic, bpc, tic)} for extracted ion , base peak or
#' total ion chromatogram.
#' @param mono if the mono enviroment should be used.
#' @param exe the exe file user by mono.
#'
#' @return chromatogram object(s) containing of a vector of \code{times} and a
#' corresponding vector of \code{intensities}.

#'
#' @author Christian Trachsel, Tobias Kockmann and
#' Christian Panse <cp@fgz.ethz.ch> 2018, 2019, 2020.
#'
#' @seealso
#' \itemize{
#' \item{Thermo Fisher NewRawfileReader C# code snippets
#' \url{https://planetorbitrap.com/rawfilereader}.}
#' \item{\url{https://CRAN.R-project.org/package=protViz}}
#' }
#'
#' @references Automated quality control sample 1 (autoQC01) analyzed across different
#' Thermo Scientific mass spectrometers,
#' \href{https://massive.ucsd.edu/ProteoSAFe/dataset.jsp?accession=MSV000086542}{MSV000086542}.
#'
#' @export
## #' @export readChromatogram
## #' @exportClass rawrrChromatogram
## #' @exportClass rawrrChromatogramSet
## #' @exportS3Method plot rawrrChromatogram
## #' @exportS3Method plot rawrrChromatogramSet
#' @importFrom utils read.csv2
#' @examples
#'
#' # Example 1: not meaning full but proof-of-concept
#' (rawfile <- sampleFilePath())
#'
#' XIC <- readChromatogram(rawfile, mass=c(669.8381, 726.8357), tol=1000)
#' plot(XIC)
#'
#' BPC <- readChromatogram(rawfile, type='bpc')
#' plot(BPC)
#'
#' TIC <- readChromatogram(rawfile, type='tic')
#' plot(TIC)
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
#' # MSV000086542
#' # MD5 (20181113_010_autoQC01.raw) = a1f5df9627cf9e0d51ec1906776957ab
#'
#' rawfile <- file.path(Sys.getenv('HOME'), "Downloads",
#'   "20181113_010_autoQC01.raw")
#'
#' X <- readChromatogram(rawfile, mZ)
#' }
#'
readChromatogram <- function(rawfile,
                             mass = NULL,
                             tol = 10,
                             filter = "ms",
                             type = 'xic',
                             mono = if(Sys.info()['sysname'] %in% c("Darwin", "Linux")) TRUE else FALSE,
                             exe = .rawrrAssembly()){

    if(isFALSE(.checkRawfileReaderDLLs())){return(FALSE)}
    if(interactive()){ stopifnot(.isRawFileReaderLicenseAccepted()) }

    if (!file.exists(rawfile)){
        stop(paste0('File ', rawfile, ' is not available.'))
    }

    if (!.isMonoAssemblyWorking()){
        stop('The mono assemblies are not available.')
    }

    tfstdout <- tempfile()
    tfi <- tempfile()
    tfo <- tempfile()
    if(type == 'xic'){
        if (is.null(mass)){
            stop('No mass vector is provided.')
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
                         attr(x , 'filename') <- rawfile
                         attr(x, 'type') <- 'xic'
                         class(x) <- 'rawrrChromatogram';
                         x})

    }else{
        if (mono){
            rvs <- system2("mono", args = c(shQuote(exe), shQuote(rawfile), "chromatogram", shQuote(filter)), stdout=tfstdout)
        }else{
            rvs <- system2(exe, args = c(shQuote(rawfile), "chromatogram",  shQuote(filter)), stdout=tfstdout)
        }
        DF <- read.csv2(tfstdout, header = TRUE, comment.char = "#", sep = ';', na.string="-1")


        if (type == 'tic'){
            rv <- list(
                times=DF$rt,
                intensities=DF$intensity.TIC)
        }else{
            # expect bpc
            rv <- list(times=DF$rt,
                       intensities=DF$intensity.BasePeak)
        }
    }
    unlink(c(tfi, tfo, tfstdout))

    attr(rv, 'filter') <- filter
    attr(rv, 'filename') <- rawfile

    if (type=='xic'){
        attr(rv, 'type') <- 'xic'
        attr(rv, 'tol') <- tol
        class(rv) <- 'rawrrChromatogramSet'
    }else if (type=='tic'){
        attr(rv, 'type') <- 'tic'
        class(rv) <- 'rawrrChromatogram'
    }else{
        attr(rv, 'type') <- 'bpc'
        class(rv) <- 'rawrrChromatogram'
    }

    rv
}


#' Create instances of class \code{rawrrSpectrum}
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
#' @author Tobias Kockmann, 2020.
#' @return Object of class \code{rawrrSpectrum}
new_rawrrSpectrum <- function(scan = numeric(), massRange = numeric(),
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
              class = "rawrrSpectrum")

}

#' Create \code{rawrrSpectrum} objects
#'
#' @description High-level constructor for instances of class
#' \code{rawrrSpectrum}, also named helper function. Currently, mainly to support
#' testing and for demonstration.
#'
#'
#' @param sim Either \code{example_1} or \code{TESTPEPTIDE}
#'
#' @return Function returns a validated \code{rawrrSpectrum} object
#' @export
#'
#' @examples plot(rawrrSpectrum(sim = "TESTPEPTIDE"))
#' rawrrSpectrum(sim = "example_1")
#'
#' @author Tobias Kockmann, 2020.
rawrrSpectrum <- function(sim = character()) {

    stopifnot(is.character(sim))

    if (sim == "example_1") {
        S <- new_rawrrSpectrum(scan = 1,
                              massRange = c(90, 1510),
                              rtinseconds = 1,
                              scanType = "simulated",
                              centroidStream = FALSE,
                              mZ = 1:15*100,
                              intensity = rep(100, times = 15)
                              )
    }

    if (sim == "TESTPEPTIDE") {
        S <- new_rawrrSpectrum(scan = 1,
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

    validate_rawrrSpectrum(S)

}

#' Validate instance of class rawrrSpectrum
#'
#' @description Checks the validity of \code{rawrrSpectrum} object attributes.
#'
#' @param x object to be validated.
#'
#' @usage validate_rawrrSpectrum(x)
#' @author Tobias Kockmann and Christian Panse, 2020.
#' @return Validated \code{rawrrSpectrum} object
#' @export
validate_rawrrSpectrum <- function(x){
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
        stop("All mZ values just be greater than zero.", call. = FALSE)
    }

    if (any(values$intensity < 0)) {
        stop("All intensity values just be greater than zero.", call. = FALSE)
    }

    if (any(values$massRange < 0)) {
        stop("All massRange values just be greater than zero.", call. = FALSE)
    }

    if (values$massRange[1] > values$massRange[2]) {
        stop("massRange[1] must be smaller than massRange[2].", call. = FALSE)
    }

    if (any(values$basePeak < 0)) {
        stop("All basePeak values must be greater than zero.", call. = FALSE)
    }

    ## still problems here: fails with sample data

    if (!values$basePeak[1] %in% values$mZ) {
        stop("basePeak[1] (position) must be found in mZ.", call. = FALSE)
    }

    if (values$basePeak[2] != max(values$intensity)) {
        stop("basePeak intensity is unequal max. intensity.", call. = FALSE)
    }

    ##

    if (values$rtinseconds < 0) {
        stop("rtinseconds must be greater than zero.", call. = FALSE)
    }

    x
}

#' Basic plotting function for instances of \code{rawrrSpectrum}
#'
#' \code{plot.rawrrSpectrum} is a low level function that calls
#' \code{base::plot} for plotting \code{rawrrSpectrum} objects. It passes all
#' additional arguments to \code{plot()}
#'
#' @description Plot method for objects of class \code{rawrrSpectrum}.
#' @details Is usually called by method dispatch.
#'
#' @param x an object of class \code{rawrrSpectrum}.
#'
#' @param relative If set to \code{TRUE} enforces plotting of relative
#' intensities rather than absolute.
#'
#' @param centroid Should centroided data be used for plotting?
#'
#' @param SN Should Signal/Noise be used for plotting?
#'
#' @param legend Should legend be printed?
#' @param diagnostic Should this option be applied? The default is \code{FALSE}.
#' @param ... function passes arbitrary additional arguments.
#' @export
#' @author Tobias Kockmann, 2020.
#' @importFrom graphics legend
plot.rawrrSpectrum <- function(x, relative = TRUE, centroid = FALSE, SN = FALSE,
                              legend = TRUE, diagnostic = FALSE, ...){

    stopifnot(is.rawrrSpectrum(x))

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
                 ylim = c(0, 1.2 *  max(x$centroid.intensity)),
                 xlab = "Centroid m/z",
                 ylab = "Centroid Intensity",
                 frame.plot = FALSE, ...
            )
            if (all(c('charges', 'resolutions') %in% names(x))){
                # ylim = c(0, 1.1*max(x$ x$centroid.intensity))
                # TODO(cp): label top 10 with z=, R=
                n <- length(x$centroid.intensity)
                if (n > 10) n <- 10
                i  <- order(x$centroid.intensity, decreasing = TRUE)[1:n]


                text(x = x$centroid.mZ[i],
                     y = x$centroid.intensity[i],
                     pos = 3,
                     labels = paste(format(x$centroid.mZ[i], nsmall = 4),
                                    "\nz = ", x$charges[i], "\nR = ",
                                    x$resolutions[i]),
                     cex = 0.5)
            }

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
                     ),
               bty = "n",
               cex=0.5)

    }

  if (diagnostic) {
    legend("left", legend = paste(c("Injection time [ms]: ",
                                    "Max. Injection time [ms]: ",
                                    "AGC target: ",
                                    "Resolution: "),
                                  c(x$`Ion Injection Time (ms)`,
                                    x$`Max. Ion Time (ms)`,
                                    format(x$`AGC Target`, scientific = TRUE),
                                    format(x$`FT Resolution`, scientific = TRUE))),
           bty = "n", cex = 0.5, text.col = "grey")
  }

  invisible(x)

}

#' Basic summary function
#' @author Christian Panse and Tobias Kockmann, 2020.
#' @param object an \code{rawrrSpectrum} object.
#' @param \ldots Arguments to be passed to methods.
#' @export
summary.rawrrSpectrum <- function(object, ...) {

    cat("Total Ion Current:\t", object$TIC, fill = TRUE)
    cat("Scan Low Mass:\t", object$massRange[1], fill = TRUE)
    cat("Scan High Mass:\t", object$massRange[2], fill = TRUE)
    cat("Scan Start Time (Min):\t", round(object$rtinseconds/60,2), fill = TRUE)
    cat("Scan Number:\t", object$scan, fill=TRUE)
    cat("Base Peak Intensity:\t", object$basePeak[2], fill = TRUE)
    cat("Base Peak Mass:\t", object$basePeak[1], fill = TRUE)
    cat("Scan Mode:\t", object$scanType, fill = TRUE)

    invisible(object)

}

#' Print method imitate the look and feel of Thermo Fisher Scientific FreeStyle's output
#' @author Christian Panse and Tobias Kockmann, 2020.
#' @param x an \code{rawrrSpectrum} object.
#' @param \ldots Arguments to be passed to methods.
#' @export
print.rawrrSpectrum <- function(x, ...){
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

        if (value == "NULL" && substr(i, 1, 2) != "=="){
            #cat(i, "\t\n", fill = TRUE)
        }else{
            cat(paste(i, x[i],sep='\t'), fill = TRUE)
        }
    }

    invisible(x)

}

#' Check if object is instance of class \code{rawrrChromatogram}
#'
#' @param x The object to be tested.
#'
#' @usage is.rawrrChromatogram(x)
#' @author Tobias Kockmann, 2020.
#'
#' @return \code{TRUE} or \code{FALSE}
#' @export
#'
#' @examples rawfile <- sampleFilePath()
#' C <- readChromatogram(rawfile, mass = 445.1181, tol = 10)
#' is.rawrrChromatogram(C[[1]])
is.rawrrChromatogram <- function(x){
    "rawrrChromatogram" %in% class(x)
}

#' Plot \code{rawrrChromatogram} objects
#'
#' @param x A \code{rawrrChromatogram} object to be plotted.
#' @param legend Should legend be printed?
#' @param ... Passes additional arguments.
#' @author Tobias Kockmann, 2020.
#' @export
#'
#' @examples rawfile <- sampleFilePath()
#' C <- readChromatogram(rawfile, mass = 445.1181, tol = 10)
#' plot(C[[1]])
plot.rawrrChromatogram <- function(x, legend = TRUE, ...){
    stopifnot(is.rawrrChromatogram(x))

    plot(x = x$times, y = x$intensities,
         xlab = "Retention Time [min]",
         ylab = "Intensity",
         type = "l",
         frame.plot = FALSE)


    if (legend) {
        if(attr(x, 'type') == 'xic'){
            legend("topleft",
                   legend = paste(c("File: ", "Filter: ", "Mass: ", "Tolerance: "),
                                  c(basename(attr(x, 'filename')),
                                    x$filter,
                                    x$mass,
                                    x$ppm)
                   ),
                   bty = "n",
                   title = toupper(attr(x, 'type')),
                   cex = 0.75)
        }else{
            legend("topleft",
                   legend = paste(c("File:"), c(basename(attr(x, 'filename')))),
                   bty = "n",
                   title = toupper(attr(x, 'type')),
                   cex = 0.75)
        }
    }

    invisible(x)

}

#' Text summary of chromatogram
#'
#' @param object A \code{rawrrChromatogram} object
#' @param ... Function passes additional arguments.
#' @return A \code{rawrrChromatogram} object
#' @export
#'
#' @examples C <- readChromatogram(rawfile = sampleFilePath(),
#' mass = c(445.1181, 519.1367))
#' summary(C[[1]])
#' summary(C[[2]])
summary.rawrrChromatogram <- function(object, ...) {

  stopifnot(is.rawrrChromatogram(object))

  cat(toupper(attr(object, "type")), "generated from", basename(attr(object, "filename")),
      "consisting of", length(object$times), "data points.", fill = TRUE)

  switch (attr(object, "type"),
    "xic" = {

      cat("   Filter :", object$filter, sep = " ", fill = TRUE)
      cat("   m/z :", object$mass, sep = " ", fill = TRUE)
      cat("   Tolerance :", object$ppm, "ppm", sep = " ", fill = TRUE)
      cat("   RT :", min(object$times), "-", max(object$times), "s" , fill = TRUE)

    },
    "bpc" = {

      cat("   Filter :", attr(object, "filter"), sep = " ", fill = TRUE)
      cat("   RT :", min(object$times), "-", max(object$times), "s" , fill = TRUE)

    },
    "tic" = {

      cat("   Filter :", attr(object, "filter"), sep = " ", fill = TRUE)
      cat("   RT :", min(object$times), "-", max(object$times), "s" , fill = TRUE)

    },
  )

  invisible(object)

}

#' Plot \code{rawrrChromatogramSet} objects
#'
#' @param x A \code{rawrrChromatogramSet} object to be plotted.
#' @param ... Passes additional arguments.
#' @param diagnostic Show diagnostic legend?
#' @author Tobias Kockmann, 2020.
#' @export
#' @importFrom grDevices hcl.colors
#' @importFrom graphics lines text
plot.rawrrChromatogramSet <- function(x, diagnostic = FALSE, ...){

    #should become is.Class function in the future
    stopifnot(attr(x, "class") == "rawrrChromatogramSet")

    ## so far only XIC branch available

    if (attr(x, 'type') == 'xic') {
        plot(0, 0, type='n',
             xlim=range(unlist(lapply(x, function(o){o$times}))),
             ylim=range(unlist(lapply(x, function(o){o$intensities}))),
             frame.plot = FALSE,
             xlab='Retention Time [min]',
             ylab='Intensities', ...
        )

        cm <- hcl.colors(length(x), "Set 2")
        mapply(function(o, co){lines(o$times, o$intensities, col=co)}, x, cm)
        legend("topleft",
               as.character(sapply(x, function(o){o$mass})),
               col=cm,
               pch=16,
               title='target mass [m/z]',
               bty='n', cex = 0.75)

        if (diagnostic) {
            legend("topright", legend = paste(c("File: ",
                                                "Filter: ",
                                                "Type: ",
                                                "Tolerance: "
                                          ),
                                          c(basename(attr(x, "file")),
                                            attr(x, "filter"),
                                            attr(x, "type"),
                                            attr(x, "tol")
                                          )
                                    ),
                   bty = "n", cex = 0.75, text.col = "black")
        }
    }

  invisible(x)

}

#' Retrieve master scan of scan listed in scan index
#'
#' @param x A scan index returned by \code{readIndex}.
#' @param scanNumber The scan number that should be inspected for the presence
#' of a master scan.
#'
#' @return Returns the scan number of the master scan or NA if no master scan
#' exists.
#' @export
#'
#' @examples Idx <- readIndex(rawfile = sampleFilePath())
#' masterScan(Idx, scanNumber = 1)
masterScan <- function(x, scanNumber){
  stopifnot(is.data.frame(x), "masterScan" %in% colnames(x),
            "dependencyType" %in% colnames(x))
  if (is.na(x[scanNumber, "masterScan"])) {
    warning(paste("Scan", scanNumber, "does NOT have a master scan! Returning NA.",
                  "The corresponding dependencyType is",
                  x[scanNumber, "dependencyType"]))
  }
  x[scanNumber, "masterScan"]
}


#' Retrieve dependent scan(s) of a scan listed in scan index
#'
#' @param x A scan index returned by \code{readIndex}.
#' @param scanNumber The scan number that should be inspected for dependent scans.
#'
#' @return The scan number of the dependent scan(s).
#' @export
#'
#' @examples Idx <- readIndex(rawfile = sampleFilePath())
#' dependentScan(Idx, scanNumber = 1)
dependentScan <- function(x, scanNumber){
    stopifnot(is.data.frame(x), "masterScan" %in% colnames(x), "scan" %in% colnames(x))
    i <- x[which(x$masterScan == scanNumber), "scan"]
    if (length(i) == 0) {
        warning(paste("NO dependent scans found for scan", scanNumber, "!"))
    }
    return(i)
}


#' Example snippet for deriving an AUC
#'
#' @param x an rawrrChromatogram
#'
#' @return a numeric value
#' @importFrom utils head tail
auc.rawrrChromatogram <- function(x){
	times <- x$times; intensities <- x$intensities
	sum(diff(times) * (head(intensities, -1) + tail(intensities, -1))) / 2
}
