.monoInfo <-function(){
    # system2("mcs", "--version", stdout = TRUE)
    system2("mono", "-V", stdout = TRUE)
}

.checkReaderFunctions <- function(rawfile = sampleFilePath()){
  
  message("checkings rawrr::readFileHeader ...")
  start_time <- Sys.time()
  header <- rawrr::readFileHeader(rawfile)
  end_time <- Sys.time()
  msg <- sprintf("read %d header information in %s seconds", length(header), round(end_time - start_time, 3))
  message(msg)
  
  message("checking rawrr::readIndex ...")
  start_time <- Sys.time()
  idx <- rawrr::readIndex(rawfile)
  end_time <- Sys.time()
  msg <- sprintf("read %d rows of index in %s seconds", nrow(idx), round(end_time - start_time, 3))
  message(msg)

  
  message("checking  rawrr::readChromatogram(type='tic') ...")
  start_time <- Sys.time()
  ctic <- rawrr::readChromatogram(rawfile=rawfile, type="tic")
  end_time <- Sys.time()
  msg <- sprintf("read %d chromatographic (tic) items in %s seconds", length(ctic$times), round(end_time - start_time, 3))
  message(msg)
  
  message("checking rawrr::readChromatogram(type='xic') ...")
  start_time <- Sys.time()
  cxic <- rawrr::readChromatogram(rawfile=rawfile, type="xic", mass = 500)
  end_time <- Sys.time()
  msg <- sprintf("read %d chromatographic (xic) items in %s seconds", length(ctic$times), round(end_time - start_time, 3))
  message(msg)
  
  message("checking rawrr::readSpectrum ...")
  start_time <- Sys.time()
  spectra <- rawrr::readSpectrum(rawfile, 1:9)
  end_time <- Sys.time()
  msg <- sprintf("read %d spectra with %d peaks in %s seconds", length(spectra),
                 sum(vapply(spectra, function(x) length(x$mZ),0)),
                 round(end_time - start_time, 3))
  message(msg)
  
  stopifnot(
    length(header) >= 37,
    nrow(idx) > 0,
    length(ctic$times) > 0,
    length(cxic[[1]]$times) > 0,
    length(spectra[[1]]$mZ) > 0
  )
}

.checkRawFile <- function(rawfile){
  if (!file.exists(rawfile)){
    msg <- sprintf("File '%s' does not exist.", rawfile)
    stop(msg)
  }
  
  # TODO(cp) check if file name end with ".raw"
}

.writeRData <-
  function(rawfile, outputfile=paste0(rawfile, ".RData"), tmpdir=tempdir()){

    scanRange <- readFileHeader(rawfile)$`Scan range`
    scanIdx <- seq(scanRange[1], scanRange[2], by=1)

    res <- lapply(scanIdx, function(x){
        rv <- readSpectrum(rawfile, scan=x, tmpdir=tmpdir)[[1]]
        list(scanType=rv$scanType, mZ=rv$mZ, intensity=rv$intensity,
             charge=rv$charge, rtinseconds = 60 * rv$StartTime)
    })
    e <- new.env()
    objName <- paste("S",basename(rawfile), sep='')
    assign(objName, res, envir = e)

    save(objName, file=outputfile, envir = e)

  }

# system2 wrapper for readFileHeader, readSpectrum, readChromatogam
.rawrrSystem2Source <-
  function(rawfile, input, rawrrArgs="scans", tmpdir=tempdir(),
           removeTempfile=TRUE){
    
    mono <- if(Sys.info()['sysname'] %in% c("Darwin", "Linux")) TRUE else FALSE
    exe <- .rawrrAssembly()
    
    tfi <- tempfile(tmpdir=tmpdir, fileext = ".txt")
    tfo <- tempfile(tmpdir=tmpdir, fileext = ".R")
    tfstdout <- tempfile(tmpdir=tmpdir, fileext = ".stdout")
    tfstderr <- tempfile(tmpdir=tmpdir, fileext = ".stderr" )
    
    cat(input, file = tfi, sep="\n")
    if(isFALSE(file.exists(tfi))){
            stop(paste0("No input file '", tfi, "' available!"))
    }
    
    if (mono){
      if (system2(command = "/usr/bin/which", args = c("mono"),
            stderr = FALSE, stdout = FALSE) != 0){
        stop("mono is not available; please check https://www.mono-project.com/")
      }
      rvs <- system2(Sys.which("mono"), args = c(shQuote(exe),
                                                 shQuote(rawfile),
                                                 rawrrArgs, shQuote(tfi),
                                                 shQuote(tfo)),
                     stdout = tfstdout,
                     stderr = tfstderr)
    }else{
      rvs <- system2(exe, args = c( shQuote(rawfile),
                                    rawrrArgs, shQuote(tfi),
                                    shQuote(tfo)), )
    }
    
    if (isFALSE(file.exists(tfo))){
      errmsg <- sprintf("Rcode file to parse does not exist. '%s' failed for an unknown reason.
Please check the debug files:\n\t%s\n\t%s\nand the System Requirements",
                        .rawrrAssembly(),
                        tfstderr, tfstdout)
      stop(errmsg)
    }
    
    
    e <- new.env()
    try(source(tfo, local=TRUE), silent = TRUE)
    
    if (length(names(e)) == 0){
      errmsg <- sprintf("Parsing the output of '%s' failed for an unknown reason.
Please check the debug files:\n\t%s\n\t%s\nand the System Requirements",
                        .rawrrAssembly(),
                        tfstderr, tfstdout)
      stop(errmsg)
    }
    
    if ('error' %in% names(e)){
      stop(e$error)
    }
    
    if(isTRUE(removeTempfile)){
      unlink(c(tfi, tfo, tfstdout, tfstderr))
    }else{
      msg <- sprintf("input file: %s\noutput file: %s\n", tfi, tfo)
      message(msg)
    }
    return(e)
  }


#' Function to check if an object is an instance of class \code{rawrrSpectrum}
#'
#' @param x any R object to be tested.
#'
#' @return \code{TRUE} or \code{FALSE}
#' @export
#'
#' @examples
#' S <- rawrr::sampleFilePath() |> rawrr::readSpectrum(scan = 1:10)
#' rawrr::is.rawrrSpectrum(S[[1]])
is.rawrrSpectrum <- function(x){
       if (isFALSE(all(c('scan', 'massRange', 'scanType',
	       'StartTime', 'centroidStream', 'mZ',
	       'intensity') %in% names(x)))){
	       return (FALSE)
	       }

       all(c(is.numeric(x$scan),
         is.numeric(x$massRange),
	 is.character(x$scanType),
         is.numeric(x$StartTime),
	 is.logical(x$centroidStream),
         is.numeric(x$mZ),
	 is.numeric(x$intensity)))
}

#' Function to check if an object is an instance of class \code{rawrrSpectrumSet}
#'
#' @param x any R object to be tested.
#'
#' @return \code{TRUE} or \code{FALSE}
#' @export
#'
#' @examples
#' rawrr::sampleFilePath() |>
#'   rawrr::readSpectrum(scan = 1:10) |>
#'   rawrr::is.rawrrSpectrumSet()
is.rawrrSpectrumSet <- function(x){
	all(vapply(x, is.rawrrSpectrum, TRUE))
}

# readFileHeader ----------

#' read file header Information
#'
#' @param rawfile the name of the raw file containing the mass spectrometry data from the Thermo Fisher Scientific instrument.
#' @description This function extracts the meta information from a given raw file.
#' @author Tobias Kockmann and Christian Panse 2018, 2019, 2020.
#' @references Thermo Fisher Scientific's NewRawfileReader C# code snippets
#' \url{https://planetorbitrap.com/rawfilereader}.
#'
#'
#' @return A list object containing the following entries: RAW file version,
#' Creation date, Operator, Number of instruments, Description,
#' Instrument model, Instrument name, Serial number, Software version,
#' Firmware version, Units, Mass resolution, Number of scans,
#' Number of ms2 scans, Scan range, Time range, Mass range,
#' Scan filter (first scan), Scan filter (last scan), Total number of filters,
#' Sample name, Sample id, Sample type, Sample comment, Sample vial,
#' Sample volume, Sample injection volume, Sample row number,
#' Sample dilution factor, or Sample barcode.
#'
#' @export readFileHeader
#'
#' @examples
#' rawrr::sampleFilePath() |> readFileHeader()
readFileHeader <- function(rawfile){
  .isAssemblyWorking()
  rawfile <- normalizePath(rawfile)
  .checkRawFile(rawfile)

  e <- .rawrrSystem2Source(rawfile, input = NULL, rawrrArgs="headerR")
  e$info
}

# readIndex-----
#' Read scan index
#'
#' @inheritParams readFileHeader
#' 
#' @return returns a \code{data.frame} with the column names
#' scan, scanType, StartTime, precursorMass, MSOrder, charge, masterScan, and 
#' dependencyType of all spectra.
#' 
#' @export readIndex
#' @importFrom utils read.table
#' @author Tobias Kockmann and Christian Panse <cp@fgz.ethz.ch>, 2020, 2021
#'
#' @examples
#' Idx <- rawrr::sampleFilePath() |> rawrr::readIndex()
#' table(Idx$scanType)
#' plot(Idx$StartTime, Idx$precursorMass, col=as.factor(Idx$charge), pch=16)
#'
#' table(Idx$MSOrder)
readIndex <- function (rawfile) 
{
  .isAssemblyWorking()
  rawfile <- normalizePath(rawfile)
  .checkRawFile(rawfile)
  mono <- if (Sys.info()["sysname"] %in% c("Darwin", "Linux")) 
    TRUE
  else FALSE
  exe <- .rawrrAssembly()
  if (mono) {
    con <- textConnection(system2(Sys.which("mono"),
                                  args = c(shQuote(exe),
                                           shQuote(rawfile), "index"),
                                  stdout = TRUE))
  }
  else {
    con <- textConnection(system2(exe, args = c(shQuote(rawfile), 
                                                "index"), stdout = TRUE))
  }
  DF <- read.table(con, header = TRUE, comment.char = "#", sep = ";", na.strings = "-1",
                   colClasses = c("integer", "character", "numeric", "numeric", "character",
                                  "integer", "integer", "integer", "numeric"))
  DF$dependencyType <- as.logical(DF$dependencyType)
  DF
}


#' determine scan numbers which match a specified filter
#'
#' @inheritParams readSpectrum
#' @param filter scan filter string, e.g., \code{ms} or \code{ms2}
#' @param precision mass precision, default is 10.
#'
#' @return a vecntor of integer values.
filter <- function(rawfile, filter = "ms", precision = 10, tmpdir=tempdir()){
  .isAssemblyWorking()
  rawfile <- normalizePath(rawfile)
  .checkRawFile(rawfile)
  mono <- if(Sys.info()['sysname'] %in% c("Darwin", "Linux")) TRUE else FALSE
  exe <- .rawrrAssembly()
  
  tfstderr <- tempfile(tmpdir=tmpdir)
  tfo <- tempfile(tmpdir=tmpdir, fileext = ".txt")
  tfstdout <- tempfile(tmpdir=tmpdir)
  
  cmd <- exe
  
  if (mono){
    rvs <- system2(Sys.which("mono"),
                   args = c(shQuote(exe), shQuote(rawfile),
                            "filter", shQuote(filter), shQuote(precision),
                            shQuote(tfo)),
                   stderr = tfstderr,
                   stdout=tfstdout)
  }else{
    rvs <- system2(exe,
                   args = c( shQuote(rawfile), "filter", shQuote(filter),
                             shQuote(precision), shQuote(tfo)),
                   stderr = tfstderr,
                   stdout=tfstdout)
  }
  
  if (isFALSE(file.exists(tfo))){
    errmsg <- sprintf("Output file to read does not exist. '%s' failed for an unknown reason.
Please check the debug files:\n\t%s\n\t%s\nand the System Requirements",
                      .rawrrAssembly(),
                      tfstderr, tfstdout)
    stop(errmsg)
  }
  
  idx <- scan(tfo, what = numeric(), quiet = TRUE)
  unlink(c(tfo, tfstdout, tfstderr))
  idx
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
#' rawrr::sampleFilePath() |> rawrr::readIndex() |> rawrr::validate_rawrrIndex() 
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

    IndexColNames <- c("scan", "scanType", "StartTime", "precursorMass",
                       "MSOrder", "charge", "masterScan", "dependencyType")

    for (i in IndexColNames){
        if (!(i %in% colnames(x))){
	    msg <- sprintf("Missing column %d.", i)
            message(msg)
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

    if (!all(seq.int(from=1, to=nrow(x)), x$scan)){
        warning("Index does not corresbond to scan numbers. Missing scans?")
    }

    stopifnot(valideIndex)

    return(x)
}



#' A small file size {\code{sample.raw}} BLOB
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
    f <- file.path(system.file(package = 'rawrr'), 'extdata', 'sample.raw')
    stopifnot(file.exists(f))
    f
}

.download_20181113_010_autoQC01 <- function(){
    f <- "ftp://massive.ucsd.edu/MSV000086542/raw/20181113_010_autoQC01.raw"
    
    cachedir <- tools::R_user_dir("rawrr", which = "cache")
    rawfile <- file.path(cachedir, basename(f))
    if (isFALSE(dir.exists(cachedir))){
        dir.create(cachedir, recursive = TRUE)
    }
    if (isFALSE(file.exists(rawfile))){
        msg <- sprintf("Downloading file from '%s' ...", f)
        message(msg)
        download.file(f, rawfile)
    }else{
        msg <- sprintf("'20181113_010_autoQC01' already in '%s'.", cachedir)
        message(msg)
    }
    
    rawfile
}


# readSpectrum ---------
#' Reads spectral data from a raw file.
#' 
#' @description The function derives spectra of a given raw file and a given
#' vector of scan numbers.
#' 
#' @inheritParams readIndex
#' @param scan a vector of requested scan numbers.
#' @param validate boolean default is \code{FALSE}.
#' @param mode if \code{mode = "barebone"} only mZ (centroidStream.Masses),
#' intensity (centroidStream.Intensities), pepmass, StartTime
#' and charge state is returned. As default mode is \code{""}.
#' @param tmpdir defines the directory used to store temporary data generated by
#' the .NET assembly \code{rawrr.exe}. The default uses the output of
#' \code{tempdir()}.
#'
#' @author Tobias Kockmann and Christian Panse <cp@fgz.ethz.ch> 2018, 2019, 2020, 2021
#'
#' @details All mass spectra are recorded by scanning detectors (mass analyzers)
#' that log signal intensities for ranges of mass to charge ratios (m/z), also
#' referred to as position. These recordings can be of continuous nature,
#' so-called profile data (p), or appear centroided (c) in case discrete
#' information (tuples of position and intensity values) are sufficient.
#' This heavily compacted data structure is often called a peak list.
#' In addition to signal intensities, a peak list can also cover additional
#' peak attributes like peak resolution (R), charge (z), or local noise
#' estimates. In short, the additional attributes further described the nature
#' of the original profile signal or help to group peak lists with respect to
#' their molecular nature or processing history. A well-known example is the
#' assignment of peaks to peak groups that constitute isotope patterns
#' (M, M+1, M+2, ...).
#' The names of objects encapsulated within \code{rawrrSpectrum} instances are
#' keys returned by the Thermo Fisher Scientific New RawFileReader API and the
#' corresponding values become data parts of the objects, typically vectors.
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
#' @seealso \url{https://massive.ucsd.edu/ProteoSAFe/dataset.jsp?accession=MSV000086542}
#' @references \itemize{
#'   \item{C# code snippets of the NewRawfileReader library
#'     \url{https://planetorbitrap.com/rawfilereader}.}
#'     \item{rawrr: \doi{10.1021/acs.jproteome.0c00866}}
#'     \item{Universal Spectrum Explorer: \url{https://www.proteomicsdb.org/use/} \doi{10.1021/acs.jproteome.1c00096}}
#'   }
#' @examples
#' 
#' # Example 1
#' S <- rawrr::sampleFilePath() |> rawrr::readSpectrum(scan = 1:9)
#'
#' S[[1]]
#'
#' names(S[[1]])
#'
#' plot(S[[1]])
#' 
#' 
#' 
#' # Example 2 - find best peptide spectrum match using the |> pipe operator
#' # fetch via ExperimentHub
#' 
#' if (require(ExperimentHub) & require(protViz)){
#' eh <- ExperimentHub::ExperimentHub()
#' EH4547 <- normalizePath(eh[["EH4547"]])
#' 
#' (rawfile <- paste0(EH4547, ".raw"))
#' if (!file.exists(rawfile)){
#'     file.link(EH4547, rawfile)
#' }
#' 
#' GAG <- "GAGSSEPVTGLDAK"
#' 
#' .bestPeptideSpectrumMatch <- function(rawfile,
#'     sequence="GAGSSEPVTGLDAK"){
#'     readIndex(rawfile) |>
#'         subset(abs((1.008 + (protViz::parentIonMass(sequence) - 1.008) / 2) -
#'             precursorMass) < 0.001, select = scan) |>
#'         unlist() |>
#'         readSpectrum(rawfile = rawfile) |>
#'         lapply(function(x) {
#'           y <- protViz::psm(sequence = GAG, spec=x, plot=FALSE);
#'           y$scan <- x$scan; y
#'         }) |>
#'         lapply(FUN= function(x){
#'           score <- sum(abs(x$mZ.Da.error) < 0.01);
#'           cbind(scan=x$scan, score=score)
#'         }) |>
#'         (function(x) as.data.frame(Reduce(rbind, x)))() |>
#'         subset(score > 0) |>
#'         (function(x) x[order(x$score, decreasing = TRUE),
#'             'scan'])() |>
#'         head(1)
#' }
#' 
#' start_time <- Sys.time()
#' bestMatch <- .bestPeptideSpectrumMatch(rawfile, GAG) |>
#'     rawrr::readSpectrum(rawfile=rawfile) |>
#'     lapply(function(x) protViz::peakplot(peptideSequence = GAG, x))
#' 
#' end_time <- Sys.time()
#' end_time - start_time
#' 
#' # Example 3
#' # using proteomicsdb \doi{10.1101/2020.09.08.287557}
#' # through https://www.proteomicsdb.org/use/
#' 
#' .UniversalSpectrumExplorer <- function(x, sequence){
#'     m <- protViz::psm( sequence, x)
#'     cat(paste(x$mZ[m$idx], "\t", x$intensity[m$idx]), sep = "\n")
#' }
#' 
#' rawrr::readSpectrum(rawfile=rawfile, 11091) |>
#'    lapply(function(x).UniversalSpectrumExplorer(x, sequence = GAG))
#'  }  
readSpectrum <- function(rawfile, scan = NULL, tmpdir = tempdir(),
                         validate = FALSE, mode = ''){
  .isAssemblyWorking()
  rawfile <- normalizePath(rawfile)
  .checkRawFile(rawfile)
  
  if (is.null(scan)){
    stop('No scan vector is provided.')
  }
  
  if (mode == "barebone"){
    e <- .rawrrSystem2Source(rawfile, input = scan, rawrrArgs="cscans", tmpdir)
  }else{
    e <- .rawrrSystem2Source(rawfile, input = scan, rawrrArgs="scans", tmpdir)
  }
  
  rv <- lapply(e$Spectrum,
               function(x){

    if (length(x$mZ) < 1){
        warning(
            "length of mZ value vector is less than 1.",
            call. = FALSE
        )
	x$mZ <- 1:10
	x$intensity <- 1:10
    }
		       class(x) <- c('rawrrSpectrum'); x})
  if(validate){
    rv <- lapply(rv, validate_rawrrSpectrum)
  }else{
    if (mode != "barebone"){
      validate_rawrrSpectrum(rv[[1]])
    }
  }
  
  class(rv) <- 'rawrrSpectrumSet'
  rv
}

.readChromatogramTicBpc <- function(rawfile,
                                    filter = "ms", type='tic',
                                    tmpdir = tempdir()){
  
  mono <- if(Sys.info()['sysname'] %in% c("Darwin", "Linux")) TRUE else FALSE
  exe <- .rawrrAssembly()
  
  tfstdout <- tempfile(fileext = ".stdout", tmpdir = tmpdir)
  tfstderr <- tempfile(fileext = ".stderr", tmpdir = tmpdir)
  tfi <- tempfile(tmpdir = tmpdir)
  tfcsv <- tempfile(fileext = ".csv", tmpdir = tmpdir)
  
  system2args <- c(shQuote(rawfile), "chromatogram", shQuote(filter), tfcsv)
  
  if (mono){
    rvs <- system2("mono", args = c(shQuote(exe), system2args), stdout=tfstdout, stderr=tfstderr)
  }else{
    rvs <- system2(exe, args = system2args, stdout=tfstdout, stderr=tfstderr)
  }
  
  if (isFALSE(file.exists(tfcsv)))
  {
    errmsg <- sprintf("csv file to read does not exist. '%s' failed for an unknown reason.
Please check the debug files:\n\t%s\n\t%s\nand the System Requirements",
                      .rawrrAssembly(),
                      tfstderr , tfstdout)
    stop(errmsg)
  }
  
  # message(tfcsv)
  DF <- read.csv2(tfcsv, header = TRUE, comment.char = "#", sep = ';', na.string="-1")
  
  if (type == 'tic'){
    rv <- list(
      times=DF$rt,
      intensities=DF$intensity.TIC)
  }else{
    # expect bpc
    rv <- list(times=DF$rt,
               intensities=DF$intensity.BasePeak)
  }
  
  
  
  if(is.null(rv$times)){
    errmsg <- sprintf("'%s' failed for an unknown reason.
Please check the debug files:\n\t%s\n\t%s\nand the System Requirements",
                      .rawrrAssembly(),
                      tfstderr, tfstdout)
    stop(errmsg)
  }
  
  unlink(c(tfi, tfcsv, tfstdout, tfstderr))
  return(rv)
}

#' Validate output of the readChromatogram function
#' @noRd
#' @param x chromatogram object
#' @return Validated chromatogram object
## TODO
validate_rawrrChromatogram <- function(x){
  return(x)
}
  
# readChromatogram ---------
#' Extracts chromatographic data from a raw file.
#'
#' @inheritParams readSpectrum
#' @param mass a vector of mass values iff \code{type = 'xic'}.
#' @param tol mass tolerance in ppm iff \code{type = 'xic'}.
#' @param filter defines the scan filter, default is \code{filter="ms"} if a
#' wrong filter is set the function will return \code{NULL} and draws a warning.
#' @param type \code{c(xic, bpc, tic)} for extracted ion , base peak or
#' total ion chromatogram.
#' 
#' @return chromatogram object(s) containing of a vector of \code{times} and a
#' corresponding vector of \code{intensities}.
#' @details 
#' Chromatograms come in different flavors but are always signal intensity
#' values as a function of time. Signal intensities can be point estimates from
#' scanning detectors or plain intensities from non-scanning detectors, e.g.,
#' UV trace. Scanning detector (mass analyzers) point estimates can be defined
#' in different ways by, for instance, summing all signals of a given spectrum
#' (total ion chromatogram or TIC), or by extracting signal around an expected
#' value (extracted ion chromatogram = XIC), or by using the maximum signal
#' contained in a spectrum (base peak chromatogram = BPC). On top, chromatograms
#' can be computed from pre-filtered lists of scans. A total ion chromatogram
#' (TIC), for instance, is typically generated by iterating over all MS1-level
#' scans.
#'
#' @author Christian Trachsel, Tobias Kockmann and
#' Christian Panse <cp@fgz.ethz.ch> 2018, 2019, 2020.
#'
#' @seealso
#' \itemize{
#' \item{The Thermo Fisher Scientific RawFileReader C# code snippets
#' \url{https://planetorbitrap.com/rawfilereader}.}
#' \item{\url{https://CRAN.R-project.org/package=protViz}}
#' \item{\url{https://massive.ucsd.edu/ProteoSAFe/dataset.jsp?accession=MSV000086542}}
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
#' # Example 1: not meaningful but proof-of-concept
#' (rawfile <- rawrr::sampleFilePath())
#'
#' rawrr::readChromatogram(rawfile, mass=c(669.8381, 726.8357), tol=1000) |>
#'     plot()
#' rawrr::readChromatogram(rawfile, type='bpc') |> plot()
#' rawrr::readChromatogram(rawfile, type='tic') |> plot()
#'
#' # Example 2: extract iRT peptides
#'  if (require(ExperimentHub) & require(protViz)){
#' iRTpeptide <- c("LGGNEQVTR", "YILAGVENSK", "GTFIIDPGGVIR", "GTFIIDPAAVIR",
#'   "GAGSSEPVTGLDAK", "TPVISGGPYEYR", "VEATFGVDESNAK",
#'   "TPVITGAPYEYR", "DGLDAASYYAPVR", "ADVTPADFSEWSK",
#'   "LFLQFGAQGSPFLK")
#'
#'
#' # fetch via ExperimentHub
#' library(ExperimentHub)
#' eh <- ExperimentHub::ExperimentHub()
#' EH4547 <- normalizePath(eh[["EH4547"]])
#' 
#' (rawfile <- paste0(EH4547, ".raw"))
#' if (!file.exists(rawfile)){
#'     file.link(EH4547, rawfile)
#' }
#' op <- par(mfrow=c(2,1))
#' readChromatogram(rawfile, type='bpc') |> plot()
#' readChromatogram(rawfile, type='tic') |> plot()
#' par(op)
#' 
#' # derive [2H+] ions
#' ((protViz::parentIonMass(iRTpeptide) + 1.008) / 2) |>
#'    readChromatogram(rawfile=rawfile) |>
#'    plot()
#' }
readChromatogram <- function(rawfile,
                             mass = NULL,
                             tol = 10,
                             filter = "ms",
                             type = 'xic',
                             tmpdir = tempdir()){
    
    .isAssemblyWorking()
    rawfile <- normalizePath(rawfile)
    .checkRawFile(rawfile)
    
    stopifnot(type %in% c('xic', 'bpc', 'tic'))

    if(type == 'xic'){
        if (is.null(mass)){
            stop('No mass vector is provided.')
        }
        
        e <- .rawrrSystem2Source(rawfile, input = mass,
                    rawrrArgs = sprintf("xic %f %s", tol, shQuote(filter)),
                    tmpdir = tmpdir)
        
        rv <- lapply(e$chromatogram,
                     function(x){
                         attr(x , 'filename') <- rawfile
                         attr(x, 'type') <- 'xic'
                         class(x) <- 'rawrrChromatogram';
                         x})
        
        if(is.null(rv[[1]]$times)){
          errmsg <- c("The extraction of xic(s) failed for an unknown reason.",
                "\nPlease check the System Requirements.")
          stop(errmsg)
        }
    }else{
      rv <- .readChromatogramTicBpc(rawfile, filter, type)
    }
    
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
#' @param StartTime Retention time in minutes
#' @param centroidStream Logical indicating if centroided data is available
#' @param mZ m/z values
#' @param intensity Intensity values
#' @author Tobias Kockmann, 2020.
#' @return Object of class \code{rawrrSpectrum}
new_rawrrSpectrum <- function(scan = numeric(), massRange = numeric(),
                             scanType = character(), StartTime = numeric(),
                             centroidStream = logical(),
                             mZ = numeric(), intensity = numeric()){

    stopifnot(is.numeric(scan), is.numeric(massRange), is.character(scanType),
              is.numeric(StartTime), is.logical(centroidStream),
              is.numeric(mZ), is.numeric(intensity)
    )

    structure(list(scan = scan,
                   basePeak = c(mZ[which.max(intensity)], intensity[which.max(intensity)]),
                   TIC = sum(intensity), massRange = massRange,
                   scanType = scanType, StartTime = StartTime,
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
rawrrSpectrum <- function(sim = "TESTPEPTIDE") {
    stopifnot(is.character(sim), nchar(sim)>0)
    
    if (sim == "example_1") {
        S <- new_rawrrSpectrum(scan = 1,
                              massRange = c(90, 1510),
                              StartTime = 1,
                              scanType = "simulated",
                              centroidStream = FALSE,
                              mZ = seq_len(15) * 100,
                              intensity = rep(100, times = 15)
                              )
    }else if (sim == "TESTPEPTIDE") {
        S <- new_rawrrSpectrum(scan = 1,
                              massRange = c(90, 1510),
                              StartTime = 1,
                              scanType = "simulated",
                              centroidStream = FALSE,
                              mZ = c(148.0604, 263.0874, 376.1714, 477.2191,
                                     574.2719, 703.3145, 800.3672, 901.4149,
                                     988.4469, 1117.4895),
                              intensity = rep(100, times = 10)
        )
    }else{
        msg <- sprintf("'%s' is not a valid option.", sim)
        stop(msg)
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
#' @examples 
#' S <- rawrr::sampleFilePath() |>
#'   rawrr::readSpectrum(scan = 1:9, validate=TRUE)
#' @export
validate_rawrrSpectrum <- function(x){
    values <- unclass(x)

    if (values$scan < 1) {
        stop("Scan values just be >= 1", call. = FALSE)
    }

    if (length(values$mZ) < 1){
        stop(
            "length of mZ value vector is less than 1.",
            call. = FALSE
        )
	values$mZ <- 1:10
	values$intensity <- 1:10
    }
    
    if (length(values$mZ) != length(values$intensity)){
	msg <- paste0(
	    "The mZ and intensity vector must have the same length.",
	    "\nOn Windows, the decimal symbol has to be configured as a '.'!",
	    "\nIf you cannot resolve the problem, please contact the maintainer.")
        stop(msg, call. = FALSE)
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

    ## TODO: still problems here: fails with sample data
    #basePeakMz <- round(values$basePeak[1], 1)
    #if (isFALSE(basePeakMz %in% round(values$mZ, 1))) {
    #  warning(sprintf(
    #      "scan = %d, basePeak mZ = %f (position) must be found in mZ.",
    #                 values$scan, basePeakMz), call. = FALSE)
    #}

    #if (values$basePeak[2] != max(values$intensity)) {
    #    warning(sprintf(
    #        "scan = %d, basePeak intensity is unequal max. intensity.",
    #        values$scan), call. = FALSE)
    #}
    #
    ##

    if (values$StartTime < 0) {
        stop("StartTime (rt) must be greater than zero.", call. = FALSE)
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
#' @return This function creates a plot.
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
                i  <- order(x$centroid.intensity, decreasing = TRUE)[seq_len(n)]


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
                       "StartTime [min]: ",
                       "Base peak mass [m/z]: ",
                       "Base peak intensity: ",
                       "TIC: "),
                     c(x$scan,
                       x$scanType,
                       x$StartTime,
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
#' @return This function creates a print message.
#' @export
summary.rawrrSpectrum <- function(object, ...) {

    cat("Total Ion Current:\t", object$TIC, fill = TRUE)
    cat("Scan Low Mass:\t", object$massRange[1], fill = TRUE)
    cat("Scan High Mass:\t", object$massRange[2], fill = TRUE)
    cat("Scan Start Time (min):\t", object$StartTime, fill = TRUE)
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
#' @return This function creates a print message.
#' @export
print.rawrrSpectrum <- function(x, ...){
    cat("Total Ion Current:\t", x$TIC, fill = TRUE)
    cat("Scan Low Mass:\t", x$massRange[1], fill = TRUE)
    cat("Scan High Mass:\t", x$massRange[2], fill = TRUE)
    cat("Scan Start Time (min):\t", x$StartTime, fill = TRUE)
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

#' Function to check if an object is an instance of class \code{rawrrChromatogram}
#'
#' @param x x any R object to be tested.
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

    all(vapply(c("times", "intensities"),
           function(a){a %in% names(x)},
           TRUE))
}

#' Plot \code{rawrrChromatogram} objects
#'
#' @param x A \code{rawrrChromatogram} object to be plotted.
#' @param legend Should legend be printed?
#' @param ... Passes additional arguments.
#' @return This function creates a plot.
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
      cat("   RT :", min(object$times), "-", max(object$times), "min" , fill = TRUE)

    },
    "bpc" = {

      cat("   Filter :", attr(object, "filter"), sep = " ", fill = TRUE)
      cat("   RT :", min(object$times), "-", max(object$times), "min" , fill = TRUE)

    },
    "tic" = {

      cat("   Filter :", attr(object, "filter"), sep = " ", fill = TRUE)
      cat("   RT :", min(object$times), "-", max(object$times), "min" , fill = TRUE)

    },
  )

  invisible(object)

}

#' Plot \code{rawrrChromatogramSet} objects
#'
#' @param x A \code{rawrrChromatogramSet} object to be plotted.
#' @param ... Passes additional arguments.
#' @param diagnostic Show diagnostic legend?
#' @return This function creates a plot.
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
#' @examples 
#' rawrr::sampleFilePath() |>  rawrr::readIndex() |> rawrr::masterScan(scanNumber = 1)
masterScan <- function(x, scanNumber){
  stopifnot(is.data.frame(x), "masterScan" %in% colnames(x),
            "dependencyType" %in% colnames(x))
  if (is.na(x[scanNumber, "masterScan"])) {
    msg <- sprintf("Scan %d does NOT have a master scan! Returning NA. The corresponding dependencyType is %d.",
      scanNumber, x[scanNumber, "dependencyType"])
    warning(msg)
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
	msg <- sprintf("NO dependent scans found for scan %d!", scanNumber)
        warning(msg)
    }
    return(i)
}

## compute mass shift of a given file and mass tag
.computeMassShift <- function(f, mass = c(758.5694, 760.5851)){
  message("reading xic from file ", basename(f), " ...")
  xic_ <- rawrr::readChromatogram(f, mass = mass)
  
  message("reading index from file ", basename(f), "...")
  Idx <- rawrr::readIndex(f)
  Idx.Ms <- Idx[which(Idx$MSOrder=="Ms"), ]
  
  lapply(1:length(mass), function(ii){
    xic <- xic_[[ii]]
    idx <- which(xic$intensities == max(xic$intensities))
    if (length(idx) != 1) {
      warning("more than one hit for ", mass[ii], " in ", basename(f))
      return(NULL)
    }
    ## pick APEX and 5 left and right
    idxs <- seq(idx - 5, idx + 5)
    
    message("merging xic and index data.frames...")
    data.frame(StartTime=xic$times[idxs]) |> merge(Idx.Ms) -> qMs
    if (nrow(qMs) == 0) {
      warning("nothing to merge ", mass[ii], " in ", f)
      return(NULL)
    }
    
    message("reading ", nrow(qMs) , " spectra from file ", basename(f), "...")
    
    ## find nearest peak
    rawrr::readSpectrum(f, scan = qMs$scan) |>
      sapply(FUN = function(x){
        protViz::findNN(mass[ii], x$centroid.mZ) -> hit;
        x$centroid.mZ[hit]
      }) -> mzDiff
    
    ## compose output in long format
    rbind(
      data.frame(
        times = xic$times[idxs],
        value = xic$intensities[idxs],
        attribute = "intensities",
        file = basename(f),
        mass = mass[ii]
      ),
      data.frame(
        times = xic$times[idxs],
        value = (mzDiff - mass[ii]),
        attribute = "mzDiff",
        file = basename(f),
        mass = mass[ii]
      ))
  }) |>
    Reduce(f = rbind) 
}

# returns a sequence of indicies representing one peak area
.extractMaximumPeak <- function(y){
  # 1st max peak
  idx <- which(y == max(y))[1]
  d <- diff(y)
  # iterate left and right maximal nmax steps 
  # until intensity increase again
  for (l in rev(2:idx))
    if (d[l - 1] <= 0) break;
  for (r in idx:(length(y)-1))
    if (d[r + 1] >= 0) break;
  seq(l, r)
}

## https://cran.r-project.org/src/contrib/Archive/deisotoper/
#' @importFrom stats predict lm
.fitChromatographicPeak <- function(x, y){
  peak <- data.frame(logy = log(y + 1), x = x)
  x.mean <- mean(peak$x)
  peak$xc <- peak$x - x.mean
  weights <- y^2
  fit <- lm(logy ~ xc + I(xc^2), data = peak, weights = weights)
  x0 <- -fit$coefficients[2] / (2 * fit$coefficients[3])
  xx <- with(peak, seq(min(xc)-0.15, max(xc)+0.15, length = 200))
  yp <- exp(predict(fit, data.frame(xc = xx)))
  data.frame(xx=xx + x.mean, yp=yp)
}



#=======pickPeak========
pickPeak.rawrrChromatogram <- function(x){
  lapply(x, function(y){
    idx <- .extractMaximumPeak(y$intensities)
    rv <- y
    rv$times <- y$times[idx]
    rv$intensities <- y$intensities[idx]
    rv
  })
}

#=======fitPeak========
fitPeak.rawrrChromatogram <- function(x){
  lapply(x, function(y){
    fittedPeak <- .fitChromatographicPeak(y$times, y$intensities)
    rv <- y
    rv$xx <- fittedPeak$xx
    rv$yp <- fittedPeak$yp
    rv
  })
}

#=======AUC========
#' deriving area under the curve (AUC)
#'
#' @param x an rawrrChromatogram object contains \code{x$times} and
#' \code{x$intensities}. \code{x$times} is assumed to be in minutes.
#'
#' @return A numeric value.
#' @importFrom utils head tail
auc.rawrrChromatogram <- function(x){
	times <- 60 * x$times
	intensities <- x$intensities
	sum(diff(times) * (head(intensities, -1) + tail(intensities, -1))) / 2
}

# readTrailer ---------
#' Read and extract scan trailer from TFS raw files.
#'
#' @inheritParams readSpectrum
#' @param label if NULL; the function scans for all available labels.
#'
#' @return an vector of trailers or values of a given trailer. Of note,
#' the values are usually returned as a character.
#'
#' @export
#'
#' @examples
#' rawrr::sampleFilePath() |> rawrr:::readTrailer()
#' rawrr::sampleFilePath() |> rawrr:::readTrailer("AGC:") |> head()
readTrailer <- function(rawfile, label = NULL) {
  .isAssemblyWorking()
  rawfile <- normalizePath(rawfile)
  .checkRawFile(rawfile)
  
  mono <- if(Sys.info()['sysname'] %in% c("Darwin", "Linux")) TRUE else FALSE
  exe <- .rawrrAssembly()
  
  cmd <- exe
  
  if (is.null(label)){
    # should return all available trailer label
    if (mono){
      con <- textConnection(system2(Sys.which("mono"),
                                    args = c(shQuote(exe), shQuote(rawfile), "trailer"),
                                    stdout = TRUE))
    }else{
      con <- textConnection(system2(exe,
                                    args = c( shQuote(rawfile), "trailer"),
                                    stdout = TRUE))
    }
  }else{
    # use case for providing a trailer label
    if (mono){
      con <- textConnection(system2(Sys.which("mono"),
                                    args = c(shQuote(exe), shQuote(rawfile),
                                             "trailer",  shQuote(label)),
                                    stdout = TRUE))
    }else{
      con <- textConnection(system2(exe,
                                    args = c(shQuote(rawfile), "trailer", shQuote(label)),
                                    stdout = TRUE))
    }
  }
  
  scan(con, what=character(),
       sep = "\n",
       quiet = TRUE,
       blank.lines.skip = FALSE)
}

#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

