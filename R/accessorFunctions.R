#R
## This file contains accessor functions for the class rawrrSpectrum
## These functions are used to encapsulate data retrival, since (1) key:value
## pairs returned by the RawFileReader API can have cryptic keys including white
## spaces and special characters, (2) returned values are mostly transfered as
## is without casting to specific types, (3) it
## is not always obvious what these key:value pairs mean due to a missing
## documentation from Thermo Fisher Scientific.

#' Make accessor function for key value pair returned by RawFileReader
#'
#' @param key An object name found in instance of class \code{rawrrSpectrum}
#' @param returnType The type used for casting of values
#'
#' @return An accessor function
#' @export
#'
#' @details This function factory creates accessor functions for class
#' \code{rawrrSpectrum}.
#' @author Tobias Kockmann, 2020
#'
#' @examples
#' S <- readSpectrum(rawfile = sampleFilePath(), 1:10)
#' maxIonTime <- makeAccessor(key = "Max. Ion Time (ms):", returnType = "double")
#' maxIonTime(S[[1]])
makeAccessor <- function(key, returnType = "integer"){

  function(x) {

    stopifnot(is.rawrrSpectrum(x))
    if (key %in% names(x)) {

      cl <- call(paste0("as.", returnType), x[[key]])
      eval(cl)

    } else {

      stop(paste0(key, " is not available!"))
    }
  }
}

#' Accessor function for scan number of \code{rawrrSpectrum} objects
#'
#' @param x A rawrrSpectrum object
#'
#' @return The scan number of type \code{integer}
#' @export
#' @details This accessor function returns the scan number of a mass spectrum
#' stored as \code{rawrrSpectrum} object. Scan numbers are equal to the scan index
#' \eqn{j} running from 1 to \eqn{n} with \eqn{n} being the last scan of a raw file.
#'
#' @examples S <- readSpectrum(rawfile = sampleFilePath(), 1:10)
#' scanNumber(S[[1]])
#' class(S[[1]]$scan) != class(scanNumber(S[[1]]))
scanNumber <- function(x) {
  stopifnot(is.rawrrSpectrum(x))
  as.integer(x$scan)
}

#' Base peak of a spectrum
#'
#' @param x A rawrrSpectrum object
#'
#' @return A double vector of length two. The first component is the base peak
#' position (m/z). The second component is the base peak intensity.
#' @export
#'
#' @examples S <- readSpectrum(rawfile = sampleFilePath(), 1)
#' basePeak(S[[1]])
basePeak <- makeAccessor(key = "basePeak", "double")

#' Total ion current of a spectrum
#'
#' @param x A rawrrSpectrum object
#'
#' @return A double vector of length one.
#' @export
#'
#' @examples S <- readSpectrum(rawfile = sampleFilePath(), 1)
#' tic(S[[1]])
tic <- makeAccessor(key = "TIC", "double")

#' Acquisition/scan range of spectrum
#'
#' @param x A rawrrSpectrum object
#'
#' @return A double vector of length two. The first component is the start m/z, the
#' second is the stop m/z value used by the detector during data acquisition.
#' Also referred to as scan range.
#' @export
#'
#' @examples S <- readSpectrum(rawfile = sampleFilePath(), 1)
#' massRange(S[[1]])
massRange <- makeAccessor(key = "massRange", "double")


#' Is FAIMS Voltage on?
#'
#' @param x A rawrrSpectrum object
#'
#' @return A boolean
#' @export
#'
#' @examples S <- readSpectrum(rawfile = sampleFilePath(), 1:10)
#' try(faimsVoltageOn(S[[1]]))
faimsVoltageOn <- function(x) {
  stopifnot(is.rawrrSpectrum(x))
  if ("FAIMS Voltage On:" %in% names(x)) {
    y <- x$`FAIMS Voltage On:`
    lookup <- c(No = "FALSE", Yes = "TRUE")
    return(as.logical(lookup[y]))
  } else {
    stop("FAIMS Voltage On: is not available!")
  }
}

## application of accessor functions for tabulation of scan data

tabulateSpectrum <- function (x, accNames) {

  stopifnot(is.rawrrSpectrum(x), is.character(accNames))

  l <- vector("list", length(accNames))

  for (i in seq_along(accNames)) {

    l[[i]] <- do.call(accNames[i], list(x))

  }

  df <- list2DF(l)
  names(df) <- accNames
  return(df)

}

tabulateSpectrumSet <- function (x, accNames){

  #if (!requireNamespace("purrr", quietly = TRUE)) {
  #  stop("Package \"pkg\" needed for this function to work. Please install it.",
  #       call. = FALSE)
  #}

  stopifnot(class(x) == "rawrrSpectrumSet")

  lapply(x, tabulateSpectrum, accNames)
  #purrr::map_df(x, tabulateSpectrum, accNames)

}
