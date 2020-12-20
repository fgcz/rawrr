## This file contains accessor functions for the class rawrrSpectrum
## These functions are used to encapsulate data retrival, since (1) key:value
## pairs returned by the RawFileReader API can have cryptic keys including white
## spaces and special characters, (2) returned values are mostly transfered as-is
## without casting to specific types, (3) it
## is not always obvious what these key:value pairs mean due to a missing
## documentation from Thermo Fisher Scientific.

#' Make accessor function for key value pair returned by RawFileReader
#'
#' @param key An object name found in instance of class \code{rawrrSpectrum}
#' @param returnType The type used for casting of values
#'
#' @return An accessor function
#' @export makeAccessor
#'
#' @details This function factory creates accessor functions for class \code{rawrrSpectrum}.
#'
#'
#' @examples S <- readSpectrum(rawfile = sampleFilePath(), 1:10)
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
#' @export scanNumber
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

#' Is FAIMS Voltage on?
#'
#' @param x A rawrrSpectrum object
#'
#' @return A boolean
#' @export faimsVoltageOn
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
