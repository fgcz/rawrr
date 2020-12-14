## This file contains accessor functions for the class rawrrSpectrum
## These functions are used to encapsulate data retrival, since (1) key:value
## pairs returned by the RawFileReader API can have cryptic keys including white
## spaces and special characters, (2) returned values are mostly transfered as-is
## without casting to specific primitive types (numeric, integer, ...), (3) it
## is not always obvious what these key:value pairs mean due to a missing
## documentation from Thermo Fisher Scientific.

#' Prototype accessor function for  \code{rawrrSpectrum} objects
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
