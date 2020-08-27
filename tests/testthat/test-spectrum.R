#R

context("spectrum")

library(rawR)

.is.peaklist <- function(x){
	sum(c("scan", "scanType", "rtinseconds", "pepmass", "charge", "mZ", "intensity") %in% names(x))>6
}

test_that("check readSpectrum function.", {

  rawfile <- file.path(path.package(package = 'rawR'), 'extdata', 'sample.raw')

  S <- readSpectrum(rawfile, 1:22)

  lapply(S, function(x){expect_true(.is.peaklist(x))})

})
