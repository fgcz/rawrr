#R

context("chromatogram")

library(rawR)


test_that("check readChromatogram", {

  rawfile <- file.path(path.package(package = 'rawR'), 'extdata', 'sample.raw')
  X <- readChromatogram(rawfile, mass=c(669.8381, 726.8357), tol=1000)
  

  lapply(X, function(x)(
    lapply(c("mass", "times", "intensities", "filename", "tol") %in% names(x),
           expect_true)))
})


