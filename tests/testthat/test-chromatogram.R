#R

context("chromatogram")

library(rawrr)


test_that("check readChromatogram", {

  rawfile <- sampleFilePath()
  
  expect_error(X <- readChromatogram(rawfile, mass=c(669.8381, 726.8357),
                                     filter="mXs"))
  
  X <- readChromatogram(rawfile, mass=c(669.8381, 726.8357), tol=1000)

  expect_s3_class(X, "rawrrChromatogramSet")
  
  expect_equal(length(X), 2)

  x <- X[[1]]
  lapply(c("mass", "times", "intensities", "filter", "ppm") %in% names(x),
           expect_true)

  expect_true(is.rawrrChromatogram(x))

  expect_error(X <- readChromatogram(rawfile, mass=c(669.8381, 726.8357),
                                       tol=1000, filter = "ms 3"))

  expect_error(X <- readChromatogram(rawfile, mass=c(669.8381, 726.8357),
                                       tol=1000, filter = "ms ms"))
  

  TIC <- readChromatogram(rawfile, type='tic')
  expect_true(is.rawrrChromatogram(TIC))

  BPC <- readChromatogram(rawfile, type='bpc')
  expect_true(is.rawrrChromatogram(BPC))
})


test_that("check readChromatogram error.", {
  rawfile <- "this file does not exists"
  expect_error(S <- readChromatogram(rawfile))

  rawfile <- sampleFilePath()
  expect_error(S <- readChromatogram(rawfile))
  expect_error(S <- readChromatogram(rawfile, mass = NULL))
})
