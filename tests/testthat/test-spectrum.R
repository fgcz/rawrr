#R

context("spectrum")

library(rawrr)

test_that("check readSpectrum object names and types.", {

  rawfile <- sampleFilePath()

  S <- readSpectrum(rawfile, 1:22)

  lapply(S, function(x){expect_true(is.rawrrSpectrum(x))})

})

test_that("check readSpectrum scan 23.", {

  rawfile <- sampleFilePath()
  S <- readSpectrum(rawfile, 23)[[1]]

  DF <- read.table(file.path(path.package(package = 'rawrr'), 'extdata', 'scan23_peakList.txt'), sep="\t", header=TRUE)

  expect_true(sum(S$mZ %in% DF$m.z) >= 720)
  expect_true(sum(S$intensity %in% DF$Intensity) >= 720)

  lapply(DF$m.z[DF$Flags == "F"] %in% S$mZ, expect_true)
  lapply(DF$m.z[DF$Flags == "M"] %in% S$mZ, expect_true)
  lapply(DF$m.z[DF$Flags == "E"] %in% S$mZ, expect_false)
})


test_that("check readSpectrum error.", {
  rawfile <- "this file does not exists"
  expect_error(S <- readSpectrum(rawfile))

  rawfile <- sampleFilePath()
  expect_error(S <- readSpectrum(rawfile))
  expect_error(S <- readSpectrum(rawfile, scan = NULL))
})


test_that("check rawrrSpectrum constructor", {
  expect_error(rawrrSpectrum(100))
  expect_error(rawrrSpectrum(""))
  expect_s3_class(rawrrSpectrum(), class = "rawrrSpectrum")
  expect_s3_class(rawrrSpectrum("TESTPEPTIDE"), class = "rawrrSpectrum")
  expect_s3_class(rawrrSpectrum("example_1"), class = "rawrrSpectrum")
})
