#R

context("header")

library(rawR)


test_that("check readFileHeader", {

  rawfile <- file.path(path.package(package = 'rawR'), 'extdata', 'sample.raw')


  M <- readFileHeader(rawfile)


  expect_identical(M$`Scan range`, c(1, 574))
  expect_vector(M$`Scan range`)
  expect_equal(M$`Scan range`, c(1, 574))
})

test_that("check readSpectrum 23.", {

  rawfile <- file.path(path.package(package = 'rawR'), 'extdata', 'sample.raw')
  S <- readSpectrum(rawfile, 23)[[1]]

  DF <- read.table(file.path(path.package(package = 'rawR'), 'extdata', 'scan23_peakList.txt'), sep="\t", header=TRUE)

  expect_true(sum(S$mZ %in% DF$m.z) >= 720)
  expect_true(sum(S$intensity %in% DF$Intensity) >= 720)

  lapply(DF$m.z[DF$Flags == "F"] %in% S$mZ, expect_true)
  lapply(DF$m.z[DF$Flags == "M"] %in% S$mZ, expect_true)
  lapply(DF$m.z[DF$Flags == "E"] %in% S$mZ, expect_false)
})

