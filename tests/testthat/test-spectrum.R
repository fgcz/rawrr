#R

context("spectrum")

library(rawrr)

test_that("check readSpectrum object names and types.", {

  S <- rawrr::sampleFilePath() |>
    rawrr::readSpectrum(1:22, validate = TRUE)

  lapply(S, function(x){ expect_true(rawrr::is.rawrrSpectrum(x)) })

  B <- rawrr::sampleFilePath() |>
    rawrr::readSpectrum(1:22, mode = 'barebone')
  
  expect_true(all(sapply(B, function(x){'scan' %in% names(x)})))
  expect_true(all(sapply(B, function(x){'mZ' %in% names(x)})))
  expect_true(all(sapply(B, function(x){'intensity' %in% names(x)})))
  expect_true(all(sapply(B, function(x){'charge' %in% names(x)})))
  expect_true(all(sapply(B, function(x){'rtinseconds' %in% names(x)})))
  
  expect_false(all(sapply(B, function(x){'TIC' %in% names(x)})))

  expect_setequal(unlist(sapply(1:22, function(i){B[[i]]$mZ})),
                  unlist(sapply(1:22, function(i){S[[i]]$mZ})))
  
  expect_setequal(unlist(sapply(1:22, function(i){S[[i]]$intensity})),
                  unlist(sapply(1:22, function(i){B[[i]]$intensity})))
  
  expect_setequal(unlist(sapply(1:22, function(i){B[[i]]$charge})),
                  unlist(sapply(1:22, function(i){S[[i]]$charge})))
  
  expect_setequal(unlist(sapply(1:22, function(i){B[[i]]$rtinseconds})),
                  unlist(sapply(1:22, function(i){S[[i]]$rtinseconds})))
  
  expect_setequal(unlist(sapply(1:22, function(i){B[[i]]$scan})),
                  unlist(sapply(1:22, function(i){S[[i]]$scan})))
  
})

test_that("check readSpectrum scan 23.", {

  
  S <- rawrr::sampleFilePath() |>
    rawrr::readSpectrum(23) |>
    unlist(recursive = FALSE)

  DF <- system.file("extdata", name = 'scan23_peakList.txt',
                    package = 'rawrr') |>
    read.table(sep="\t", header=TRUE)

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
