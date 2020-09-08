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


