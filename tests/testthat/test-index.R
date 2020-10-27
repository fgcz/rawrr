#R

context("index")

library(rawR)


test_that("check readIndex.", {
  rawfile <- file.path(path.package(package = 'rawR'), 'extdata', 'sample.raw')

  S <- readIndex(rawfile)

  expect_equivalent(dim(S) == c(573, 6), c(TRUE, TRUE))
})


test_that("check readIndex error.", {
  rawfile <- "this file does not exists"
  expect_error(S<-readIndex(rawfile))
})
