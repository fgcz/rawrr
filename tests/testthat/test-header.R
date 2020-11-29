#R

context("header")

library(rawR)

test_that("check readFileHeader", {

  M <- readFileHeader(sample())


  expect_identical(M$`Scan range`, c(1, 574))
  expect_vector(M$`Scan range`)
  expect_equal(M$`Scan range`, c(1, 574))
})

test_that("check readFileHeader error.", {
  rawfile <- "this file does not exists"
  expect_error(S<-readFileHeader(rawfile))
})
