#R

context("index")

library(rawR)


test_that("check readIndex.", {

  S <- readIndex(sample())

  expect_equivalent(dim(S) == c(573, 6), c(TRUE, TRUE))
})


test_that("check readIndex error.", {
  rawfile <- "this file does not exists"
  expect_error(S<-readIndex(rawfile))
})
