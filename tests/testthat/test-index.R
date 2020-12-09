#R

context("index")

library(rawrr)


test_that("check readIndex.", {

  S <- readIndex(sampleFilePath())

  expect_equivalent(dim(S) == c(573, 6),
      c(TRUE, TRUE))
})


test_that("check readIndex error.", {
  rawfile <- "this file does not exists"
  expect_error(S <- readIndex(rawfile))
})
