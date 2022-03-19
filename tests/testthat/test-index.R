#R

context("index")

library(rawrr)


test_that("check readIndex.", {
  x <- readIndex(sampleFilePath())
  expect_equivalent(dim(x) == c(573, 9),
      c(TRUE, TRUE))
  expect_true(is.data.frame(x))
  IndexColNames <- c("scan", "scanType", "rtinseconds", "precursorMass",
                     "MSOrder", "charge", "masterScan", "dependencyType", "monoisotopicMz")
  expect_true(is.integer(x$scan))
  expect_true(all(IndexColNames %in% colnames(x)))
  expect_true(is.logical(x$dependencyType))
  expect_true(all(1:nrow(x), x$scan))
})


test_that("check readIndex error.", {
  rawfile <- "this file does not exists"
  expect_error(S <- readIndex(rawfile))
})
