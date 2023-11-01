#R

context("index")

library(rawrr)

test_that("check readIndex.", {
  x <- rawrr::sampleFilePath() |>
    rawrr::readIndex() |>
    rawrr::validate_rawrrIndex()

  expect_equivalent(dim(x) == c(574, 9),
      c(TRUE, TRUE))
  expect_true(is.data.frame(x))
  IndexColNames <- c("scan", "scanType", "StartTime", "precursorMass",
                     "MSOrder", "charge", "masterScan", "dependencyType", "monoisotopicMz")
  expect_true(is.integer(x$scan))
  expect_true(all(IndexColNames %in% colnames(x)))
  expect_true(is.logical(x$dependencyType))
  expect_true(all(1:nrow(x), x$scan))
})


test_that("check readIndex error.", {
  expect_error("this_file_does_not_exists.raw" |> readIndex())
})
