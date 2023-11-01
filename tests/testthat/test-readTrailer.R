#R

context("index")

library(rawrr)

test_that("check readTrailer.", {
    n <- rawrr::sampleFilePath() |>
      rawrr::readIndex() |>
      nrow()

    rawrr::sampleFilePath() |>
      rawrr:::readIndex() |>
      nrow() |>
      expect_equal(n)
  
    collisionEnergy <- rawrr::sampleFilePath() |>
      rawrr::readTrailer("HCD Energy:")

    collisionEnergy |>
      length() |>
      expect_equal(n)

    collisionEnergy |>
      as.numeric() |>
      is.na() |>
      sum() |>
      expect_equal(27)


    rawrr::sampleFilePath() |>
      rawrr:::readTrailer() |>
      length() |>
      expect_equal(62)

})
