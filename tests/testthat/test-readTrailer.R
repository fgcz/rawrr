#R

context("index")

library(rawrr)


test_that("check readTrailer.", {
  n <- rawrr::sampleFilePath() |>
      rawrr:::readIndex() |>
      nrow()
  expect_equal(n, 573)
  
  
  collisionEnergy <- rawrr::sampleFilePath() |>
      rawrr:::readTrailer("HCD Energy:")

  m <- collisionEnergy |>
      length()

  expect_equal(n, m)


  expect_equal(collisionEnergy |> as.numeric() |> is.na() |> sum(), 27)
})
