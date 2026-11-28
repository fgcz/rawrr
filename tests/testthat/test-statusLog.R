#R

context("statusLog")

library(rawrr)

test_that("check readDevices.", {
  D <- rawrr::sampleFilePath() |> rawrr::readDevices()
  expect_true(is.data.frame(D))
  expect_true(nrow(D) >= 1)
  expect_true("MS" %in% D$deviceType)
  expect_equal(colnames(D),
               c("deviceType", "deviceIndex", "name", "model", "serialNumber"))
})

test_that("check readStatusLog.", {
  f <- rawrr::sampleFilePath()
  L <- rawrr::readStatusLog(f, device = "MS")
  expect_equal(colnames(L), c("position", "label"))
  skip_if(nrow(L) == 0, "sample file has no plottable MS status log")

  S <- rawrr::readStatusLog(f, device = "MS", label = L$label[1])
  expect_equal(colnames(S), c("times", "values"))
  expect_true(nrow(S) > 0)
  expect_true(is.numeric(S$values))
  expect_false(is.unsorted(S$times))

  # values are read unformatted, i.e., not rounded to the display precision
  # of the device (formatted: 274.96)
  Tcap <- rawrr::readStatusLog(f, device = "MS", label = "Capillary Temperature (°C)")
  expect_equal(Tcap$values[1], 274.9595629749443, tolerance = 1e-12)

  expect_error(rawrr::readStatusLog(f, device = "MS", label = "no such label"))
  expect_error(rawrr::readStatusLog(f, device = "noSuchDevice"))
})

test_that("check readPressureProfile.", {
  f <- rawrr::sampleFilePath()
  P <- suppressMessages(rawrr::readPressureProfile(f))
  expect_equal(colnames(P),
               c("deviceType", "deviceIndex", "name", "label", "times", "pressure"))
  skip_if(nrow(P) == 0, "sample file has no pressure status log")
  expect_true(is.numeric(P$pressure))
})
