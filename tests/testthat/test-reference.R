#R

context("reference values")

library(rawrr)

## Compares reader output against known values of sample.raw. These tests
## catch silent changes in the values returned by the Thermo RawFileReader
## (e.g. after a version bump) or by a binary built for another runtime.

test_that("scan 23 header matches reference.", {
  S <- rawrr::sampleFilePath() |>
    rawrr::readSpectrum(23) |>
    unlist(recursive = FALSE)

  expect_equal(S$scan, 23)
  expect_equal(S$TIC, 79509784)
  expect_equal(S$basePeak, c(445.12, 5902283.5), tolerance = 1e-4)
  expect_equal(S$massRange, c(350, 1800))
  expect_equal(S$scanType, "FTMS + c NSI Full ms [350.0000-1800.0000]")
  expect_equal(S$StartTime, 0.03, tolerance = 0.01)
})

test_that("scan 23 trailer values match scan23_ScanHeader.txt.", {
  S <- rawrr::sampleFilePath() |>
    rawrr::readSpectrum(23) |>
    unlist(recursive = FALSE)

  R <- system.file("extdata", "scan23_ScanHeader.txt", package = "rawrr") |>
    read.table(sep = "\t", quote = "", comment.char = "", fill = TRUE,
               strip.white = TRUE, col.names = c("key", "value", "empty"),
               colClasses = "character")

  keys <- intersect(R$key, names(S))
  ## the reference file lists more than 60 trailer entries
  expect_true(length(keys) >= 60)

  for (k in keys){
    expect_identical(trimws(paste(S[[k]], collapse = " ")),
                     trimws(R$value[match(k, R$key)]),
                     label = k)
  }
})

test_that("file header matches reference.", {
  H <- rawrr::readFileHeader(rawrr::sampleFilePath())

  expect_equal(H$`RAW file`, "sample.raw")
  expect_equal(H$`RAW file version`, "66")
  ## .NET/ICU may format the date with a narrow no-break space (U+202F)
  ## before AM/PM, depending on platform and ICU version; normalize it
  expect_equal(gsub("[\u00A0\u202F[:space:]]+", " ", H$`Creation date`),
               "2/13/2018 11:09:03 AM")
  expect_equal(H$`Operator`, "ExactiveUser")
  expect_equal(H$`Number of instruments`, 1)
  expect_equal(H$`Instrument model`, "Q Exactive HF-X Orbitrap")
  expect_equal(H$`Serial number`, "Exactive Series slot #6114")
  expect_equal(H$`Software version`, "2.9-290033/2.9.0.2926")
  expect_equal(H$`Scan range`, c(1, 574))
})
