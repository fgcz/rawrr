#R

context("Generics for printing and ploting of objects")

library(rawrr)

S <- readSpectrum(rawfile = sampleFilePath(), 1)

test_that("print, summary, and plot calls right generic for rawrrSpectrum object",{

  expect_s3_class(print(S[[1]]), class = "rawrrSpectrum")
  expect_output(print(S[[1]]), regexp = "Total Ion Current")
  expect_s3_class(summary(S[[1]]), class = "rawrrSpectrum")
  expect_output(summary(S[[1]]), regexp = "Total Ion Current")

  png(filename = "rawrrSpectrumPlot.png")
  expect_s3_class(plot(S[[1]]), class = "rawrrSpectrum")
  dev.off()
  expect_true(file.exists("rawrrSpectrumPlot.png"))
  if (file.exists("rawrrSpectrumPlot.png")) {
    file.remove("rawrrSpectrumPlot.png")
  }

})
