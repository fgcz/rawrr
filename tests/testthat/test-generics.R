#R

context("Generics for printing and ploting of rawrrSpectrum rawrrChromatogram objects")

library(rawrr)

S <- readSpectrum(rawfile = sampleFilePath(), 1)

test_that("print, summary, and plot calls right generic for rawrrSpectrum object",{

  expect_s3_class(print(S[[1]]), class = "rawrrSpectrum")
  expect_output(print(S[[1]]), regexp = "Total Ion Current")
  expect_s3_class(summary(S[[1]]), class = "rawrrSpectrum")
  expect_output(summary(S[[1]]), regexp = "Total Ion Current")

  # TODO : change to snapshot tests for plotting
  # vignette("snapshotting")

  png(filename = "rawrrSpectrumPlot.png")
  expect_s3_class(plot(S[[1]]), class = "rawrrSpectrum")
  dev.off()
  expect_true(file.exists("rawrrSpectrumPlot.png"))
  if (file.exists("rawrrSpectrumPlot.png")) {
    file.remove("rawrrSpectrumPlot.png")
  }

})

C <- readChromatogram(rawfile = sampleFilePath(), mass = c(445.1181, 519.1367))

test_that("plot calls right generic for rawrrChromatogram(Set) object", {

  # TODO : change to snapshot tests for plotting

  png(filename = "rawrrChromatogramPlot.png")
  expect_s3_class(plot(C[[1]]), class = "rawrrChromatogram")
  dev.off()
  expect_true(file.exists("rawrrChromatogramPlot.png"))
  if (file.exists("rawrrChromatogramPlot.png")) {
    file.remove("rawrrChromatogramPlot.png")
  }

  png(filename = "rawrrChromatogramSetPlot.png")
  expect_s3_class(plot(C), class = "rawrrChromatogramSet")
  dev.off()
  expect_true(file.exists("rawrrChromatogramSetPlot.png"))
  if (file.exists("rawrrChromatogramSetPlot.png")) {
    file.remove("rawrrChromatogramSetPlot.png")
  }


})
