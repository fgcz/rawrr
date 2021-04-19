#R

context("EH4547")

library(rawrr)
library(ExperimentHub)


test_that("check tic of EH4547", {
  if (getRversion() >= 4.1){

    eh <- ExperimentHub::ExperimentHub()
    EH4547 <- normalizePath(eh[["EH4547"]])
    rawfile <- paste0(EH4547, ".raw")
    if (!file.exists(rawfile)){ file.copy(EH4547, rawfile) }
    
    expect_true(file.exists(EH4547))
    expect_true(file.exists(rawfile))
    
    x <- rawrr::readChromatogram(rawfile = rawfile, type = "tic")
    
    expect_equal(length(x$times), length(x$intensities))
    expect_equal(length(x$intensities), 995)
    
    expect_true(sum(x$intensities) == 63682815178)
  }
})
