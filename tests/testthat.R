#R

library(testthat)

suppressPackageStartupMessages(library(rawrr))

#if (rawrr:::.getRawrrAssemblyVersion() < "1.17.2"){
  rawrr::installRawrrExe(force = TRUE)
#}

test_check("rawrr")
