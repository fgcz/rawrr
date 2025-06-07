#R

context(".Net assembly")

test_that("check assembly file path", {
  expect_true(file.exists(rawrr:::.rawrrAssembly()))
})

test_that("check assembly execute", {
  expect_true(.isAssemblyWorking())
})

test_that("check assembly version", {
  expect_true(rawrr:::.getRawrrAssemblyVersion() > "1.17.1")
})
