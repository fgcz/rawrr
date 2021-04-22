#R

context(".Net assembly")

test_that("check tic of EH4547", {
  expect_true(file.exists(rawrr:::.rawrrAssembly()))
  expect_true(.isAssemblyWorking())
})
