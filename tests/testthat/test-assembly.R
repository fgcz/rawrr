#R

context(".Net assembly")

test_that("check assembly file path", {
  expect_true(file.exists(rawrr:::.rawrrAssembly()))
})

test_that("check assembly execute", {
  expect_true(.isAssemblyWorking())
})

test_that(".isAssemblyWorking reports a non-executable assembly.", {
  skip_on_os("windows")

  exe <- tempfile("rawrr.")
  writeLines("not an executable", exe)
  Sys.chmod(exe, mode = "0644")
  on.exit(unlink(exe))

  ## used to fail with "argument is of length zero" (#96)
  expect_error(rawrr:::.isAssemblyWorking(exe = exe), "exit status 126")
  expect_warning(rv <- rawrr:::.isAssemblyWorking(FUN = warning, exe = exe),
                 "is not working")
  expect_false(isTRUE(rv))
})

test_that(".isAssemblyWorking reports a missing assembly.", {
  expect_error(rawrr:::.isAssemblyWorking(exe = tempfile("missing.")),
               "not found")
})

test_that("check assembly version", {
  expect_true(rawrr:::.getRawrrAssemblyVersion() > "1.17.1")
})
