#R

context("assembly documentation")

library(rawrr)

# the option table of rawrr.exe is the single source of the CLI documentation;
# every option must have a '## `option`' section in inst/rawrrassembly/README.md
test_that("every rawrr.exe option is documented in the rawrrassembly README", {
  exe <- rawrr:::.rawrrAssembly()
  skip_if_not(file.exists(exe), "rawrr.exe not installed")

  md <- suppressWarnings(system2(exe, c("help", "--markdown"),
                                 stdout = TRUE, stderr = TRUE))
  skip_if(!is.null(attr(md, "status")),
          "installed rawrr.exe does not support 'help --markdown'; run rawrr::buildRawrrExe()")

  options <- sub("^\\| `([^`]+)`.*", "\\1", grep("^\\| `", md, value = TRUE))
  expect_true(length(options) > 0)

  readme <- system.file("rawrrassembly", "README.md", package = "rawrr")
  expect_true(file.exists(readme))
  sections <- sub("^## `([^`]+)`.*", "\\1",
                  grep("^## `", readLines(readme, warn = FALSE), value = TRUE))

  undocumented <- setdiff(options, sections)
  expect_equal(undocumented, character(0),
               info = "add a '## `option`' section to inst/rawrrassembly/README.md")

  # every option must also have a 'help <option>' page
  for (o in options) {
    h <- suppressWarnings(system2(exe, c("help", o), stdout = TRUE, stderr = TRUE))
    expect_null(attr(h, "status"), info = o)
    expect_match(h[1], sprintf("^Usage: rawrr.exe <raw file> %s", o), info = o)
  }
})
