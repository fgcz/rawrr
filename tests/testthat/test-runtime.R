#R

context(".NET runtime identifier")

## pure R; does not require the rawrr assembly or dotnet

test_that(".rawrrRuntime maps OS and architecture to a .NET RID.", {
  rid <- rawrr:::.rawrrRuntime

  expect_equal(rid("Darwin",  "aarch64"), "osx-arm64")
  expect_equal(rid("Darwin",  "arm64"),   "osx-arm64")
  expect_equal(rid("Darwin",  "x86_64"),  "osx-x64")
  expect_equal(rid("Linux",   "aarch64"), "linux-arm64")
  expect_equal(rid("Linux",   "x86_64"),  "linux-x64")
  expect_equal(rid("Windows", "aarch64"), "win-arm64")
  expect_equal(rid("Windows", "x86_64"),  "win-x64")
})

test_that(".rawrrRuntime of the running session is a supported RID.", {
  expect_true(rawrr:::.rawrrRuntime() %in%
                c("linux-x64", "osx-x64", "win-x64",
                  "linux-arm64", "osx-arm64", "win-arm64"))
})

test_that(".rawrrAssembly path is <rid>/rawrr[.exe].", {
  p <- rawrr:::.rawrrAssembly()
  rid <- rawrr:::.rawrrRuntime()

  expect_equal(basename(dirname(p)), rid)
  expect_equal(basename(p),
               if (startsWith(rid, "win-")) "rawrr.exe" else "rawrr")
})
