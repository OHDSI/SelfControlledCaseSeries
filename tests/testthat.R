getOs <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf["sysname"]
    if (os == "Darwin")
      os <- "mac"
  } else {
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "mac"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}

# Only run linux unit tests in GitHub to avoid weird CRAN flavors where SQLite (EUNOMIA) can be problematic:
if (Sys.getenv("GITHUB_REPOSITORY") != "" || getOs() %in% c("windows", "mac")) {
  # Workaround to fix unit tests in check as described here:
  # https://github.com/hadley/testthat/issues/86
  Sys.setenv(R_TESTS = "")

  library(testthat)
  library(SelfControlledCaseSeries)
  test_check("SelfControlledCaseSeries")
}
