testthat::context("create CWB directories")

test_that(
  "cwb_registry_dir() - NULL if CORPUS_REGISTRY unset",
  {
    registry <- Sys.getenv("CORPUS_REGISTRY")
    Sys.setenv(CORPUS_REGISTRY = "")
    expect_null(cwb_registry_dir())
    Sys.setenv(CORPUS_REGISTRY = registry)
  }
)


test_that(
  "cwb_registry_dir() - use polmineR dir",
  {
    registry <- Sys.getenv("CORPUS_REGISTRY")
    rcppcwb_registry <- system.file(package = "RcppCWB", "extdata", "cwb", "registry")
    Sys.setenv(CORPUS_REGISTRY = rcppcwb_registry)
    expect_identical(rcppcwb_registry, cwb_registry_dir())
    Sys.setenv(CORPUS_REGISTRY = registry)
  }
)

test_that(
  "create cwb directories",
  {
    registry <- Sys.getenv("CORPUS_REGISTRY")
    Sys.setenv(CORPUS_REGISTRY = "")
    cwb_dirs <- create_cwb_directories(prefix = tempdir(), ask = FALSE)
    lapply(cwb_dirs, unlink)
    Sys.setenv(CORPUS_REGISTRY = registry)
  }
)

