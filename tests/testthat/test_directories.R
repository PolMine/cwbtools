testthat::context("create CWB directories")

test_that(
  "is polmineR loaded?",
  {
    expect_null(getOption("polmineR.corpus_registry"))
  }
)

test_that(
  "cwb_registry_dir() - NULL if CORPUS_REGISTRY unset",
  {
    Sys.setenv(CORPUS_REGISTRY = "")
    expect_null(cwb_registry_dir())
  }
)


test_that(
  "cwb_registry_dir() - use polmineR dir",
  {
    polmineR_registry <- system.file(package = "polmineR", "extdata", "cwb", "registry")
    Sys.setenv(CORPUS_REGISTRY = polmineR_registry)
    expect_identical(polmineR_registry, cwb_registry_dir())
  }
)

test_that(
  "cwb_registry_dir()",
  {
    # Ensure that cwb_registry_dir() will get the registry that has been defined 
    # by CORPUS_REGISTRY before polmineR was loaded
    polmineR_registry <- system.file(package = "polmineR", "extdata", "cwb", "registry")
    Sys.setenv(CORPUS_REGISTRY = polmineR_registry)
    library(polmineR)
    expect_identical(polmineR_registry, getOption("polmineR.corpus_registry"))
  }
)    


test_that(
  "cwb_registry_dir()",
  {
    # Ensure that cwb_registry_dir() will get the registry that has been defined 
    # by CORPUS_REGISTRY before polmineR was loaded
    polmineR_registry <- system.file(package = "polmineR", "extdata", "cwb", "registry")
    Sys.setenv(CORPUS_REGISTRY = polmineR_registry)
    library(polmineR)
    expect_identical(polmineR_registry, cwb_registry_dir())
  }
)    

test_that(
  "test function cwb_corpus_dir()",
  {
    polmineR_registry <- system.file(package = "polmineR", "extdata", "cwb", "registry")
    Sys.setenv(CORPUS_REGISTRY = polmineR_registry)
    library(polmineR)
    expect_identical(
      system.file(package = "polmineR", "extdata", "cwb", "indexed_corpora"),
      cwb_corpus_dir(registry_dir = cwb_registry_dir())
    )
  }
)

test_that(
  "create cwb directories",
  {
    Sys.setenv(CORPUS_REGISTRY = "")
    cwb_dirs <- create_cwb_directories(prefix = tempdir(), ask = FALSE)
  }
)