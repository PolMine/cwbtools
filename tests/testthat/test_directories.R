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
    library(polmineR) # looks as if it was called twice on Travis
    options("polmineR.corpus_registry" = polmineR_registry)
    expect_identical(polmineR_registry, cwb_registry_dir())
  }
)    

test_that(
  "test function cwb_corpus_dir()",
  {
    polmineR_registry <- system.file(package = "polmineR", "extdata", "cwb", "registry")
    Sys.setenv(CORPUS_REGISTRY = polmineR_registry)
    library(polmineR)
    options("polmineR.corpus_registry" = polmineR_registry)
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

test_that(
  "abort creating cwb directories when write permissions are not there",
  {
    # scenario 1: cwb directory exists, but user does not have write permissions
    prefix1 <- file.path(tempdir(), "cwb1")
    dir.create(prefix1)
    Sys.chmod(prefix1, mode = "0500")
    testthat::expect_error(create_cwb_directories(prefix = prefix1))
    Sys.chmod(prefix1, mode = "0644") # cleaning up step 1
    file.remove(prefix1) # cleaning up step 2
    
    prefix2 <- file.path(tempdir(), "data")
    dir.create(prefix2)
    Sys.chmod(prefix2, mode = "0500")
    testthat::expect_error(create_cwb_directories(prefix = file.path(prefix2, "cwb")))
    Sys.chmod(prefix2, mode = "0644")
    file.remove(prefix2)
  }
)