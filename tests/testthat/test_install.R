testthat::context("install")

test_that(
  "corpus_install from tarball",
  {
    library(RcppCWB)
    cwb_dir_tmp <- file.path(tempdir(), "cwb_tmp")
    cwb_dirs <- create_cwb_directories(prefix = cwb_dir_tmp, ask = FALSE, verbose = FALSE)
    if (cqp_is_initialized()){
      cqp_reset_registry(registry = cwb_dirs[["registry_dir"]])
    } else {
      cqp_initialize(registry = cwb_dirs[["registry_dir"]])
    }
    
    tmp_tarball <- tempfile(fileext = ".tar.gz")
    rcppcwb_registry <- system.file(package = "RcppCWB", "extdata", "cwb", "registry")
    cwbtools::corpus_as_tarball(
      corpus = "REUTERS",
      registry_dir = rcppcwb_registry,
      data_dir <- system.file(package = "RcppCWB", "extdata", "cwb", "indexed_corpora", "reuters"),
      tarfile = tmp_tarball,
      verbose = TRUE
    )
    
    corpus_install(
      tarball = tmp_tarball, 
      registry_dir = cwb_dirs[["registry_dir"]], 
      corpus_dir = cwb_dirs[["corpus_dir"]]
    )
    
    cqp_reset_registry(registry = cwb_dirs[["registry_dir"]])
    
    expect_true("REUTERS" %in% RcppCWB::cqp_list_corpora())
    
    # unlink(cwb_dir_tmp, recursive = TRUE)
  }
)


test_that(
  "download and install UNGAMINI when no system registry is present",
  {
    skip_on_cran()
    Sys.setenv(CORPUS_REGISTRY = "")

    corpus_install(doi = "10.5281/zenodo.3823245", ask = FALSE)
  }
)



test_that(
  "download and install UNGAMINI",
  {
    skip_on_cran()
    old_registry <- Sys.getenv("CORPUS_REGISTRY")
    Sys.setenv(CORPUS_REGISTRY = "")

    cwb_dirs <- cwbtools::create_cwb_directories(prefix = tempdir(), ask = FALSE, verbose = FALSE)
    Sys.setenv(CORPUS_REGISTRY = cwb_dirs[["registry_dir"]])
    
    if (RcppCWB::cqp_is_initialized()){
      RcppCWB::cqp_reset_registry(registry = cwb_dirs[["registry_dir"]])
    } else {
      RcppCWB::cqp_initialize(registry = cwb_dirs[["registry_dir"]])
    }
    
    corpus_install(
      doi = "https://doi.org/10.5281/zenodo.3748858",
      registry_dir = cwb_dirs[["registry_dir"]],
      corpus_dir = cwb_dirs[["corpus_dir"]],
      ask = FALSE
    )
    
    RcppCWB::cqp_reset_registry(registry = cwb_dirs[["registry_dir"]]) # should not be necessary
    expect_true("UNGAMINI" %in% RcppCWB::cqp_list_corpora())
    
    unlink(cwb_dirs[["corpus_dir"]], recursive = TRUE)
    unlink(cwb_dirs[["registry_dir"]], recursive = TRUE)
    Sys.setenv("CORPUS_REGISTRY" = old_registry)
  }
)

test_that(
  "check that corpus_install() fails gracefully if DOI is incorrect",
  {
    skip_on_cran()
    y <- corpus_install(doi = "10.5281/zenodo.3823245123", verbose = FALSE)
    expect_false(y, FALSE)
  }
)


test_that(
  "check that corpus_install() aborts of md5 checksum is invalid",
  {
    skip_on_cran()
    
    old_registry <- Sys.getenv("CORPUS_REGISTRY")
    Sys.setenv(CORPUS_REGISTRY = "")
    
    cwb_dirs <- cwbtools::create_cwb_directories(prefix = tempdir(), ask = FALSE, verbose = FALSE)
    Sys.setenv(CORPUS_REGISTRY = cwb_dirs[["registry_dir"]])
    
    if (RcppCWB::cqp_is_initialized()){
      RcppCWB::cqp_reset_registry(registry = cwb_dirs[["registry_dir"]])
    } else {
      RcppCWB::cqp_initialize(registry = cwb_dirs[["registry_dir"]])
    }

    doi <- "10.5281/zenodo.3823245"
    zenodo_record <- zen4R::ZenodoManager$new()$getRecordByDOI(doi = doi)
    zenodo_files <- sapply(zenodo_record[["files"]], function(x) x[["links"]][["download"]])
    tarball <- grep("^.*?_(v|)\\d+\\.\\d+\\.\\d+\\.tar\\.gz$", zenodo_files, value = TRUE)

    y <- corpus_install(
      tarball = tarball,
      checksum = "cd542b95b3e6c80600f896c2a288c5d3",
      registry_dir = cwb_dirs[["registry_dir"]],
      corpus_dir = cwb_dirs[["corpus_dir"]],
      verbose = FALSE
    )
    expect_true(y)
    
    y <- corpus_install(
      tarball = tarball,
      checksum = "cd542b95b3e6c80600f896c2a288c5d4",
      registry_dir = cwb_dirs[["registry_dir"]],
      corpus_dir = cwb_dirs[["corpus_dir"]],
      ask = FALSE,
      verbose = FALSE
    )
    expect_false(y)
    
    unlink(cwb_dirs[["corpus_dir"]], recursive = TRUE)
    unlink(cwb_dirs[["registry_dir"]], recursive = TRUE)
    Sys.setenv("CORPUS_REGISTRY" = old_registry)
  }
)
