testthat::context("install")

test_that(
  "corpus_install from tarball",
  {
    library(RcppCWB)
    
    cwb_dir_tmp <- file.path(tempdir(), "cwb_tmp")
    cwb_dirs <- create_cwb_directories(prefix = cwb_dir_tmp, ask = FALSE, verbose = FALSE)
    cqp_reset_registry(registry = cwb_dirs[["registry_dir"]])

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
    
    unlink(cwb_dir_tmp, recursive = TRUE)
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

    expect_true("UNGAMINI" %in% RcppCWB::cqp_list_corpora())
    
    unlink(cwb_dirs[["corpus_dir"]], recursive = TRUE)
    unlink(cwb_dirs[["registry_dir"]], recursive = TRUE)
    Sys.setenv("CORPUS_REGISTRY" = old_registry)
  }
)
