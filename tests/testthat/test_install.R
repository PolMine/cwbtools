testthat::context("install")


test_that(
  "corpus_install from pkg",
  {
    cwb_dir_tmp <- fs::path(tempdir(), "cwb_tmp")
    cwb_dirs <- create_cwb_directories(prefix = cwb_dir_tmp, ask = FALSE, verbose = FALSE)
    
    if (cqp_is_initialized()){
      cqp_reset_registry(registry = cwb_dirs[["registry_dir"]])
    } else {
      cqp_initialize(registry = cwb_dirs[["registry_dir"]])
    }

    corpus_install(
      pkg = "RcppCWB",
      registry_dir = cwb_dirs[["registry_dir"]],
      corpus_dir = cwb_dirs[["corpus_dir"]]
    )
    
    cqp_reset_registry(registry = cwb_dirs[["registry_dir"]])
    
    expect_true("UNGA" %in% RcppCWB::cqp_list_corpora())
    expect_true("REUTERS" %in% RcppCWB::cqp_list_corpora())
    
    unlink(cwb_dir_tmp, recursive = TRUE)
  }
)


test_that(
  "corpus_install from tarball",
  {
    library(RcppCWB)
    cwb_dir_tmp <- fs::path(tempdir(), "cwb_tmp")
    cwb_dirs <- create_cwb_directories(prefix = cwb_dir_tmp, ask = FALSE, verbose = FALSE)
    if (cqp_is_initialized()){
      cqp_reset_registry(registry = cwb_dirs[["registry_dir"]])
    } else {
      cqp_initialize(registry = cwb_dirs[["registry_dir"]])
    }

    tmp_tarball <- fs::path_tidy(tempfile(fileext = ".tar.gz"))
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

    cwb_dirs <- cwbtools::create_cwb_directories(prefix = fs::path_temp(), ask = FALSE, verbose = FALSE)
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
    
    # On this occasion, we also test the corpus_get_version() function
    v <- corpus_get_version("UNGAMINI")
    expect_true(v == numeric_version("0.0.1"))

    unlink(cwb_dirs[["corpus_dir"]], recursive = TRUE)
    unlink(cwb_dirs[["registry_dir"]], recursive = TRUE)
    Sys.setenv("CORPUS_REGISTRY" = old_registry)
  }
)

test_that(
  "check that corpus_install() fails gracefully if DOI is incorrect",
  {
    skip_on_cran()
    
    expect_null(
      zenodo_get_tarballurl(url = "10.5281/zenodo.3823245123")
    )
    
    expect_false(
      corpus_install(doi = "10.5281/zenodo.3823245123", verbose = FALSE)
    )
  }
)


test_that(
  "zenodo_get_tarball()",
  {
    skip_on_ci()
    
    old_registry <- Sys.getenv("CORPUS_REGISTRY")
    Sys.setenv(CORPUS_REGISTRY = "")
    
    cwb_dirs <- cwbtools::create_cwb_directories(prefix = tempdir(), ask = FALSE, verbose = FALSE)
    Sys.setenv(CORPUS_REGISTRY = cwb_dirs[["registry_dir"]])
    
    gparl_url_pub <- "https://doi.org/10.5281/zenodo.3823245"
    tarball_tmp <- zenodo_get_tarball(url = gparl_url_pub)
    success <- corpus_install(tarball = tarball_tmp, load = FALSE)
    testthat::expect_true(success)
    
    tarball_tmp <- zenodo_get_tarball(url = gparlsample_url_restricted)
    if (is.null(tarball_tmp)) testthat::skip_on_cran()
    success <- corpus_install(tarball = tarball_tmp, ask = FALSE, load = FALSE)
    testthat::expect_true(success)
    
    unlink(cwb_dirs[["corpus_dir"]], recursive = TRUE)
    unlink(cwb_dirs[["registry_dir"]], recursive = TRUE)
    Sys.setenv("CORPUS_REGISTRY" = old_registry)
  }
)

test_that(
  "check that corpus_install() aborts of md5 checksum is invalid",
  {
    skip_on_ci()

    old_registry <- Sys.getenv("CORPUS_REGISTRY")
    Sys.setenv(CORPUS_REGISTRY = "")

    cwb_dirs <- cwbtools::create_cwb_directories(
      prefix = tempdir(),
      ask = FALSE,
      verbose = FALSE
    )
    Sys.setenv(CORPUS_REGISTRY = cwb_dirs[["registry_dir"]])

    if (RcppCWB::cqp_is_initialized()){
      RcppCWB::cqp_reset_registry(registry = cwb_dirs[["registry_dir"]])
    } else {
      RcppCWB::cqp_initialize(registry = cwb_dirs[["registry_dir"]])
    }

    doi <- "10.5281/zenodo.3823245"
    tryCatch(
      zenodo_record <- zen4R::ZenodoManager$new()$getRecordByDOI(doi = doi),
      error = function(e) testthat::skip("Zenodo not available")
    )
    if (!exists("zenodo_record")) testthat::skip()
    zenodo_files <- sapply(zenodo_record[["files"]], function(x) x[["download"]])
    
    tarball <- grep(
      "^.*?_(v|)\\d+\\.\\d+\\.\\d+\\.tar\\.gz/content$",
      zenodo_files,
      value = TRUE
    )

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
