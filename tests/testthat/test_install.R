testthat::context("install")

test_that(
  "corpus_install from tarball",
  {
    library(polmineR)
    file.remove(list.files(registry(), full.names = TRUE))
    registry_reset(registryDir = registry())

    tmp_tarball <- tempfile(fileext = ".tar.gz")
    polmineR_registry <- system.file(package = "polmineR", "extdata", "cwb", "registry")
    cwbtools::corpus_as_tarball(
      corpus = "REUTERS",
      registry_dir = polmineR_registry,
      data_dir <- system.file(package = "polmineR", "extdata", "cwb", "indexed_corpora", "reuters"),
      tarfile = tmp_tarball,
      verbose = TRUE
    )
    
    prefix <- file.path(tempdir(), "cwb")
    if (dir.exists(prefix)) unlink(prefix, recursive = TRUE)
    dir.create(prefix)
    
    cwb_dirs <- create_cwb_directories(prefix = prefix, ask = FALSE)
    
    corpus_install(
      tarball = tmp_tarball, 
      registry_dir = cwb_dirs[["registry_dir"]], 
      corpus_dir = cwb_dirs[["corpus_dir"]]
    )
    
    file.copy(
      from = file.path(cwb_dirs[["registry_dir"]], "reuters"),
      to = file.path(registry(), "reuters")
    )
    registry_reset(registryDir = registry())
    
    expect_true(corpus()[["corpus"]] == "REUTERS")
  }
)


test_that(
  "download and install UNGAMINI",
  {
    skip_on_cran()
    Sys.setenv(CORPUS_REGISTRY = "")
    cwb_dirs <- cwbtools::create_cwb_directories(prefix = tempdir(), ask = FALSE)
    options(polmineR.corpus_registry = cwb_dirs[["registry_dir"]])
    Sys.setenv(CORPUS_REGISTRY = cwb_dirs[["registry_dir"]])
    
    corpus_install(doi = "https://doi.org/10.5281/zenodo.3748858", registry_dir = cwb_dirs[["registry_dir"]], corpus_dir = cwb_dirs[["corpus_dir"]])
    
    library(polmineR)
    file.copy(
      from = file.path(cwb_dirs[["registry_dir"]], "ungamini"),
      to = file.path(registry(), "ungamini")
    )
    registry_reset(registryDir = registry())
    
    expect_true("UNGAMINI" %in% corpus()[["corpus"]])
  }
)
