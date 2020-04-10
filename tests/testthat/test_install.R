testthat::context("install")

test_that(
  "corpus_install",
  {
    library(polmineR)
    file.remove(list.files(registry(), full.names = TRUE))
    registry_reset(registryDir = registry())
    
    tmp_tarball <- tempfile(pattern = ".tar.gz")
    polmineR_registry <- system.file(package = "polmineR", "extdata", "cwb", "registry")
    cwbtools::corpus_as_tarball(
      corpus = "REUTERS",
      registry_dir = polmineR_registry,
      tarfile = tmp_tarball,
      verbose = TRUE
    )
    
    prefix <- file.path(tempdir(), "cwb")
    if (!dir.exists(prefix)) dir.create(prefix)
    cwb_dirs <- create_cwb_directories(prefix = prefix, ask = FALSE)
    
    corpus_install(tarball = tmp_tarball, registry_dir = cwb_dirs[["registry_dir"]])
    
    file.copy(
      from = file.path(cwb_dirs[["registry_dir"]], "reuters"),
      to = file.path(registry(), "reuters")
      )
    registry_reset(registryDir = registry())
    
    expect_true(corpus()[["corpus"]] == "REUTERS")
  }
)
