testthat::context("install")

test_that(
  "corpus_install from tarball",
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
    
    corpus_install(tarball = tmp_tarball, registry_dir = cwb_dirs[["registry_dir"]], corpus_dir = cwb_dirs[["registry_dir"]])
    
    file.copy(
      from = file.path(cwb_dirs[["registry_dir"]], "reuters"),
      to = file.path(registry(), "reuters")
    )
    registry_reset(registryDir = registry())
    
    expect_true(corpus()[["corpus"]] == "REUTERS")
  }
)


test_that(
  "",
  {
    doi <- "https://doi.org/10.5281/zenodo.3748858"
    record_id <- gsub("^.*?10\\.5281/zenodo\\.(\\d+)$", "\\1", doi)
    zenodo_api_url <- sprintf("https://zenodo.org/api/records/%d", as.integer(record_id))
    zenodo_data <- jsonlite::fromJSON(RCurl::getURL(zenodo_api_url))
    tarball <- grep("\\.tar\\.gz$", zenodo_data[["files"]][["links"]][["self"]], value = TRUE)
    
    cwbtools_tmpdir <- file.path(normalizePath(tempdir(), winslash = "/"), "cwbtools_tmpdir", fsep = "/")
    dir.create(cwbtools_tmpdir)
    corpus_tarball <- file.path(cwbtools_tmpdir, basename(tarball), fsep = "/")
    curl::curl_download(url = tarball, destfile = corpus_tarball, quiet = FALSE)
    untar(tarfile = corpus_tarball, exdir = cwbtools_tmpdir)
    tmp_registry_dir <- file.path(normalizePath(cwbtools_tmpdir, winslash = "/"), "registry", fsep = "/")
    tmp_data_dir <- file.path(normalizePath(cwbtools_tmpdir, winslash = "/"), "indexed_corpora", fsep = "/")
    home_dir <- file.path(tmp_data_dir, tolower("ungamini"), fsep = "/")
    files <- list.files(home_dir)
    
    expect_identical(length(files), 64L)
    registry_data <- registry_file_parse(corpus = "ungamini", registry_dir = tmp_registry_dir)
    registry_data[["home"]] <- home_dir
    registry_file_write(data = registry_data, corpus = "ungamini", registry_dir = tmp_registry_dir)
    data_files_to_copy <- list.files(home_dir, full.names = TRUE)
    expect_identical(length(data_files_to_copy), 64L)
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
    # library(polmineR)
    # options("")
    # registry_reset(registryDir = registry())
    # expect_true("UNGAMINI" %in% corpus()[["corpus"]])
  }
)
