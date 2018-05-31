#' Install and Manage corpora. 
#' 
#' Utitlity functions for making the installation of indexed CWB corpora wrapped
#' into R data packages as easy as possible. A data package with a CWB corpus is
#' assumed to include a directory \code{/extdata/cwb/registry} for registry
#' files and a directory \code{/extdata/cwb/indexed_corpora} for the inexed
#' corpus files.
#' 
#' @details The \code{corpus_install} function combines two steps necessary to
#'   install a CWB corpus wrapped into a R data package. First, it calls
#'   \code{install.packages}, then it resets the path pointing to the directory
#'   with the indexed corpus files in the registry file. The package will be
#'   installed to the standard library directory for installing R packages
#'   (\code{.libPaths()[1]}). Another location can be used by stating the param
#'   'lib' explicitly (see documentation for \code{\link{install.packages}}).
#'   The function can also be used to install a corpus from a password protected
#'   repository. Further parameters are handed over to install.packages, so you
#'   might add \code{method = "wget" extra = "--user donald --password duck"}.
#'   See examples how to check whether the directory has been set correctly.
#' @param old name of the (old) corpus
#' @param new name of the (new) corpus
#' @param pkg data package name
#' @param repo URL of the repository 
#' @param lib directory for R packages, defaults to \code{.libPaths()[1]}; the path may not 
#' include a whitespace sign
#' @param verbose logical, whether to be verbose
#' @param registry_dir directory of registry
#' @param corpus a CWB corpus
#' @param tarfile filename of tarball
#' @param corpus_version version of the corpus to compress
#' @param ... further parameters that will be passed into \code{install.packages}
#' @name install.corpus
#' @seealso For managing registry files, see \code{\link{registry_file_parse}}
#' for switching to a packaged corpus. 
#' @importFrom utils available.packages contrib.url install.packages
#' @importFrom utils installed.packages
#' @importFrom RCurl url.exists
#' @rdname corpus_utils
#' @export corpus_install
corpus_install <- function(pkg = NULL, repo = "http://polmine.sowi.uni-due.de/packages", tarball = NULL, lib = .libPaths()[1], verbose = TRUE, ...){
  if (is.null(tarball)){
    if (!pkg %in% utils::available.packages(utils::contrib.url(repos = repo))) {
      stop(sprintf("package '%s' not available at repo '%s'", pkg, repo))
    }
    
    if (file.access(lib, "6") == -1){
      stop("You do not have write permissions for directory ", lib,
           ". Please run R with the required privileges, or provide another directory (param 'lib').")
    }
    install.packages(pkgs = package, repos = repo, lib = lib, ...)
    pkg_registry <- system.file(package = package, "extdata", "cwb", "registry")
    corpora <- list.files(pkg_registry)
    for (corpus in corpora){
      regdata <- registry_file_parse(corpus = corpus, registry_dir = pkg_registry)
      data_dir <- system.file(package = package, "extdata", "cwb", "indexed_corpora", corpus)
      if (regdata[["home"]] != data_dir){
        regdata[["home"]] <- data_dir
        registry_file_write(data = regdata, corpus = corpus, registry_dir = pkg_registry)
      }
    }
  } else {
    tmp_dir <- tempdir()
    cwbtools_tmpdir <- file.path(tmp_dir, "cwbtools_tmpdir")
    if (file.exists(cwbtools_tmpdir)) unlink(cwbtools_tmpdir, recursive = TRUE)
    dir.create(cwbtools_tmpdir)
    corpus_tarball <- file.path(cwbtools_tmpdir, basename(tarball))
    if (grepl("^http", tarball)){
      if (!url.exists(tarball)) stop("tarball is not available")
      download.file(url = tarball, destfile = corpus_tarball)
    } else {
      if (!file.exists(tarball)) stop(sprintf("tarball '%s' does not exist", tarball))
      file.copy(from = tarball, to = corpus_tarball)
    }
    
    if (verbose) message("... extracting tarball")
    untar(tarfile = corpus_tarball, exdir = cwbtools_tmpdir)
    unlink(corpus_tarball)
    
    tmp_registry_dir <- file.path(cwbtools_tmpdir, "registry")
    tmp_data_dir <- file.path(cwbtools_tmpdir, "indexed_corpora")
    corpora <- list.files(tmp_registry_dir)
    for (corpus in corpora){
      registry_data <- registry_file_parse(corpus = corpus, registry_dir = tmp_registry_dir)
      registry_data[["home"]] <- file.path(tmp_data_dir, tolower(registry_data[["id"]]))
      registry_file_write(data = registry_data, corpus = corpus, registry_dir = tmp_registry_dir)
      if (!is.null(pkg)){
        pkg_add_corpus(pkg = pkg, corpus = corpus, registry = tmp_registry_dir)
      } else {
        stop("installation of a tarred corpus to general corpus storage is not yet implemented")
      }
      
    }
    unlink(cwbtools_tmpdir, recursive = TRUE)
  }
  invisible(NULL)
}

#' @details \code{corpus_packages} will detect the packages that include CWB
#'   corpora.
#' @rdname corpus_utils
#' @export corpus_packages
corpus_packages <- function(){
  matrices <- lapply(
    .libPaths(),
    function(lib){
      vectors <- lapply(
        installed.packages(lib.loc = lib)[,"Package"],
        function(package){
          c(
            package = package,
            lib = lib,
            registry = system.file(package = package, "extdata", "cwb", "registry")
          )
        }
      )
      do.call(rbind, vectors)
    }
  )
  M <- data.table(do.call(rbind, matrices))
  M <- M[which(nchar(M[["registry"]]) > 0)]
  M
}

#' @details \code{corpus_copy} will create a copy of a corpus (useful for
#'   experimental modifications, for instance).
#' @rdname corpus_utils
#' @export corpus_copy
corpus_copy <- function(old, new, registry_dir = Sys.getenv("CORPUS_REGISTRY"), verbose = TRUE){
  stopifnot(tolower(old) %in% list.files(registry_dir))
  
  # copy data directory
  message("copying data directory")
  regdata <- registry_file_parse(corpus = old, registry_dir = registry_dir)
  data_dir_new <- file.path(dirname(regdata[["home"]]), tolower(new))
  if (file.exists(data_dir_new)){
    if (readline(prompt = "Data directory already exists. Proceed anyway? (type 'Y' to continue, anything else to abort)") != "Y")
      stop("Aborting the operation.")
  } else {
    dir.create(data_dir_new)
  }
  files_to_copy <- list.files(regdata[["home"]], full.names = TRUE)
  success <- pbapply::pblapply(
    files_to_copy,
    function(x) file.copy(from = x, to = data_dir_new, recursive = TRUE)
    )
  if (!all(unlist(success))){
    stop("copying the data directory failed")
  } else {
    message("... copying data directory succeeded")
  }
  
  # generate copy of registry file
  message("make copy of registry file")
  registry_file_new <- file.path(registry_file_new, tolower(new))
  if (file.exists(registry_file_new)){
    if (readline(prompt = "New registry file already exists. Proceed anyway? (type 'Y' to continue, anything else to abort)") != "Y"){
      file.remove(registry_file_new)
    } else {
      stop("Aborting the operation.")
    }
      
  }
  success <- file.copy(
    from = file.path(registry_dir, tolower(old)),
    to = file.path(registry_dir, tolower(new))
  )
  if (!success){
    stop("copying the registry file failed")
  } else {
    message("... copying registry file succeeded")
  }
  
  # modify the new registry file 
  message("updating new registry file")
  regdata <- registry_file_parse(corpus = new, registry_dir = registry_dir)
  regdata[["id"]] <- tolower(new)
  regdata[["home"]] <- data_dir_new
  registry_file_write(data = regdata, corpus = tolower(new), registry_dir = registry_dir)
  invisible(NULL)
}

#' @details \code{corpus_rename} will rename a corpus, affecting the name of the
#'   registry file, the corpus id, and the name of the directory where data
#'   files reside.
#' @rdname corpus_utils
#' @export corpus_rename
corpus_rename <- function(old, new, registry_dir = Sys.getenv("CORPUS_REGISTRY"), verbose = TRUE){
  # check that old corpus exists
  stopifnot(tolower(old) %in% list.files(registry_dir))
  # check that new corpus does not yet exist
  if (tolower(new) %in% list.files(registry_dir)){
    stop("Corpus provided by 'new' already exists - do not overwrite an existing corpus")
  }
  
  # rename registry file
  message("renaming registry file")
  registry_old <- file.path(registry_dir, tolower(old))
  registry_new <- file.path(registry_dir, tolower(new))
  success <- file.rename(from = registry_old, to = registry_new)
  if (!success) stop("renaming the registry file failed")
  
  # rename data directory
  message("renaming data directory")
  regdata <- registry_file_parse(corpus = new, registry_dir = registry_dir)
  data_directory_old <- regdata[["home"]]
  data_directory_new <- file.path(dirname(data_directory_old), tolower(new))
  success <- file.rename(from = data_directory_old, to = data_directory_new)
  if (!success) stop("renaming the data directory failed")
  
  # modify and save registry file
  message("modifying and saving registry file")
  regdata[["home"]] <- data_directory_new
  regdata[["id"]] <- tolower(new)
  registry_file_write(data = regdata, corpus = tolower(new), registry_dir = registry_dir)
  invisible(NULL)
}

#' @details \code{corpus_remove} can be used to drop a corpus.
#' @rdname corpus_utils
#' @export corpus_remove
corpus_remove <- function(corpus, registry_dir = Sys.getenv("CORPUS_REGISTRY")){
  
  stopifnot(tolower(corpus) %in% list.files(registry_dir)) # check that corpus exists
  
  reg <- registry_file_parse(corpus = tolower(corpus), registry_dir = registry_dir)
  data_directory <- reg[["home"]]
  if (readline(prompt = sprintf("Are you sure you want to data files for corpus '%s'? ('Y' to continue, anything else to abort", corpus)) != "Y"){
    for (x in list.files(data_directory, full.names = TRUE)) file.remove(x)
    file.remove(data_directory)
  }
  if (readline(prompt = sprintf("Are you sure you want to delete the corpus '%s'? ('Y' to continue, anything else to abort)", corpus)) != "Y"){
    file.remove(file.path(registry_dir, tolower(x)))
  }
}

#' @details \code{corpus_as_tarball} will create a tarball (.tar.gz-file) with
#'   two subdirectories. The 'registry' subdirectory will host the registry file
#'   for the tarred corpus. The data files will be put in a subdirectory with
#'   the corpus name in the 'indexed_corpora' subdirectory.
#' @rdname corpus_utils
#' @export corpus_as_tarball
corpus_as_tarball <- function(corpus, registry_dir, tarfile, verbose = TRUE){
  
  registry_file <- file.path(registry_dir, tolower(corpus))
  if (!file.exists(registry_file))
    stop(
      sprintf("registry file for corpus '%s' does not exist in registry directory '%s'",
              corpus, registry_dir)
      )
  home_dir <- registry_file_parse(corpus = corpus, registry_dir = registry_dir)[["home"]]

  if (verbose) message("... moving registry file and data files to temporary directory for creating tarball")
  tmp_dir <- tempdir()
  archive_dir <- file.path(tmp_dir, tolower(corpus))
  if (file.exists(archive_dir)) unlink(archive_dir, recursive = TRUE)
  dir.create(archive_dir)

  archive_registry_dir <- file.path(archive_dir, "registry")
  archive_data_dir <- file.path(archive_dir, "indexed_corpora")
  archive_corpus_dir <- file.path(archive_dir, "indexed_corpora", tolower(corpus))
  dir.create(archive_registry_dir)
  dir.create(archive_data_dir)
  dir.create(archive_corpus_dir)
  
  file.copy(from = registry_file, to = file.path(archive_registry_dir, tolower(corpus)))
  for (x in list.files(home_dir, full.names = TRUE)){
    file.copy(from = x, to = file.path(archive_corpus_dir, basename(x)))
  }

  old_wd <- setwd(archive_dir)
  
  if (verbose) message("... creating tarball")
  tar(tarfile = tarfile)
  
  if (verbose) message("... cleaning up")

  unlink(archive_dir, recursive = TRUE)
  setwd(old_wd)
  invisible( NULL )
}
