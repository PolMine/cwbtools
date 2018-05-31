#' Create and manage packages with corpus data.
#' 
#' Putting CWB indexed corpora into R data packages is a convenient way to ship
#' and share corpora, and to keep documentation and supplementary functionality 
#' with the data.
#' 
#' @param pkg Path to directory of data package.
#' @param verbose Logical, whether to be verbose.
#' 
#' @examples 
#' y <- tempdir()
#' pkg_create_cwb_dirs(pkg = y)
#' pkg_add_description(pkg = y, package = "reuters", description = "Reuters data package")
#' pkg_add_corpus(
#'   pkg = y, corpus = "REUTERS",
#'   registry = system.file(package = "RcppCWB", "extdata", "cwb", "registry")
#' )
#' pkg_add_gitattributes_file(pkg = y)
#' pkg_add_configure_scripts(pkg = y)
#' pkg_add_creativecommons_license(pkg = y)
#' @rdname pkg_utils
#' @name pkg_utils
NULL


#' @details \code{pkg_creage_cwb_dirs} will create the standard directory
#'   structure for storing registry files and indexed corpora within a package
#'   (\code{./inst/extdata/cwb/registry} and
#'   \code{./inst/extdata/cwb/indexed_corpora}, respectively).
#' @export pkg_create_cwb_dirs
#' @rdname pkg_utils
pkg_create_cwb_dirs = function(pkg = ".", verbose = TRUE){
  .check_pkg_dir(pkg)
  dirs_to_create <- c(
    "R",
    "man",
    "inst",
    file.path("inst", "extdata"),
    file.path("inst", "extdata", "cwb"),
    file.path("inst", "extdata", "cwb", "registry"),
    file.path("inst", "extdata", "cwb", "indexed_corpora")
  )
  for (dir in dirs_to_create) {
    new_dir <- file.path(pkg, dir)
    if (!file.exists(new_dir)){
      if (verbose) message("... creating directory: ", new_dir)
      dir.create(new_dir)
    } else {
      if (verbose) message("... directory already exists:", new_dir)
      dirs_to_create <- dirs_to_create[-which(dirs_to_create == "new_dir")]
    }
  }
  invisible(dirs_to_create)
}

#' @param corpus name of the CWB corpus to insert into the package
#' @param registry registry directory
#' @details \code{pkg_add_corpus} will add the corpus described in registry directory to
#' the package defined by \code{pkg}.
#' @rdname pkg_utils
#' @export pkg_add_corpus
#' @importFrom pbapply pblapply
pkg_add_corpus = function(pkg = ".", corpus, registry = Sys.getenv("CORPUS_REGISTRY"), verbose = TRUE){
  
  if (pkg %in% rownames(installed.packages())){
    pkg <- system.file(package = pkg)
    dest_registry <- file.path(pkg, "extdata", "cwb", "registry")
    data_dir <- file.path(pkg, "extdata", "cwb", "indexed_corpora")
  } else {
    dest_registry <- file.path(pkg, "inst", "extdata", "cwb", "registry")
    data_dir <- file.path(pkg, "inst", "extdata", "cwb", "indexed_corpora")
  }
  if (!file.exists(dest_registry)) stop(sprintf("registry directory '%s' does not exist", dest_registry))
  if (!file.exists(data_dir)) stop(sprintf("data directory '%s' does not exist", data_dir))
  target_dir <- file.path(data_dir, tolower(corpus))
  if (!file.exists(target_dir)){
    message("... directory for indexed corpus does not yet exist, creating: ", target_dir)
    dir.create(target_dir)
  }

  # copy registry file
  if (verbose) message("... copying registry file")
  file.copy(from = file.path(registry, tolower(corpus)), to = dest_registry)
  
  # copy files in dir for indexed corpora
  if (verbose) message("... copying data files")
  files_to_copy <- list.files(
    registry_file_parse(corpus, registry_dir = registry)[["home"]],
    full.names = TRUE
  )
  pblapply(files_to_copy, function(x) file.copy(from = x, to = target_dir))
  
  # adjust registry dir
  if (verbose) message("... adjusting paths in registry file")
  regdata <- registry_file_parse(corpus = corpus, registry = dest_registry)
  regdata[["home"]] <- target_dir
  registry_file_write(data = regdata, corpus = corpus, registry_dir = dest_registry)
  invisible( TRUE )
}


#' @details \code{add_configure_script} will add standardized and tested
#'   configure scripts \code{configure} for Linux and macOS, and
#'   \code{configure.win} for Windows to the top level directory of the data
#'   package, and file \code{setpaths.R} to \code{tools} subdirectory. The
#'   configuration mechanism ensures that the data directory is specified
#'   correctly in the registry files during the installation of the data
#'   package.
#' @export pkg_add_configure_scripts
#' @rdname pkg_utils
pkg_add_configure_scripts = function(pkg = "."){
  tool_dir <- file.path(pkg, "tools")
  if (!file.exists(tool_dir)) dir.create(tool_dir)
  file.copy(
    from = system.file(package = "cwbtools", "Rscript", "setpaths.R"),
    to = file.path(tool_dir, "setpaths.R")
  )
  writeLines(
    text = c('#!/bin/bash', '${R_HOME}/bin/Rscript ./tools/setpaths.R --args "$R_PACKAGE_DIR"'),
    con = file.path(pkg, "configure")
  )
  writeLines(
    text = c('#!/bin/sh', '${R_HOME}/bin${R_ARCH_BIN}/Rscript.exe ./tools/setpaths.R --args "$R_PACKAGE_DIR"'),
    con = file.path(pkg, "configure.win")
  )
  invisible( TRUE )
}


#' @details \code{pkg_add_description} will add a description file to the package.
#' @param package The package name, may not include special chars, and no underscores ('_').
#' @param author The author of the package, either character vector or object of class \code{person}.
#' @param version The version number of the corpus (defaults to "0.0.1")
#' @param date The date of creation, defaults to \code{Sys.Date()}.
#' @param maintainer Maintainer, R package style, either \code{character} vector or \code{person}.
#' @param description description of the data package.
#' @param license The license.
#' @seealso The \code{\link[usethis]{use_description}} function in the usethis-package will also create a DESCRIPTION file.
#' @export pkg_add_description
#' @rdname pkg_utils
pkg_add_description = function(pkg = ".", package = NULL, version = "0.0.1", date = Sys.Date(), author = "", maintainer = NULL, description = "", license = "", verbose = TRUE){
  if (file.exists(file.path(pkg, "DESCRIPTION"))){
    user_input <- readline(prompt = "DESCRIPTION file already exists. Proceed anyway (Y / any other key to abort)?")
    if (user_input != "Y"){
      message("file DESCRIPTION is not generated anew")
      return( invisible( FALSE ) )
    }
  }
  
  # sanity checks, to be extended
  stopifnot(
    class(author)[1] %in% c("character", "person"),
    length(date) == 1,
    is.character(date) || class(date)[1] == "Date" 
  )
  
  if (verbose) message("... creating DESCRIPTION file")
  if (is.null(pkg)){
    pkg <- basename(pkg)
    if (verbose) message("... package name not provided, using basename: ", pkg)
  }
  description_list <- c(
    Package = package,
    Type = "Package",
    Title = pkg,
    Version = version,
    Date = as.character(date),
    Author = paste0(as.character(author), collapse = "\n\t"),
    Maintainer = if (is.null(maintainer)) paste0(as.character(author), collapse = "\n\t") else paste0(as.character(maintainer), collapse = "\n\t"),
    Depends = "",
    LazyData = "yes",
    Description = description,
    License = license
  )
  desc <- paste0(names(description_list), ": ", unname(description_list))
  writeLines(desc, con = file.path(pkg, "DESCRIPTION"))
  invisible( TRUE )
}

#' @param file path to file with fulltext of Creative Commons license.
#' @details \code{pkg_add_creativecommons_license} will license information to
#'   the DESCRIPTION file, and move file LICENSE to top level directory of the
#'   package.
#' @rdname pkg_utils
#' @export pkg_add_creativecommons_license
#' @seealso \link[usethis]{use_mit_license}
pkg_add_creativecommons_license = function(pkg = ".", license = "CC-BY-NC-SA", file = system.file(package = "cwbtools", "txt", "licenses", "CC_BY-NC-SA_3.0.txt")){
  description_file <- file.path(pkg, "DESCRIPTION")
  if (!file.exists(description_file)){
    stop("The DESCRIPTION file does exist - please generate it first using the add_description method")
  }
  desc <- readLines(description_file)
  desc[grep("^Licen[cs]e", desc)] <- paste("License:", license, "| file LICENSE", sep = " ")
  writeLines(text = desc, con = file.path(pkg, "DESCRIPTION"))
  file.copy(from = file, to = file.path(pkg, "LICENSE"))
  invisible( TRUE )
}



#' @details \code{pkg_add_gitattributes_file} will add a file '.gitattributes'
#'   to the package. The file defines types of files that will be tracked by Git
#'   LFS, i.e. they will not be under conventional version control. This is
#'   suitable for large binary files, which is the scenario applicable for
#'   indexed corpus data.
#' @export pkg_add_gitattributes_file
#' @rdname pkg_utils
pkg_add_gitattributes_file = function(pkg = "."){
  extensions <- c(
    "RData", "cnt", "crc", "crx", "hcd", "huf", "syn",
    "lexicon", "idx", "srt", "rng", "avs", "avx"
  )
  writeLines(
    text = sprintf("*.%s filter=lfs diff=lfs merge=lfs -text", extensions),
    con = file.path(pkg, ".gitattributes")
  )
  invisible( TRUE )
}


.check_pkg_dir <- function(pkg){
  if (!file.exists(pkg)) stop("directory does not exist")
  if (!file.info(pkg)$isdir) stop("dir exists, but is not a directory")
}
