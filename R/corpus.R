#' Install and manage corpora. 
#' 
#' Utility functions to assist the installation of indexed CWB corpora. 
#' 
#' @details A data package with a CWB corpus is assumed to include a directory
#'   \code{/extdata/cwb/registry} for registry files and a directory
#'   \code{/extdata/cwb/indexed_corpora} for the indexed corpus files. The
#'   \code{corpus_install} function combines two steps necessary to install a
#'   CWB corpus. First, it calls \code{install.packages}, then it resets the
#'   path pointing to the directory with the indexed corpus files in the
#'   registry file. The package will be installed to the standard library
#'   directory for installing R packages (\code{.libPaths()[1]}). Another
#'   location can be used by stating the param 'lib' explicitly (see
#'   documentation for \code{\link{install.packages}}).
#'   The function can also be used to install a corpus from a password protected
#'   repository. Further parameters are handed over to install.packages, so you
#'   might add \code{method = "wget" extra = "--user donald --password duck"}.
#'   See examples how to check whether the directory has been set correctly.
#' @details 
#' \preformatted{
#'   .
#'   |- registry/
#'   |  |- corpus1 
#'   |  +- corpus2
#'   |
#'   + indexed_corpora/
#'     |- corpus1/
#'     |  |- file1
#'     |  |- file2
#'     |  +- file3
#'     |
#'     +- corpus2/
#'        |- file1
#'        |- file2
#'        +- file3
#' }
#' @param old Name of the (old) corpus.
#' @param new Name of the (new) corpus.
#' @param pkg Name of the data package.
#' @param repo URL of the repository.
#' @param tarball The URL or local path to a tarball with a CWB indexed corpus.
#' @param doi The DOI (Digital Object Identifier) of the corpus tarball at
#'   zenodo, presented as a hyperlink. (For testing purposes, use
#'   ("https://doi.org/10.5281/zenodo.3748858".)
#' @param lib Directory for R packages, defaults to \code{.libPaths()[1]}.
#' @param verbose Logical, whether to be verbose.
#' @param ask A \code{logical} value, whether to ask for user input when
#'   choices are required.
#' @param registry_dir Directory of registry.
#' @param corpus A CWB corpus.
#' @param tarfile Filename of tarball.
#' @param corpus_dir The directory that contains the data directories of indexed
#'   corpora.
#' @param ... Further parameters that will be passed into
#'   \code{install.packages}, if argument \code{tarball} is \code{NULL}, or into
#'   or \code{download.file}, if \code{tarball} is specified.
#' @param user A user name that can be specified to download a corpus from a password protected site.
#' @param password A password that can be specified to download a corpus from a password protected site. 
#' @name corpus_install
#' @return Logical value \code{TRUE} if installation has been successful.
#' @seealso For managing registry files, see \code{\link{registry_file_parse}}
#' for switching to a packaged corpus. 
#' @importFrom utils available.packages contrib.url install.packages
#' @importFrom utils installed.packages tar
#' @importFrom curl curl_download new_handle handle_setopt
#' @importFrom RCurl url.exists getURL
#' @importFrom jsonlite fromJSON
#' @importFrom utils menu
#' @importFrom stringi stri_enc_mark
#' @rdname corpus_utils
#' @export corpus_install
corpus_install <- function(pkg = NULL, repo = "https://PolMine.github.io/drat/", tarball = NULL, doi = NULL, lib = .libPaths()[1], registry_dir = cwbtools::cwb_registry_dir(), corpus_dir = cwb_corpus_dir(registry_dir), ask = interactive(), verbose = TRUE, user = NULL, password = NULL, ...){
  if (is.null(tarball)){
    if (isFALSE(is.null(pkg))){
      if (!pkg %in% utils::available.packages(contriburl = utils::contrib.url(repos = repo))) {
        stop(sprintf("package '%s' not available at repo '%s'", pkg, repo))
      }
      
      if (file.access(lib, "6") == -1){
        stop("You do not have write permissions for directory ", lib,
             ". Please run R with the required privileges, or provide another directory (param 'lib').")
      }
      install.packages(pkgs = pkg, repos = repo, lib = lib, ...)
      pkg_registry <- system.file(package = pkg, "extdata", "cwb", "registry")
      corpora <- list.files(pkg_registry)
      for (corpus in corpora){
        regdata <- registry_file_parse(corpus = corpus, registry_dir = pkg_registry)
        data_dir <- system.file(package = pkg, "extdata", "cwb", "indexed_corpora", corpus)
        if (regdata[["home"]] != data_dir){
          regdata[["home"]] <- data_dir
          registry_file_write(data = regdata, corpus = corpus, registry_dir = pkg_registry)
        }
      }
      return(invisible(TRUE))
    } else {
      # Turn DOI into tarball
      
      if (is.null(doi)) stop("If argument 'tarball' is NULL, either argument 'pkg' or argument 'doi' are required.")
      if (isFALSE(grepl("^.*?10\\.5281/zenodo\\.\\d+$", doi))){
        stop("Argument 'doi' is expected to offer a DOI (Digital Object Identifier) that refers to data",
             "hosted with zenodo, i.e. starting with 10.5281/zenodo.")
      }
      zenodo_info <- .zenodo_info(doi = doi)
      tarball <- grep(
        "^.*?_v\\d+\\.\\d+\\.\\d+\\.tar\\.gz$",
        zenodo_info[["files"]][["links"]][["self"]],
        value = TRUE
      )
      if (length(tarball) > 1L && isTRUE(ask)){
        userchoice <- utils::menu(
          choices = basename(tarball),
          title = "Several tarballs assumed to contain an indexed corpus are available. Which tarball shall be downloaded?"
        )
        tarball <- tarball[userchoice]
      }
    }
  }
  
  # Create CWB directory structure if necessary --------------
  
  cwb_dirs <- cwb_directories(registry_dir = registry_dir, corpus_dir = corpus_dir)
  if (any(is.null(cwb_dirs))) cwb_dirs <- create_cwb_directories() # will trigger interactive dialogue

  if (length(tarball) == 1L){
    
    # Ask user before overwriting existing corpus ----------------
    corpus <- gsub("^(.*?)(_v\\d+\\.\\d+\\.\\d+|)\\.tar\\.gz$", "\\1", basename(tarball))
    version <- if (exists("zenodo_info")){
      zenodo_info[["metadata"]][["version"]]
    } else {
      if (grepl("^.*?_v\\d+\\.\\d+\\.\\d+\\.tar\\.gz$", basename(tarball))){
        gsub("^.*?_(v\\d+\\.\\d+\\.\\d+)\\.tar\\.gz$", "\\1", basename(tarball))
      } else {
        "unknown"
      }
    }
    if (tolower(corpus) %in% list.files(cwb_dirs[["registry_dir"]])){
      regdata <- registry_file_parse(corpus = toupper(corpus), registry_dir = cwb_dirs[["registry_dir"]])
      version_old <- if ("version" %in% names(regdata[["properties"]])){
        regdata[["properties"]][["version"]]
      } else {
        "unknown"
      }
      if (ask){
        if (version_old == version){
          userinput <- menu(
            choices = c("Yes / continue", "No / abort"), 
            title = sprintf(
              paste(
                "Local %s version and the version of %s to be downloaded are identical (%s).",
                "Are you sure you want to proceed and to replace the local corpus by a fresh download?"
              ),
              toupper(corpus), toupper(corpus), version
            )
          )
          if (userinput != 1L){
            stop(sprintf("Aborting - existing version of %s remains unchanged. ", toupper(corpus)))
          }
        } else {
          userinput <- menu(
            choices = c("Yes / continue", "No / abort"), 
            title = sprintf(
              "Corpus %s (version: %s) is already installed. Do you want to replace it by version %s?",
              toupper(corpus), version_old, version
            )
          )
          if (userinput != 1L){
            stop(
              sprintf(paste("Aborting - existing version of %s remains unchanged. ",
                            "If you want to keep the existing version (%s), rename corpus using cwbtools::corpus_rename(), ",
                            "and call corpus_install() again."
              ),
              toupper(corpus), version
              )
            ) 
          }
        }
      }
      corpus_remove(corpus = toupper(corpus), registry_dir = cwb_dirs[["registry_dir"]], ask = ask)
    }
    
    # Now download corpus -------------------
    cwbtools_tmpdir <- file.path(normalizePath(tempdir(), winslash = "/"), "cwbtools_tmpdir", fsep = "/")
    if (file.exists(cwbtools_tmpdir)) unlink(cwbtools_tmpdir, recursive = TRUE)
    dir.create(cwbtools_tmpdir)
    corpus_tarball <- file.path(cwbtools_tmpdir, basename(tarball), fsep = "/")
    if (grepl("^http", tarball)){
      if (is.null(user)){
        if (!RCurl::url.exists(tarball)) stop("tarball is not available")
        if (.Platform$OS.type == "windows"){
          # use download.file because it is able to cope with murky user names / path names
          download.file(url = tarball, destfile = corpus_tarball, quiet = !verbose)
        } else {
          curl::curl_download(url = tarball, destfile = corpus_tarball, quiet = !verbose)
        }
      } else {
        if (is.null(password)) stop("If user name is offered, a password needs to be specified as well.")
        if (.Platform$OS.type == "windows"){
          # On Windows, download.file is used because curl will break if destfile includes
          # special characters. The user and the password are passed in as follows
          # "https://user:password@polmine.sowi.uni-due.de"
          prefix <- gsub("^(https://|http://).*?$", "\\1", tarball)
          tarball <- gsub("^(https://|http://)(.*?)$", "\\2", tarball)
          download.file(
            url = sprintf("%s%s:%s@%s", prefix, user, password, tarball),
            destfile = corpus_tarball
          )
        } else {
          curl::curl_download(
            url = tarball, destfile = corpus_tarball,
            handle = handle_setopt(new_handle(), userpwd = sprintf("%s:%s", user, password)),
            quiet = !verbose
          )
        }
      }
    } else {
      # If tqrball is not a URL, it is assumed to be present on the local machine
      if (!file.exists(tarball)) stop(sprintf("tarball '%s' not found locally", tarball))
      file.copy(from = tarball, to = corpus_tarball)
    }
    if (.Platform$OS.type == "windows" && stri_enc_mark(corpus_tarball) != "ASCII"){
      corpus_tarball <- utils::shortPathName(corpus_tarball)
    }
    if (.Platform$OS.type == "windows" && stri_enc_mark(cwbtools_tmpdir) != "ASCII"){
      cwbtools_tmpdir <- utils::shortPathName(cwbtools_tmpdir)
    }
    if (verbose) message("... extracting tarball")
    
    untar(tarfile = corpus_tarball, exdir = cwbtools_tmpdir)
    unlink(corpus_tarball)
    
    tmp_registry_dir <- file.path(normalizePath(cwbtools_tmpdir, winslash = "/"), "registry", fsep = "/")
    tmp_data_dir <- file.path(normalizePath(cwbtools_tmpdir, winslash = "/"), "indexed_corpora", fsep = "/")
    corpora <- list.files(tmp_registry_dir)
    
    for (corpus in corpora){

      registry_data <- registry_file_parse(corpus = corpus, registry_dir = tmp_registry_dir)
      
      home_dir <- file.path(tmp_data_dir, tolower(registry_data[["id"]]), fsep = "/")
      if (.Platform$OS.type == "windows" && stri_enc_mark(home_dir) != "ASCII")
        home_dir <- utils::shortPathName(home_dir)
      registry_data[["home"]] <- home_dir
      
      info_file <- file.path(registry_data[["home"]], basename(registry_data[["info"]]), fsep = "/")
      if (.Platform$OS.type == "windows" && stri_enc_mark(info_file) != "ASCII")
        home_dir <- utils::shortPathName(info_file)
      registry_data[["info"]] <- info_file
      if (!is.null(doi)) registry_data[["properties"]][["doi"]] <- doi
      if (!"version" %in% names(registry_data[["properties"]])){
        registry_data[["properties"]][["version"]] <- version
      }

      registry_file_write(data = registry_data, corpus = corpus, registry_dir = tmp_registry_dir)
      
      if (!is.null(pkg)){
        pkg_add_corpus(pkg = pkg, corpus = corpus, registry = tmp_registry_dir)
      } else {
        if (is.null(cwb_dirs[["registry_dir"]])) stop("Could not determine registry directory.")
        if (is.null(cwb_dirs[["corpus_dir"]])) stop("Could not determine corpus directory.")
        data_dir <- file.path(corpus_dir, tolower(corpus), fsep = "/")
        if (!file.exists(data_dir)) dir.create(data_dir)
        corpus_copy(
          corpus = corpus,
          registry_dir = tmp_registry_dir, data_dir = home_dir, # the temporary place
          registry_dir_new = registry_dir, data_dir_new = data_dir # final location
        )
      }
      
      # Resetting registry and copying registry file to polmineR's temporary registry, 
      # if polmineR is loaded necessary
      if (isNamespaceLoaded("polmineR")){
        file.copy(
          from = file.path(cwb_dirs[["registry_dir"]], tolower(corpus)),
          to = file.path(polmineR::registry(), tolower(corpus)),
          overwrite = TRUE
        )
        polmineR::registry_reset(registryDir = polmineR::registry())
      }

    }
    unlink(cwbtools_tmpdir, recursive = TRUE)
    return(invisible(NULL))
  } else {
    for (tarfile in tarball){
      corpus_install(
        tarball = tarfile,
        registry_dir = registry_dir,
        corpus_dir = corpus_dir,
        ask = ask,
        verbose = verbose,
        user = user,
        password = password,
        ...
      )
    }
  }
}

#' @details \code{corpus_packages} will detect the packages that include CWB
#'   corpora. Note that the directory structure of all installed packages is
#'   evaluated which may be slow on network-mounted file systems.
#' @rdname corpus_utils
#' @export corpus_packages
corpus_packages <- function(){
  matrices <- lapply(
    .libPaths(),
    function(lib){
      vectors <- lapply(
        list.files(path = lib),
        function(pkg){
          pkgdir <- file.path(lib, pkg)
          c(
            package = pkgdir,
            lib = lib,
            registry = system.file(package = pkg, "extdata", "cwb", "registry", lib.loc = lib)
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
  registry_old <- file.path(registry_dir, tolower(old), fsep = "/")
  registry_new <- file.path(registry_dir, tolower(new), fsep = "/")
  success <- file.rename(from = registry_old, to = registry_new)
  if (!success) stop("renaming the registry file failed")
  
  # rename data directory
  message("renaming data directory")
  regdata <- registry_file_parse(corpus = new, registry_dir = registry_dir)
  data_directory_old <- regdata[["home"]]
  data_directory_new <- file.path(dirname(data_directory_old), tolower(new), fsep = "/")
  success <- file.rename(from = data_directory_old, to = data_directory_new)
  if (!success) stop("renaming the data directory failed")
  
  # modify and save registry file
  message("modifying and saving registry file")
  regdata[["home"]] <- data_directory_new
  regdata[["id"]] <- tolower(new)
  registry_file_write(data = regdata, corpus = tolower(new), registry_dir = registry_dir)
  invisible(NULL)
}

#' @param ask A \code{logical} value, whether to ask user for confirmation
#'   before removing a corpus.
#' @details \code{corpus_remove} can be used to drop a corpus.
#' @rdname corpus_utils
#' @export corpus_remove
corpus_remove <- function(corpus, registry_dir = cwb_registry_dir(), ask = interactive()){
  
  stopifnot(tolower(corpus) %in% list.files(registry_dir)) # check that corpus exists
  
  reg <- registry_file_parse(corpus = tolower(corpus), registry_dir = registry_dir)
  data_directory <- reg[["home"]]
  if (ask){
    userinput <- menu(
      choices = c("Yes / continue", "No / abort"),
      title = sprintf("Are you sure you want to delete registry and data files for corpus '%s'?", corpus)
    )
    if (userinput != 1L) stop("Aborting")
  }
  for (x in list.files(data_directory, full.names = TRUE)) file.remove(x)
  file.remove(data_directory)
  file.remove(file.path(registry_dir, tolower(corpus), fsep = "/"))
  TRUE
}

#' @details \code{corpus_as_tarball} will create a tarball (.tar.gz-file) with
#'   two subdirectories. The 'registry' subdirectory will host the registry file
#'   for the tarred corpus. The data files will be put in a subdirectory with
#'   the corpus name in the 'indexed_corpora' subdirectory.
#' @rdname corpus_utils
#' @export corpus_as_tarball
corpus_as_tarball <- function(corpus, registry_dir, tarfile, verbose = TRUE){
  
  registry_file <- file.path(registry_dir, tolower(corpus), fsep = "/")
  if (!file.exists(registry_file))
    stop(
      sprintf("registry file for corpus '%s' does not exist in registry directory '%s'",
              corpus, registry_dir)
      )
  home_dir <- registry_file_parse(corpus = corpus, registry_dir = registry_dir)[["home"]]

  if (verbose) message("... moving registry file and data files to temporary directory for creating tarball")
  tmp_dir <- normalizePath(tempdir(), winslash = "/")
  archive_dir <- file.path(tmp_dir, tolower(corpus), fsep = "/")
  if (file.exists(archive_dir)) unlink(archive_dir, recursive = TRUE)
  dir.create(archive_dir)

  archive_registry_dir <- file.path(archive_dir, "registry", fsep = "/")
  archive_data_dir <- file.path(archive_dir, "indexed_corpora", fsep = "/")
  archive_corpus_dir <- file.path(archive_dir, "indexed_corpora", tolower(corpus), fsep = "/")
  dir.create(archive_registry_dir)
  dir.create(archive_data_dir)
  dir.create(archive_corpus_dir)
  
  file.copy(from = registry_file, to = file.path(archive_registry_dir, tolower(corpus), fsep = "/"))
  for (x in list.files(home_dir, full.names = TRUE)){
    file.copy(from = x, to = file.path(archive_corpus_dir, basename(x), fsep = "/"))
  }

  if (verbose) message("... creating tarball")
  old_wd <- getwd()
  on.exit(setwd(path.expand(old_wd)))
  setwd(archive_dir)
  tar(tarfile = tarfile, compression = "gzip")
  
  if (verbose) message("... cleaning up")
  setwd(path.expand(old_wd))
  unlink(archive_dir, recursive = TRUE)
  
  invisible( NULL )
}

#' @param data_dir The data directory where the files of the CWB corpus live.
#' @param registry_dir_new Target directory with for (new) registry files.
#' @param data_dir_new Target directory for corpus files.
#' @param progress Logical, whether to show a progress bar.
#' @details \code{corpus_copy} will create a copy of a corpus (useful for
#'   experimental modifications, for instance).
#' @export corpus_copy
#' @rdname corpus_utils
#' @examples
#' registry_file_new <- file.path(
#'   normalizePath(tempdir(), winslash = "/"),
#'   "cwb", "registry", "reuters", fsep = "/"
#'   )
#' if (file.exists(registry_file_new)) file.remove(registry_file_new)
#' corpus_copy(
#'   corpus = "REUTERS",
#'   registry_dir = system.file(package = "RcppCWB", "extdata", "cwb", "registry"),
#'   data_dir = system.file(
#'     package = "RcppCWB",
#'     "extdata", "cwb", "indexed_corpora", "reuters"
#'   )
#' )
#' unlink(file.path(
#'   normalizePath(tempdir(), winslash = "/"),
#'   "cwb", fsep = "/"),
#'   recursive = TRUE)
corpus_copy <- function(
  corpus, registry_dir, data_dir = NULL,
  registry_dir_new = file.path(normalizePath(tempdir(), winslash = "/"), "cwb", "registry", fsep = "/"),
  data_dir_new = file.path(normalizePath(tempdir(), winslash = "/"), "cwb", "indexed_corpora", tolower(corpus), fsep = "/"),
  verbose = interactive(),
  progress = TRUE
  ){
  
  registry_file_old <- file.path(registry_dir, tolower(corpus), fsep = "/")
  if (!file.exists(registry_file_old)) stop(sprintf("Aborting - registry file %s does not exist.", registry_file_old))
  if (is.null(data_dir)) data_dir <- registry_file_parse(corpus = corpus, registry_dir = registry_dir)[["home"]]
  
  registry_file_new <- file.path(registry_dir_new, tolower(corpus), fsep = "/")
  if (file.exists(registry_file_new)) stop(sprintf("Aborting - registry file %s already exists in target registy", registry_file_new))
  
  if (!dir.exists(registry_dir_new)) dir.create(registry_dir_new, recursive = TRUE)
  if (!dir.exists(data_dir_new)) dir.create(data_dir_new, recursive = TRUE)

  if (verbose) message(sprintf("... creating copy of adjusted registry file"))
  rf <- registry_file_parse(corpus = corpus, registry_dir = registry_dir)
  rf[["home"]] <- data_dir_new
  registry_file_write(rf, corpus = corpus, registry_dir = registry_dir_new)
  
  if (verbose) message(sprintf("... copying data files"))
  data_files_to_copy <- list.files(data_dir, full.names = TRUE)
  if (progress) pb <- txtProgressBar(min = 1L, max = length(data_files_to_copy), style = 3)
  for (i in 1L:length(data_files_to_copy)){
    if (progress) setTxtProgressBar(pb, value = i)
    file.copy(
      from = data_files_to_copy[i],
      to = file.path(data_dir_new, basename(data_files_to_copy[i]), fsep = "/")
      )
  }
  if (progress) close(pb)
  invisible(NULL)
}





#' @param to Character string describing the target encoding of the corpus.
#' @param skip A character vector with s_attributes to skip.
#' @export corpus_recode
#' @rdname corpus_utils
#' @examples 
#' corpus <- "REUTERS"
#' pkg <- "RcppCWB"
#' s_attr <- "places"
#' Q <- '"oil"'
#' 
#' registry_dir_src <- system.file(package = pkg, "extdata", "cwb", "registry")
#' data_dir_src <- system.file(package = pkg, "extdata", "cwb", "indexed_corpora", tolower(corpus))
#' 
#' registry_dir_tmp <- file.path(
#'   normalizePath(tempdir(), winslash = "/"),
#'   "cwb", "registry", fsep = "/"
#' )
#' registry_file_tmp <- file.path(registry_dir_tmp, tolower(corpus), fsep = "/")
#' data_dir_tmp <- file.path(
#'   normalizePath(tempdir(), winslash = "/"),
#'   "cwb", "indexed_corpora", tolower(corpus), fsep = "/"
#' )
#' 
#' if (file.exists(registry_file_tmp)) file.remove(registry_file_tmp)
#' if (!dir.exists(data_dir_tmp)){
#'    dir.create(data_dir_tmp, recursive = TRUE)
#' } else {
#'   if (length(list.files(data_dir_tmp)) > 0L)
#'     file.remove(list.files(data_dir_tmp, full.names = TRUE))
#' }
#' 
#' corpus_copy(
#'   corpus = corpus,
#'   registry_dir = registry_dir_src,
#'   data_dir = data_dir_src,
#'   registry_dir_new = registry_dir_tmp,
#'   data_dir_new = data_dir_tmp
#' )
#' 
#' RcppCWB::cl_charset_name(corpus = corpus, registry = registry_dir_tmp)
#' 
#' corpus_recode(
#'   corpus = corpus,
#'   registry_dir = registry_dir_tmp,
#'   data_dir = data_dir_tmp,
#'   to = "UTF-8"
#' )
#' 
#' RcppCWB::cl_delete_corpus(corpus = corpus, registry = registry_dir_tmp)
#' RcppCWB::cqp_initialize(registry_dir_tmp)
#' RcppCWB::cl_charset_name(corpus = corpus, registry = registry_dir_tmp)
#' 
#' n_strucs <- RcppCWB::cl_attribute_size(
#'   corpus = corpus, attribute = s_attr, attribute_type = "s", registry = registry_dir_tmp
#' )
#' strucs <- 0L:(n_strucs - 1L)
#' struc_values <- RcppCWB::cl_struc2str(
#'   corpus = corpus, s_attribute = s_attr, struc = strucs, registry = registry_dir_tmp
#' )
#' speakers <- unique(struc_values)
#' 
#' Sys.setenv("CORPUS_REGISTRY" = registry_dir_tmp)
#' if (RcppCWB::cqp_is_initialized()) RcppCWB::cqp_reset_registry() else RcppCWB::cqp_initialize()
#' RcppCWB::cqp_query(corpus = corpus, query = Q)
#' cpos <- RcppCWB::cqp_dump_subcorpus(corpus = corpus)
#' ids <- RcppCWB::cl_cpos2id(
#'   corpus = corpus, p_attribute = "word", registry = registry_dir_tmp, cpos = cpos
#' )
#' str <- RcppCWB::cl_id2str(
#'   corpus = corpus, p_attribute = "word", registry = registry_dir_tmp, id = ids
#' )
#' unique(str)
#' 
#' unlink(file.path(normalizePath(tempdir(), winslash = "/"), "cwb", fsep = "/"), recursive = TRUE)
corpus_recode <- function(corpus, registry_dir = Sys.getenv("CORPUS_REGISTRY"), data_dir = registry_file_parse(corpus, registry_dir)[["home"]], skip = character(), to = c("latin1", "UTF-8"), verbose = TRUE){
  
  if (to == "UTF-8") to <- "UTF8"
  
  regdata <- registry_file_parse(corpus = corpus, registry_dir = registry_dir)
  if (regdata$properties[["charset"]] == to) stop("Aborting - target encoding identical with present encoding.")
  
  for (s_attr in regdata$s_attributes){
    if (!s_attr %in% skip){
      if (verbose) message("Recoding s-attribute: ", s_attr)
      s_attribute_recode(
        data_dir = data_dir,
        s_attribute = s_attr,
        from = regdata$properties[["charset"]],
        to = toupper(to)
      )
    }
  }
  
  for (p_attr in regdata$p_attributes){
    if (verbose) message("Recoding p-attribute: ", p_attr)
    p_attribute_recode(
      data_dir = data_dir,
      p_attribute = p_attr,
      from = regdata$properties[["charset"]],
      to = toupper(to)
    )
  }
  
  regdata$properties[["charset"]] <- tolower(to)
  registry_file_write(
    data = regdata,
    corpus = corpus,
    registry_dir = registry_dir
    )
  
  invisible(NULL)
}
