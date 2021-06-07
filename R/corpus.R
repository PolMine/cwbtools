#' Install and manage corpora.
#'
#' Utility functions to assist the installation and management of indexed CWB
#' corpora.
#'
#' @details A CWB corpus consists a set of binary files with corpus data
#'   kept together in a data directory, and a registry file, which is a
#'   plain test file that details the corpus id, corpus properties,
#'   structural and positional attributes. The registry file also specifies
#'   the path to the corpus data directory. Typically, the registry directory
#'   and a corpus directory with the data directories for individual corpora
#'   are within one parent folder (which might be called "cwb" by default).
#'   See the following stylized directory structure.
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
#' @details The \code{corpus_install} function will assist the installation of a
#'   corpus. The following scenarios are offered:
#' \itemize{
#'   \item{If argument \code{tarball} is a local tarball, the tarball will
#'   be extracted and files will be moved.}
#'   \item{If \code{tarball} is a URL, the tarball will be downloaded from
#'   the online location. It is possible to state user credentials using the
#'   arguments \code{user} and \code{password}. Then the aforementioned
#'   installation (scenario 1) is executed. If argument \code{pkg} is the
#'   name of an installed package, corpus files will be moved into this
#'   package.}
#'   \item{If argument \code{doi} is Document Object Identifier (DOI), the URL
#'   from which a corpus tarball can be downloaded is derived from the
#'   information available at that location. The tarball is downloaded and the
#'   corpus installed. If argument \code{pkg} is defined, files will be moved
#'   into a R package, the syste registry and corpus directories are used
#'   otherwise. Note that at this stage, it is assumed that the DOI has been
#'   awarded by \href{https://zenodo.org/}{Zenodo}}
#'   \item{If argument \code{pkg} is provided and specifies an R package (and
#'   \code{tarball} is \code{NULL}), the corpus package available at a
#'   CRAN-style repository specified by argument \code{repo} will be installed.
#'   Internally, the \code{install.packages} function is called and further
#'   arguments can be passed into this function call. This can be used to pass
#'   user credentials, e.g. by adding \code{method = "wget" extra = "--user
#'   donald --password duck"}.
#'   }
#' }
#' If the corpus to be installed is already available, a dialogue will ask the
#' user whether an existing corpus shall be deleted and installed anew, if
#' argument \code{ask} is \code{TRUE}.
#' @param old Name of the (old) corpus.
#' @param new Name of the (new) corpus.
#' @param pkg Name of the data package.
#' @param repo URL of the repository.
#' @param tarball URL,  S3-URI or local filename of a tarball with a CWB indexed
#'   corpus. If \code{NULL} (default) and argument \code{doi} is stated, the
#'   whereabouts of a corpus tarball will be derived from DOI.
#' @param doi The DOI (Digital Object Identifier) of a corpus deposited at
#'   Zenodo (e.g. "10.5281/zenodo.3748858".)
#' @param lib Directory for R packages, defaults to \code{.libPaths()[1]}.
#' @param verbose Logical, whether to be verbose.
#' @param ask A \code{logical} value, whether to ask for user input when
#'   choices are required.
#' @param registry_dir The corpus registry directory. If missing, the result of
#'   `cwb_registry_dir()`.
#' @param corpus A CWB corpus.
#' @param tarfile Filename of tarball.
#' @param checksum A length-one \code{character} vector with a MD5 checksum to
#'   check for the integrity of a downloaded tarball. If the tarball is
#'   downloaded from Zenodo by stating a DOI (argument \code{doi}), the checksum
#'   included in the metadata for the record is used for the check.
#' @param corpus_dir The directory that contains the data directories of indexed
#'   corpora. If missing, the value of `cwb_corpus_dir()` will be used.
#' @param ... Further parameters that will be passed into
#'   \code{install.packages}, if argument \code{tarball} is \code{NULL}, or into
#'   or \code{download.file}, if \code{tarball} is specified.
#' @param user A user name that can be specified to download a corpus from a password protected site.
#' @param password A password that can be specified to download a corpus from a password protected site.
#' @name corpus_install
#' @return Logical value \code{TRUE} if installation has been successful, or \code{FALSE} if not.
#' @seealso For managing registry files, see \code{\link{registry_file_parse}}
#' for switching to a packaged corpus.
#' @importFrom utils available.packages contrib.url install.packages
#' @importFrom utils installed.packages tar
#' @importFrom curl curl_download new_handle handle_setopt
#' @importFrom httr http_error
#' @importFrom jsonlite fromJSON
#' @importFrom utils menu
#' @importFrom tools md5sum file_path_sans_ext
#' @importFrom stringi stri_enc_mark
#' @importFrom tools file_path_sans_ext
#' @importFrom zen4R ZenodoManager
#' @importFrom cli cli_rule cli_alert_success cli_process_start cli_process_done
#'   cli_alert_info col_cyan cli_alert_danger cli_text col_blue col_red cli_alert_warning cli_process_failed
#' @importFrom RcppCWB cqp_is_initialized cqp_get_registry cqp_reset_registry
#' @rdname corpus_utils
#' @export corpus_install
corpus_install <- function(pkg = NULL, repo = "https://PolMine.github.io/drat/", tarball = NULL, doi = NULL, checksum = NULL, lib = .libPaths()[1], registry_dir, corpus_dir, ask = interactive(), verbose = TRUE, user = NULL, password = NULL, ...){

  if (missing(registry_dir)) registry_dir <- cwb_registry_dir(verbose = FALSE)
  # cat(registry_dir)
  print(registry_dir)
  if (missing(corpus_dir)) corpus_dir <- cwb_corpus_dir(verbose = FALSE)
  # cat(corpus_dir)
  print(corpus_dir)


  modify_renviron <- FALSE

  if (is.null(tarball)){
    if (isFALSE(is.null(pkg))){
      # A pkg is stated
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
      if (grepl("https://doi.org/", doi)) doi <- gsub("https://doi.org/", "", doi)
      if (isFALSE(grepl("^.*?10\\.5281/zenodo\\.\\d+$", doi))){
        warning("argument 'doi' is expected to offer a DOI (Digital Object Identifier) that refers to data",
             "hosted with Zenodo, i.e. starting with 10.5281/zenodo")
        return(invisible(FALSE))
      }

      if (verbose) cli_rule("Resolve DOI")
      if (verbose) cli_process_start("get Zenodo record referred to by DOI")
      tryCatch(
        zenodo_record <- ZenodoManager$new()$getRecordByDOI(doi = doi),
        error = function(e){
          if (verbose){
            cli_process_failed()
          } else {
            cli_alert_danger(sprintf("'no Zenodo record found for DOI '%s'", doi))
          }
        }
      )
      if (!exists("zenodo_record")){
        return(invisible(FALSE)) # unlikely scenario that can only result from error detected by tryCatch()
      } else if (is.null(zenodo_record)){
        if (verbose) cli_alert_danger(sprintf("no Zenodo record found for DOI '%s'", doi))
        return(invisible(FALSE))
      } else {
        if (verbose) cli_process_done()
      }

      zenodo_files <- sapply(zenodo_record[["files"]], function(x) x[["links"]][["download"]])
      tarball <- grep("^.*?_(v|)\\d+\\.\\d+\\.\\d+\\.tar\\.gz$", zenodo_files, value = TRUE)

      if (length(tarball) > 1L && isTRUE(ask)){
        userchoice <- utils::menu(
          choices = basename(tarball),
          title = "Several tarballs assumed to contain an indexed corpus are available. Which tarball shall be downloaded?"
        )
        tarball <- tarball[userchoice]
      }
      if (verbose) cli_alert_info(sprintf("tarball to be downloaded: %s", col_blue(basename(tarball))))
      zenodo_file_record <- zenodo_record[["files"]][[which(zenodo_files == tarball)]]
    }
  }

  # Create CWB directory structure if necessary --------------

  if (verbose) cli_rule("Get CWB directories")

  cwb_dirs <- cwb_directories(registry_dir = registry_dir, corpus_dir = corpus_dir, verbose = verbose)
  if (any(is.null(cwb_dirs))){
    cwb_dirs <- create_cwb_directories(ask = ask)
    modify_renviron <- TRUE
  }

  if (length(tarball) == 1L){

    # Ask user before overwriting existing corpus ----------------
    corpus <- gsub("^(.*?)_(v|)(\\d+\\.\\d+\\.\\d+|)\\.tar\\.gz$", "\\1", basename(tarball))
    version <- if (exists("zenodo_record")){
      zenodo_record[["metadata"]][["version"]]
    } else {
      if (grepl("^.*?\\d+\\.\\d+\\.\\d+\\.tar\\.gz$", basename(tarball))){
        gsub("^.*?_(v|)(\\d+\\.\\d+\\.\\d+)\\.tar\\.gz$", "v\\2", basename(tarball))
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
        cli_rule("Remove existing corpus")
        if (version_old == version){
          cli_text(sprintf(
              paste(
                "Local %s version and the version of %s to be downloaded are identical (%s).",
                "If you proceed, the local corpus will be replaced by a fresh download."
              ),
              col_red(toupper(corpus)), col_red(toupper(corpus)), col_blue(version)
            )
          )
        } else {
          cli_text(
            sprintf(
              "Corpus %s (version: %s) is already installed. If you proceed, it will be replaced by version %s.",
              toupper(corpus), version_old, version
            )
          )
        }
      }
      purged <- corpus_remove(corpus = toupper(corpus), registry_dir = cwb_dirs[["registry_dir"]], ask = ask)
      if (isFALSE(purged)) return(invisible(FALSE))
    }

    # Now download corpus -------------------
    cwbtools_tmpdir <- file.path(normalizePath(tempdir(), winslash = "/"), "cwbtools_tmpdir", fsep = "/")
    if (file.exists(cwbtools_tmpdir)) unlink(cwbtools_tmpdir, recursive = TRUE)
    dir.create(cwbtools_tmpdir)
    if (grepl("^http", tarball)){
      if (verbose) cat_rule("Download corpus tarball")
      if (verbose) cli_alert_info(sprintf("download corpus tarball {col_cyan('%s')}", basename(tarball)))

      corpus_tarball <- file.path(cwbtools_tmpdir, basename(tarball), fsep = "/")

      if (is.null(user)){

        tryCatch(
          tarball_not_available <- http_error(tarball),
          error = function(e) cli_alert_danger("could not connect to server - check internet connection")
        )
        if (!exists("tarball_not_available")) return(invisible())
        if (isTRUE(tarball_not_available)){
          cli_alert_danger(sprintf("the corpus tarball is not available: '%s'", tarball))
          return(invisible(FALSE))
        }

        if (.Platform$OS.type == "windows"){
          # Use download.file() because it is able to cope with murky user names / path names
          # Progress messages are better of download.file(). However curl_download() is more
          # robust, so we use it when download.file() has failed.
          trystatus <- try(
            download.file(
              url = tarball,
              destfile = corpus_tarball,
              quiet = !verbose,
              cacheOK = FALSE,
              method = if (isTRUE(capabilities("libcurl"))) "libcurl" else getOption("download.file.method")
            )
          )
          if (is(trystatus)[[1]] == "try-error"){
            retry <- TRUE
          } else {
            retry <- if (trystatus == 1L) TRUE else FALSE
          }
          if (isTRUE(retry)) curl::curl_download(url = tarball, destfile = corpus_tarball, quiet = !verbose)
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
      if (verbose) cli_alert_success(sprintf("download corpus tarball {col_cyan('%s')} ... done", basename(tarball)))
      if (exists("zenodo_file_record")){
        if (!is.null(checksum)){
          if (verbose) cli_alert_warning("argument checksum is not NULL but md5 checksum can be derived from Zenodo record - using checksum issued by Zenodo")
        }
        checksum <- zenodo_file_record[["checksum"]]
      }
      if (isFALSE(is.null(checksum))){
        if (verbose){
          msg <- sprintf("check md5 checksum for tarball %s (expected %s)", basename(tarball), checksum)
          cli_process_start(msg)
        }
        corpus_tarball_checksum <- tools::md5sum(corpus_tarball)
        if (corpus_tarball_checksum == checksum){
          if (verbose) cli_process_done()
        } else {
          if (verbose) cli_process_failed()
          cli_alert_danger(
            sprintf(
              "md5 checksum of downloaded tarball '%s' is '%s', but Zenodo archive md5 checksum is '%s'",
              basename(corpus_tarball), corpus_tarball_checksum, checksum
            )
          )
          return(invisible(FALSE))
        }
      } else if (FALSE){
        if (verbose) cli_alert_warning("no md5 checksum provided or available to check downloaded tarball - note that checking the integrity of downloaded data is good practice")
      }
    } else if (grepl("^[sS]3:", tarball)){
      if (!requireNamespace("aws.s3", quietly = TRUE)){
        stop(
          "To download a corpus tarball from S3, package 'aws.s3' is required. ",
          "Package 'aws.s3' is not installed. ",
          "Install package 'aws.s3' by calling install.packages('aws.s3') and retry."
        )
      }
      if (verbose) cat_rule("Download corpus tarball from S3")
      if (verbose) cli_alert_info(sprintf("download corpus tarball {col_cyan('%s')}", basename(tarball)))
      corpus_tarball <- file.path(cwbtools_tmpdir, basename(tarball), fsep = "/")

      bucketname <- aws.s3::get_bucketname(tarball)
      location <- aws.s3::get_location(bucketname)
      obj <- aws.s3::get_objectkey(tarball)

      if (isFALSE(aws.s3::object_exists(obj, bucket = bucketname, region = location))){
        warning("S3 object does not exist - aborting")
        return(invisible(FALSE))
      }

      aws.s3::save_object(
        object = obj,
        file = corpus_tarball, # temporary local destfile
        bucket = bucketname,
        region = location,
        show_progress = TRUE
      )
    } else {
      # If tarball is not a URL, it is assumed to be present on the local machine
      if (!file.exists(tarball)) stop(sprintf("tarball '%s' not found locally", tarball))
      corpus_tarball <- tarball
    }

    if (.Platform$OS.type == "windows"){
      if (stri_enc_mark(corpus_tarball) != "ASCII") corpus_tarball <- utils::shortPathName(corpus_tarball)
      if (stri_enc_mark(cwbtools_tmpdir) != "ASCII") cwbtools_tmpdir <- utils::shortPathName(cwbtools_tmpdir)
    }

    if (verbose) cli_process_start("extract tarball")
    untar(tarfile = corpus_tarball, exdir = cwbtools_tmpdir)
    if (verbose) cli_process_done()

    if (tarball != corpus_tarball){
      # Remove corpus_tarball only if it has been downloaded
      if (verbose) cli_process_start("remove tarball")
      unlink(corpus_tarball)
      if (verbose) cli_process_done()
    }

    # The registry directory and the data directory might be within a subdirectory
    # with the same name of the tarball (or name of tarball without date)
    subdir1 <- file_path_sans_ext(basename(tarball), compression = TRUE)
    subdir2 <- gsub("^(.*?)(-|_)\\d{4}-\\d{2}-\\d{2}$", "\\1", subdir1)

    if (dir.exists(file.path(normalizePath(cwbtools_tmpdir, winslash = "/"), subdir1, fsep = "/"))){
      subdir <- subdir1
    } else if (dir.exists(file.path(normalizePath(cwbtools_tmpdir, winslash = "/"), subdir2, fsep = "/"))){
      subdir <- subdir2
    } else {
      subdir <- ""
    }

    tmp_registry_dir <- file.path(
      normalizePath(cwbtools_tmpdir, winslash = "/"),
      subdir, "registry", fsep = "/"
    )
    filenames <- list.files(file.path(normalizePath(cwbtools_tmpdir, winslash = "/"), subdir))
    dirname <- if ("indexed_corpora" %in% filenames) "indexed_corpora" else "data"
    tmp_data_dir <- file.path(
      normalizePath(cwbtools_tmpdir, winslash = "/"),
      subdir, dirname, fsep = "/"
    )
    corpora <- list.files(tmp_registry_dir)

    for (corpus in corpora){

      registry_data <- registry_file_parse(corpus = corpus, registry_dir = tmp_registry_dir)

      # In corpus tarball, directory for binary files is not necessarily name of corpus
      # we assume that last (sub-)directory stated in HOME is correct
      tmp_home_dir <- file.path(tmp_data_dir, basename(registry_data[["home"]]), fsep = "/")
      if (isFALSE(dir.exists(tmp_home_dir))){
        # Maybe data is not in another
        tmp_home_dir <- tmp_data_dir
        if (isFALSE(dir.exists(tmp_home_dir))) stop("don't know where to look for data directory")
      }

      if (.Platform$OS.type == "windows" && stri_enc_mark(tmp_home_dir) != "ASCII")
        tmp_home_dir <- utils::shortPathName(tmp_home_dir)
      registry_data[["home"]] <- tmp_home_dir

      info_file <- file.path(registry_data[["home"]], basename(registry_data[["info"]]), fsep = "/")
      if (.Platform$OS.type == "windows" && stri_enc_mark(info_file) != "ASCII")
        info_file <- utils::shortPathName(info_file)
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
        data_dir_target <- file.path(cwb_dirs[["corpus_dir"]], tolower(corpus), fsep = "/")
        if (!file.exists(data_dir_target)) dir.create(data_dir_target)
        corpus_copy(
          corpus = corpus,
          registry_dir = tmp_registry_dir,
          data_dir = tmp_home_dir, # the temporary place
          registry_dir_new = cwb_dirs[["registry_dir"]],
          data_dir_new = data_dir_target, # final location
          verbose = verbose,
          remove = TRUE
        )
      }

      # Make corpus available if RcppCWB has been initialized
      if (RcppCWB::cqp_is_initialized()){
        srcfile <- file.path(cwb_dirs[["registry_dir"]], tolower(corpus))
        destfile <- file.path(RcppCWB::cqp_get_registry(), tolower(corpus))
        if (srcfile != destfile){
          file.copy(from = srcfile, to = destfile, overwrite = TRUE)
        }
        RcppCWB::cqp_reset_registry(registry = cwb_dirs[["registry_dir"]])
      }
    }
    unlink(cwbtools_tmpdir, recursive = TRUE)

    # Check whether newly installed corpoa can be loaded
    # for (corpus_id in corpora){
    #   corpus_testload(corpus = corpus_id, registry_dir = cwb_dirs[["registry_dir"]])
    # }

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
  if (isTRUE(modify_renviron) && isTRUE(ask)){
    cli_alert_info(
      paste(
        sprintf("You have created the registry directory {.path %s} anew.", cwb_dirs[["registry_dir"]]),
        "The environment variable {.envvar CORPUS_REGISTRY} needs to refer to this directory to make the newly installed corpus available.",
        sprintf("You can call {.code Sys.getenv(CORPUS_REGISTRY=\"%s\")} whenever the environment variable needs to be set.", cwb_dirs[["registry_dir"]]),
        "It is necessary to set the {.envvar CORPUS_REGISTRY} environment variable before loading polmineR.",
        sprintf("To make this setting persist across sessions, you can add the line {.code CORPUS_REGISTRY=%s} to the {.path .Renviron} file.", cwb_dirs[["registry_dir"]]),
        collapse = " "
      ),
      wrap = TRUE
    )
    cat("\n")
    answer <- menu(
      choices = c("Yes", "No"),
      title = cli_text("Do you want to set the {.envvar CORPUS_REGISTRY} environment variable in the {.path .Renviron}-file now?")
    )
    if (answer == 1){
      renviron_file <- use_corpus_registry_envvar(registry_dir = cwb_dirs[["registry_dir"]])
    } else {
      cli_alert_warning(
        paste(
          "The {.path .Renviron} file will not be changed.",
          "Remember to set the {.envvar CORPUS_REGISTRY} environment variable temporarily ",
          sprintf('by calling {.code Sys.getenv(CORPUS_REGISTRY = "%s")}', cwb_dirs[["registry_dir"]]),
          "or permanently by defining it in the {.path .Renviron} file!"
        ),
        wrap = TRUE
      )
    }

  }
  invisible(TRUE)
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
#' @details \code{corpus_remove} can be used to delete a corpus.
#' @rdname corpus_utils
#' @importFrom cli cli_alert_success
#' @export corpus_remove
corpus_remove <- function(corpus, registry_dir, ask = interactive(), verbose = TRUE){

  if (missing(registry_dir)) registry_dir <- cwb_registry_dir(verbose = FALSE)

  stopifnot(tolower(corpus) %in% list.files(registry_dir)) # check that corpus exists

  reg <- registry_file_parse(corpus = tolower(corpus), registry_dir = registry_dir)
  data_directory <- reg[["home"]]
  if (ask){
    userinput <- menu(
      choices = c("Yes", "No"),
      title = sprintf("Are you sure you want to delete registry and data files for corpus '%s'?", cli::col_red(corpus))
    )
    if (userinput != 1L){
      cli_alert_warning("User abort")
      return(invisible(FALSE))
    }
  }
  for (x in list.files(data_directory, full.names = TRUE)) file.remove(x)
  file.remove(data_directory)
  file.remove(file.path(registry_dir, tolower(corpus), fsep = "/"))
  if (verbose) cli_alert_success("corpus has been removed (registry and data files)")
  invisible(TRUE)
}

#' @details \code{corpus_as_tarball} will create a tarball (.tar.gz-file) with
#'   two subdirectories. The 'registry' subdirectory will host the registry file
#'   for the tarred corpus. The data files will be put in a subdirectory with
#'   the corpus name in the 'indexed_corpora' subdirectory.
#' @rdname corpus_utils
#' @export corpus_as_tarball
corpus_as_tarball <- function(corpus, registry_dir, data_dir, tarfile, verbose = TRUE){

  registry_file <- file.path(registry_dir, tolower(corpus), fsep = "/")
  if (!file.exists(registry_file)){
    stop(
      sprintf(
        "registry file for corpus '%s' does not exist in registry directory '%s'",
        corpus, registry_dir)
    )
  }

  home_dir <- if (missing(data_dir)){
    registry_file_parse(corpus = corpus, registry_dir = registry_dir)[["home"]]
  } else {
    data_dir
  }

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
#' @param remove A \code{logical} value, whether to remove orginal files after
#'   having created the copy.
#' @param progress Logical, whether to show a progress bar.
#' @details \code{corpus_copy} will create a copy of a corpus (useful for
#'   experimental modifications, for instance).
#' @importFrom cli make_spinner ansi_with_hidden_cursor cli_alert_success
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
  remove = FALSE,
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

  spinner <- make_spinner(template = sprintf("{spin} copy corpus data files for corpus {col_cyan('%s')} to target data directory", toupper(corpus)))
  copy_with_spinner <- function(){
    lapply(
      list.files(data_dir, full.names = TRUE),
      function(f){
        file.copy(from = f, to = file.path(data_dir_new, basename(f), fsep = "/"))
        spinner$spin()
        if (remove) file.remove(f)
      }
    )
    spinner$finish()
  }
  ansi_with_hidden_cursor(copy_with_spinner())
  if (verbose) cli_alert_success(sprintf("copy corpus data files for corpus {col_cyan('%s')} to target data directory ... done", toupper(corpus)))

  if (verbose) cli_process_start(sprintf("parse registry file for corpus {col_cyan('%s')}", toupper(corpus)))
  rf <- registry_file_parse(corpus = corpus, registry_dir = registry_dir)
  if (verbose) cli_process_done()

  # It is a common mistake that the info file is not stated correctly in the registry file,
  # so this is a forgiving solution to remedy errors
  info_file_old <- file.path(data_dir, basename(rf[["info"]]))
  if (!file.exists(info_file_old)){
    info_file_guessed <- grep(
      "^.*?/(|\\.)info(\\.md|)$",
      list.files(data_dir, full.names = TRUE),
      value = TRUE
    )
    if (length(info_file_guessed) > 0L){
      if (verbose){
        cli_alert_info(
          paste(
            "info file stated in registry does not exist,",
            "using the following file which likely to be an info file:",
            sprintf("{.path %s}", info_file_guessed[1])
          )
        )
      }
      rf[["info"]] <- info_file_guessed[1]
    } else {
      if (verbose) cli_alert_warning("no info file found")
    }
  }

  if (verbose) cli_process_start("update registry data and save registry file")
  rf[["home"]] <- data_dir_new
  rf[["info"]] <- file.path(data_dir_new, basename(rf[["info"]]))
  registry_file_write(rf, corpus = corpus, registry_dir = registry_dir_new)
  if (verbose) cli_process_done()

  invisible(TRUE)
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


#' @importFrom cli cli_process_start
#' @export corpus_testload
#' @rdname corpus_utils
corpus_testload <- function(corpus, registry_dir = Sys.getenv("CORPUS_REGISTRY"), verbose = TRUE){

  msg <- sprintf("check whether corpus %s can be loaded", corpus)
  cli_process_start(msg, msg_done = paste(msg, "... OK"))

  # Initializing CQP is a workaround to address an odd issue that cqp_get_registry()
  # will not work as expected if cqp_initializ() has not been called before,
  # see https://github.com/PolMine/RcppCWB/issues/14
  if (isFALSE(RcppCWB::cqp_is_initialized())){
    RcppCWB::cqp_initialize(registry = Sys.getenv("CORPUS_REGISTRY"))
  }

  if (isFALSE(identical(RcppCWB::cqp_get_registry(), registry_dir))){
    registry_to_restore <- RcppCWB::cqp_get_registry()
    RcppCWB::cqp_reset_registry(registry = registry_dir)
  } else {
    registry_to_restore <- NA
  }

  n <- RcppCWB::cl_attribute_size(
    corpus = corpus,
    attribute = "word",
    attribute_type = "p",
    registry = registry_dir
  )

  if (n > 0L){
    cli::cli_process_done()
    retval <- TRUE
  } else {
    cli::cli_process_failed()
    retval <- FALSE
  }

  if (isFALSE(is.na(registry_to_restore))){
    # Check whether dir exists because RcppCWB::cqp_get_registry() is buggy
    if (dir.exists(registry_to_restore)){
      RcppCWB::cqp_reset_registry(registry = registry_to_restore)
    }
  }

  invisible(retval)
}
