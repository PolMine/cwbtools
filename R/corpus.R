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
#' @details The `corpus_install()` function will assist the installation of a
#'   corpus. The following scenarios are offered:
#' \itemize{
#'   \item{If argument `tarball` is a local tarball, the tarball will
#'   be extracted and files will be moved.}
#'   \item{If `tarball` is a URL, the tarball will be downloaded from the online
#'   location. It is possible to state user credentials using the arguments
#'   `user` and `password`. Then the aforementioned installation (scenario 1) is
#'   executed. If argument `pkg` is the name of an installed package, corpus
#'   files will be moved into this package.}
#'   \item{If argument `doi` is Document Object Identifier (DOI), the URL from
#'   which a corpus tarball can be downloaded is derived from the information
#'   available at that location. The tarball is downloaded and the corpus
#'   installed. If argument `pkg` is defined, files will be moved into a R
#'   package, the syste registry and corpus directories are used otherwise. Note
#'   that at this stage, it is assumed that the DOI has been awarded by
#'   \href{https://zenodo.org/}{Zenodo}}
#'   \item{If argument `pkg` is provided and `tarball` is `NULL`, corpora
#'   included in the package will be installed as system corpora, using the
#'   storage location specified by `registry_dir`. }
#' }
#' If the corpus to be installed is already available, a dialogue will ask the
#' user whether an existing corpus shall be deleted and installed anew, if
#' argument `ask` is `TRUE`.
#' @param old Name of the (old) corpus.
#' @param new Name of the (new) corpus.
#' @param pkg Name of a package (length-one `character` vector).
#' @param repo URL of the repository.
#' @param tarball URL,  S3-URI or local filename of a tarball with a CWB indexed
#'   corpus. If `NULL` (default) and argument `doi` is stated, the
#'   whereabouts of a corpus tarball will be derived from DOI.
#' @param doi The DOI (Digital Object Identifier) of a corpus deposited at
#'   Zenodo (e.g. "10.5281/zenodo.3748858".)
#' @param lib Directory for R packages, defaults to `.libPaths()[1]`.
#' @param verbose Logical, whether to be verbose.
#' @param ask A `logical` value, whether to ask for user input when
#'   choices are required.
#' @param registry_dir The corpus registry directory. If missing, the result of
#'   `cwb_registry_dir()`.
#' @param corpus The ID of a CWB indexed corpus (in upper case).
#' @param tarfile Filename of tarball.
#' @param checksum A length-one `character` vector with a MD5 checksum to
#'   check for the integrity of a downloaded tarball. If the tarball is
#'   downloaded from Zenodo by stating a DOI (argument `doi`), the checksum
#'   included in the metadata for the record is used for the check.
#' @param corpus_dir The directory that contains the data directories of indexed
#'   corpora. If missing, the value of `cwb_corpus_dir()` will be used.
#' @param load A `logical` value, whether to load corpus after installation.
#' @param ... Further parameters that will be passed into `download.file()`, if
#'   `tarball` is specified.
#' @param user A user name that can be specified to download a corpus from a
#'   password protected site.
#' @param password A password that can be specified to download a corpus from a
#'   password protected site.
#' @name corpus_install
#' @return Logical value `TRUE` if installation has been successful, or `FALSE`
#'   if not.
#' @seealso For managing registry files, see \code{\link{registry_file_parse}}
#' for switching to a packaged corpus.
#' @importFrom utils available.packages contrib.url
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
#'   cli_alert_info col_cyan cli_alert_danger cli_text col_blue col_red
#'   cli_alert_warning cli_process_failed
#' @importFrom RcppCWB cqp_is_initialized cqp_get_registry cqp_reset_registry
#'   cl_load_corpus cqp_load_corpus
#' @importFrom fs path_tidy path_temp
#' @rdname corpus_utils
#' @export corpus_install
corpus_install <- function(pkg = NULL, repo = "https://PolMine.github.io/drat/", tarball = NULL, doi = NULL, checksum = NULL, lib = .libPaths()[1], registry_dir, corpus_dir, ask = interactive(), load = TRUE, verbose = TRUE, user = NULL, password = NULL, ...){

  if (missing(registry_dir)) registry_dir <- cwb_registry_dir(verbose = FALSE)
  if (!is.null(registry_dir)) registry_dir <- as.character(path_tidy(registry_dir))
  if (missing(corpus_dir)) corpus_dir <- cwb_corpus_dir(verbose = FALSE)
  if (!is.null(corpus_dir)) corpus_dir <- as.character(fs::path_tidy(corpus_dir))

  modify_renviron <- FALSE

  if (is.null(tarball)){
    if (isFALSE(is.null(pkg))){
      
      if (nchar(system.file(package = pkg)) == 0L){
        warning(sprintf("Package `%s` is not available.", pkg))
        return(invisible(FALSE))
      }
      
      pkg_registry <- system.file(package = pkg, "extdata", "cwb", "registry")
      if (!dir.exists(pkg_registry)){
        warning("Package stated by `pkg` does not include corpora.")
        return(invisible(FALSE))
      }
      
      corpora <- list.files(pkg_registry)
      for (corpus in corpora){
        if (verbose) cli_rule(sprintf("Copy corpus `%s`", corpus))
        corpus_copy(
          corpus = corpus,
          registry_dir = pkg_registry,
          data_dir = NULL,
          registry_dir_new = registry_dir,
          data_dir_new = fs::path(corpus_dir, tolower(corpus)),
          remove = FALSE,
          verbose = verbose, progress = interactive()
        )
      }
      return(invisible(TRUE))
    } else {
      # Turn DOI into tarball

      if (is.null(doi))
        stop("If argument 'tarball' is NULL, either argument 'pkg' or argument 'doi' are required.")
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
            cli_alert_danger("'no Zenodo record found for DOI {.href {doi}}")
          }
        }
      )

      if (!exists("zenodo_record")){
        # unlikely scenario that can only result from error detected by tryCatch()
        return(invisible(FALSE)) 
      } else if (is.null(zenodo_record)){
        if (verbose)
          cli_alert_danger("'no Zenodo record found for DOI {.href {doi}}")
        return(invisible(FALSE))
      } else {
        if (verbose) cli_process_done()
      }

      zenodo_files <- sapply(zenodo_record[["files"]], function(x) x[["download"]])
      
      tarball <- grep(
        "^.*?_(v|)\\d+\\.\\d+\\.\\d+\\.tar\\.gz(/content|)$",
        zenodo_files,
        value = TRUE
      )

      if (length(tarball) > 1L && isTRUE(ask)){
        userchoice <- utils::menu(
          choices = basename(tarball),
          title = "Several tarballs assumed to contain an indexed corpus are available. Which tarball shall be downloaded?"
        )
        tarball <- tarball[userchoice]
      }
      
      if (is.null(tarball)){
        if (verbose){
          cli_alert_danger("'no Zenodo record found for DOI {.href {doi}}")
        }
        return(invisible(FALSE))
      }
      if (verbose) cli_process_done()
      
      if (verbose) cli_alert_info(
        sprintf(
          "tarball to be downloaded: %s",
          col_blue(basename(unname(tarball)))
        )
      )
      
      zenodo_file_record <- zenodo_record[["files"]][[which(zenodo_files == tarball)]]
    }
  }

  # Create CWB directory structure if necessary --------------

  if (verbose) cli_rule("Get CWB directories")

  cwb_dirs <- cwb_directories(registry_dir = registry_dir, corpus_dir = corpus_dir, verbose = FALSE)
  if (any(is.null(cwb_dirs))){
    cwb_dirs <- create_cwb_directories(ask = ask)
    modify_renviron <- TRUE
  }
  if (verbose) cli_alert_info(
    sprintf("registry directory: {.path %s}", cwb_dirs[["registry_dir"]])
  )
  if (verbose) cli_alert_info(
    sprintf("data directory: {.path %s}", cwb_dirs[["corpus_dir"]])
  )
  

  if (length(tarball) == 1L){
    
    cwbtools_tmpdir <- path(tempdir(), "cwbtools_tmpdir")
    if (file.exists(cwbtools_tmpdir)){
      # remove files first
      f <- list.files(cwbtools_tmpdir, include.dirs = FALSE, full.names = TRUE, recursive = TRUE)
      file.remove(f)
      # and then directories
      dirs <- list.files(cwbtools_tmpdir, include.dirs = TRUE, full.names = TRUE, recursive = TRUE)
      file.remove(dirs)
    } else {
      dir.create(cwbtools_tmpdir)
    }
    

    # Download corpus -------------------
    if (grepl("^http", tarball)){
      if (verbose) cat_rule("Download corpus tarball")
      if (verbose) cli_alert_info("download corpus tarball: {.href {tarball}}")

      corpus_tarball <- path(
        cwbtools_tmpdir,
        basename(gsub("/content$", "", tarball))
      )

      if (is.null(user)){

        tryCatch(
          tarball_not_available <- http_error(tarball),
          error = function(e)
            cli_alert_danger(
              "could not connect to server - check internet connection"
            )
        )
        if (!exists("tarball_not_available")){
          cli_alert_danger("corpus tarball is not available: {.href {tarball}}")
          return(invisible())
        }
        if (isTRUE(tarball_not_available)){
          cli_alert_danger("corpus tarball is not available: {.href {tarball}}")
          return(invisible(FALSE))
        }
        
        # An earlier version of cwbtools used download.file() on Windows which
        # used to offer better output messages. Has improved and download.file()
        # does not work with Zenodo

        trystatus <- tryCatch(
          curl::curl_download(
            url = tarball,
            destfile = corpus_tarball,
            quiet = !verbose
          )
        )
        if (is(trystatus)[[1]] == "try-error"){
          cli_alert_danger("download failed")
          return(NULL)
        }
      } else {
        if (is.null(password)) stop("If user name is offered, a password needs to be specified as well.")
        if (.Platform$OS.type == "windows"){
          # On Windows, download.file is used because curl will break if
          # destfile includes special characters. The user and the password are
          # passed in as follows
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
            handle = handle_setopt(
              new_handle(),
              userpwd = sprintf("%s:%s", user, password)
            ),
            quiet = !verbose
          )
        }
      }
      if (verbose)
        cli_alert_success("download corpus tarball {.href {tarball}} ... done")
      
      if (exists("zenodo_file_record")){
        if (!is.null(checksum)){
          if (verbose) 
            cli_alert_warning(
              "argument checksum is not NULL but md5 checksum can be derived from Zenodo record - using checksum issued by Zenodo"
            )
        }
        checksum <- zenodo_file_record[["checksum"]]
      }
      if (isFALSE(is.null(checksum))){
        if (verbose){
          msg <- "check md5 checksum for tarball {.href {tarball}} (expected: {.val {checksum}})"
          cli_process_start(msg)
        }
        corpus_tarball_checksum <- tools::md5sum(corpus_tarball)
        if (corpus_tarball_checksum == checksum){
          if (verbose) cli_process_done()
        } else {
          if (verbose) cli_process_failed()
          cli_alert_danger(text = c(
            "md5 checksum of downloaded tarball {.path {corpus_tarball}} is ",
            "{.val {corpus_tarball_checksum}}, but Zenodo archive md5 checksum ",
            "is {.val {checksum}}"
          ), wrap = TRUE)
          return(invisible(FALSE))
        }
      } else if (FALSE){
        if (verbose)
          cli_alert_warning(
            "no md5 checksum provided or available to check downloaded tarball - note that checking the integrity of downloaded data is good practice"
          )
      }
    } else if (grepl("^[sS]3:", tarball)){
      if (!requireNamespace("aws.s3", quietly = TRUE)){
        stop(
          "To download a corpus tarball from S3, package 'aws.s3' is required. ",
          "Package 'aws.s3' is not installed. ",
          "Install package 'aws.s3' by calling `install.packages('aws.s3')` and retry."
        )
      }
      if (verbose) cat_rule("Download corpus tarball from S3")
      if (verbose) cli_alert_info(
        sprintf("download corpus tarball {col_cyan('%s')}", basename(tarball))
      )
      corpus_tarball <- path(cwbtools_tmpdir, basename(tarball))

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
      if (!file.exists(tarball))
        stop(sprintf("tarball '%s' not found locally", tarball))
      corpus_tarball <- tarball
    }

    if (.Platform$OS.type == "windows"){
      if (stri_enc_mark(corpus_tarball) != "ASCII")
        corpus_tarball <- utils::shortPathName(corpus_tarball)
      if (stri_enc_mark(cwbtools_tmpdir) != "ASCII")
        cwbtools_tmpdir <- utils::shortPathName(cwbtools_tmpdir)
    }
    
    if (verbose) cat_rule("Install corpus")

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
    subdir1 <- file_path_sans_ext(basename(corpus_tarball), compression = TRUE)
    subdir2 <- gsub("^(.*?)(-|_)\\d{4}-\\d{2}-\\d{2}$", "\\1", subdir1)

    if (dir.exists(path(path_tidy(cwbtools_tmpdir), subdir1))){
      subdir <- subdir1
    } else if (dir.exists(path(path_tidy(cwbtools_tmpdir), subdir2))){
      subdir <- subdir2
    } else {
      subdir <- ""
    }

    tmp_registry_dir <- path(path_tidy(cwbtools_tmpdir), subdir, "registry")
    corpora <- list.files(tmp_registry_dir)
    
    # Ask user before overwriting existing corpus ----------------
    for (corpus in corpora){
      if (exists("zenodo_record")){
        version <- zenodo_record[["metadata"]][["version"]]
      } else {
        rf <- registry_file_parse(
          corpus = corpus,
          registry_dir = tmp_registry_dir
        )
        if ("version" %in% names(rf[["properties"]])){
          version <- rf[["properties"]][["version"]]
        } else if (grepl("^.*?\\d+\\.\\d+\\.\\d+\\.tar\\.gz(/content|)$", basename(corpus_tarball))){
          version <- gsub("^.*?_(v|)(\\d+\\.\\d+\\.\\d+)\\.tar\\.gz(/content|)$", "v\\2", basename(corpus_tarball))
        } else {
          version <- "unknown"
        }
      }
      
      if (tolower(corpus) %in% list.files(cwb_dirs[["registry_dir"]])){
        regdata <- registry_file_parse(
          corpus = toupper(corpus),
          registry_dir = cwb_dirs[["registry_dir"]]
        )
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
                "Corpus %s is already installed with version %s.",
                "The version of the installation candidate is %s.",
                "Proceed?"
              ),
              col_red(toupper(corpus)), col_blue(version_old), col_blue(version)
            )
            )
          } else {
            cli_text(
              sprintf(
                "Corpus %s (version: %s) is already installed. If you proceed, it will be replaced by version %s.",
                col_red(toupper(corpus)), col_blue(version_old), col_blue(version)
              )
            )
          }
        }
        purged <- corpus_remove(
          corpus = toupper(corpus),
          registry_dir = cwb_dirs[["registry_dir"]],
          ask = ask
        )
        if (isFALSE(purged)) return(invisible(FALSE))
      }
    }

    filenames <- list.files(path(path_tidy(cwbtools_tmpdir), subdir))
    dirname <- if ("indexed_corpora" %in% filenames) "indexed_corpora" else "data"
    tmp_data_dir <- path(path_tidy(cwbtools_tmpdir), subdir, dirname)

    for (corpus in corpora){

      registry_data <- registry_file_parse(corpus = corpus, registry_dir = tmp_registry_dir)

      # In corpus tarball, directory for binary files is not necessarily name of corpus
      # we assume that last (sub-)directory stated in HOME is correct
      tmp_home_dir <- path(tmp_data_dir, basename(registry_data[["home"]]))
      if (isFALSE(dir.exists(tmp_home_dir))){
        # Maybe data is not in another
        tmp_home_dir <- tmp_data_dir
        if (isFALSE(dir.exists(tmp_home_dir))) stop("don't know where to look for data directory")
      }

      if (.Platform$OS.type == "windows" && stri_enc_mark(tmp_home_dir) != "ASCII")
        tmp_home_dir <- utils::shortPathName(tmp_home_dir)
      registry_data[["home"]] <- tmp_home_dir

      if (length(registry_data[["info"]]) == 1L){
        info_file <- path(registry_data[["home"]], basename(registry_data[["info"]]))
        if (.Platform$OS.type == "windows" && stri_enc_mark(info_file) != "ASCII")
          info_file <- utils::shortPathName(info_file)
        registry_data[["info"]] <- info_file
      }

      if (!is.null(doi)) registry_data[["properties"]][["doi"]] <- doi

      if (!"version" %in% names(registry_data[["properties"]])){
        registry_data[["properties"]][["version"]] <- version
      }

      registry_file_write(
        data = registry_data,
        corpus = corpus,
        registry_dir = tmp_registry_dir
      )

      if (!is.null(pkg)){
        pkg_add_corpus(pkg = pkg, corpus = corpus, registry = tmp_registry_dir)
      } else {
        if (is.null(cwb_dirs[["registry_dir"]])) stop("Could not determine registry directory.")
        if (is.null(cwb_dirs[["corpus_dir"]])) stop("Could not determine corpus directory.")
        data_dir_target <- path(cwb_dirs[["corpus_dir"]], tolower(corpus))
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

      # Load corpus
      if (isTRUE(load)){
        if (verbose) cli_process_start("load corpus")
        cl <- cl_load_corpus(corpus = corpus, registry = cwb_dirs[["registry_dir"]])
        cqp <- cqp_load_corpus(corpus = toupper(corpus), registry = cwb_dirs[["registry_dir"]])
        if (cl && cqp)
          if (verbose) cli_process_done()
        else
          cli_alert_danger("loading corpus NOT successful")
      }
    }
    unlink(cwbtools_tmpdir, recursive = TRUE)

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

#' @details `corpus_packages()` will detect the packages that include CWB
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
          pkgdir <- fs::path(lib, pkg)
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


#' @details `corpus_rename()` will rename a corpus, affecting the name of the
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
  registry_old <- fs::path(registry_dir, tolower(old))
  registry_new <- fs::path(registry_dir, tolower(new))
  success <- file.rename(from = registry_old, to = registry_new)
  if (!success) stop("renaming the registry file failed")

  # rename data directory
  message("renaming data directory")
  regdata <- registry_file_parse(corpus = new, registry_dir = registry_dir)
  data_directory_old <- regdata[["home"]]
  data_directory_new <- fs::path(dirname(data_directory_old), tolower(new))
  success <- file.rename(from = data_directory_old, to = data_directory_new)
  if (!success) stop("renaming the data directory failed")

  # modify and save registry file
  message("modifying and saving registry file")
  regdata[["home"]] <- data_directory_new
  regdata[["id"]] <- tolower(new)
  registry_file_write(data = regdata, corpus = tolower(new), registry_dir = registry_dir)
  invisible(NULL)
}

#' @param ask A `logical` value, whether to ask user for confirmation before
#'   removing a corpus.
#' @details `corpus_remove()` can be used to delete a corpus.
#' @rdname corpus_utils
#' @importFrom cli cli_alert_success
#' @export corpus_remove
corpus_remove <- function(corpus, registry_dir, ask = interactive(), verbose = TRUE){

  if (missing(registry_dir)) registry_dir <- cwb_registry_dir(verbose = FALSE)
  if (!tolower(corpus) %in% list.files(registry_dir)){
    cli_alert_warning("abort removing corpus {.val {corpus}}: no registry file")
    return(FALSE)
  }
  if (verbose) cli_rule("remove corpus {col_blue({corpus})}")
  if (verbose) cli_alert_info("registry directory: {.path {registry_dir}}")

  reg <- registry_file_parse(
    corpus = tolower(corpus),
    registry_dir = registry_dir
  )
  data_directory <- reg[["home"]]
  
  if (verbose) cli_alert_info("data directory: {.path {data_directory}}")
  
  if (ask){
    userinput <- menu(
      choices = c("Yes", "No"),
      title = sprintf(
        "Are you sure you want to delete registry and data files for corpus '%s'?",
        cli::col_red(corpus)
      )
    )
    if (userinput != 1L){
      cli_alert_warning("User abort")
      return(invisible(FALSE))
    }
  }
  
  if (!is.null(cl_find_corpus(corpus = corpus, registry = registry_dir))){
    cl_delete_corpus(corpus = corpus, registry = registry_dir)
  }
  
  if (verbose) cli_progress_step("remove files in data directory")
  file.remove(list.files(data_directory, full.names = TRUE))
  if (verbose) cli_progress_step("remove data directory")
  unlink(data_directory, recursive = TRUE)
  if (verbose) cli_progress_step("remove registry file")
  file.remove(fs::path(registry_dir, tolower(corpus)))
  if (verbose) cli_progress_done()
  if (verbose)
    cli_alert_success("corpus {.var {corpus}} has been removed")
  invisible(TRUE)
}

#' @details `corpus_as_tarball()` will create a tarball (.tar.gz-file) with
#'   two subdirectories. The 'registry' subdirectory will host the registry file
#'   for the tarred corpus. The data files will be put in a subdirectory with
#'   the corpus name in the 'indexed_corpora' subdirectory.
#' @rdname corpus_utils
#' @export corpus_as_tarball
corpus_as_tarball <- function(corpus, registry_dir, data_dir, tarfile, verbose = TRUE){

  registry_file <- fs::path(registry_dir, tolower(corpus))
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

  if (verbose)
    cli_process_start("copy registry file and data files to temporary directory for creating tarball")
  archive_dir <- fs::path(fs::path_temp(), tolower(corpus))
  if (file.exists(archive_dir)) unlink(archive_dir, recursive = TRUE)
  dir.create(archive_dir)

  archive_registry_dir <- fs::path(archive_dir, "registry")
  archive_data_dir <- fs::path(archive_dir, "indexed_corpora")
  archive_corpus_dir <- fs::path(archive_dir, "indexed_corpora", tolower(corpus))
  dir.create(archive_registry_dir)
  dir.create(archive_data_dir)
  dir.create(archive_corpus_dir)

  file.copy(
    from = registry_file,
    to = fs::path(archive_registry_dir, tolower(corpus))
  )
  for (x in list.files(home_dir, full.names = TRUE)){
    file.copy(from = x, to = fs::path(archive_corpus_dir, basename(x)))
  }
  if (verbose) cli_process_done()

  if (verbose) cli_process_start("creating tarball")
  old_wd <- getwd()
  on.exit(setwd(path.expand(old_wd)))
  setwd(archive_dir)
  tar(tarfile = tarfile, compression = "gzip")
  if (verbose) cli_process_done()

  if (verbose) cli_process_start("cleaning up")
  setwd(path.expand(old_wd))
  unlink(archive_dir, recursive = TRUE)
  if (verbose) cli_process_done()

  invisible( NULL )
}

#' @param data_dir The data directory where the files of the CWB corpus live.
#' @param registry_dir_new Target directory with for (new) registry files.
#' @param data_dir_new Target directory for corpus files.
#' @param remove A `logical` value, whether to remove orginal files after having
#'   created the copy.
#' @param progress Logical, whether to show a progress bar.
#' @details `corpus_copy()` will create a copy of a corpus (useful for
#'   experimental modifications, for instance).
#' @importFrom cli make_spinner ansi_with_hidden_cursor cli_alert_success
#' @export corpus_copy
#' @rdname corpus_utils
#' @examples
#' registry_file_new <- fs::path(tempdir(), "cwb", "registry", "reuters")
#' if (file.exists(registry_file_new)) file.remove(registry_file_new)
#' corpus_copy(
#'   corpus = "REUTERS",
#'   registry_dir = system.file(package = "RcppCWB", "extdata", "cwb", "registry"),
#'   data_dir = system.file(
#'     package = "RcppCWB",
#'     "extdata", "cwb", "indexed_corpora", "reuters"
#'   )
#' )
#' unlink(fs::path(tempdir(), "cwb"), recursive = TRUE)
corpus_copy <- function(
  corpus,
  registry_dir,
  data_dir = NULL,
  registry_dir_new = fs::path(tempdir(), "cwb", "registry"),
  data_dir_new = fs::path(tempdir(), "cwb", "indexed_corpora", tolower(corpus)),
  remove = FALSE,
  verbose = interactive(),
  progress = TRUE
  ){

  registry_file_old <- fs::path(registry_dir, tolower(corpus))
  if (!file.exists(registry_file_old)){
    cli_alert_danger("registry file {.path {registry_file_old}} does not exist.")
    return(FALSE)
  }
    
  
  if (is.null(data_dir)){
    data_dir <- registry_file_parse(
      corpus = corpus,
      registry_dir = registry_dir
    )[["home"]]
  }

  registry_file_new <- fs::path(registry_dir_new, tolower(corpus))

  if (file.exists(registry_file_new)){
    cli_alert_danger(
      "target registry file {.path {registry_file_new}} already exists"
    )
    return(FALSE)
  }

  if (!dir.exists(registry_dir_new))
    dir.create(registry_dir_new, recursive = TRUE)
  
  if (!dir.exists(data_dir_new))
    dir.create(data_dir_new, recursive = TRUE)

  spinner <- make_spinner(
    template = "{spin} copy corpus data files to target data directory"
  )
  copy_with_spinner <- function(){
    lapply(
      list.files(data_dir, full.names = TRUE),
      function(f){
        file.copy(from = f, to = fs::path(data_dir_new, basename(f)))
        spinner$spin()
        if (remove) file.remove(f)
      }
    )
    spinner$finish()
  }
  ansi_with_hidden_cursor(copy_with_spinner())
  if (verbose)
    cli_alert_success("copy corpus data to target directory ... done")

  if (verbose) cli_process_start("parse registry file")
  rf <- registry_file_parse(corpus = corpus, registry_dir = registry_dir)
  if (verbose) cli_process_done()

  if (length(rf[["info"]]) == 1L){
    # It is a common mistake that the info file is not stated correctly in the
    # registry file, so this is a forgiving solution to remedy errors
    info_file_old <- fs::path(data_dir, basename(rf[["info"]]))
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
        if (verbose) cli_alert_warning("no info file")
      }
    }
  }

  if (verbose) cli_process_start("update registry data and save registry file")
  rf[["home"]] <- data_dir_new
  if (length(rf[["info"]]) == 1L){
    rf[["info"]] <- path(data_dir_new, basename(rf[["info"]]))
  }
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
#' registry_dir_tmp <- fs::path(tempdir(), "cwb", "registry")
#' registry_file_tmp <- fs::path(registry_dir_tmp, tolower(corpus))
#' data_dir_tmp <- fs::path(tempdir(), "cwb", "indexed_corpora", tolower(corpus))
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
#' unlink(fs::path(tempdir(), "cwb"), recursive = TRUE)
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

#' @details `corpus_get_version` parses the registry file and derives the
#'   corpus version number from the corpus properties. The return value is a
#'   `numeric_version` class object. The corpus version is expected to follow
#'   semantic versioning (three digits, e.g. '0.8.1'). If the corpus version
#'   has another format or if it is not available, the return value is `NA`.
#' @export corpus_get_version
#' @rdname corpus_utils
corpus_get_version <- function(corpus, registry_dir = Sys.getenv("CORPUS_REGISTRY")){
  v_string <- registry_file_parse(corpus, registry_dir = registry_dir)[["properties"]][["version"]]
  if (is.null(v_string)) return(NA)
  # Remove leading "v" if present
  if (grepl("^\\s*v\\d+\\.\\d+\\.\\d+$", v_string)){
    v_string <- gsub("^(\\s*v)(\\d+\\.\\d+\\.\\d+)\\s*$", "\\2", v_string)
  }
  if (!grepl("^\\d+\\.\\d+\\.\\d+$", v_string)){
    warning(sprintf(
      "Found version '%s' in properties file for corpus '%s'. The version does not meet the expectation of a three-digit version number. Returning NA value.",
      v_string, corpus
      )
    )
    return(NA)
  }
  numeric_version(v_string)
}


#' @details `corpus_reload()` will unload a corpus if necessary and reload it.
#'   Useful to make new features of a corpus available after modification. 
#'   Returns logical value `TRUE` if succesful, `FALSE` if not.
#' @rdname corpus_utils
#' @export corpus_reload
corpus_reload <- function(corpus, registry_dir, verbose = TRUE){
  cl_delete_corpus(corpus = corpus, registry = registry_dir)
  if (is.null(cl_find_corpus(corpus = corpus, registry = registry_dir))){
    if (verbose) cli_alert_success("corpus unloaded")
  } else {
    if (verbose) cli_alert_danger("unloading corpus failed")
    return(FALSE)
  }
  
  # Not too intuitive, but this (re)loads the corpus
  size <- cl_attribute_size(
    corpus = corpus,
    attribute = "word",
    attribute_type = "p",
    registry = registry_dir
  )
  
  if (typeof(cl_find_corpus(corpus, registry = registry_dir)) == "externalptr"){
    if (verbose)
      cli_alert_success("corpus reloaded (size: {.val {size}})")
  } else {
    if (verbose) cli_alert_danger("reloading corpus failed (CL representation")
    return(FALSE)
  }
  
  if (!cqp_load_corpus(corpus = corpus, registry = registry_dir)){
    if (verbose)
      cli_alert_danger(
        "reloading corpus failed (CQP representation not available)"
      )
    return(FALSE)
  }
  TRUE
}
