#' Utilities to install Corpus Workbench.
#' 
#' Some steps for encoding corpora can be performed by calling CWB utilities
#' from the command line, which requires an installation of the CWB, either as
#' part of the CWB package, or using the default installation location of the
#' CWB.
#' @return The path of the CWB binaries or `NULL` if downloading and installing
#'   the CWB has failed.
#' @param url_cwb URL for downloading the CWB.
#' @param cwb_dir The directory where the CWB shall be installed.
#' @param md5 The md5 checksum of the compressed file to be downloaded. 
#' @param verbose A `logical` value, whether to output messages.
#' @rdname cwb
#' @export cwb_install
#' @importFrom utils unzip untar download.file
#' @importFrom httr http_error
#' @importFrom fs path_temp path_tidy path
#' @importFrom tools md5sum
cwb_install <- function(url_cwb = cwb_get_url(), md5 = attr(url_cwb, "md5"), cwb_dir = fs::path(fs::path_temp(), "cwb"), verbose = TRUE){
  
  if (!dir.exists(cwb_dir)){
    success <- dir.create(cwb_dir)
    if (isFALSE(success)){
      warning(sprintf("Directory '%s' does not exist - cannot create it", cwb_dir))
      return(NULL)
    }
  } else {
    if (length(list.files(cwb_dir))) warning("Directory '%s' is not empty", cwb_dir)
  }
  
  subdir <- gsub("^(.*?)(-UPDATED|)(\\.tar\\.gz|\\.zip)$", "\\1", basename(url_cwb))

  # Download CWB, return NULL in case of failure
  if (isTRUE(http_error(url_cwb))){
    warning(sprintf("HTTP error / cannot access URL %s () ", url_cwb))
    return(NULL)
  }
  tryCatch(
    success <- download.file(url_cwb, destfile = path(path_temp(), basename(url_cwb))),
    error = function(e){
      warning(sprintf("Downloading CWB from URL %s failed - returning NULL", url_cwb))
      return(NULL)
    }
  )
  if (success != 0){
    warning(sprintf("Downloading CWB from URL %s failed - returning NULL", url_cwb))
    return(NULL)
  }
  
  if (is.character(md5)){
    stopifnot(nchar(md5) > 0L)
    if (verbose){
      cli_process_start(sprintf("checking whether md5 sum of downloaded file is %s", md5))
    }
    if (md5sum(path(path_temp(), basename(url_cwb))) != md5){
      cli_process_failed()
      return(NULL)
    } else {
      cli_process_done()
    }
  }

  if (.Platform$OS.type == "windows"){
    unzip(file.path(path_temp(), basename(url_cwb), fsep = "/"), exdir = path_temp())
    bin_dir <- file.path(cwb_dir, "bin")
    if (!dir.exists(bin_dir)) dir.create(bin_dir)
    for (x in list.files(file.path(path_temp(), subdir, "bin"), full.names = TRUE)){
      file.copy(
        from = x,
        to = file.path(normalizePath(bin_dir, winslash = "/"), basename(x))
        )
    }
  } else {
    untar(file.path(path_temp(), basename(url_cwb), fsep = "/"), exdir = path_temp())
    install_script_file <- file.path(path_temp(), subdir, "install-cwb.sh", fsep = "/")
    install_script <- readLines(install_script_file)
    install_script[grep("^PREFIX=", install_script)] <- sprintf("PREFIX='%s'", cwb_dir)
    # the installation script assumes that it is started from the directory of the script
    # however, changing into the directory would violate R package checks
    # so hard links are needed ...
    install_script <- gsub("bin/\\*", sprintf("%s/*", file.path(path_temp(), subdir, "bin", fsep = "/")), install_script)
    install_script <- gsub("bin/cwb-config", file.path(path_temp(), subdir, "bin", "cwb-config", fsep = "/"), install_script)
    install_script <- gsub("instutils/cwb-config.in", file.path(path_temp(), subdir, "instutils", "cwb-config.in", fsep = "/"), install_script)
    install_script <- gsub("instutils/install.sh", file.path(path_temp(), subdir, "instutils", "install.sh", fsep = "/"), install_script)
    install_script <- gsub("lib/libcl.a", file.path(path_temp(), subdir, "lib", "libcl.a", fsep = "/"), install_script)
    install_script <- gsub("include/cwb/cl.h", file.path(path_temp(), subdir, "include", "cwb", "cl.h", fsep = "/"), install_script)
    install_script <- gsub("include/cwb/cqi.h", file.path(path_temp(), subdir, "include", "cwb", "cqi.h", fsep = "/"), install_script)
    install_script <- gsub("man/man1/*", file.path(path_temp(), subdir, "man", "man1", "*", fsep = "/"), install_script)
    cat(install_script, file = install_script_file, sep = "\n")
    system(install_script_file)
  }
  unlink(path(path_temp(), basename(url_cwb)))
  unlink(path(path_temp(), subdir))
  cwb_bindir <- file.path(cwb_dir, "bin")
  Sys.setenv("CWB_BINDIR" = cwb_bindir)
  cwb_bindir
}

#' @details \code{cwb_get_url} will return the URL for downloading the
#'   appropriate binary (Linux / macOS) of the Corpus Workbench, or the source
#'   tarball (Linux). The md5 checksum of the file to be downloaded is part of
#'   the return value as "md5" attribute.
#' @rdname cwb
#' @export cwb_get_url
cwb_get_url <- function(){
  if (.Platform$OS.type == "unix"){
    if (Sys.info()["sysname"] == "Darwin"){
      url_cwb <- "https://sourceforge.net/projects/cwb/files/cwb/cwb-3.0.0/cwb-3.0.0-osx-10.5-universal.tar.gz"
      attr(url_cwb, "md5") <- "c6c47a4d0fda021c949f239337123611"
    } else if (Sys.info()["sysname"] == "Linux"){
      url_cwb <- "https://sourceforge.net/projects/cwb/files/cwb/cwb-3.0.0/cwb-3.0.0-linux-x86_64.tar.gz"
      attr(url_cwb, "md5") <- "ee2f36abadd0242bbfcd84e2381399ea"
    } else {
      stop("Platform is 'unix', but Sys.info()['sysname'] is neither 'Darwin' (i.e. MacOS) nor 'Linux'")
    }
  } else if (.Platform$OS.type == "windows"){
    url_cwb <- "https://sourceforge.net/projects/cwb/files/cwb/cwb-3.4-beta/cwb-3.4.10-windows-i586-UPDATED.zip"
    attr(url_cwb, "md5") <- "5b9c8eb2e24528da12bbd9c94342b8bf"
  }
  url_cwb
}

#' @param bindir The directory with CWB binaries.
#' @details \code{cwb_get_bindir} will return the directory where the cwb
#'   utility programs reside. If \code{cwb_install()} has been used to install
#'   the CWB, the function returns the directory within the \code{cwbtools}
#'   package. Alternatively, a check for a local installation is performed.
#' @export cwb_get_bindir
#' @rdname cwb
cwb_get_bindir <- function(bindir = Sys.getenv("CWB_BINDIR")){
  if (file.exists(bindir)){
    return( bindir )
  } else {
    cwb_config <- "/usr/local/bin/cwb-config"
    if (file.exists(cwb_config)){
      bindir <- system(paste(cwb_config, "--bindir", sep = " "), intern = TRUE)
      return( bindir )
    } else {
      return( NULL )
    }
  } 
}

#' @details \code{cwb_is_installed} will check whether the CWB is installed.
#' @export cwb_is_installed
#' @rdname cwb
cwb_is_installed <- function(){
  if (is.null(cwb_get_bindir())) FALSE else TRUE
}
