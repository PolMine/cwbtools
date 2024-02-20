#' Utilities to install the Corpus Workbench (CWB)
#' 
#' Some steps for encoding corpora can be performed by calling CWB utilities
#' from the command line. This requires an installation of the CWB, either as
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
cwb_install <- function(
    url_cwb = cwb_get_url(),
    md5 = attr(url_cwb, "md5"),
    cwb_dir = fs::path(fs::path_temp(), "cwb"),
    verbose = TRUE
  ){
  
  if (!dir.exists(cwb_dir)){
    success <- dir.create(cwb_dir)
    if (isFALSE(success)){
      cli_alert_danger(
        "Directory {.path {cwb_dir}} does not exist - cannot create it"
      )
      return(NULL)
    }
  } else {
    if (length(list.files(cwb_dir)))
      cli_alert_danger("Directory {.path {cwb_dir}} is not empty", )
  }
  
  subdir <- gsub(
    "^(.*?)(-UPDATED|)(\\.tar\\.gz|\\.zip)$", "\\1",
    basename(url_cwb)
  )

  # Download CWB, return NULL in case of failure
  if (isTRUE(http_error(url_cwb))){
    cli_alert_danger("HTTP error / cannot access URL {.path {url_cwb}}")
    return(NULL)
  }
  tryCatch(
    success <- curl::curl_download(
      url_cwb,
      destfile = path(path_temp(), basename(url_cwb)),
      quiet = !verbose
    ),
    error = function(e){
      cli_alert_danger(
        "Downloading CWB from URL {.path {url_cwb}} failed - returning NULL"
      )
      return(NULL)
    }
  )
  if (is.null(success) | !file.exists(success)){
    cli_alert_danger(
      "Downloading CWB from URL {.path {url_cwb}} failed - returning NULL"
    )
    return(NULL)
  }
  
  if (is.character(md5)){
    stopifnot(nchar(md5) > 0L)
    if (verbose){
      cli_process_start(
        "checking whether md5 sum of downloaded file is {.val {md5}}"
      )
    }
    if (md5sum(path(path_temp(), basename(url_cwb))) != md5){
      cli_process_failed()
      return(NULL)
    } else {
      cli_process_done()
    }
  }

  if (.Platform$OS.type == "windows"){
    unzip(fs::path(path_temp(), basename(url_cwb)), exdir = path_temp())
    bin_dir <- fs::path(cwb_dir, "bin")
    if (!dir.exists(bin_dir)) dir.create(bin_dir)
    for (x in list.files(fs::path(path_temp(), subdir, "bin"), full.names = TRUE)){
      file.copy(from = x, to = fs::path(bin_dir, basename(x)))
    }
  } else if (Sys.info()["sysname"] == "Darwin"){
    untar(fs::path(path_temp(), basename(url_cwb)), exdir = path_temp())
    bin_dir <- fs::path(cwb_dir, "bin")
    if (!dir.exists(bin_dir)) dir.create(bin_dir)
    for (x in list.files(fs::path(path_temp(), subdir, "bin"), full.names = TRUE)){
      file.copy(from = x, to = fs::path(bin_dir, basename(x)))
    }
    lib_dir <- fs::path(cwb_dir, "lib")
    if (!dir.exists(lib_dir)) dir.create(lib_dir)
    for (x in list.files(fs::path(path_temp(), subdir, "lib"), full.names = TRUE)){
      file.copy(from = x, to = fs::path(lib_dir, basename(x)))
    }
  } else {
    untar(fs::path(path_temp(), basename(url_cwb)), exdir = path_temp())
    install_script_file <- fs::path(path_temp(), subdir, "install-cwb.sh")
    install_script <- readLines(install_script_file)
    install_script[grep("^PREFIX=", install_script)] <- sprintf("PREFIX='%s'", cwb_dir)
    # the installation script assumes that it is started from the directory of
    # the script however, changing into the directory would violate R package
    # checks so hard links are needed ...
    install_script <- gsub(
      "bin/\\*",
      sprintf("%s/*", fs::path(path_temp(), subdir, "bin")),
      install_script
    )
    install_script <- gsub(
      "bin/cwb-config",
      fs::path(path_temp(), subdir, "bin", "cwb-config"),
      install_script
    )
    install_script <- gsub(
      "instutils/cwb-config.in",
      fs::path(path_temp(), subdir, "instutils", "cwb-config.in"),
      install_script
    )
    install_script <- gsub(
      "instutils/install.sh",
      fs::path(path_temp(), subdir, "instutils", "install.sh"),
      install_script
    )
    install_script <- gsub(
      "lib/libcl.a",
      fs::path(path_temp(), subdir, "lib", "libcl.a"),
      install_script
    )
    install_script <- gsub(
      "include/cwb/cl.h",
      fs::path(path_temp(), subdir, "include", "cwb", "cl.h"),
      install_script
    )
    install_script <- gsub(
      "include/cwb/cqi.h",
      fs::path(path_temp(), subdir, "include", "cwb", "cqi.h"),
      install_script
    )
    install_script <- gsub(
      "man/man1/*",
      fs::path(path_temp(), subdir, "man", "man1", "*"),
      install_script
    )
    
    cat(install_script, file = install_script_file, sep = "\n")
    system(install_script_file)
  }
  unlink(path(path_temp(), basename(url_cwb)))
  unlink(path(path_temp(), subdir))
  cwb_bindir <- fs::path(cwb_dir, "bin")
  Sys.setenv("CWB_BINDIR" = cwb_bindir)
  cwb_bindir
}

#' @details `cwb_get_url()` will return the URL for downloading the appropriate
#'   binary (Linux / macOS) of the Corpus Workbench, or the source tarball
#'   (Linux). The md5 checksum of the file to be downloaded is part of the
#'   return value as "md5" attribute.
#' @rdname cwb
#' @export cwb_get_url
cwb_get_url <- function(){
  if (.Platform$OS.type == "unix"){
    if (Sys.info()["sysname"] == "Darwin"){
      if (Sys.info()["machine"] == "arm64"){
        url_cwb <- "https://sourceforge.net/projects/cwb/files/cwb/cwb-3.5/darwin/cwb-3.5.0-macos-11.0-arm64.tar.gz"
        attr(url_cwb, "md5") <- "fcf0516e02624cf991a3f77d4cbefcab"
      } else {
        url_cwb <- "https://sourceforge.net/projects/cwb/files/cwb/cwb-3.5/darwin/cwb-3.5.0-macos-10.13-x86_64.tar.gz"
        attr(url_cwb, "md5") <- "29ab6a93ffe9e740e73411f654c19957"
      }
    } else if (Sys.info()["sysname"] == "Linux"){
      # url_cwb <- "https://sourceforge.net/projects/cwb/files/cwb/cwb-3.0.0/cwb-3.0.0-linux-x86_64.tar.gz"
      # attr(url_cwb, "md5") <- "ee2f36abadd0242bbfcd84e2381399ea"
      url_cwb <- "https://sourceforge.net/projects/cwb/files/cwb/cwb-3.5/source/cwb-3.5.0-src.tar.gz"
      attr(url_cwb, "md5") <- "fb02beb3e3ba637bc6fadd33d51f73af"
    } else {
      stop("Platform is 'unix', but Sys.info()['sysname'] is neither 'Darwin' (i.e. MacOS) nor 'Linux'")
    }
  } else if (.Platform$OS.type == "windows"){
    url_cwb <- "https://sourceforge.net/projects/cwb/files/cwb/cwb-3.5/windows/cwb-3.5.0-win64-x86_64.zip"
    attr(url_cwb, "md5") <- "3295703d562eec7fe5ad45c0d36a93b7"
  }
  url_cwb
}

#' @param bindir The directory with CWB binaries.
#' @details `cwb_get_bindir()` will return the directory where the cwb utility
#'   programs reside. If `cwb_install()` has been used to install the CWB, the
#'   function returns the directory within the `cwbtools` package.
#'   Alternatively, a check for a local installation is performed. Returns
#'   `NULL` if CWB installation is not found.
#' @export cwb_get_bindir
#' @rdname cwb
cwb_get_bindir <- function(bindir = Sys.getenv("CWB_BINDIR")){
  if (file.exists(bindir)){
    return(bindir)
  } else {
    cwb_config <- "/usr/local/bin/cwb-config"
    if (file.exists(cwb_config)){
      bindir <- system(paste(cwb_config, "--bindir", sep = " "), intern = TRUE)
      return(bindir)
    } else {
      return(NULL)
    }
  }
  return(NULL)
}

#' @details `cwb_is_installed()` will check whether the CWB is installed.
#' @export cwb_is_installed
#' @rdname cwb
cwb_is_installed <- function(){
  if (is.null(cwb_get_bindir())) FALSE else TRUE
}
