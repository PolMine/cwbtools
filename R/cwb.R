#' Utilities to install Corpus Workbench.
#' 
#' Some steps for encoding corpora can be performed by calling CWB utilities
#' from the command line, which requires an installation of the CWB, either as
#' part of the CWB package, or using the default installation location of the
#' CWB.
#' @param url_cwb The URL from where the CWB can be downloaded.
#' @rdname cwb
#' @export cwb_install
#' @importFrom utils unzip untar download.file
cwb_install <- function(url_cwb = cwb_get_url()){
  subdir <- gsub("^(.*?)(-UPDATED|)(\\.tar\\.gz|\\.zip)$", "\\1", basename(url_cwb))
  cwb_dir <- system.file(package = "cwbtools", "extdata", "cwb")
  tmp_dir <- normalizePath(tempdir(), winslash = "/")
  download.file(url_cwb, destfile = file.path(tmp_dir, basename(url_cwb), fsep = "/"))
  
  if (.Platform$OS.type == "windows"){
    unzip(file.path(tmp_dir, basename(url_cwb), fsep = "/"))
    install_script_file <- file.path(tmp_dir, subdir, "install-cwb-win.bat", fsep = "/")
    install_script <- readLines(install_script_file)
    cwbDir <- gsub("/", "\\\\", cwb_dir)
    install_script[grep("^set PREFIX=", install_script)] <- sprintf("set PREFIX=%s", cwb_dir)
    cat(install_script, file = install_script_file, sep = "\n")
    # setwd(file.path(tmp_dir, subdir, fsep = "/"))
    shell(install_script_file)
  } else {
    untar(file.path(tmp_dir, basename(url_cwb), fsep = "/"), exdir = tmp_dir)
    install_script_file <- file.path(tmp_dir, subdir, "install-cwb.sh", fsep = "/")
    install_script <- readLines(install_script_file)
    install_script[grep("^PREFIX=", install_script)] <- sprintf("PREFIX='%s'", cwb_dir)
    # the installation script assumes that it is started from the directory of the script
    # however, changing into the directory would violate R package checks
    # so hard links are needed ...
    install_script <- gsub("bin/\\*", sprintf("%s/*", file.path(tmp_dir, subdir, "bin", fsep = "/")), install_script)
    install_script <- gsub("bin/cwb-config", file.path(tmp_dir, subdir, "bin", "cwb-config", fsep = "/"), install_script)
    install_script <- gsub("instutils/cwb-config.in", file.path(tmp_dir, subdir, "instutils", "cwb-config.in", fsep = "/"), install_script)
    install_script <- gsub("instutils/install.sh", file.path(tmp_dir, subdir, "instutils", "install.sh", fsep = "/"), install_script)
    install_script <- gsub("lib/libcl.a", file.path(tmp_dir, subdir, "lib", "libcl.a", fsep = "/"), install_script)
    install_script <- gsub("include/cwb/cl.h", file.path(tmp_dir, subdir, "include", "cwb", "cl.h", fsep = "/"), install_script)
    install_script <- gsub("include/cwb/cqi.h", file.path(tmp_dir, subdir, "include", "cwb", "cqi.h", fsep = "/"), install_script)
    install_script <- gsub("man/man1/*", file.path(tmp_dir, subdir, "man", "man1", "*", fsep = "/"), install_script)
    cat(install_script, file = install_script_file, sep = "\n")
    system(install_script_file)
  }
  unlink(tmp_dir)
  invisible( NULL )
}

#' @details \code{cwb_get_url} will return the URL for downloading the
#'   appropriate binary (Linux / macOS / Windows) of the Corpus Workbench.
#' @rdname cwb
#' @export cwb_get_url
cwb_get_url <- function(){
  if (.Platform$OS.type == "unix"){
    if (Sys.info()["sysname"] == "Darwin"){
      url_cwb <- "https://sourceforge.net/projects/cwb/files/cwb/cwb-3.0.0/cwb-3.0.0-osx-10.5-universal.tar.gz"
    } else if (Sys.info()["sysname"] == "Linux"){
      url_cwb <- "https://sourceforge.net/projects/cwb/files/cwb/cwb-3.0.0/cwb-3.0.0-linux-x86_64.tar.gz"
    } else {
      stop("Platform is 'unix', but Sys.info()['sysname'] is neither 'Darwin' (i.e. MacOS) nor 'Linux'")
    }
  } else if (.Platform$OS.type == "windows"){
    url_cwb <- "https://sourceforge.net/projects/cwb/files/cwb/cwb-3.4-beta/cwb-3.4.10-windows-i586-UPDATED.zip"
  }
  url_cwb
}

#' @details \code{cwb_get_bindir} will return the directory where the cwb
#'   utility programs reside. If \code{cwb_install()} has been used to install
#'   the CWB, the function returns the directory within the \code{cwbtools}
#'   package. Alternatively, a check for a local installation is performed.
#' @export cwb_get_bindir
#' @rdname cwb
cwb_get_bindir <- function(){
  pkg_cwbtools_bindir <- system.file(package = "cwbtools", "extdata", "cwb", "bin")
  if (file.exists(pkg_cwbtools_bindir)){
    return( pkg_cwbtools_bindir )
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
