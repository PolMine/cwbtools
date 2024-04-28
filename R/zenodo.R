#' Download corpus tarball from Zenodo
#' 
#' Download corpus tarball from Zenodo. Downloading both freely available data
#' and data with restricted access is supported.
#' @return The filename of the downloaded corpus tarball, designed to serve as
#'   input for \code{\link{corpus_install}} (as argument `tarball`). If the 
#'   resource is not available, `NULL` is returned.
#' @examples
#' \donttest{
#' # Temporary directory structure as a preparatory step
#' Sys.setenv(CORPUS_REGISTRY = "")
#' cwb_dirs <- create_cwb_directories(
#'   prefix = tempdir(),
#'   ask = FALSE,
#'   verbose = FALSE
#' )
#' Sys.setenv(CORPUS_REGISTRY = cwb_dirs[["registry_dir"]])
#' 
#' # Download and install open access resource
#' gparl_url_pub <- "https://doi.org/10.5281/zenodo.3823245"
#' tarball_tmp <- zenodo_get_tarball(url = gparl_url_pub)
#' if (!is.null(tarball_tmp)) corpus_install(tarball = tarball_tmp)
#' 
#' # Download and install resource with restricted access
#' tarball_tmp <- zenodo_get_tarball(url = gparlsample_url_restricted)
#' if (!is.null(tarball_tmp)) corpus_install(tarball = tarball_tmp)
#' }
#' @export
#' @importFrom curl curl new_handle handle_cookies curl_fetch_memory
#'   curl_download
#' @importFrom xml2 read_html xml_find_all xml_attr
#' @importFrom cli col_magenta
#' @param url Landing page at Zenodo for resource. Can also be the URL for
#'   restricted access (?token= appended with a long key), or a DOI referencing
#'   objects deposited with Zenodo.
#' @param destfile A `character` vector with the file path where the downloaded
#'   file is to be saved. Tilde-expansion is performed. Defaults to a temporary
#'   file.
#' @param checksum A `logical` value, whether to check md5 sum.
#' @param verbose A `logical` value, whether to output progess messages.
#' @param progress A `logical` value, whether to report progress during
#'   download.
#' @return The path of the downloaded resource, or `NULL` if the operation has
#'   not been successful.
#' @rdname zenodo
zenodo_get_tarball <- function(
    url,
    destfile = tempfile(fileext = ".tar.gz"),
    checksum = TRUE,
    verbose = TRUE,
    progress = TRUE
  ){
  
  stopifnot(
    is.character(url),
    length(url) == 1L,
    is.character(destfile),
    length(destfile) == 1L,
    is.logical(verbose),
    length(verbose) == 1L,
    is.logical(progress),
    length(progress) == 1L
  )
  
  destfile <- fs::path_expand(destfile)
  
  h <- new_handle()
  
  if (grepl("\\?token=", url)){
    if (verbose) cat_rule("Download restricted resource from Zenodo")
    if (verbose) cli_process_start("get handle for restricted resource")
    
    trystatus <- try(curl_fetch_memory(url = url, handle = h))
    if (is(trystatus)[[1]] == "try-error"){
      cli_alert_warning("Zenodo not available. Try again later.")
      return(NULL)
    }
    
    trystatus <- try(page <- curl(url = url, handle = h))
    if (is(trystatus)[[1]] == "try-error"){
      cli_alert_warning("Zenodo not available. Try again later.")
      return(NULL)
    }
    
    trystatus <- try(
      website <- read_xml(
        paste(readLines(page, warn = FALSE), collapse = "\n"),
        as_html = TRUE,
        options = c("NOERROR", "NOWARNING", "RECOVER")
      ),
    )
    if (is(trystatus)[[1]] == "try-error"){
      warning("Zenodo not available. Try again later.")
      return(NULL)
    }
    
    a <- xml_find_all(website, xpath = "//a")
    files <- xml2::xml_attr(a, attr = "href")
    tarball_index <- grep("\\.tar\\.gz\\?download=", files)
    if (length(tarball_index) == 0L){
      cli_alert_warning("No tarball to be downloaded.")
      return(NULL)
    }
    
    tarball <- sprintf("https://zenodo.org/%s", files[tarball_index[1]])

    md5_el <- xml_find_first(a[[tarball_index[1]]], xpath = "../../small")
    md5sum_zenodo <- gsub("^md5:(.*?)\\s*$", "\\1", xml_text(md5_el))
    
    if (verbose) cli_process_done()
    cli_alert_info("tarball to download: {.href {tarball}}")
  } else {
    
    if (verbose) cat_rule("Download resource from Zenodo")
    
    tryCatch(
      zenodo_record <- ZenodoManager$new()$getRecordByDOI(doi = url),
      error = function(e){
        if (verbose){
          cli_process_failed()
        } else {
          cli_alert_danger("'no Zenodo record found for DOI {.href {url}}")
          return(NULL)
        }
      }
    )
    
    if (verbose) cli_process_start("get tarball URL from Zenodo record")
    
    zenodo_files <- sapply(zenodo_record[["files"]], `[[`, "download")
    
    if (verbose) cli_process_done()
    tarball_index <- grep("\\.tar\\.gz/content", zenodo_files)
    if (length(tarball_index) > 1L)
      stop("more than one tarball could be downloaded")
    tarball <- zenodo_files[tarball_index]
    
    md5sum_zenodo <- zenodo_record[["files"]][[tarball_index]][["checksum"]]
    
    cli_alert_info("tarball to download: {.path {basename(dirname(tarball))}}")
  }

  trystatus <- try(
    curl_download(
      url = tarball,
      destfile = destfile,
      quiet = !progress, 
      handle = h
    )
  )
  if (is(trystatus)[[1]] == "try-error"){
    cli_alert_warning("Zenodo not available. Try again later.")
    return(NULL)
  }

  if (isTRUE(checksum)){
    if (verbose)
      cli_process_start(
        "check that md5 checksum is ({col_cyan({md5sum_zenodo})})"
      )
    tarball_checksum <- tools::md5sum(destfile)
    if (tarball_checksum == md5sum_zenodo){
      if (verbose) cli_process_done()
    } else {
      if (verbose) cli_process_failed()
      cli_alert_danger(
        sprintf(
          "md5 checksum of corpus tarball is '%s', differs from expectation",
          col_magenta(tarball_checksum)
        )
      )
      return(invisible(NULL))
    }
  } else {
    if (verbose)
      cli_alert_warning(c(
        "md5 checksum not checked (argument 'checksum' not 'TRUE') - ",
        "note that checking the integrity of downloaded data is good practice"),
        wrap = TRUE
      )
  }

  destfile
}




#' @details A sample subset of the GermaParl corpus is deposited at Zenodo for
#'   testing purposes. There are identical open access and restricted versions
#'   of GermaParlSample to test different flavours of downloading a resource
#'   from Zenodo. The URL for restricted access includes an access token which
#'   is very lengthy. This URL is included as a dataset in the package to avoid
#'   excessive line in sample code. Note that URLs that give access to
#'   restricted data are usually not to be shared.
#' @docType data
#' @rdname zenodo
#' @keywords datasets
#' @export
gparlsample_url_restricted <- "https://zenodo.org/records/6546810?token=eyJhbGciOiJIUzUxMiJ9.eyJpZCI6ImYzZGZlNmVkLWQ3ZDgtNDdiYy05ZGQxLTE2MmRjMThlZWE4MiIsImRhdGEiOnt9LCJyYW5kb20iOiIwMjkzZTFjNjMzZTcwNTQxNTlhMTI4NmUwMGQ1OTQyYyJ9.xiVSz2iVv60l8K9BhgnRGqcU9i7rz5fYs4wHCVuOCfvDNGIuLRJ43jLYfgBo5LJv9GJaqwoaqvA3e74te_Y57A"