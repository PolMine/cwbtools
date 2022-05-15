#' Download corpus tarball from Zenodo
#' 
#' Download corpus tarball from Zenodo. Downloading both freely available data
#' and data with restricted access is supported.
#' @return The filename of the downloaded corpus tarball, designed to serve as
#'   input for \code{\link{corpus_install}} (as argument `tarball`).
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
#' corpus_install(tarball = tarball_tmp)
#' 
#' # Download and install resource with restricted access
#' tarball_tmp <- zenodo_get_tarball(url = gparlsample_url_restricted)
#' corpus_install(tarball = tarball_tmp)
#' }
#' @export
#' @importFrom curl curl new_handle handle_cookies curl_fetch_memory curl_download
#' @importFrom xml2 read_html xml_find_all xml_attr
#' @importFrom cli col_magenta
#' @param url Landing page at Zenodo for resource. Can also be the URL for
#'   restricted access (?token= appended with a long key).
#' @param destfile A `character` vector with the file path where the downloaded
#'   file is to be saved. Tilde-expansion is performed. Defaults to a temporary
#'   file.
#' @param checksum A `logical` value, whether to check md5 sum.
#' @param verbose A `logical` value, whether to output progess messages.
#' @param progress A `logical` value, whether to report progress during
#'   download.
#' @rdname zenodo
zenodo_get_tarball <- function(url, destfile = tempfile(fileext = ".tar.gz"), checksum = TRUE, verbose = TRUE, progress = TRUE){
  
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
  
  restricted <- grepl("\\?token=", url)
  
  if (restricted){
    if (verbose) cat_rule("Download restricted resource from Zenodo")
    if (verbose) cli_process_start("get handle for restricted resource")
    curl_fetch_memory(url = url, handle = h)
    page <- curl(url = strsplit(x = url, split = "\\?token=")[[1]][1], handle = h)
    if (verbose) cli_process_done()
  } else {
    if (verbose) cat_rule("Download resource from Zenodo")
    page <- curl(url = url)
  }
  
  if (verbose) cli_process_start("extract tarball URL from Zenodo website")
  website <- read_html(page)
  file_elems <- xml_find_all(website, xpath = "//a[@class='filename']")
  filenames <- sapply(
    strsplit(
      x = sapply(file_elems, xml_attr, attr = "href"),
      split = "\\?download"
    ),
    `[[`, 1L
  )
  if (verbose) cli_process_done()
  tarball_index <- grep("\\.tar\\.gz", filenames)
  if (length(tarball_index) > 1L) stop("more than one tarball could be downloaded")
  tarball <- filenames[tarball_index]
  
  md5_el <- xml_find_first(file_elems[[tarball_index]], xpath = "../small")
  md5sum_zenodo <- gsub("^md5:(.*?)\\s*$", "\\1", xml_text(md5_el))
  
  cli_alert_info(
    paste("tarball to download:", sprintf("{.path %s}", basename(tarball)))
  )
  
  curl_download(
    url = fs::path("https://zenodo.org", tarball),
    destfile = destfile,
    quiet = !progress, 
    handle = h
  )
  
  if (isTRUE(checksum)){
    if (verbose){
      msg <- sprintf(
        "checking whether md5 checksum meets expectation (%s)", col_cyan(md5sum_zenodo)
      )
      cli_process_start(msg)
    }
    tarball_checksum <- tools::md5sum(destfile)
    if (tarball_checksum == md5sum_zenodo){
      if (verbose) cli_process_done()
    } else {
      if (verbose) cli_process_failed()
      cli_alert_danger(
        sprintf(
          "md5 checksum of corpus tarball is '%s' and fails to meet expectation",
          col_magenta(tarball_checksum)
        )
      )
      return(invisible(NULL))
    }
  } else {
    if (verbose)
      cli_alert_warning(
        paste("md5 checksum not checked (argument 'checksum' not 'TRUE') -",
        "note that checking the integrity of downloaded data is good practice")
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
gparlsample_url_restricted <- "https://zenodo.org/record/6546810?token=eyJhbGciOiJIUzUxMiIsImV4cCI6MTY4MjgwNTU5OSwiaWF0IjoxNjUyNDU2NjMwfQ.eyJkYXRhIjp7InJlY2lkIjo2NTQ2ODEwfSwiaWQiOjIzMDk5LCJybmQiOiJiNzEyY2JkMCJ9.6PGYPSxvlLNQ_3cdfncSwF6Hm5BSK742BM73jvIist7A2qeseNIwqU0alqkBN-TmvhYz32UQy69RXfAvL9Ag7Q"