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
#' @importFrom curl curl new_handle handle_cookies curl_fetch_memory curl_download
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
  
  if (grepl("\\?token=", url)){
    if (verbose) cat_rule("Download restricted resource from Zenodo")
    if (verbose) cli_process_start("get handle for restricted resource")
    
    trystatus <- try(curl_fetch_memory(url = url, handle = h))
    if (is(trystatus)[[1]] == "try-error"){
      warning("Zenodo not available. Try again later.")
      return(NULL)
    }
    
    trystatus <- try(
      page <- curl(
        url = strsplit(x = url, split = "\\?token=")[[1]][1],
        handle = h
      )
    )
    if (is(trystatus)[[1]] == "try-error"){
      warning("Zenodo not available. Try again later.")
      return(NULL)
    }
    
    if (verbose) cli_process_done()
  } else {
    
    if (grepl("^10\\.5281/zenodo", url))
      url <- sprintf("https://doi.org/%s", url)

    if (verbose) cat_rule("Download resource from Zenodo")

    trystatus <- try(page <- curl(url = url))
    if (is(trystatus)[[1]] == "try-error"){
      warning("Zenodo not available. Try again later.")
      return(NULL)
    }

  }
  
  if (verbose) cli_process_start("extract tarball URL from Zenodo website")
  
  trystatus <- try(website <- read_html(page))
  if (is(trystatus)[[1]] == "try-error"){
    warning("Zenodo not available. Try again later.")
    return(NULL)
  }

  file_elems <- xml_find_all(website, xpath = "//a[@class='filename']")
  if (length(file_elems) == 0L){
    warning("Website does not reference files to download.")
    return(NULL)
  }
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
  
  trystatus <- try(
    curl_download(
      url = fs::path("https://zenodo.org", tarball),
      destfile = destfile,
      quiet = !progress, 
      handle = h
    )
  )
  if (is(trystatus)[[1]] == "try-error"){
    warning("Zenodo not available. Try again later.")
    return(NULL)
  }

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

#' @export
#' @rdname zenodo
#' @details `zenodo_get_tarballurl` is a (temporary) helper function to
#'   accomplish a temporarily bugged functionality of the zen4R package.
zenodo_get_tarballurl <- function(url){
  
  stopifnot(is.character(url), length(url) == 1L)
  
  if (grepl("\\?token=", url)){
    warning("Not implemented for restricted data.")
    return(NULL)
  }
  if (grepl("^10\\.5281/zenodo", url)) url <- sprintf("https://doi.org/%s", url)
  if (!grepl("10\\.5281/zenodo", url)){
    warning("Not a Zenodo page, returning `NULL`.")
    return(NULL)
  }
  
  if (isTRUE(http_error(url))){
    warning(sprintf("HTTP error / cannot access URL %s () ", url))
    return(NULL)
  }

  trystatus <- try(page <- curl(url = url))
  if (is(trystatus)[[1]] == "try-error"){
    warning("Zenodo not available. Try again later.")
    return(NULL)
  }
  
  trystatus <- try(website <- read_html(page))
  if (is(trystatus)[[1]] == "try-error"){
    warning("Zenodo not available. Try again later.")
    return(NULL)
  }
  
  file_elems <- xml_find_all(website, xpath = "//a[@class='filename']")
  if (length(file_elems) == 0L){
    warning("Website does not reference files to download.")
    return(NULL)
  }
  filenames <- sapply(
    strsplit(
      x = sapply(file_elems, xml_attr, attr = "href"),
      split = "\\?download"
    ),
    `[[`, 1L
  )
  tarball_index <- grep("\\.tar\\.gz", filenames)
  if (length(tarball_index) > 1L){
    warning("more than one tarball could be downloaded")
    return(NULL)
  }
  if (length(tarball_index) == 0L){
    warning("No tarball to be downloaded.")
    return(NULL)
  }
  
  tarball <- filenames[tarball_index]
  
  md5_el <- xml_find_first(file_elems[[tarball_index]], xpath = "../small")
  md5sum_zenodo <- gsub("^md5:(.*?)\\s*$", "\\1", xml_text(md5_el))
  
  setNames(
    fs::path("https://zenodo.org", tarball),
    md5sum_zenodo
  )
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
gparlsample_url_restricted <- "https://zenodo.org/record/6546810?token=eyJhbGciOiJIUzUxMiIsImV4cCI6MTY3MjQzNzU5OSwiaWF0IjoxNjU4MzE5NzgwfQ.eyJkYXRhIjp7InJlY2lkIjo2NTQ2ODEwfSwiaWQiOjI0ODA4LCJybmQiOiI5N2RkYjdiZCJ9.Bi4bvXO1UXB7h6wT_RlhPWfesRb1CpIH2GNOQZ2Twf9M18k8tx38LBYvACVrKYmLZWTHIrVyN3yl_rCJ9s1PlA"
