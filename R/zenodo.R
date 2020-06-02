#' @include corpus.R
NULL


#' Get and process information from Zenodo.
#' 
#' Zenodo is an open science repository hosted by CERN which can be readily be
#' used to deposit corpora (see \href{https://zenodo.org/}{Zenodo website}). The
#' 'cwbtools' package includes basic functionality to retrieve and process the
#' metadata of Zenodo records.
#' 
#' @param doi The Document Object Identifier (doi) pointing to a Zenodo record.
#' @param timeout.ms Time allowed for the Zenodo API to respond (in
#'   milliseconds). Defaults to 5000 (milliseconds).
#' @param x An object of class \code{zenodo_record_metadata}.
#' @export get_zenodo_record_metadata
#' @importFrom RCurl getURL curlOptions
#' @importFrom jsonlite fromJSON
#' @rdname zenodo
#' @details The function \code{get_zenodo_record_metadata} will retrieve
#'   information from the Zenodo API for the record identified via a DOI
#'   (argument \code{doi}). The return value is an object of class
#'   \code{zenodo_record_metadata} (S3 class).
#' @examples 
#' \dontrun{
#' gps_meta <- get_zenodo_record_metadata(doi = "https://doi.org/10.5281/zenodo.3823245")
#' as_bibentry(gps_meta)
#' }
get_zenodo_record_metadata <- function(doi, timeout.ms = 5000){
  record_id <- gsub("^.*?10\\.5281/zenodo\\.(\\d+)$", "\\1", doi)
  zenodo_api_url <- sprintf("https://zenodo.org/api/records/%d", as.integer(record_id))
  json_raw <- getURL(zenodo_api_url, .opts = curlOptions(timeout.ms = timeout.ms))
  # In the string returned from Zenodo, newline characters are only escaped once, 
  # but jsonlite::fromJSON requires the newline character to be escaped twice,
  # see this issue: https://github.com/jeroen/jsonlite/issues/47
  json_escaped <- gsub("\\n", "\\\\n", json_raw)
  y <- fromJSON(json_escaped)
  class(y) <- c("zenodo_record_metadata", class(y))
  y
}

#' @rdname zenodo
#' @export
as_bibentry <- function (x) UseMethod("as_bibentry", x)


#' @details A method \code{as_bibentry} is available for the class
#'   \code{zenodo_record_metadata}. The resulting \code{bibentry}-class object
#'   can be turned into a recommended citation easily using the \code{format} or
#'   \code{print} methods for \code{bibentry} class objects (see
#'   \link[utils]{bibentry}).
#' @rdname zenodo
#' @importFrom utils bibentry
#' @export
as_bibentry.zenodo_record_metadata <- function(x){
  
  # Extract information from incoming object
  authors <- paste(
    apply(
      do.call(rbind, strsplit(x[["metadata"]][["creators"]][["name"]], split = "\\s*,\\s*"))[,c(2,1)],
      1,
      function(x) paste(x, collapse = " ")
    ), collapse = " and ")
  year <- format(as.Date(x[["metadata"]][["publication_date"]]), "%Y")
  title <- x[["metadata"]][["title"]]
  version <- x[["metadata"]][["version"]]
  doi_http <- x[["links"]][["self"]]
  
  txt_version <- sprintf(
    "%s (%s): %s (%s) [Data set]. Zenodo. %s.",
    authors,
    year,
    title,
    version,
    doi_http
  )
  
  bibentry(
    bibtype = "Manual",
    author = authors,
    title = title,
    year = year,
    note = sprintf("Version: %s. doi: %s.", version, doi_http), 
    textVersion = txt_version
  )
}
