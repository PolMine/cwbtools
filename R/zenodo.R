#' @include corpus.R
NULL


.zenodo_info <- function(doi){
  record_id <- gsub("^.*?10\\.5281/zenodo\\.(\\d+)$", "\\1", doi)
  zenodo_api_url <- sprintf("https://zenodo.org/api/records/%d", as.integer(record_id))
  fromJSON(getURL(zenodo_api_url))
}