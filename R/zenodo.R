#' @include corpus.R
NULL


.zenodo_info <- function(doi){
  record_id <- gsub("^.*?10\\.5281/zenodo\\.(\\d+)$", "\\1", doi)
  zenodo_api_url <- sprintf("https://zenodo.org/api/records/%d", as.integer(record_id))
  json_raw <- getURL(zenodo_api_url)
  # In the string returned from Zenodo, newline characters are only escaped once, 
  # but jsonlite::fromJSON requires the newline character to be escaped twice,
  # see this issue: https://github.com/jeroen/jsonlite/issues/47
  json_escaped <- gsub("\\n", "\\\\n", json_raw)
  fromJSON(json_escaped)
}