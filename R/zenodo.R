#' @include corpus.R
NULL


#' Download corpus from Zenodo
#' 
#' Zenodo is an open science repository hosted by CERN.
#' @param corpus ID of corpus to be downloaded.
#' @param doi The DOI (Digital Object Identifier) of the corpus tarball at
#'   zenodo, presented as a hyperlink. Defaults to the DOI of a small sample
#'   corpus (UNGAMINI).
#' @param ask A \code{logical} value, whether to ask for user input before
#'   replacing an existing corpus.
#' @param registry_dir Path to the system registry directory. Defaults to value
#'   of \code{cwbtools::cwb_registry_dir()}.
#' @param quiet Whether to suppress progress messages, defaults to \code{FALSE}.
#' @importFrom RCurl url.exists getURL
#' @importFrom jsonlite fromJSON
#' @importFrom utils menu
#' @return A logical value, \code{TRUE} if the corpus has been installed
#'   successfully.
#' @export zenodo_download_corpus
#' @rdname zenodo
zenodo_download_corpus <- function(corpus = "UNGAMINI", doi = "https://doi.org/10.5281/zenodo.3748858", registry_dir = cwbtools::cwb_registry_dir(), quiet = FALSE, ask = interactive()){
  if (isFALSE(is.logical(quiet))) stop("Argument 'quiet' needs to be a logical value.")
  zenodo_info <- zenodo_info(doi = doi)
  
  if (!is.null(registry_dir)){
    if (tolower(corpus) %in% list.files(registry_dir)){
      regdata <- registry_file_parse(corpus = toupper(corpus), registry_dir = registry_dir)
      if ("version" %in% names(regdata[["properties"]])){
        version_old <- regdata[["properties"]][["version"]]
      } else {
        version_old <- "unknown"
      }
      if (ask){
        if (version_old == zenodo_info[["metadata"]][["version"]]){
          userinput <- menu(
            choices = c("Yes / continue", "No / abort"), 
            title = sprintf(
              paste(
                "Local %s version and the version of %s to be retrieved from Zenodo are identical (%s).",
                "Are you sure you want to proceed and to replace the local corpus by a fresh download?"
              ),
              toupper(corpus), toupper(corpus),
              zenodo_info[["metadata"]][["version"]]
            )
          )
          if (userinput != 1L){
            stop(sprintf("Aborting - existing version of %s remains unchanged. ", toupper(corpus)))
          }
        } else {
          userinput <- menu(
            choices = c("Yes / continue", "No / abort"), 
            title = sprintf(
              "Corpus %s (version: %s) is already installed. Do you want to replace it by version %s?",
              toupper(corpus), version_old, zenodo_info[["metadata"]][["version"]]
            )
          )
          if (userinput != 1L){
            stop(
              sprintf(paste("Aborting - existing version of %s remains unchanged. ",
              "If you want to keep the existing %s version, rename it using cwbtools::corpus_rename(), ",
              "and call zenodo_download_corpus() again."
              ),
              toupper(corpus),
              toupper()
              )
            ) 
          }
        }
      }
      corpus_remove(corpus = toupper(corpus), registry_dir = registry_dir, ask = ask)
    }
  }
  
  cwb_dirs <- cwb_directories()
  if (is.null(cwb_dirs)) cwb_dirs <- create_cwb_directories() # will trigger interactive dialogue
  
  corpus_tarball <- grep(
    sprintf("^.*/%s_v\\d+\\.\\d+\\.\\d+\\.tar\\.gz$", tolower(corpus)),
    zenodo_info[["files"]][["links"]][["self"]],
    value = TRUE
  )
  if (isFALSE(quiet)) message("... downloading tarball: ", corpus_tarball)
  
  corpus_install(
    pkg = NULL, 
    tarball = corpus_tarball,
    registry_dir = cwb_dirs[["registry_dir"]],
    corpus_dir = cwb_dirs[["corpus_dir"]], 
    verbose = !quiet
  )
  
  regdata <- registry_file_parse(corpus = tolower(corpus), registry_dir = cwb_dirs[["registry_dir"]])
  regdata[["properties"]][["doi"]] <- doi
  regdata[["properties"]][["version"]] <- zenodo_info[["metadata"]][["version"]]
  regdata[["home"]] <- file.path(cwb_dirs[["corpus_dir"]], tolower(corpus))
  registry_file_write(data = regdata, corpus = tolower(corpus),  registry_dir = cwb_dirs[["registry_dir"]])
  
  if (isNamespaceLoaded("polmineR")){
    file.copy(
      from = file.path(cwb_dirs[["registry_dir"]], tolower(corpus)),
      to = file.path(polmineR::registry(), tolower(corpus)),
      overwrite = TRUE
    )
    polmineR::registry_reset(registryDir = polmineR::registry())
  }
  return(TRUE)
}


#' @description \code{zenodo_info} is a helper to extract
#' @export zenodo_info
#' @rdname zenodo
zenodo_info <- function(doi){
  if (isFALSE(grepl("^.*?10\\.5281/zenodo\\.\\d+$", doi))){
    stop("Argument 'doi' is expected to offer a DOI (Digital Object Identifier) that refers to data",
         "hosted with zenodo, i.e. starting with 10.5281/zenodo.")
  }
  record_id <- gsub("^.*?10\\.5281/zenodo\\.(\\d+)$", "\\1", doi)
  zenodo_api_url <- sprintf("https://zenodo.org/api/records/%d", as.integer(record_id))
  fromJSON(getURL(zenodo_api_url))
}