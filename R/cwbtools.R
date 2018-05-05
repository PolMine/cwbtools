#' R-Package 'cwbtools'.
#' 
#' Toolset to Manage CWB Corpora.
#'
#' @aliases cwbtools-package cwbtools
#' @docType package
#' @name cwbtools
#' @rdname cwbtools
#' @author Andreas Blaette
#' @import R6
#' @examples 
#' library(tm)
#' reut21578 <- system.file("texts", "crude", package = "tm")
#' reuters.tm <- VCorpus(DirSource(reut21578), list(reader = readReut21578XMLasPlain))
#' 
#' library(tidytext)
#' reuters.tibble <- tidy(reuters.tm)
#' reuters.tibble[["topics_cat"]] <- sapply(
#'   reuters.tibble[["topics_cat"]],
#'   function(x) paste(reuters.tibble[["topics_cat"]], collapse = "|")
#' )
#' reuters.tibble[["places"]] <- sapply(
#'   reuters.tibble[["places"]],
#'   function(x) paste(x, collapse = "|")
#' )
#' reuters.tidy <- unnest_tokens(
#'   reuters.tibble, output = "word", input = "text", to_lower = FALSE
#' )
#' 
#' cdata <- list(
#'   tokenstream = as.data.table(reuters.tidy[, c("id", "word")]),
#'   metadata = as.data.table(reuters.tibble[,c("id", "topics_cat", "places", "language")])
#'   )
#' cdata <- add_corpus_positions(cdata)
#' 
#' registry_dir_tmp <- tempdir()
#' data_dir_tmp <- tempdir()
#' 
#' encode_corpusdata(
#'   cdata, corpus = "REUTERS", encoding = "utf8",
#'   registry_dir = registry_dir_tmp, data_dir = data_dir_tmp
#'   )
#' 
NULL
