#' Tokenize 
#' 
#' @export tokenize_corpusdata
tokenize_corpusdata <- function(x, verbose = TRUE){
  x[["tokenstream"]] <- x[["text"]][,{tokenize_words(.SD[["text"]], lowercase = FALSE)}, by = "id"]
  setnames(x[["tokenstream"]], old = "V1", new = "word")
  add_corpus_positions(x, verbose = verbose)
}