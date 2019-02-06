setOldClass("String")
setOldClass("Annotation")


#' Decode Corpus as Annotation Object.
#' 
#' @param x An object that can be processed by \code{polmineR::get_token_stream}.
#' @param type Either "word" or "sentence".
#' @return A \code{Annotation} object.
#' @examples
#' \dontrun{
#' use("RcppCWB")
#' p <- partition("UNGA", date = "2018-03-20", who = "Al Hussein")
#' s <- NLP::String(as.String(p))
#' a <- as.Annotation(p, type = "word")
#' sent <- as.Annotation(p, type = "sentence")
#' a1 <- c(sent, a)
#' 
#' words <- s[a1[a1$type == "word"]]
#' sentences <- s[a1[a1$type == "sentence"]]
#' }
#' @name as.Annotation
decode <- function(x, type = c("word", "sentence")){
  if (requireNamespace(package = "NLP", quietly = TRUE))
    stop("Package 'NLP' required but not available")
  if (requireNamespace(package = "polmineR", quietly = TRUE))
    stop("Package 'polmineR' required but not available")
  
  if (type == "word"){
    word <- polmineR::get_token_stream(x, p_attribute = "word")
    pos <- polmineR::get_token_stream(x, p_attribute = "pos")
    whitespace_after <- c(ifelse(pos %in% c("$.", "$,", ":", ",", "$"), FALSE, TRUE)[2L:length(pos)], FALSE)
    word_with_whitespace <- paste(word, ifelse(whitespace_after, " ", ""), sep = "")
    s <- paste(word_with_whitespace, collapse = "")
    word_length <- sapply(word, nchar)
    left_offset <- c(1, (cumsum(sapply(word_with_whitespace, nchar)) + 1L)[1L:(length(word) - 1L)] )
    names(left_offset) <- word
    right_offset <- left_offset + word_length - 1L
    names(right_offset) <- word
    cpos <- unlist(apply(x@cpos, 1, function(x) x[1]:x[2]))
    y <- NLP::Annotation(
      id = cpos,
      rep.int("word", length(cpos)),
      start = left_offset,
      end = right_offset
    )
    return(y)
  } else if (type == "sentence"){
    word <- polmineR::get_token_stream(x, p_attribute = "word")
    pos <- polmineR::get_token_stream(x, p_attribute = "pos")
    whitespace_after <- c(ifelse(pos %in% c("$.", "$,", ":", ",", "$"), FALSE, TRUE)[2L:length(pos)], FALSE)
    word_with_whitespace <- paste(word, ifelse(whitespace_after, " ", ""), sep = "")
    s <- paste(word_with_whitespace, collapse = "")
    word_length <- sapply(word, nchar)
    left_offset <- c(1, (cumsum(sapply(word_with_whitespace, nchar)) + 1L)[1L:(length(word) - 1L)] )
    names(left_offset) <- word
    right_offset <- left_offset + word_length
    names(right_offset) <- word
    m <- matrix(data = c(left_offset, right_offset), ncol = 2, byrow = FALSE)
    f <- cut(x = 1L:length(pos), breaks = unique(c(1L, grep("\\$\\.", pos), length(pos))), include.lowest = TRUE)
    chunks <- split(x = m, f = f)
    sentence_left <- sapply(chunks, min)
    sentence_right <- sapply(chunks, max) - 1L
    y <- NLP::Annotation(
      id = 1L:length(sentence_left),
      rep.int("sentence", length(sentence_left)),
      start = sentence_left,
      end = sentence_right
    )
    return(y)
  }
}