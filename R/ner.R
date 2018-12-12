setOldClass("String")
setOldClass("Annotation")

#' @examples
#' library(polmineR)
#' use("GermaParl")
#' p <- partition("GERMAPARL", date = "2016-12-16")
#' y <- as(p, "String")
setAs(from = "partition", to = "String", def = function(from){
  word <- get_token_stream(from, p_attribute = "word")
  whitespace_after <- c(ifelse(word %in% c(".", ",", ":", "!", "?", "(", "["), FALSE, TRUE)[2L:length(word)], FALSE)
  word_with_whitespace <- paste(word, ifelse(whitespace_after, " ", ""), sep = "")
  y <- paste(word_with_whitespace, collapse = "")
  NLP::String(y)
})

#' @examples
#' library(polmineR)
#' use("UNGA")
#' p <- partition("UNGA", date = "2018-03-20", who = "Rybakov")
#' p <- partition("UNGA", date = "2018-03-20", who = "Al Hussein")
#' p <- partition("UNGA", date = "2018-03-20")
#' s <- as(p, "String")
#' a <- as(p, "Annotation")
#' 
#' s[a[a$type == "sentence"]] # show sentences
#' 
#' library(openNLP)
#' install.packages("openNLPmodels.de", repos = "http://datacube.wu.ac.at/", type = "source")
#' 
#' entity_annotator <- Maxent_Entity_Annotator(language = "en", kind = "person")
#' entity_annotator <- Maxent_Entity_Annotator(language = "en", kind = "date")
#' entity_annotator <- Maxent_Entity_Annotator(language = "en", kind = "location")
#' entity_annotator <- Maxent_Entity_Annotator(language = "en", kind = "organization")
#' ner <- NLP::annotate(s, entity_annotator, a)
#' s[ner[ner$type == "entity"]]
setAs(from = "partition", to = "Annotation", def = function(from){
  word <- get_token_stream(from, p_attribute = "word")
  whitespace_after <- c(ifelse(word %in% c(".", ",", ":", "!", "?", "(", "["), FALSE, TRUE)[2L:length(word)], FALSE)
  word_with_whitespace <- paste(word, ifelse(whitespace_after, " ", ""), sep = "")
  word_length <- sapply(word, nchar)
  left_offset <- c(1, (cumsum(sapply(word_with_whitespace, nchar)) + 1L)[1L:(length(word) - 1L)] )
  names(left_offset) <- word
  right_offset <- left_offset + word_length - 1L 
  names(right_offset) <- word
  cpos <- unlist(apply(from@cpos, 1, function(x) x[1]:x[2]))
  word_annotation <- NLP::Annotation(
    id = cpos,
    rep.int("word", length(cpos)),
    start = left_offset,
    end = right_offset
  )
  
  m <- matrix(data = c(left_offset, right_offset), ncol = 2, byrow = FALSE)
  f <- cut(
    x = 1L:length(word),
    breaks = unique(c(1L, grep("[.!?:]", word), length(word))),
    include.lowest = TRUE
  )
  chunks <- split(x = m, f = f)
  sentence_left <- sapply(chunks, min)
  sentence_right <- sapply(chunks, max)
  sentence_annotation <- NLP::Annotation(
    id = 1L:length(sentence_left),
    rep.int("sentence", length(sentence_left)),
    start = sentence_left,
    end = sentence_right
  )
  c(word_annotation, sentence_annotation)
})


#' @examples
#' library(polmineR)
#' use("GermaParl")
#' use("UNGA")
#' p <- partition("KEYWORDS_EN", text_date = "2012-12-31")
#' s <- NLP::String(as.String(p))
#' a <- as.Annotation(p, type = "word")
#' sent <- as.Annotation(p, type = "sentence")
#' a1 <- c(sent, a)
#' foo <- s[a1[a1$type == "word"]]
#' 
#' sentences <- s[a1[a1$type == "sentence"]]
#' 
#' library(openNLP)
#' install.packages("openNLPmodels.de", repos = "http://datacube.wu.ac.at/", type = "source")
#' 
#' entity_annotator <- Maxent_Entity_Annotator(language = "de")
#' ner <- annotate(s, entity_annotator, a1)
#' s[ner]
#' s[entity_annotator(s, a2)]
as.Annotation <- function(x, type = c("word", "sentence")){
  if (type == "word"){
    word <- get_token_stream(x, p_attribute = "word")
    pos <- get_token_stream(x, p_attribute = "pos")
    whitespace_after <- c(ifelse(pos %in% c("$.", "$,", ":", ",", "$"), FALSE, TRUE)[2L:length(pos)], FALSE)
    word_with_whitespace <- paste(word, ifelse(whitespace_after, " ", ""), sep = "")
    s <- paste(word_with_whitespace, collapse = "")
    word_length <- sapply(word, nchar)
    left_offset <- c(1, (cumsum(sapply(word_with_whitespace, nchar)) + 1L)[1L:(length(word) - 1L)] )
    names(left_offset) <- word
    right_offset <- left_offset + word_length - 1L 
    names(right_offset) <- word
    cpos <- unlist(apply(x@cpos, 1, function(x) x[1]:x[2]))
    y <- Annotation(
      id = cpos,
      rep.int("word", length(cpos)),
      start = left_offset,
      end = right_offset
    )
    return(y)
  } else if (type == "sentence"){
    word <- get_token_stream(x, p_attribute = "word")
    pos <- get_token_stream(x, p_attribute = "pos")
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
    y <- Annotation(
      id = 1L:length(sentence_left),
      rep.int("sentence", length(sentence_left)),
      start = sentence_left,
      end = sentence_right
    )
    return(y)
  }
}