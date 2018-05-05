#' Encode CWB Corpus.
#' 
#' Create indexed and compressed positional attribute from a character vector of
#' tokens (the token stream), and structural annotations.
#' 
#' Four steps generate the binary CWB corpus data format for positional
#' attributes: First, encode a character vector (the token stream) using
#' \code{p_attribute_encode}. Second, create reverse index using
#' \code{p_attribute_makeall}. Third, compress token stream using
#' \code{p_attribute_huffcode}. Fourth, compress index files using
#' \code{p_attribute_compress_rdx}.
#' 
#' The implementation for the first two steps (\code{p_attribute_encode} and
#' \code{p_attribute_makeall}) is a pure R implementation (so far). These two
#' steps are enough to use the CQP functionality.
#'
#' To run \code{p_attribute_huffcode} and \code{p_attribute_compress_rdx}, an
#' installation of the CWB is required.
#' 
#' See the CQP Corpus Encoding Tutorial
#' (\url{http://cwb.sourceforge.net/files/CWB_Encoding_Tutorial.pdf}) for an
#' explanation of the procedure (section 3, ``Indexing and compression without
#' CWB/Perl'').
#' 
#' @param corpus the CWB corpus (needed by \code{p_attribute_huffcode} and \code{p_attribute_compress_rdx})
#' @param registry_dir registry directory (needed by \code{p_attribute_huffcode} and \code{p_attribute_compress_rdx})
#' @param token_stream a character vector with the tokens of the corpus
#' @param p_attribute the positional attribute
#' @param s_attribute the structural attribute
#' @param data_dir the data directory for the corpus with the binary files
#' @param values values for structural annotations
#' @param region_matrix a matrix with left and right corpus positions (regions) of structural annotations
#' @export p_attribute_encode
#' @name encode
#' @rdname encode
#' @examples
#' tokens <- readLines(system.file(package = "RcppCWB", "extdata", "examples", "reuters.txt"))
#' 
#' tmpdir <- tempdir()
#' if (.Platform$OS.type == "windows") tmpdir <- normalizePath(tmpdir, winslash = "/")
#' registry_tmp <- file.path(tmpdir, "registry")
#' data_dir_tmp <- file.path(tmpdir, "data_dir")
#' dir.create (registry_tmp)
#' dir.create(data_dir_tmp)
#' 
#' p_attribute_encode(
#'   token_stream = tokens, p_attribute = "word",
#'   data_dir = data_dir_tmp
#'   )
#' p_attribute_makeall(
#'   token_stream = tokens, p_attribute = "word",
#'   data_dir = data_dir_tmp
#' )
#' 
#' regfile <- registry_file_write(
#'   registry_dir = registry_tmp, corpus = "REUTERS", data_dir = data_dir_tmp,
#'   corpus_properties = c(encoding = "utf-8", language = "en"), p_attributes = "word"
#'   )
#' if (cqp_is_initialized()) cqp_reset_registry(registry_tmp) else cqp_initialize(registry_tmp)
#' 
#' cqp_query(corpus = "REUTERS", query = '[]{3} "oil" []{3};')
#' regions <- cqp_dump_subcorpus(corpus = "REUTERS")
#' kwic <- apply(regions, 1, function(region){
#'   ids <- cl_cpos2id("REUTERS", "word", registry_tmp, cpos = region[1]:region[2])
#'   words <- cl_id2str(corpus = "REUTERS", p_attribute = "word", registry = registry_tmp, id = ids)
#'   paste0(words, collapse = " ")
#' })
#' kwic[1:10]
#' @export p_attribute_encode
p_attribute_encode <- function(token_stream, p_attribute = "word", data_dir){
  tokenstream_factor <- factor(token_stream, levels = unique(token_stream))
  ids <- as.integer(tokenstream_factor) - 1L
  lexicon <- levels(tokenstream_factor)
  idx <- c(0L, grep("\\|", strsplit(paste(lexicon, collapse = "|"), "")[[1]]))
  
  corpus_file <- file.path(data_dir, paste(p_attribute, "corpus", sep = "."))
  lexicon_file <- file.path(data_dir, paste(p_attribute, "lexicon", sep = "."))
  lexicon_index_file <- file.path(data_dir, paste(p_attribute, "lexicon.idx", sep = "."))
  
  writeBin(object = ids, size = 4L, endian = "big", con = corpus_file)
  writeBin(object = lexicon, con = lexicon_file)
  writeBin(object = idx, size = 4L, endian = "big", con = lexicon_index_file)
  return( TRUE )
}


#' @rdname encode
#' @export p_attribute_makeall
p_attribute_makeall <- function(token_stream, p_attribute = "word", data_dir){
  
  tokenstream_factor <- factor(token_stream, levels = unique(token_stream))
  ids <- as.integer(tokenstream_factor) - 1L
  lexicon <- unique(token_stream)
  
  df <- data.frame(id = ids, word = token_stream, stringsAsFactors = FALSE)
  df[["cpos"]] <- 0L:(nrow(df) - 1L)
  
  corpus_cnt_file <- file.path(data_dir, paste(p_attribute, "corpus.cnt", sep = "."))
  corpus_rev_file <- file.path(data_dir, paste(p_attribute, "corpus.rev", sep = "."))
  corpus_rdx_file <- file.path(data_dir, paste(p_attribute, "corpus.rdx", sep = "."))
  lexicon_srt_file <- file.path(data_dir, paste(p_attribute, "lexicon.srt", sep = "."))
  
  # generate cnt file
  cnt_array <- tapply(X = df[["cpos"]], INDEX = df[["id"]], FUN = length)
  cnt_matrix <- as.matrix(cnt_array)
  cnt_vector <- unname(cnt_matrix[,1])
  writeBin(object = cnt_vector, size = 4, endian = "big", con = corpus_cnt_file)
  
  # generate rev file
  df_rev <- df[order(df[["id"]]),]
  writeBin(object = df_rev[["cpos"]], size = 4, endian = "big", con = corpus_rev_file)
  
  # generate rdx file
  positions <- c(0L, cumsum(cnt_vector)[1:(length(cnt_vector) - 1)])
  writeBin(object = positions, size = 4, endian = "big", con = corpus_rdx_file)
  
  # generate srt file
  sorted <- order(lexicon) - 1L
  writeBin(object = sorted, size = 4, endian = "big", con = lexicon_srt_file)
  
  return( TRUE )
}

#' @rdname encode
#' @export p_attribute_huffcode
p_attribute_huffcode <- function(corpus, p_attribute, registry_dir = Sys.getenv("CORPUS_REGISTRY")){
  cmd <- c(
    "cwb-huffcode",
    "-r", registry_dir,
    "-P", p_attribute,
    toupper(corpus)
  )
  system(paste(cmd, collapse = " "))
}


#' @rdname encode
#' @export p_attribute_compress_rdx
p_attribute_compress_rdx <- function(corpus, p_attribute, registry_dir = Sys.getenv("CORPUS_REGISTRY")){
  cmd <- c(
    "cwb-compress-rdx",
    "-r", registry_dir,
    "-P", "word",
    toupper(corpus)
  )
  system(paste(cmd, collapse = " "))
}



#' @rdname encode
#' @export s_attribute_encode
s_attribute_encode <- function(values, data_dir, s_attribute, region_matrix){
  avs_file <- file.path(data_dir, paste(s_attribute, "avs", sep = ".")) # attribute values
  avx_file <- file.path(data_dir, paste(s_attribute, "avx", sep = ".")) # attribute value index
  rng_file <- file.path(data_dir, paste(s_attribute, "rng", sep = ".")) # ranges
  
  # generate and write attrib.avs
  values_unique <- unique(values)
  writeBin(object = values_unique, con = avs_file, useBytes = TRUE)
  
  # generate and write attrib.avx
  offset <- c(0L, cumsum(nchar(values_unique)[1L:(length(values_unique) - 1L)]))
  avx_matrix <- matrix(
    c(0L:(length(values) - 1L), offset[match(values, values_unique)]),
    ncol = 2, byrow = FALSE
  )
  avx_vector <- as.integer(t(avx_matrix))
  writeBin(object = avx_vector, con = avx_file, size = 4L, endian = "big")
  
  # generate and write attrib.rng
  region_vector <- as.vector(region_matrix)
  writeBin(object = region_vector, con = rng_file, size = 4L, endian = "big")
  return( TRUE )
}
