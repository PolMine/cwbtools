#' Encode Positional Attributes.
#' 
#' Pure R implementation to generate positional attribute from a character vector of
#' tokens (the token stream).
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
#' steps are enough to use the CQP functionality. To run \code{p_attribute_huffcode}
#' and \code{p_attribute_compress_rdx}, an installation of the CWB is required.
#' 
#' See the CQP Corpus Encoding Tutorial
#' (\url{http://cwb.sourceforge.net/files/CWB_Encoding_Tutorial.pdf}) for an
#' explanation of the procedure (section 3, ``Indexing and compression without
#' CWB/Perl'').
#' 
#' @param corpus the CWB corpus (needed by \code{p_attribute_huffcode} and \code{p_attribute_compress_rdx})
#' @param registry_dir registry directory (needed by \code{p_attribute_huffcode} and \code{p_attribute_compress_rdx})
#' @param token_stream a character vector with the tokens of the corpus
#' @param make logical
#' @param verbose logical
#' @param method either 'CWB' or 'R'
#' @param p_attribute the positional attribute
#' @param data_dir the data directory for the corpus with the binary files
#' @param encoding encoding of the data
#' @export p_attribute_encode
#' @rdname p_attribute_encode
#' @examples
#' library(RcppCWB)
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
#'   data_dir = data_dir_tmp, method = "R"
#'   )
#' p_attribute_makeall(
#'   token_stream = tokens, p_attribute = "word",
#'   data_dir = data_dir_tmp
#' )
#' 
#' regdata <- registry_data(
#'   id = "REUTERS", name = "Reuters Sample Corpus", home = data_dir_tmp,
#'   properties = c(encoding = "utf-8", language = "en"), p_attributes = "word"
#' )
#' regfile <- registry_file_write(
#'   data = regdata, corpus = "REUTERS",
#'   registry_dir = registry_tmp, data_dir = data_dir_tmp,
#' )
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
#' @importFrom RcppCWB cl_attribute_size
p_attribute_encode <- function(
  token_stream, p_attribute = "word", registry_dir, corpus, data_dir, method = c("R", "CWB"),
  verbose = TRUE, encoding = "latin1", make = NULL
  ){
  if (method == "R"){
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
  } else if (method == "CWB"){
    if (p_attribute == "word"){
      if (verbose) message("Creating new CWB indexed corpus ", corpus)
      if (any(grepl("^\\s*<.*?>\\s*$", token_stream)))
        warning("there is markup in the character vector - cwb-encode will issue warnings")
      
      # adjust encoding, if necessary
      input_enc <- get_encoding(token_stream)
      if (input_enc != encoding){
        token_stream <- iconv(token_stream, from = input_enc, to = encoding)
        Encoding(token_stream) <- encoding
      }
      
      if (verbose) message("... writing token stream to disk")
      vrt_tmp_file <- tempfile()
      data.table::fwrite(
        token_stream, file = vrt_tmp_file,
        col.names = FALSE, quote = FALSE, showProgress = TRUE
      )
      
      if (verbose) message("... running cwb-encode")
      cwb_encode_cmd_vec <- c(
        "cwb-encode",
        "-d", data_dir, 
        "-f", vrt_tmp_file,
        "-R", file.path(registry_dir, tolower(corpus)),
        "-c", encoding
      )
    } else {
      # Add positional attribute to a corpus that already exists
      # some checks
      if (length(token_stream) != cl_attribute_size(corpus = toupper(corpus), attribute = "word", attribute_type = "p", registry = registry_dir))
        stop("Length of character vector must be identical with size of corpus - not TRUE")
      
      if (p_attribute %in% registry_file_parse(tolower(corpus))[["p_attribute"]])
        stop("pAttribute already exists")
      
      if (any(grepl("^\\s*<.*?$", token_stream)))
        warning("there is markup in the character vector - cwb-encode will issue warnings")
      
      # adjust encoding, if necessary
      input_enc <- get_encoding(token_stream)
      if (input_enc != encoding){
        token_stream <- iconv(token_stream, from = input_enc, to = encoding)
        Encoding(token_stream) <- encoding
      }
      
      if (verbose) message("... writing vector to disk for p-attribute ", p_attribute)
      vrt_tmp_file <- tempfile()
      data.table::fwrite(
        token_stream, file = vrt_tmp_file,
        col.names = FALSE, quote = FALSE, showProgress = interactive()
      )
      
      if (verbose) message("... calling cwb-encode")
      p_attrs_old <- registry_file_parse(tolower(corpus))[["p_attributes"]] # for checking later if anything is missing
      cwb_encode_cmd_vec <- c(
        "cwb-encode",
        "-d", data_dir, # directory with indexed corpus files
        "-f", vrt_tmp_file,
        "-R", file.path(registry_dir, tolower(corpus)),
        "-p", "-", "-P", p_attribute,
        "-c", encoding
      )
      # cwb-encode may drop attributes from registry file apart from the newly encoded one ... 
      regdata <- registry_file_parse(tolower(corpus))
      regdata[["p_attributes"]] <- unique(c(p_attrs_old, regdata[["p_attributes"]]))
    }
    
    cwb_encode_cmd <- paste0(cwb_encode_cmd_vec, collapse = " ")
    
    system(cwb_encode_cmd)
    
    if (make) cwb_make(corpus = corpus, registry_dir = registry_dir, verbose = verbose)
    # polmineR::use(dir = registry_dir)
  }
}


#' @rdname p_attribute_encode
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

#' @rdname p_attribute_encode
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


#' @rdname p_attribute_encode
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
