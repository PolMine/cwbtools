#' Encode Positional Attribute(s).
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
#' steps are enough to use the CQP functionality. To run
#' \code{p_attribute_huffcode} and \code{p_attribute_compress_rdx}, an
#' installation of the CWB may be necessary.
#' 
#' See the CQP Corpus Encoding Tutorial
#' (\url{http://cwb.sourceforge.net/files/CWB_Encoding_Tutorial.pdf}) for an
#' explanation of the procedure (section 3, ``Indexing and compression without
#' CWB/Perl'').
#' 
#' @param corpus The CWB corpus (needed by \code{p_attribute_huffcode} and \code{p_attribute_compress_rdx}).
#' @param registry_dir Registry directory (needed by \code{p_attribute_huffcode} and \code{p_attribute_compress_rdx}).
#' @param token_stream A character vector with the tokens of the corpus.
#' @param compress Logical.
#' @param verbose Logical.
#' @param method Either 'CWB' or 'R'.
#' @param p_attribute The positional attribute.
#' @param data_dir The data directory for the corpus with the binary files.
#' @param encoding Encoding as defined in the charset corpus property of the
#'   registry file for the corpus ('latin1' to 'latin9', and 'utf8').
#' @export p_attribute_encode
#' @rdname p_attribute_encode
#' @examples
#' library(RcppCWB)
#' if (!cwb_is_installed()) cwb_install()
#' tokens <- readLines(system.file(package = "RcppCWB", "extdata", "examples", "reuters.txt"))
#' 
#' tmpdir <- normalizePath(tempdir(), winslash = "/")
#' if (.Platform$OS.type == "windows") tmpdir <- normalizePath(tmpdir, winslash = "/")
#' registry_tmp <- file.path(tmpdir, "registry", fsep = "/")
#' data_dir_tmp <- file.path(tmpdir, "data_dir", fsep = "/")
#' if (dir.exists(registry_tmp)) unlink(registry_tmp, recursive = TRUE)
#' if (dir.exists(data_dir_tmp)) unlink(data_dir_tmp, recursive = TRUE)
#' dir.create (registry_tmp)
#' dir.create(data_dir_tmp)
#' 
#' p_attribute_encode(
#'   corpus = "reuters",
#'   token_stream = tokens, p_attribute = "word",
#'   data_dir = data_dir_tmp, method = "R",
#'   registry_dir = registry_tmp,
#'   compress = FALSE,
#'   encoding = "utf8"
#'   )
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
#' @importFrom stringi stri_detect_regex stri_replace_all
p_attribute_encode <- function(
  token_stream, p_attribute = "word", registry_dir, corpus, data_dir, method = c("R", "CWB"),
  verbose = TRUE, encoding = get_encoding(token_stream),
  compress = NULL
){
  if (!encoding %in% c("ascii", paste0("latin", 1:9), "utf8")){
    stop("encoding required to be ascii, latin1 to latin9, utf8 by cwb-encode")
  }
  
  # the registry file will not accept tilde as a shortcut for the user's home directory
  # so we expand it
  registry_dir <- path.expand(registry_dir)
  if (!file.exists(registry_dir)) stop("registry_dir does not exist")
  data_dir <- path.expand(data_dir)
  if (!file.exists(data_dir)) stop("data_dir does not exist")
  registry_file <- file.path(registry_dir, tolower(corpus), fsep = "/")
  
  if (method == "R"){
    
    if (verbose) message("... writing tokenstream to disk (directly from R, equivalent to cwb-encode)")
    corpus_file <- file.path(data_dir, paste(p_attribute, "corpus", sep = "."), fsep = "/")
    lexicon_file <- file.path(data_dir, paste(p_attribute, "lexicon", sep = "."), fsep = "/")
    lexicon_index_file <- file.path(data_dir, paste(p_attribute, "lexicon.idx", sep = "."), fsep = "/")
    
    if (verbose) message("... creating indices (in memory)")
    tokenstream_factor <- factor(token_stream, levels = unique(token_stream))
    rm(token_stream); gc()
    
    ids <- as.integer(tokenstream_factor) - 1L
    if (verbose) message("... writing file: ", basename(corpus_file))
    writeBin(object = ids, size = 4L, endian = "big", con = corpus_file)
    rm(ids); gc()
    
    lexicon <- levels(tokenstream_factor)
    rm(tokenstream_factor); gc()
    
    if (encoding == "latin1"){
      lexicon_hex_list <- iconv(x = lexicon, from = "UTF-8", to = toupper(encoding), toRaw = TRUE)
    } else {
      lexicon_hex_list <- lapply(lexicon, charToRaw)
    }
    rm(lexicon); gc()
    
    lexicon_hex_list <- lapply(lexicon_hex_list, function(x) c(x, as.raw(0)))
    lexicon_hex_vec <- unlist(lexicon_hex_list)
    if (verbose) message("... writing file: ", basename(lexicon_file))
    writeBin(object = lexicon_hex_vec, con = lexicon_file)
    
    idx_raw <- cumsum(sapply(lexicon_hex_list, length))
    rm(lexicon_hex_list)
    idx <- c(0L, idx_raw[1:(length(idx_raw) - 1L)])
    if (verbose) message("... writing file: ", basename(lexicon_index_file))
    writeBin(object = idx, size = 4L, endian = "big", con = lexicon_index_file)
    rm(idx_raw); gc()
    ### equivalent to cwb-makeall (build index files)
    
    if (FALSE){
      df <- data.frame(id = ids, word = token_stream, stringsAsFactors = FALSE)
      df[["cpos"]] <- 0L:(nrow(df) - 1L)
      
      corpus_cnt_file <- file.path(data_dir, paste(p_attribute, "corpus.cnt", sep = "."), fsep = "/")
      corpus_rev_file <- file.path(data_dir, paste(p_attribute, "corpus.rev", sep = "."), fsep = "/")
      corpus_rdx_file <- file.path(data_dir, paste(p_attribute, "corpus.rdx", sep = "."), fsep = "/")
      lexicon_srt_file <- file.path(data_dir, paste(p_attribute, "lexicon.srt", sep = "."), fsep = "/")
      
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
      # This is what I do not understand sufficiently:
      # it works for REUTERS, but not for AUSTEN or UNGA
      # potantially, it is a character encoding issue
      sorted <- order(lexicon) - 1L
      writeBin(object = sorted, size = 4, endian = "big", con = lexicon_srt_file)
    }
    
  } else if (method == "CWB"){
    vrt_tmp_file <- tempfile()
    data.table::fwrite(
      list(token_stream = token_stream), file = vrt_tmp_file,
      col.names = FALSE, quote = FALSE, showProgress = interactive()
    )
    
    if (p_attribute == "word"){
      if (verbose) message("... running cwb-encode")
      executable <- if (.Platform$OS.type == "windows") "cwb-encode.exe" else "cwb-encode"
      system2(
        command = file.path(cwb_get_bindir(), executable, fsep = "/"),
        args = c(sprintf("-d %s", data_dir), sprintf("-f %s", vrt_tmp_file), sprintf("-R %s", registry_file), sprintf("-c %s", encoding), "-v")
      )
    } else {
      # Add positional attribute to a corpus that already exists
      # some checks at first
      if (length(token_stream) != cl_attribute_size(corpus = toupper(corpus), attribute = "word", attribute_type = "p", registry = registry_dir))
        stop("Length of character vector must be identical with size of corpus - not TRUE")
      
      if (p_attribute %in% registry_file_parse(corpus = tolower(corpus), registry_dir = registry_dir)[["p_attribute"]])
        stop("p-attribute already exists")
      
      if (verbose) message("... calling cwb-encode")
      p_attrs_old <- registry_file_parse(corpus = tolower(corpus), registry_dir = registry_dir)[["p_attributes"]] # for checking later if anything is missing
      executable <- if (.Platform$OS.type == "windows") "cwb-encode.exe" else "cwb-encode"
      cwb_encode_cmd_vec <- c(
        file.path(cwb_get_bindir(), executable, fsep = "/"),
        "-d", data_dir, "-f", vrt_tmp_file, "-R", registry_file,
        "-p", "-", "-P", p_attribute, "-c", encoding
      )
      system(paste0(cwb_encode_cmd_vec, collapse = " "))
    }
    
  }
  
  # create or augment registry file
  if (file.exists(registry_file)){
    if (verbose) message("... reading existing registry file")
    regdata <- registry_file_parse(corpus = tolower(corpus), registry_dir = registry_dir)
    
    p_attributes <- c(regdata[["p_attributes"]], if (exists("p_attrs_old")) p_attrs_old else character(), p_attribute)
    regdata[["p_attributes"]] <- unique(p_attributes)
  } else {
    if (verbose) message("... creating data for new registry file")
    regdata <- registry_data(
      name = toupper(corpus), id = tolower(corpus),
      home = data_dir, properties = c(charset = encoding),
      p_attributes = p_attribute
    )
  }
  if (verbose) message("... writing registry file")
  registry_file_write(regdata, corpus = tolower(corpus), registry_dir = registry_dir)
  
  # create reverse index using cwb-makeall
  if (verbose) message("... calling cwb-makeall")
  system2(
    command = file.path(
      cwb_get_bindir(),
      if (.Platform$OS.type == "windows") "cwb-makeall.exe" else "cwb-makeall",
      fsep = "/"
      ),
    args = c(sprintf("-r %s", registry_dir), sprintf("-P %s", p_attribute), "-V", toupper(corpus))
  )
  
  if (compress){
    compression_cmd_args <- c(sprintf("-r %s", registry_dir), sprintf("-P %s", p_attribute), toupper(corpus))
    system2(
      command = file.path(
        cwb_get_bindir(),
        if (.Platform$OS.type == "windows") "cwb-huffcode.exe" else "cwb-huffcode",
        fsep = "/"),
      args = compression_cmd_args, stdout = TRUE
      )
    system2(
      command = file.path(
        cwb_get_bindir(),
        if (.Platform$OS.type == "windows") "cwb-compress-rdx.exe" else "cwb-compress-rdx",
        fsep = "/"
        ),
      args = compression_cmd_args,
      stdout = TRUE
      )
    files_to_remove <- c(
      rdx_file = file.path(data_dir, sprintf("%s.corpus.rdx", p_attribute), fsep = "/"),
      rev_file = file.path(data_dir, sprintf("%s.corpus.rev", p_attribute), fsep = "/")
    )
    for (x in files_to_remove){
      if (file.exists(x)) {
        if (file.remove(x)) if (verbose) message("... file successfully removed: ", basename(x))
      }
    }
  }
  
}


#' @details \code{p_attribute_recode} will recode the values in the avs-file and change
#' the attribute value index in the avx file. The rng-file remains unchanged. The registry
#' file remains unchanged, and it is highly recommended to consider \code{s_attribute_recode}
#' as a helper for \code{corpus_recode} that will recode all s-attributes, all p-attributes,
#' and will reset the encoding in the registry file.
#' @param from Character string describing the current encoding of the attribute.
#' @param to Character string describing the target encoding of the attribute.
#' @rdname p_attribute_encode
#' @rdname p_attribute
p_attribute_recode <- function(data_dir, p_attribute, from = c("UTF-8", "latin1"), to = c("UTF-8", "latin1")){
  
  p_attr_lexicon_file <- file.path(data_dir, sprintf("%s.lexicon", p_attribute), fsep = "/")
  
  lexicon <- readBin(
    con = p_attr_lexicon_file,
    what = character(),
    n = file.info(p_attr_lexicon_file)$size
  )
  Encoding(lexicon) <- from
  
  lexicon_hex_list <- iconv(x = lexicon, from = toupper(from), to = toupper(to), toRaw = TRUE)
  lexicon_hex_list <- lapply(lexicon_hex_list, function(x) c(x, as.raw(0)))
  lexcion_hex_vec <- unlist(lexicon_hex_list)
  
  writeBin(object = lexcion_hex_vec, con = p_attr_lexicon_file)
  
  # generate and write index file
  p_attr_lexicon_index_file <- file.path(data_dir, sprintf("%s.lexicon.idx", p_attribute), fsep = "/")
  
  index_new <- cumsum(sapply(lexicon_hex_list, length))
  index_new <- c(0L, index_new[1L:(length(index_new) - 1L)])
  
  writeBin(
    object = index_new,
    con = p_attr_lexicon_index_file, size = 4L,
    endian = "big"
    )
  
  invisible( NULL )
}
