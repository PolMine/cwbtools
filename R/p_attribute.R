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
#' The implementation for the first two steps (`p_attribute_encode()` and
#' `p_attribute_makeall()`) is a pure R implementation (so far). These two
#' steps are enough to use the CQP functionality. To run
#' `p_attribute_huffcode()` and `p_attribute_compress_rdx()`, an
#' installation of the CWB may be necessary.
#' 
#' See the CQP Corpus Encoding Tutorial
#' (\url{https://cwb.sourceforge.io/files/CWB_Encoding_Tutorial.pdf}) for an
#' explanation of the procedure (section 3, ``Indexing and compression without
#' CWB/Perl'').
#' 
#' @param corpus The CWB corpus (needed by `p_attribute_huffcode()` and
#'   `p_attribute_compress_rdx()`).
#' @param registry_dir Registry directory (needed by `p_attribute_huffcode()`
#'   and `p_attribute_compress_rdx()`).
#' @param token_stream A `character` vector with the tokens of the corpus. The
#'   maximum length is 2 147 483 647 (2^31 - 1); a warning is issued if this
#'   threshold is exceeded. See the [CWB Encoding
#'   Tutorial](https://cwb.sourceforge.io/files/CWB_Encoding_Tutorial.pdf) for
#'   size limitations of corpora. May also be a file.
#' @param compress A `logical` value. 
#' @param verbose A `logical` value.
#' @param method Either 'CWB' or 'R'.
#' @param p_attribute The positional attribute. May be more than one, if
#'   `method` is "CWB". If method is "R", only one positional attribute may be
#'   supplied.
#' @param data_dir The data directory for the corpus with the binary files.
#' @param encoding Encoding as defined in the charset corpus property of the
#'   registry file for the corpus ('latin1' to 'latin9', and 'utf8').
#' @export p_attribute_encode
#' @rdname p_attribute_encode
#' @examples
#' library(RcppCWB)
#' 
#' # In this example, we pursue a "pure R" approach. To rely on the "CWB"
#' # method, you can use the cwb_install() function, which will download and
#' # install the CWB command line # tools within the package.
#' 
#' tokens <- readLines(system.file(package = "RcppCWB", "extdata", "examples", "reuters.txt"))
#' 
#' # Create new (and empty) directory structure
#' 
#' tmpdir <- normalizePath(tempdir(), winslash = "/")
#' registry_tmp <- fs::path(tmpdir, "registry")
#' data_dir_tmp <- fs::path(tmpdir, "data_dir", "reuters")
#' if (file.exists(fs::path(data_dir_tmp, "word.corpus"))){
#'   file.remove(fs::path(data_dir_tmp, "word.corpus"))
#' }
#' if (dir.exists(registry_tmp)) unlink(registry_tmp, recursive = TRUE)
#' if (dir.exists(data_dir_tmp)) unlink(data_dir_tmp, recursive = TRUE)
#' dir.create(registry_tmp)
#' dir.create(data_dir_tmp, recursive = TRUE)
#' 
#' # Now encode token stream
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
#' # Create minimal registry file
#' 
#' regdata <- registry_data(
#'   id = "REUTERS", name = "Reuters Sample Corpus", home = data_dir_tmp,
#'   properties = c(encoding = "utf-8", language = "en"), p_attributes = "word"
#' )
#' 
#' regfile <- registry_file_write(
#'   data = regdata, corpus = "REUTERS",
#'   registry_dir = registry_tmp, data_dir = data_dir_tmp,
#' )
#' 
#' # Reload corpus and run query as a test
#' 
#' if (cqp_is_initialized()) cqp_reset_registry(registry_tmp) else cqp_initialize(registry_tmp)
#' 
#' cqp_query(corpus = "REUTERS", query = '[]{3} "oil" []{3};')
#' regions <- cqp_dump_subcorpus(corpus = "REUTERS")
#' kwic <- apply(
#'   regions, 1,
#'   function(region){
#'     ids <- cl_cpos2id("REUTERS", "word", registry_tmp, cpos = region[1]:region[2])
#'     words <- cl_id2str(corpus = "REUTERS", p_attribute = "word", registry = registry_tmp, id = ids)
#'     paste0(words, collapse = " ")
#'   }
#' )
#' kwic[1:10]
#' @export p_attribute_encode
#' @importFrom RcppCWB cl_attribute_size cwb_makeall cwb_huffcode cwb_compress_rdx
#' @importFrom stringi stri_detect_regex stri_replace_all
p_attribute_encode <- function(
  token_stream, p_attribute = "word", registry_dir, corpus, data_dir, method = c("R", "CWB"),
  verbose = TRUE, encoding = get_encoding(token_stream),
  compress = FALSE
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
  registry_file <- fs::path(registry_dir, tolower(corpus))
  
  if (length(token_stream) >= 2L^31L){
    warning(
      sprintf(
        "The maximum corpus size is 2 147 483 647 tokens, the length of the input vector is %d which is likely to fail.",
        length(token_stream)
      )
    )
  }
  
  if (method == "R"){
    
    if (length(token_stream) == 1L){
      if (!file.exists(token_stream)){
        stop("`token_stream` is a length 1 vector, but is not an existing file")
      }
    }
    
    if (length(p_attribute) != 1L)
      stop("If `method` is 'R', only one p-attribute can be processed.")
    
    if (verbose) message("... writing tokenstream to disk (directly from R, equivalent to cwb-encode)")
    corpus_file <- fs::path(data_dir, paste(p_attribute, "corpus", sep = "."))
    lexicon_file <- fs::path(data_dir, paste(p_attribute, "lexicon", sep = "."))
    lexicon_index_file <- fs::path(data_dir, paste(p_attribute, "lexicon.idx", sep = "."))
    
    if (verbose) message("... creating indices (in memory)")
    tokenstream_factor <- factor(token_stream, levels = unique(token_stream))
    rm(token_stream); gc()
    
    ids <- as.integer(tokenstream_factor) - 1L

    if (getRversion() < R_system_version("4.0.0")){
      max_integer <- trunc((2^31 - 1) / 4)
      if (length(ids) > trunc((2^31 - 1) / 4)){
        stop(sprintf(
          paste(
            "You are using R %s. Due to a limitation of writeBin() to process long integer vectors before R v4.0.0,",
            "writing the p-Attribute to disk is not possible:",
            "The length of the p-Attribute is %d but the maximum length that can be processed is %d.",
            "Proposed solution: Install R v4.0.0 or higher.",
            collapse = " "
          ),
          getRversion(), length(ids), max_integer
        ))
      }
    }
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

  } else if (method == "CWB"){
    if (length(token_stream) == 1L){
      if (file.exists(token_stream)){
        vrt_file <- token_stream
      } else {
        stop("Argument `token_stream` is length 1, but not an existing file")
      }
    } else {
      vrt_file <- tempfile()
      data.table::fwrite(
        list(token_stream = token_stream), file = vrt_file,
        col.names = FALSE, quote = FALSE, showProgress = interactive()
      )
    }
    
    if ("word" %in% p_attribute){
      if (verbose) message("... running cwb-encode")
      system2(
        command = fs::path(
          cwb_get_bindir(),
          if (.Platform$OS.type == "windows") "cwb-encode.exe" else "cwb-encode"
        ),
        args = c(
          sprintf("-d %s", normalizePath(data_dir)),
          sprintf("-f %s", normalizePath(vrt_file)),
          sprintf("-R %s", normalizePath(registry_file, mustWork = FALSE)),
          sprintf("-c %s", encoding), "-v")
      )
    } else {
      # Add positional attribute to a corpus that already exists
      # some checks at first
      corpus_size <- cl_attribute_size(
        corpus = toupper(corpus),
        attribute = "word",
        attribute_type = "p",
        registry = registry_dir
      )
      if (length(token_stream) != 1L){
          if (length(token_stream) != corpus_size)
            stop("Length of `token_stream` and corpus size differ!")
      }
      
      p_attrs_old <- registry_file_parse(
        corpus = tolower(corpus),
        registry_dir = registry_dir
      )[["p_attributes"]]
      if (any(p_attribute %in% p_attrs_old)) stop("existing p-attribute")
      
      if (verbose) message("... calling cwb-encode")
      cwb_encode_cmd_vec <- c(
        fs::path(
          cwb_get_bindir(),
          if (.Platform$OS.type == "windows") "cwb-encode.exe" else "cwb-encode"
        ),
        "-d", normalizePath(data_dir),
        "-f", normalizePath(vrt_file),
        "-p", "-",
        paste("-P", p_attribute, sep = " "),
        "-c", encoding,
        "-v"
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
      name = toupper(corpus),
      id = tolower(corpus),
      home = data_dir,
      properties = c(charset = encoding),
      p_attributes = p_attribute
    )
  }
  if (verbose) message("... writing registry file")
  registry_file_write(
    regdata,
    corpus = tolower(corpus),
    registry_dir = registry_dir
  )
  
  if (method == "CWB"){
    if (verbose) message("... calling cwb-makeall")
    for (p_attr in p_attribute){
      system2(
        command = fs::path(
          cwb_get_bindir(),
          if (.Platform$OS.type == "windows") "cwb-makeall.exe" else "cwb-makeall"
        ),
        args = c(
          sprintf("-r %s", normalizePath(registry_dir)),
          sprintf("-P %s", p_attr),
          "-V", toupper(corpus))
      )
    }
    
    
    if (compress){
      for (p_attr in p_attribute){
        compression_cmd_args <- c(
          sprintf("-r %s", normalizePath(registry_dir)),
          sprintf("-P %s", p_attr),
          toupper(corpus)
        )
        system2(
          command = normalizePath(fs::path(
            cwb_get_bindir(),
            if (.Platform$OS.type == "windows") "cwb-huffcode.exe" else "cwb-huffcode"
          )),
          args = compression_cmd_args, stdout = TRUE
        )
        system2(
          command = normalizePath(fs::path(
            cwb_get_bindir(),
            if (.Platform$OS.type == "windows") "cwb-compress-rdx.exe" else "cwb-compress-rdx"
          )),
          args = compression_cmd_args,
          stdout = TRUE
        )
      }
    }
  } else if (method == "R"){
    
    # Check whether corpus has been loaded and delete corpus if necessary
    # cwb_makeall will crash if corpus is loaded
    
    corpus_size <- RcppCWB::cl_attribute_size(
      corpus = corpus, attribute = "word", attribute_type = "p",
      registry = registry_dir
    )
    if (corpus_size > 0L) cl_delete_corpus(corpus = corpus, registry = registry_dir)

    cwb_makeall(corpus = corpus, p_attribute = p_attribute, registry = registry_dir)
    if (compress){
      cwb_huffcode(corpus = corpus, p_attribute = p_attribute, registry = registry_dir)
      cwb_compress_rdx(corpus = corpus, p_attribute = p_attribute, registry = registry_dir)
    }
  }
  
  if (compress){
    files_to_remove <- c(
      corpus_file = path(data_dir, sprintf("%s.corpus", p_attribute)),
      rdx_file = path(data_dir, sprintf("%s.corpus.rdx", p_attribute)),
      rev_file = path(data_dir, sprintf("%s.corpus.rev", p_attribute))
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
p_attribute_recode <- function(data_dir, p_attribute, from = c("UTF-8", "latin1"), to = c("UTF-8", "latin1")){
  
  p_attr_lexicon_file <- fs::path(data_dir, sprintf("%s.lexicon", p_attribute))
  
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
  p_attr_lexicon_index_file <- fs::path(data_dir, sprintf("%s.lexicon.idx", p_attribute))
  
  index_new <- cumsum(sapply(lexicon_hex_list, length))
  index_new <- c(0L, index_new[1L:(length(index_new) - 1L)])
  
  writeBin(
    object = index_new,
    con = p_attr_lexicon_index_file, size = 4L,
    endian = "big"
    )
  
  invisible( NULL )
}

#' @details Function \code{p_attribute_rename} can be used to rename a
#'   positional attribute. Note that the corpus is not refreshed (unloaded,
#'   re-loaded), so it may be necessary to restart R for changes to become
#'   effective.
#' @param old A `character` vector with p-attributes to be renamed.
#' @param new A `character` vector with new names of p-attributes. The vector
#'   needs to have the same length as vector `old`.
#' @param dryrun A `logical` value, whether to suppress actual renaming operation
#'   for inspecting output messages 
#' @export p_attribute_rename
#' @rdname p_attribute_encode
#' @author Christoph Leonhardt, Andreas Blaette
p_attribute_rename <- function(corpus, old, new, registry_dir, verbose = TRUE, dryrun = FALSE) {
  
  stopifnot(
    is.character(corpus),
    length(corpus) == 1L,
    is.character(registry_dir), 
    length(registry_dir) == 1L,
    dir.exists(registry_dir),
    is.character(old),
    is.character(new),
    length(verbose) == 1L,
    is.logical(verbose),
    length(dryrun) == 1L,
    is.logical(dryrun)
  )
  
  if (length(old) != length(new)) {
    warning("Length of arguments 'old' and 'new' not identical.")
    return(FALSE)
  }
  if (dryrun) verbose <- TRUE
  
  if (!file.exists(fs::path(registry_dir, tolower(corpus)))) {
    warning(
      sprintf(
        "No registry file for corpus '%s' in registry directory '%s'",
        corpus, registry_dir
      )
    )
  }
  rf <- registry_file_parse(corpus = corpus, registry_dir = registry_dir)
  
  for (i in 1L:length(old)){
    
    if (verbose) cli::cli_alert(
      sprintf("renaming p_attribute '%s' to '%s'", old[i], new[i])
    )
    
    if (!old[i] %in% rf[["p_attributes"]]){
      warning(sprintf("p_attribute '%s' does not exist", old[i]))
      return(FALSE)
    }
    
    if (new[i] %in% rf[["p_attributes"]]) {
      warning(sprintf("new p_attribute '%s' already exists", new[i]))
      return(FALSE)
    }
    
    files <- grep(
      sprintf("^%s.", old[i]),
      list.files(rf[["home"]]),
      value = TRUE
    )
    
    for (f in files){
      oldfile <- path(rf[["home"]], f)
      newfile <- path(
        rf[["home"]],
        gsub(sprintf("^%s(\\..*?)$", old[i]), sprintf("%s\\1", new[i]), f)
      )
      if (verbose) cli::cli_alert(sprintf("rename file %s to %s", col_blue(basename(oldfile)), col_blue(basename(newfile))))
      if (isFALSE(dryrun)) file.rename(from = oldfile, to = newfile)
    }
    
    rf[["p_attributes"]][which(rf[["p_attributes"]] == old[i])] <- new[i]
  }
  
  if (verbose) cli::cli_alert("update and write registry file")
  registry_file_write(data = rf, corpus = corpus, registry_dir = registry_dir)
  
  return(TRUE)
}

