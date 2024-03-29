#' Encode Positional Attribute(s).
#' 
#' Generate positional attribute from a character vector of
#' tokens (the token stream).
#' 
#' Four steps generate the binary CWB corpus data format for positional
#' attributes: (1) Encode the token stream of the corpus, (2) create index
#' files, (3) compress token stream and (4) compress index files. Whereas steps
#' 1 and 2 are required to make a corpus work, steps 3 and 4 are optional yet
#' useful to reduce disk usage and improve performance. See the [CQP Corpus
#' Encoding
#' Tutorial](https://cwb.sourceforge.io/files/CWB_Encoding_Tutorial.pdf)
#' (sections 2-4) for an explanation of the procedure.
#' 
#' `p_attribute_encode()` offers an R and a CWB implementation controlled by
#' argument `method`. When choosing method 'R', the token stream is encoded in
#' 'pure R', then the C implementation of CWB functionality as exposed to R via
#' the RcppCWB package is used (functions `RcppCWB::cwb_makeall()` for indexing,
#' `RcppCWB::cwb_huffcode()` and `RcppCWB::cwb_compress_rdx()` for compression).
#' When choosing method 'CWB', the token stream is written to disk, then CWB
#' command line utilities 'cwb-encode', cwb-makeall', 'cwb-huffcode' and
#' 'cwb-compress-rdx' are called using `system2()`. The CWB-method requires an
#' installation of the 'CWB'. The `cwb_install()` function will download and #
#' install the CWB command line tools within the package. The 'CWB'-method is
#' still supported as it is used in the test suite of the packaage. The
#' 'R'-method is robust and is recommended.
#' 
#' @param corpus ID of the CWB corpus to create.
#' @param registry_dir Registry directory.
#' @param token_stream A `character` vector with the tokens of the corpus. The
#'   maximum length is 2 147 483 647 (2^31 - 1); a warning is issued if this
#'   threshold is exceeded. See the [CWB Encoding
#'   Tutorial](https://cwb.sourceforge.io/files/CWB_Encoding_Tutorial.pdf) for
#'   size limitations of corpora. May also be a file.
#' @param compress A `logical` value, whether to run `RcppCWB::cwb_huffcode()`
#'   and `RcppCWB::cwb_compress_rdx()` (method 'R'), or command line tools
#'   `cwb-huffcode` and `cwb-compress-rdx` (method 'CWB'). Defaults to `FALSE`
#'   as compression is not stable on Windows.
#' @param verbose A `logical` value, whether to output progress messages.
#' @param quietly A `logical` value passed into `RcppCWB::cwb_makeall()`,
#'   `RcppCWB::cwb_huffcode()` and `RcppCWB::cwb_compress_rdx` to control 
#'   verbosity of these functions.
#' @param method Either 'CWB' or 'R', defaults to 'R'. See section 'Details'.
#' @param p_attribute The positional attribute to create - a `character` vector
#'   containing only lowercase ASCII characters (a-z), digits (0-9), -, and _:
#'   No non-ASCII or uppercase letters allowed. If method is "R", only one
#'   positional attribute can be encoded at a time. If `method` is "CWB", more
#'   than one p-attribute allowed.
#' @param data_dir The data directory for the binary files of the corpus.
#' @param encoding Encoding as defined in the charset corpus property of the
#'   registry file for the corpus ('latin1' to 'latin9', and 'utf8').
#' @param reload A `logical` value that defaults to `TRUE` to ensure that all
#'   features are available.
#' @return `TRUE` is returned invisibly, if encoding has been successful.
#'   `FALSE` indicates an error has occurred.
#' @export p_attribute_encode
#' @rdname p_attribute_encode
#' @examples
#' # In this example, we follow a "pure R" approach. 
#' library(dplyr)
#' 
#' reu <- system.file(package = "RcppCWB", "extdata", "examples", "reuters.txt")
#' tokens <- readLines(reu)
#' 
#' # Create new (and empty) directory structure
#' 
#' registry_tmp <- fs::path(tempdir(), "registry")
#' data_dir_tmp <- fs::path(tempdir(), "data_dir", "reuters")
#' 
#' if (dir.exists(registry_tmp)) unlink(registry_tmp, recursive = TRUE)
#' if (dir.exists(data_dir_tmp)) unlink(data_dir_tmp, recursive = TRUE)
#' 
#' dir.create(registry_tmp)
#' dir.create(data_dir_tmp, recursive = TRUE)
#' 
#' # Encode token stream (without compression)
#' 
#' p_attribute_encode(
#'   corpus = "reuters",
#'   token_stream = tokens,
#'   p_attribute = "word",
#'   data_dir = data_dir_tmp,
#'   registry_dir = registry_tmp,
#'   method = "R",
#'   compress = FALSE,
#'   quietly = TRUE,
#'   encoding = "utf8"
#' )
#' 
#' # Augment registry file 
#' 
#' registry_file_parse(corpus = "REUTERS", registry_dir = registry_tmp) %>%
#'   registry_set_name("Reuters Sample Corpus") %>%
#'   registry_set_property("charset", "utf8") %>%
#'   registry_set_property("language", "en") %>%
#'   registry_set_property("build_date", as.character(Sys.Date())) %>%
#'   registry_file_write()
#' 
#' # Run query as a test
#' 
#' library(RcppCWB)
#' 
#' cqp_query(corpus = "REUTERS", query = '[]{3} "oil" []{3};')
#' regions <- cqp_dump_subcorpus(corpus = "REUTERS")
#' 
#' kwic <- apply(
#'   regions, 1,
#'   function(region){
#'     ids <- cl_cpos2id(
#'       "REUTERS",
#'       p_attribute = "word",
#'       registry = registry_tmp,
#'       cpos = region[1]:region[2]
#'     )
#'     words <- cl_id2str(
#'       corpus = "REUTERS",
#'       p_attribute = "word",
#'       registry = registry_tmp,
#'       id = ids
#'     )
#'     paste0(words, collapse = " ")
#'   }
#' )
#' kwic[1:10]
#' @export p_attribute_encode
#' @importFrom RcppCWB cl_attribute_size cwb_makeall cwb_huffcode
#'   cwb_compress_rdx
#' @importFrom stringi stri_detect_regex stri_replace_all
p_attribute_encode <- function(
  token_stream,
  p_attribute = "word",
  registry_dir,
  corpus,
  data_dir,
  method = c("R", "CWB"),
  verbose = TRUE,
  quietly = FALSE,
  encoding = get_encoding(token_stream),
  compress = FALSE,
  reload = TRUE
){
  if (!encoding %in% c("ascii", paste0("latin", 1:9), "utf8")){
    cli_alert_danger("encoding required to be ascii, latin1 to latin9 or utf8")
    return(FALSE)
  }
  
  if (isFALSE(.check_attribute_name(p_attribute))) return(FALSE)
  if (any(is.na(token_stream))){
    if (verbose)
      cli_alert_info(
        "replacing `NA` values in input token stream with empty strings"
      )
    token_stream[which(is.na(token_stream))] <- ""
  }
  
  # the registry file will not accept tilde as a shortcut for the user's home
  # directory so we expand it
  registry_dir <- path.expand(registry_dir)
  if (!file.exists(registry_dir)){
    cli_alert_danger("registry_dir does not exist")
    return(FALSE)
  } 
  data_dir <- path.expand(data_dir)
  if (!file.exists(data_dir)){
    cli_alert_danger("data_dir does not exist")
    return(FALSE)
  }
  registry_file <- fs::path(registry_dir, tolower(corpus))
  
  if (length(token_stream) >= 2L^31L){
    cli_alert_danger(
      "The maximum corpus size is 2 147 483 647 tokens, the length of the input vector is {.val {length(token_stream)}} which is likely to fail.",
    )
  }
  
  if (method == "R"){
    if (length(p_attribute) != 1L){
      cli_alert_danger(
        "If `method` is 'R', only one p-attribute can be processed."
      )
      return(FALSE)
    }

    corpus_file <- fs::path(data_dir, paste(p_attribute, "corpus", sep = "."))
    lexicon_file <- fs::path(data_dir, paste(p_attribute, "lexicon", sep = "."))
    lexicon_index_file <- fs::path(
      data_dir,
      paste(p_attribute, "lexicon.idx", sep = ".")
    )
    for (f in c(corpus_file, lexicon_file, lexicon_index_file)){
      if (file.exists(f)){
        cli_alert_danger("File {.path {f}} already exists (remove and retry)")
        return(FALSE)
      }
    }
    
    # to be checked: Why could this optionally be a file here?!

    if (length(token_stream) == 1L){
      if (!file.exists(token_stream)){
        cli_alert_danger(
          "`token_stream` is a length 1 vector, but is not an existing file"
        )
        return(FALSE)
      }
    }

    if (verbose) cli_progress_step("creating indices (in memory)")
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
    if (verbose)
      cli_progress_step("writing file: {.path {basename(corpus_file)}}")
    writeBin(object = ids, size = 4L, endian = "big", con = corpus_file)

    rm(ids); gc()
    
    lexicon <- levels(tokenstream_factor)
    rm(tokenstream_factor); gc()
    
    if (encoding == "latin1"){
      lexicon_hex_list <- iconv(
        x = lexicon,
        from = "UTF-8",
        to = toupper(encoding),
        toRaw = TRUE
      )
    } else {
      lexicon_hex_list <- lapply(lexicon, charToRaw)
    }
    rm(lexicon); gc()
    
    lexicon_hex_list <- lapply(lexicon_hex_list, function(x) c(x, as.raw(0)))
    lexicon_hex_vec <- unlist(lexicon_hex_list)
    if (verbose)
      cli_progress_step("writing file: {.path {basename(lexicon_file)}}")
    writeBin(object = lexicon_hex_vec, con = lexicon_file)
    
    idx_raw <- cumsum(sapply(lexicon_hex_list, length))
    rm(lexicon_hex_list)
    idx <- c(0L, idx_raw[1:(length(idx_raw) - 1L)])
    if (verbose)
      cli_progress_step("writing file: {.path {basename(lexicon_index_file)}}")
    writeBin(object = idx, size = 4L, endian = "big", con = lexicon_index_file)
    if (verbose) cli_progress_done()
    rm(idx_raw); gc()
  } else if (method == "CWB"){
    if (!cwb_is_installed()){
      cli_alert_danger(
        "method 'CWB' selected, but CWB is not installed (use `cwb_install()`)"
      )
      return(invisible(FALSE))
    }
    
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
      cmd <- fs::path(
        cwb_get_bindir(),
        if (.Platform$OS.type == "windows") "cwb-encode.exe" else "cwb-encode"
      )
      args <- c(
        sprintf("-d %s", fs::path(data_dir)),
        sprintf("-f %s", fs::path(vrt_file)),
        sprintf("-c %s", encoding),
        "-v"
      )
      
      if (verbose){
        cli_alert_info(
          "run cwb-encode: {.code {paste(c(cmd, args), collapse = ' ')}}"
        )
      }
      
      system2(command = cmd, args = args)
      
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
      cwb_bindir <- cwb_get_bindir()
      if (is.null(cwb_bindir) || (length(cwb_bindir) == ""))
        stop("CWB binaries not found using `cwb_get_bindir()`")
      cwb_encode_cmd_vec <- c(
        fs::path(
          cwb_bindir,
          if (.Platform$OS.type == "windows") "cwb-encode.exe" else "cwb-encode"
        ),
        "-d", fs::path(data_dir),
        "-f", fs::path(vrt_file),
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
    if (verbose)
      cli_alert_info("updating registry file: {.path {registry_file}}")
    regdata <- registry_file_parse(
      corpus = tolower(corpus),
      registry_dir = registry_dir
    )
    
    p_attributes <- c(
      regdata[["p_attributes"]],
      if (exists("p_attrs_old")) p_attrs_old else character(),
      p_attribute
    )
    regdata[["p_attributes"]] <- unique(p_attributes)
  } else {
    if (verbose)
      cli_alert_info("creating new registry file: {.path {registry_file}}")
    regdata <- registry_data(
      name = toupper(corpus),
      id = tolower(corpus),
      home = data_dir,
      properties = c(charset = encoding),
      p_attributes = p_attribute
    )
  }
  
  registry_file_write(
    regdata,
    corpus = tolower(corpus),
    registry_dir = registry_dir
  )
  
  if (method == "CWB"){
    cmd <- fs::path(
      cwb_get_bindir(),
      if (.Platform$OS.type == "windows") "cwb-makeall.exe" else "cwb-makeall"
    )
    
    for (p_attr in p_attribute){
      args_makeall <- c(
        sprintf("-r %s", fs::path(registry_dir)),
        sprintf("-P %s", p_attr),
        "-V", toupper(corpus)
      )
      if (verbose) cli_alert_info(paste0(
        "run cwb-makeall for p-attribute {.val {p_attr}}: ",
        "{.code {paste(c(cmd, args_makeall), collapse = ' ')}}"
      ))
      system2(command = cmd, args = args_makeall)
    }
    
    
    if (compress){
      if (verbose) cli_rule("cwb-huffcode")
      cmd_huffcode <- fs::path(
        cwb_get_bindir(),
        if (.Platform$OS.type == "windows")
          "cwb-huffcode.exe" else "cwb-huffcode"
      )
      
      cmd_compress <- fs::path(
        cwb_get_bindir(),
        if (.Platform$OS.type == "windows")
          "cwb-compress-rdx.exe" else "cwb-compress-rdx"
      )
      
      for (p_attr in p_attribute){
        args_huffcode <- c(
          sprintf("-r %s", fs::path(registry_dir)),
          sprintf("-P %s", p_attr),
          toupper(corpus)
        )
        if (verbose) cli_alert_info(paste0(
          "run cwb-huffcode for p-attribute {.val {p_attr}}: ",
          "{.code {paste(c(cmd_huffcode, args_huffcode), collapse = ' ')}}"
        ))
        system2(
          command = cmd_huffcode,
          args = args_huffcode,
          stdout = if (quietly) FALSE else ""
        )
        
        if (verbose) cli_rule("cwb-compress-rdx")
        
        args_compress <- c(
          sprintf("-r %s", fs::path(registry_dir)),
          sprintf("-P %s", p_attr),
          toupper(corpus)
        )
        if (verbose) cli_alert_info(paste0(
          "run cwb-compress-rdx for p-attribute {.val {p_attr}}: ",
          "{.code {paste(c(cmd_compress, args_compress), collapse = ' ')}}"
        ))
        system2(
          command = cmd_compress,
          args = args_compress,
          stdout = if (quietly) FALSE else ""
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
    if (corpus_size > 0L)
      cl_delete_corpus(corpus = corpus, registry = registry_dir)
    
    if (verbose){
      if (quietly){
        cli_progress_step("run `Rcpp::cwb_makeall()`")
      } else {
        cli_alert_info("`run Rcpp::cwb_makeall()`")
      }
    }
    cwb_makeall(
      corpus = corpus,
      p_attribute = p_attribute,
      registry = registry_dir,
      quietly = quietly
    )
    if (verbose & quietly) cli_progress_done()
    
    if (compress){
      
      if (verbose){
        if (quietly){
          cli_progress_step("run `Rcpp::cwb_huffcode()`")
        } else {
          cli_alert_info("`run Rcpp::cwb_huffcode()`")
        }
      }
      cwb_huffcode(
        corpus = corpus,
        p_attribute = p_attribute,
        registry = registry_dir,
        quietly = quietly
      )
      if (verbose & quietly) cli_progress_done()
      
      if (verbose){
        if (quietly){
          cli_progress_step("run `Rcpp::cwb_compress_rdx()`")
        } else {
          cli_alert_info("`run Rcpp::cwb_compress_rdx()`")
        }
      }
      cwb_compress_rdx(
        corpus = corpus,
        p_attribute = p_attribute,
        registry = registry_dir,
        quietly = quietly
      )
      if (verbose & quietly) cli_progress_done()
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
        if (file.remove(x)) if (verbose)
          cli_alert_info("file successfully removed: {.path {basename(x)}}")
      }
    }
  }
  
  if (reload)
    corpus_reload(
      corpus = corpus,
      registry_dir = registry_dir,
      verbose = verbose
    )
  
  invisible(TRUE)
}


#' @details `p_attribute_recode()` will recode the values in the avs-file and
#'   change the attribute value index in the avx file. The rng-file remains
#'   unchanged. The registry file remains unchanged, and it is highly
#'   recommended to consider `s_attribute_recode()` as a helper for
#'   `corpus_recode()` that will recode all s-attributes, all p-attributes, and
#'   will reset the encoding in the registry file.
#' @param from Character string describing the current encoding of the attribute.
#' @param to Character string describing the target encoding of the attribute.
#' @rdname p_attribute_encode
p_attribute_recode <- function(data_dir, p_attribute, from = c("UTF-8", "latin1"), to = c("UTF-8", "latin1")){
  
  # missing check! Does file exist?!
  
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

#' @details Function `p_attribute_rename()` can be used to rename a
#'   positional attribute. Note that the corpus is not refreshed (unloaded,
#'   re-loaded), so it may be necessary to restart R for changes to become
#'   effective.
#' @param old A `character` vector with p-attributes to be renamed.
#' @param new A `character` vector with new names of p-attributes. The vector
#'   needs to have the same length as vector `old`.
#' @param dryrun A `logical` value, whether to suppress actual renaming
#'   operation for inspecting output messages
#' @export p_attribute_rename
#' @rdname p_attribute_encode
#' @author Christoph Leonhardt, Andreas Blaette
p_attribute_rename <- function(
    corpus,
    old,
    new,
    registry_dir,
    verbose = TRUE,
    dryrun = FALSE
) {
  
  if (isFALSE(.check_attribute_name(new))) return(FALSE)
  
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
      if (verbose) cli::cli_alert(
        "rename file {.path {basename(oldfile)}} to {.path{basename(newfile)}}"
      )
      if (isFALSE(dryrun)) file.rename(from = oldfile, to = newfile)
    }
    
    rf[["p_attributes"]][which(rf[["p_attributes"]] == old[i])] <- new[i]
  }
  
  if (verbose) cli::cli_alert("update and write registry file")
  registry_file_write(data = rf, corpus = corpus, registry_dir = registry_dir)
  
  TRUE
}

