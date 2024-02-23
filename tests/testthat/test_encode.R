testthat::context("encode")

test_that(
  "encode corpus",
  {
    library(data.table)

    cwb_dirs <- create_cwb_directories(prefix = tempdir(), ask = FALSE, verbose = FALSE)
    austen_data_dir_tmp <- fs::path(cwb_dirs[["corpus_dir"]], "austen")
    dir.create(austen_data_dir_tmp)

    Austen <- CorpusData$new()

    # Generate tokenstream --------------------------

    books <- janeaustenr::austen_books()
    tbl <- tidytext::unnest_tokens(books, word, text, to_lower = FALSE)
    Austen$tokenstream <- as.data.table(tbl)
    Austen$tokenstream[, stem := SnowballC::wordStem(tbl[["word"]], language = "english")]
    Austen$tokenstream[, cpos := 0L:(nrow(tbl) - 1L)]
    setcolorder(Austen$tokenstream, c("cpos", "word", "stem"))

    # Generate metadata ----------------------------------------

    cpos_max_min <- function(x) list(cpos_left = min(x[["cpos"]]), cpos_right = max(x[["cpos"]]))
    Austen$metadata <- Austen$tokenstream[, cpos_max_min(.SD), by = book]
    Austen$metadata[, book := as.character(book)]
    setcolorder(Austen$metadata, c("cpos_left", "cpos_right", "book"))

    # Clean up --------------------------------------------------

    Austen$tokenstream[, book := NULL]

    # Encode -----------------------------------------

    Austen$encode(
      corpus = "AUSTEN",
      encoding = "utf8",
      p_attributes = c("word", "stem"),
      s_attributes = "book",
      registry_dir = cwb_dirs[["registry_dir"]],
      data_dir = austen_data_dir_tmp,
      method = "R",
      compress = FALSE
    )

    # Load corpus -------------------------------------------

    # if (RcppCWB::cqp_is_initialized()){
    #   RcppCWB::cqp_reset_registry(registry = cwb_dirs[["registry_dir"]])
    # } else {
    #   RcppCWB::cqp_initialize(registry = cwb_dirs[["registry_dir"]])
    # }

    # Check -------------------------------------------------

    id <- RcppCWB::cl_str2id(
      corpus = "AUSTEN", p_attribute = "word",
      str = "pride", registry = cwb_dirs[["registry_dir"]]
    )
    cpos <- RcppCWB::cl_id2cpos(
      corpus = "AUSTEN", p_attribute = "word",
      id = id, registry = cwb_dirs[["registry_dir"]]
    )

    expect_identical(length(cpos), length(which(Austen$tokenstream[["word"]] == "pride")))

    # Check CWB encoded version against R version

    skip_on_os(os = "windows") # CWB not available for Solaris
    tryCatch(
      cwb_install(cwb_dir = fs::path(tempdir(), "cwb")),
      error = function(e) testthat::skip("cannot download CWB")
    )


    Austen$encode(
      corpus = "AUSTEN2",
      encoding = "utf8",
      p_attributes = c("word", "stem"),
      s_attributes = "book",
      registry_dir = cwb_dirs[["registry_dir"]],
      data_dir = austen_data_dir_tmp,
      method = "CWB",
      compress = FALSE,
      reload = TRUE
    )

    id2 <- RcppCWB::cl_str2id(
      corpus = "AUSTEN2",
      p_attribute = "word",
      str = "pride",
      registry = cwb_dirs[["registry_dir"]]
    )
    cpos2 <- RcppCWB::cl_id2cpos(
      corpus = "AUSTEN2",
      p_attribute = "word",
      id = id2,
      registry = cwb_dirs[["registry_dir"]]
    )

    expect_identical(length(cpos), length(cpos2))
    expect_identical(cpos, cpos2)

    expect_identical(
      RcppCWB::cl_attribute_size(corpus = "AUSTEN", attribute = "word", attribute_type = "p"),
      RcppCWB::cl_attribute_size(corpus = "AUSTEN2", attribute = "word", attribute_type = "p")
    )

    lex1 <- RcppCWB::cl_lexicon_size(corpus = "AUSTEN", p_attribute = "word")
    lex2 <- RcppCWB::cl_lexicon_size(corpus = "AUSTEN2", p_attribute = "word")

    expect_identical(lex1, lex2)
    vocab1 <- RcppCWB::cl_id2str(corpus = "AUSTEN", p_attribute = "word", id = 0L:(lex1 - 1L))
    vocab2 <- RcppCWB::cl_id2str(corpus = "AUSTEN2", p_attribute = "word", id = 0L:(lex1 - 1L))
    expect_identical(vocab1, vocab2)
    
    # check availability of s-attributes
    s_attrs1 <- RcppCWB::corpus_s_attributes(
      corpus = "AUSTEN",
      registry = cwb_dirs[["registry_dir"]]
    )
    s_attrs2 <- RcppCWB::corpus_s_attributes(
      corpus = "AUSTEN2",
      registry = cwb_dirs[["registry_dir"]]
    )
    expect_identical(s_attrs1, s_attrs2)
    
    
    lapply(cwb_dirs, unlink) # clean up
    
    
  }
)

test_that(
  "check p_attribute with R/CWB, without/with compression",
  {
    registry_tmp <- fs::path(tempdir(), "registry")
    dir.create (registry_tmp)
    
    token_stream <- tidytext::unnest_tokens(
      janeaustenr::austen_books(),
      word, text, to_lower = FALSE
    )[["word"]]
    
    if (!cwb_is_installed()) cwb_install()
    
    # without compression ----------------------------------------------------------
    
    data_dir_tmp1a <- fs::path(tempdir(), "data_dir", "austen1a")
    dir.create(data_dir_tmp1a, recursive = TRUE)
    
    data_dir_tmp2a <- fs::path(tempdir(), "data_dir", "austen2a")
    dir.create(data_dir_tmp2a, recursive = TRUE)

    p_attribute_encode(
      token_stream = token_stream,
      registry_dir = registry_tmp,
      corpus = "AUSTEN1A",
      data_dir = data_dir_tmp1a,
      method = "R",
      verbose = TRUE,
      quietly = FALSE,
      encoding = "utf8",
      compress = FALSE
    )
    
    p_attribute_encode(
      token_stream = token_stream,
      registry_dir = registry_tmp,
      corpus = "AUSTEN2A",
      data_dir = data_dir_tmp2a,
      method = "CWB",
      verbose = TRUE,
      quietly = FALSE,
      encoding = "utf8",
      compress = FALSE
    )
    
    austen1a_md5 <- tools::md5sum(list.files(data_dir_tmp1a, full.names = TRUE))
    names(austen1a_md5) <- basename(names(austen1a_md5))
    
    austen2a_md5 <- tools::md5sum(list.files(data_dir_tmp2a, full.names = TRUE))
    names(austen2a_md5) <- basename(names(austen2a_md5))
    
    testthat::expect_identical(names(austen1a_md5), names(austen2a_md5))
    
    for (file in names(austen1a_md5))
      expect_identical(
        austen1a_md5[[file]],
        austen2a_md5[[file]]
      )
    
    # with compression -------------------------------------------------------------

    skip_on_os("windows")
    
    data_dir_tmp1b <- fs::path(tempdir(), "data_dir", "austen1b")
    dir.create(data_dir_tmp1b, recursive = TRUE)
    
    data_dir_tmp2b <- fs::path(tempdir(), "data_dir", "austen2b")
    dir.create(data_dir_tmp2b, recursive = TRUE)

    p_attribute_encode(
      token_stream = token_stream,
      registry_dir = registry_tmp,
      corpus = "AUSTEN1B",
      data_dir = data_dir_tmp1b,
      method = "R",
      verbose = TRUE,
      quietly = FALSE,
      encoding = "utf8",
      compress = TRUE
    )
    
    p_attribute_encode(
      token_stream = token_stream,
      registry_dir = registry_tmp,
      corpus = "AUSTEN2B",
      data_dir = data_dir_tmp2b,
      method = "CWB",
      verbose = TRUE,
      quietly = FALSE,
      encoding = "utf8",
      compress = TRUE
    )
    
    austen1b_md5 <- tools::md5sum(list.files(data_dir_tmp1b, full.names = TRUE))
    names(austen1b_md5) <- basename(names(austen1b_md5))

    austen2b_md5 <- tools::md5sum(list.files(data_dir_tmp2b, full.names = TRUE))
    names(austen2b_md5) <- basename(names(austen2b_md5))

    testthat::expect_identical(names(austen1b_md5), names(austen2b_md5))

    for (file in names(austen1b_md5))
      expect_identical(
        austen1b_md5[[file]],
        austen2b_md5[[file]]
      )
  }
)

