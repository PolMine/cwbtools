testthat::context("encode")

test_that(
  "encode corpus",
  {
    library(data.table)
    
    cwb_dirs <- create_cwb_directories(prefix = tempdir(), ask = FALSE, verbose = FALSE)
    austen_data_dir_tmp <- file.path(cwb_dirs[["corpus_dir"]], "austen")
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
      corpus = "AUSTEN", encoding = "utf8",
      p_attributes = c("word", "stem"), s_attributes = "book",
      registry_dir = cwb_dirs[["registry_dir"]], data_dir = austen_data_dir_tmp,
      method = "R", compress = FALSE
    )
    
    # Load corpus -------------------------------------------

    if (RcppCWB::cqp_is_initialized()){
      RcppCWB::cqp_reset_registry(registry = cwb_dirs[["registry_dir"]])
    } else {
      RcppCWB::cqp_initialize(registry = cwb_dirs[["registry_dir"]])
    }
    
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
    
    cwb_install(cwb_dir = file.path(tempdir(), "cwb"))
    
    Austen$encode(
      corpus = "AUSTEN2", encoding = "utf8",
      p_attributes = c("word", "stem"), s_attributes = "book",
      registry_dir = cwb_dirs[["registry_dir"]], data_dir = austen_data_dir_tmp,
      method = "CWB", compress = FALSE
    )
    
    RcppCWB::cqp_reset_registry(registry = cwb_dirs[["registry_dir"]])
    
    id2 <- RcppCWB::cl_str2id(
      corpus = "AUSTEN2", p_attribute = "word",
      str = "pride", registry = cwb_dirs[["registry_dir"]]
    )
    cpos2 <- RcppCWB::cl_id2cpos(
      corpus = "AUSTEN2", p_attribute = "word",
      id = id2, registry = cwb_dirs[["registry_dir"]]
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
  }
)


