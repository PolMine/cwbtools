#' Manage Corpus Data and Encode CWB Corpus.
#' 
#' @param x A single filename, a character vector of filenames, or a directory with XML files.
#' @param body An xpath expression defining the body of the xml document.
#' @param verbose Logical, whether to be verbose.
#' @param progress Logical, whether to show progress bar.
#' @param meta A named character vector with xpath expressions.
#' @param mc A numeric/integer value, number of cores to use.
#' @param compress Logical, whether to compress corpus.
#' @param encoding Encoding/charset of the CWB corpus.
#' @param registry_dir Corpus registry, the directory where registry files are stored.
#' @param corpus The name of the CWB corpus.
#' @param p_attributes Positional attributes.
#' @param s_attributes Columns that will be encoded as structural attributes.
#' @param data_dir Directory where to create directory for indexed corpus files.
#' @param method Either "R" or "CWB".
#' @param filenames XXX
#' @param replacements XXX
#' @param ... Arguments that are passed into \code{tokenizers::tokenize_words()}.
#' 
#' @field chunktable A \code{data.table} with column "id" (unique values),
#'   columns with metadata, and a column with text chunks.
#' @field tokenstream A \code{data.table} with a column "cpos" (corpus position), and columns
#'   with positional attributes, such as "word", "lemma", "pos", "stem".
#'   
#' @field metadata A \code{data.table} with a column "id", to link data with chunks/tokenstream,
#'   columns with document-level metadata, and a column "cpos_left" and "cpos_right", which can
#'   be generated using method \code{$add_corpus_positions()}.
#' @field sentences A \code{data.table}.
#' @field named_entities A \code{data.table}.
#' 
#' @export CorpusData
#' @importFrom data.table setnames rbindlist .GRP .SD := fread fwrite setorderv as.data.table data.table
#' @importFrom data.table uniqueN setkeyv
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom xml2 read_xml xml_attrs xml_find_all xml_find_first xml_name xml_parents xml_text
#' @importFrom pbapply pblapply timerProgressBar setTimerProgressBar
#' @importFrom stats setNames
#' @importFrom stringi stri_detect_regex
#' @rdname CorpusData
#' @name CorpusData
#' @examples 
#' library(RcppCWB)
#' library(data.table)
#' 
#' # this example relies on the R method to write data to disk, there is also a method "CWB"
#' # that relies on CWB tools to generate the indexed corpus. The CWB can downloaded
#' # and installed within the package by calling cwb_install()
#' 
#' # create temporary registry file so that data in RcppCWB package can be used
#' 
#' registry_rcppcwb <- system.file(package = "RcppCWB", "extdata", "cwb", "registry")
#' registry_tmp <- fs::path(tempdir(), "registry")
#' if (!dir.exists(registry_tmp)) dir.create(registry_tmp)
#' r <- registry_file_parse("REUTERS", registry_dir = registry_rcppcwb)
#' r[["home"]] <- system.file(package = "RcppCWB", "extdata", "cwb", "indexed_corpora", "reuters")
#' registry_file_write(r, corpus = "REUTERS", registry_dir = registry_tmp)
#' 
#' # decode structural attribute 'places'
#' 
#' s_attrs_places <- RcppCWB::s_attribute_decode(
#'   corpus = "REUTERS",
#'   data_dir = system.file(package = "RcppCWB", "extdata", "cwb", "indexed_corpora", "reuters"),
#'   s_attribute = "places", method = "R"
#' )
#' s_attrs_places[["id"]] <- 1L:nrow(s_attrs_places)
#' setnames(s_attrs_places, old = "value", new = "places")
#' 
#' # decode positional attribute 'word'
#' 
#' tokens <- apply(s_attrs_places, 1, function(row){
#'   ids <- cl_cpos2id(
#'     corpus = "REUTERS", cpos = row[1]:row[2],
#'     p_attribute = "word", registry = registry_tmp
#'   )
#'   cl_id2str(corpus = "REUTERS", id = ids, p_attribute = "word", registry = registry_tmp)
#' })
#' tokenstream <- rbindlist(
#' lapply(
#'   1L:length(tokens),
#'   function(i) data.table(id = i, word = tokens[[i]]))
#'   )
#' tokenstream[["cpos"]] <- 0L:(nrow(tokenstream) - 1L)
#' 
#' # create CorpusData object (see vignette for further explanation)
#' 
#' CD <- CorpusData$new()
#' CD$tokenstream <- as.data.table(tokenstream)
#' CD$metadata <- as.data.table(s_attrs_places)
#' 
#' # Remove temporary registry with home dir still pointing to RcppCWB data dir
#' # to prevent data from being deleted
#' file.remove(fs::path(registry_tmp, "reuters"))
#' file.remove(registry_tmp)
#' 
#' # create temporary directories (registry directory and one for indexed corpora)
#' 
#' tmpdir <- normalizePath(tempdir(), winslash = "/")
#' if (.Platform$OS.type == "windows") tmpdir <- normalizePath(tmpdir, winslash = "/")
#' registry_tmp <- fs::path(tempdir(), "registry")
#' data_dir_tmp <- fs::path(tempdir(), "data_dir")
#' if (!dir.exists(registry_tmp)) dir.create(registry_tmp)
#' if (!dir.exists(data_dir_tmp)) dir.create(data_dir_tmp)
#' 
#' CD$encode(
#'   corpus = "REUTERS", encoding = "utf8",
#'   p_attributes = "word", s_attributes = "places",
#'   registry_dir = registry_tmp, data_dir = data_dir_tmp,
#'   method = "R"
#' )
#' reg <- registry_data(name = "REUTERS", id = "REUTERS", home = data_dir_tmp, p_attributes = "word")
#' registry_file_write(data = reg, corpus = "REUTERS", registry_dir = registry_tmp)
#' 
#' # see whether it works
#' 
#' cl_cpos2id(corpus = "REUTERS", p_attribute = "word", cpos = 0L:4049L, registry = registry_tmp)
CorpusData <- R6::R6Class(
  
  classname = "CorpusData",
  
  public = list(
    
    chunktable = NULL, # a data.table
    tokenstream = NULL, # a data.table
    metadata = NULL, # a data.table
    sentences = NULL, # a data.table
    named_entities = NULL, # a data.table
    
    #' @description 
    #' Initialize a new instance of class \code{CorpusData}.
    #' @return A class \code{CorpusData} object.
    initialize = function(){
      self
    },
    
    #' @description
    #' Print summary of \code{CorpusData} object.
    print = function(){
      if (is.null(self$chunktable)){
        cat("chunktable: NULL\n")
      } else {
        cat(sprintf("chunktable: %d columns / %d rows\n", ncol(self$chunktable), nrow(self$chunktable)))
      }
      
      if (is.null(self$tokenstream)){
        cat("tokenstream: NULL\n")
      } else {
        cat(sprintf("tokenstream: %d columns / %d rows\n", ncol(self$tokenstream), nrow(self$tokenstream)))
      }
      
      if (is.null(self$metadata)){
        cat("metadata: NULL\n")
      } else {
        cat(sprintf("metadata: %d columns / %d rows\n", ncol(self$metadata), nrow(self$metadata)))
      }
    },
    
    #' @description 
    #' Simple tokenization of text in chunktable.
    tokenize = function(..., verbose = TRUE, progress = TRUE){
      if (requireNamespace("tokenizers", quietly = TRUE)){
        if (!inherits(self$chunktable, "data.table"))
          stop("the chunktable needs to be a data. table")

        if (progress) pb <- txtProgressBar(min = 0, max = uniqueN(self$chunktable[["id"]]), style = 3)
        .tokenize <- function(.SD, .GRP){
          if (progress) setTxtProgressBar(pb, value = .GRP)
          tokenizers::tokenize_words(.SD[["text"]], ...)
        }
        self$tokenstream <- self$chunktable[, .tokenize(.SD, .GRP), by = "id"]
        if (progress) close(pb)
        
      } else {
        self$tokenstream <- self$chunktable[,{strsplit(.SD[["text"]], split = "(\\s|[\\.:;\\?!])")[[1]]}, by = "id"]
      }
      setnames(self$tokenstream, old = "V1", new = "word")
      self$add_corpus_positions(verbose = verbose)
    },
    
    #' @details 
    #' Import XML files.
    #' @return The \code{CorpusData} object is returned invisibly.
    import_xml = function(filenames, body = "//body", meta = NULL, mc = NULL, progress = TRUE){
      .xml_reader <- function(x){
        doc <- xml2::read_xml(x)
        textnodes <- xml2::xml_find_all(doc, xpath = sprintf("%s//text()", body))
        .get_parents_attributes <- function(textnode){
          meta <- lapply(
            xml2::xml_parents(textnode),
            function(ancestor){
              sattrs <- xml2::xml_attrs(ancestor)
              if (length(sattrs) > 0){
                names(sattrs) <- paste(xml2::xml_name(ancestor), names(sattrs), sep = "_")
                return( sattrs )
              } else {
                return( setNames(TRUE, xml2::xml_name(ancestor)) )
              }
              
            }
          )
          data <- as.list(unlist(meta))
          as.data.table(data)
        }
        dt <- rbindlist(lapply(textnodes, .get_parents_attributes), fill = TRUE)
        if (!is.null(meta)){
          for (x in names(meta)){
            dt[, eval(x) := xml2::xml_text(xml2::xml_find_first(doc, meta[x])), with = TRUE]
          }
        }
        y <- list(
          text = data.table(id = 1L:length(textnodes), text = sapply(textnodes, xml2::xml_text)),
          metadata = dt
        )
        return(y)
      }
      
      if (!all(file.exists(filenames))) stop("all files provided by x need to exist (not fulfilled)")
      if (is.null(mc) || mc == 1L){
        data <- if (progress) pblapply(filenames, .xml_reader) else lapply(filenames, .xml_reader)
      } else if (is.numeric(mc)){
        data <- if (progress) pblapply(filenames, .xml_reader, cl = mc) else mclapply(filenames, .xml_reader, mc.cores = mc)
      } else {
        stop("If argument 'mc' is not NULL nor 1, it is required to be an integer value.")
      }
      self$chunktable <- rbindlist(lapply(data, function(x) x[["text"]]))
      self$chunktable[["id"]] <- 1L:nrow(self$chunktable)
      self$metadata <- rbindlist(lapply(data, function(x) x[["metadata"]]), fill = TRUE)
      self$metadata[["id"]] <- 1L:nrow(self$metadata)
      invisible(self)
    },
    
    #' @description 
    #' Add column \code{cpos} to tokenstream and columns \code{cpos_left} and
    #' \code{cpos_right} to metadata.
    add_corpus_positions = function(verbose = TRUE){
      
      if (!"id" %in% colnames(self$metadata)) stop("id column required")
      self$tokenstream[, "cpos" := 0L:(nrow(self$tokenstream) - 1L)]
      
      if (verbose) message("... adding corpus positions to table 'metadata'")
      grpn <- uniqueN(self$tokenstream[["id"]])
      if (interactive()) pb <- timerProgressBar(min = 0, max = grpn, width = getOption("pboptions")[["txt.width"]])
      .fn <- function(.SD, .GRP){
        if (interactive()) setTimerProgressBar(pb, .GRP);
        list(cpos_left = min(.SD[["cpos"]]), cpos_right = max(.SD[["cpos"]]))
      }
      cpos_dt <- self$tokenstream[, .fn(.SD, .GRP), by = "id"]
      if (interactive()) close(pb)
      
      setkeyv(cpos_dt, cols = "id")
      setkeyv(self$metadata, cols = "id")
      self$metadata <- self$metadata[cpos_dt]
      invisible(self)
    },
    
    #' @description
    #' Remove patterns from chunkdata that are known to cause problems. This is
    #' done most efficiently at the chunkdata level of data preparation as the
    #' length of the character vector to handle is much smaller than when
    #' tokenization/annotation has been performed.
    purge = function(replacements = list(c("^\\s*<.*?>\\s*$", ""), c("\u2019", "'"))){
      for (i in 1L:length(replacements)){
        if (verbose) message("... checking for presence of regex: ", replacements[[i]][1])
        matches <- stri_detect_regex(str = self$chunkdata[["text"]], pattern = replacements[[i]][1])
        if (any(matches)){
          self$chunkdata[["text"]] <- stri_replace_all(
            str = self$chunkdata[["text"]],
            regex = replacements[[i]][1],
            replacement = replacements[[i]][2]
            )
        }
      }
      invisible(self)
    },
    
    #' @description 
    #' Encode corpus. If the corpus already exists, it will be removed.
    encode = function(corpus, p_attributes = "word", s_attributes = NULL, encoding, registry_dir = Sys.getenv("CORPUS_REGISTRY"), data_dir = NULL, method = c("R", "CWB"), verbose = TRUE, compress = FALSE){
      
      if (file.exists(registry_dir))
        if (file.info(registry_dir)[["isdir"]] != TRUE)
          stop("registry_dir is not a directory")
      registry_file <- fs::path(registry_dir, tolower(corpus))
      
      if (file.exists(registry_file)){
        message(sprintf("registry file for corpus '%s' already exists - it should be removed", corpus))
        corpus_remove(corpus = corpus, registry_dir = registry_dir)
      }
      message(sprintf("Creating new corpus '%s'", corpus))
      if (is.null(data_dir)){
        super_dir <- dirname(registry_dir)
        potential_data_dir <- grep("index", list.files(super_dir), value = TRUE, perl = TRUE)
        if (length(potential_data_dir) != 1) stop("no data_dir provided, no candidate found")
        data_dir <- fs::path(super_dir, potential_data_dir, tolower(corpus))
        message(sprintf("suggesting data_dir: %s\n", data_dir))
        feedback <- readline(prompt = "Use this data directory? (type 'Y' to confirm, anything else to abort)")
        if (feedback != "Y") stop("aborting")
        if (!file.exists(data_dir)) dir.create(data_dir)
      } else {
        if (!file.exists(data_dir)) dir.create(data_dir)
      }
      
      if (!encoding %in% c("ascii", paste("latin", 1:9, sep = ""), "utf8")){
        stop("encoding is required to be either ascii, latin1 to latin9, or utf8")
      }
      
      if (verbose) message("... encoding p-attribute 'word'")
      p_attribute_encode(
        token_stream = self$tokenstream[["word"]], corpus = corpus, encoding = encoding,
        registry_dir = registry_dir, data_dir = data_dir, method = method, verbose = verbose,
        compress = compress
      )
      
      # add other p-attributes than 'word'
      if (length(p_attributes > 1)){
        for (new_attribute in p_attributes[which(p_attributes != "word")]){
          if (verbose) message(sprintf("... encoding p-attribute '%s'", new_attribute))
          p_attribute_encode(
            token_stream = self$tokenstream[[new_attribute]], corpus = corpus, 
            p_attribute = new_attribute, encoding = encoding,
            registry_dir = registry_dir, data_dir = data_dir, method = method, verbose = FALSE,
            compress = compress
          )
        }
      }
      
      for (s_attr in s_attributes){
        if (verbose) message(sprintf("... encoding s-attribute '%s'", s_attr))
        s_attribute_encode(
          values = self$metadata[[s_attr]], corpus = corpus,
          s_attribute = s_attr,
          region_matrix = as.matrix(self$metadata[,c("cpos_left", "cpos_right")]),
          data_dir = data_dir, registry_dir = registry_dir, encoding = encoding,
          method = method, verbose = FALSE
        )
      }
      
      reg_data <- registry_data(
        name = toupper(corpus), id = tolower(corpus),
        home = path.expand(data_dir), properties = c(charset = encoding), 
        p_attributes = p_attributes, s_attributes = s_attributes
      )
      registry_file_write(data = reg_data, corpus = tolower(corpus), registry_dir = registry_dir)
      invisible(self)
    }
    
  )
)

