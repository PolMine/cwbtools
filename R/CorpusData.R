#' Manage Corpus Data and Encode CWB Corpus.
#' 
#' #' @section Arguments:
#' \describe{
#'   \item{\code{x}}{single filename, a character vector of filenames, or a directory with XML files}
#'   \item{\code{body}}{xpath expression defining the body of the xml document}
#'   \item{\code{meta}}{named character vector with xpath expressions}
#'   \item{\code{mc}}{numeric/integer value, number of cores to use}
#'   \item{\code{compress}}{logical, whether to compress corpus}
#'   \item{\code{encoding}}{encoding/charst of the CWB corpus }
#'   \item{\code{registry_dir}}{corpus registry, the directory where registry files are stored}
#'   \item{\code{corpus}}{the name of the CWB corpus}
#'   \item{\code{p_attributes}}{...}
#'   \item{\code{s_attributes}}{columns of ... that will be encoded as structural attributes}
#'   \item{\code{data_dir}}{directory where to create directory for indexed corpus files}
#'   \item{\code{method}}{either "R" or "CWB"}
#'   \item{\code{...}}{arguments that are passed into tokenizers::tokenize_words()}
#'   \item{\code{verbose}}{logical, whether to be verbose}
#'   \item{\code{progress}} Logical, whether to show progress bar.
#' }
#' @section Methods:
#' \describe{
#'   \item{\code{$new()}}{Initialize a new instance of class \code{CorpusData}.}
#'   \item{\code{$print()}}{Print summary of \code{CorpusData} object.}
#'   \item{\code{$purge(replacements = list(c("^\\s*<.*?>\\s*$", ""),
#'   c("\u2019", "'")))}}{Remove patterns from chunkdata that are known to cause
#'   problems. This is done most efficiently at the chunkdata level of data
#'   preparation as the length of the character vector to handle is much smaller
#'   than when tokenization/annotation has been performed.}
#'   \item{\code{$tokenize(verbose = TRUE)}}{Simple tokenization of text in chunktable.}
#'   \item{\code{$add_corpus_positions(verbose = TRUE)}}{Add column \code{cpos} to tokenstream and
#'   columns \code{cpos_left} and \code{cpos_right} to metadata.}
#'   \item{\code{$encode(corpus, p_attributes = "word", s_attributes = NULL,
#'   encoding, registry_dir = Sys.getenv("CORPUS_REGISTRY"), data_dir = NULL,
#'   method = c("R", "CWB"), verbose = TRUE, compress = FALSE)}}{Encode corpus.
#'   If the corpus already exists, it will be removed.}
#'   \item{\code{$import_xml(filenames, body = "//body", meta = NULL, mc = NULL, progress = TRUE)}}{}
#' }
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
#' if (!cwb_is_installed()) cwb_install()
#' 
#' registry_rcppcwb <- system.file(package = "RcppCWB", "extdata", "cwb", "registry")
#' 
#' s_attrs_places <- s_attribute_decode(
#'   corpus = "REUTERS",
#'   data_dir = registry_file_parse("REUTERS", registry_dir = registry_rcppcwb)[["home"]],
#'   s_attribute = "places", method = "R"
#' )
#' s_attrs_places[["id"]] <- 1:nrow(s_attrs_places)
#' setnames(s_attrs_places, old = "value", new = "places")
#' 
#' tokens <- apply(s_attrs_places, 1, function(row){
#'   ids <- cl_cpos2id(
#'     corpus = "REUTERS", cpos = row[1]:row[2],
#'     p_attribute = "word", registry = registry_rcppcwb
#'   )
#'   cl_id2str(corpus = "REUTERS", id = ids, p_attribute = "word", registry = registry_rcppcwb)
#' })
#' tokenstream <- rbindlist(
#' lapply(
#'   1:length(tokens),
#'   function(i) data.table(id = i, word = tokens[[i]]))
#'   )
#' tokenstream[["cpos"]] <- 1:nrow(tokenstream)
#' 
#' CD <- CorpusData$new()
#' CD$tokenstream <- as.data.table(tokenstream)
#' CD$metadata <- as.data.table(s_attrs_places)
#' 
#' tmpdir <- tempdir()
#' if (.Platform$OS.type == "windows") tmpdir <- normalizePath(tmpdir, winslash = "/")
#' dir.create (registry_tmp <- file.path(tmpdir, "registry"))
#' dir.create(data_dir_tmp <- file.path(tmpdir, "data_dir"))
#' 
#' CD$encode(corpus = "REUTERS", encoding = "utf8",
#'           p_attributes = "word", s_attributes = "places",
#'           registry_dir = registry_tmp,
#'           data_dir = data_dir_tmp, method = "R"
#' )
#' reg <- registry_data(name = "REUTERS", id = "REUTERS", home = data_dir_tmp, p_attributes = "word")
#' registry_file_write(data = reg, corpus = "REUTERS", registry_dir = registry_tmp)
#' cl_cpos2id(corpus = "REUTERS", p_attribute = "word", cpos = 0L:4049L, registry = registry_tmp)
CorpusData <- R6::R6Class(
  
  classname = "CorpusData",
  
  public = list(
    
    chunktable = NULL, # a data.table
    tokenstream = NULL, # a data.table
    metadata = NULL, # a data.table
    
    initialize = function(){
      self
    },
    
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
    
    tokenize = function(..., verbose = TRUE, progress = TRUE){
      if (requireNamespace("tokenizers", quietly = TRUE)){
        if (class(self$chunktable)[1] != "data.table")
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
      data <- if (progress) pblapply(filenames, .xml_reader) else lapply(filenames, .xml_reader)
      self$chunktable <- rbindlist(lapply(data, function(x) x[["text"]]))
      self$chunktable[["id"]] <- 1L:nrow(self$chunktable)
      self$metadata <- rbindlist(lapply(data, function(x) x[["metadata"]]), fill = TRUE)
      self$metadata[["id"]] <- 1L:nrow(self$metadata)
      invisible(self)
    },
    
    
    add_corpus_positions = function(verbose = TRUE){
      
      if (!"id" %in% colnames(self$metadata)) stop("id column required")
      self$tokenstream[, "cpos" := 0L:(nrow(self$tokenstream) - 1L)]
      
      if (verbose) message("... adding corpus positions to table 'metadata'")
      grpn <- uniqueN(self$tokenstream[["id"]])
      if (interactive()) pb <- timerProgressBar(min = 0, max = grpn, width = getOption("pboptions")[["txt.width"]])
      cpos_dt <- self$tokenstream[
        ,{
          if (interactive()) setTimerProgressBar(pb, .GRP);
          list(cpos_left = min(.SD[["cpos"]]), cpos_right = max(.SD[["cpos"]]))
        }, by = "id"
        ]
      if (interactive()) close(pb)
      
      setkeyv(cpos_dt, cols = "id")
      setkeyv(self$metadata, cols = "id")
      self$metadata <- self$metadata[cpos_dt]
      invisible(self)
    },
    
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
    
    check_encodings = function(){
      # adjust encoding, if necessary
      # if (verbose) message("... checking encoding of input vector")
      # input_enc <- get_encoding(token_stream)
      # if (input_enc == "UTF-8") input_enc <- "utf8" # different capitalization in registry format
      # if (input_enc != encoding){
      #   if (verbose) message(sprintf("... encoding of input vector is '%s', adjusting to '%s'", input_enc, encoding))
      #   token_stream <- iconv(token_stream, from = input_enc, to = encoding)
      #   Encoding(token_stream) <- encoding
      # }
    },
    
    encode = function(corpus, p_attributes = "word", s_attributes = NULL, encoding, registry_dir = Sys.getenv("CORPUS_REGISTRY"), data_dir = NULL, method = c("R", "CWB"), verbose = TRUE, compress = FALSE){
      
      if (file.exists(registry_dir))
        if (file.info(registry_dir)[["isdir"]] != TRUE)
          stop("registry_dir is not a directory")
      registry_file <- file.path(registry_dir, tolower(corpus))
      
      if (file.exists(registry_file)){
        message(sprintf("registry file for corpus '%s' already exists - it should be removed", corpus))
        corpus_remove(corpus = corpus, registry_dir = registry_dir)
      }
      message(sprintf("Creating new corpus '%s'", corpus))
      if (is.null(data_dir)){
        super_dir <- dirname(registry_dir)
        potential_data_dir <- grep("index", list.files(super_dir), value = TRUE, perl = TRUE)
        if (length(potential_data_dir) != 1) stop("no data_dir provided, no candidate found")
        data_dir <- file.path(super_dir, potential_data_dir, tolower(corpus))
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
      
      # add other pAttributes than 'word'
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

