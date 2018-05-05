#' Encode CWB corpus.
#' 
#' \code{encode_structural_attribute} will add a structural attribute to a corpus 
#' from a data.table with three columns: The left corpus position, the right corpus position and the value of a s-attribute
#' that will be encoded.
#' 
#' @param corpus corpus name of the CWB corpus
#' @param encoding encoding/charst of the CWB corpus 
#' @param .Object input object
#' @param registry_dir corpus registry, the directory where registry files are stored
#' @param data_dir directory with indexed corpus files
#' @param corpus the name of the CWB corpus
#' @param p_attributes columns of .Object with tokens (such as word/pos/lemma)
#' @param s_attribute a single s-attribute
#' @param s_attributes columns of .Object that will be encoded as structural attributes
#' @param data_dir directory where to create directory for indexed corpus files
#' @param verbose logical, whether to be verbose
#' @importFrom data.table uniqueN setkeyv fread fwrite setorderv
#' @rdname encode
#' @export encode_corpusdata
encode_corpusdata <- function(x, corpus, p_attributes = "word", s_attributes = NULL, encoding, registry_dir = Sys.getenv("CORPUS_REGISTRY"), data_dir = NULL, verbose = TRUE){
  if (file.exists(registry_dir))
    if (file.info(registry_dir)[["isdir"]] != TRUE)
      stop("registryDir is not a directory")
  registry_file <- file.path(registry_dir, tolower(corpus))
  if (file.exists(registry_file)){
    message("corpus already exists")
    rf <- polmineR::RegistryFile$new(corpus = toupper(corpus), registry = registry_dir)
    data_dir <- rf$getHome()
  } else {
    message("corpus does not yet exist")
    if (is.null(data_dir)){
      super_dir <- dirname(registry_dir)
      potential_data_dir <- grep("index", list.files(super_dir), value = TRUE, perl = TRUE)
      if (length(potential_data_dir) != 1) stop("no data_dir provided, no candidate found")
      data_dir <- file.path(super_dir, potential_data_dir, tolower(corpus))
      message(sprintf("suggesting data_dir: %s (Y/N) ", data_dir))
      feedback <- readline(prompt = "")
      if (feedback != "Y") stop("aborting")
      if (!file.exists(data_dir)) dir.create(data_dir)
    } else {
      if (!file.exists(data_dir)) dir.create(data_dir)
    }
  }
  
  if (!encoding %in% c("ascii", paste("latin", 1:9, sep = ""), "utf8")){
    stop("encoding is required to be among ascii; latin1 to latin9; utf8")
  }

  encode_positional_attribute(
    x = x, p_attribute = "word", corpus = corpus, encoding = encoding,
    registry_dir = registry_dir, data_dir = data_dir, verbose = verbose
    )
  
  # add other pAttributes than 'word'
  if (length(p_attributes > 1)){
    for (new_attribute in p_attributes[which(p_attributes != "word")]){
      message(new_attribute)
      encode_positional_attribute(
        x = x, p_attribute = new_attribute, corpus = corpus, encoding = encoding,
        registry_dir = registry_dir, data_dir = data_dir, verbose = verbose
        )
    }
  }
  
  for (s_attr in s_attributes){
    if (verbose) message("... encoding s-attribute ", s_attr)
    encode_structural_attribute(x = x, s_attribute = s_attr, corpus = corpus, data_dir = data_dir, registry_dir = registry_dir, encoding = encoding, verbose = verbose)
  }
}



#' @export encode_structural_attribute
#' @rdname encode
encode_structural_attribute = function(x, corpus, s_attribute, data_dir, registry_dir, encoding, verbose = TRUE){

  tab <- x[["metadata"]][, c("cpos_left", "cpos_right", s_attribute), with = FALSE]
  setorderv(tab, cols = "cpos_left", order = 1L)
  
  # adjust encoding, if necessary
  input_enc <- get_encoding(as.character(tab[[s_attribute]]))
  if (input_enc != encoding){
    tab[[s_attribute]] <- iconv(tab[[s_attribute]], from = input_enc, to = encoding)
    Encoding(tab[[s_attribute]]) <- encoding
  }
  
  if (verbose) message("... writing table to disk")
  tmp_file <- tempfile()
  data.table::fwrite(x = tab, file = tmp_file, quote = FALSE, sep = "\t", col.names = FALSE)
  
  if (verbose) message("... running cwb-s-encode")
  cmd <- c(
    "cwb-s-encode",
    "-d", data_dir,
    "-f", tmp_file,
    "-V", s_attribute
  )
  
  system(paste(cmd, collapse = " "))
  
  if (!s_attribute %in% polmineR::sAttributes(toupper(corpus))){
    if (verbose) message("... adding sAttribute to registry")
    R <- polmineR::RegistryFile$new(toupper(corpus))
    R$read()
    R$addSAttribute(s_attribute)
    R$write()
  }
  
  polmineR::use(dir = registry_dir)
  
}


#' @rdname encode
#' @export encode_positional_attribute
encode_positional_attribute <- function(x, p_attribute = "word", corpus, encoding, registry_dir, data_dir, make = TRUE, verbose = TRUE){
  stopifnot("tokenstream" %in% names(x))
  data <- x[["tokenstream"]]
  if (p_attribute == "word"){
    if (verbose) message("Creating new CWB indexed corpus ", corpus)
    if (!"word" %in% colnames(data)) stop("column 'word' required to be in table 'tokenstream'")
    if (any(grepl("^\\s*<.*?>\\s*$", data[["word"]])))
      warning("there is markup in the character vector - cwb-encode will issue warnings")
    
    # adjust encoding, if necessary
    input_enc <- get_encoding(data[["word"]])
    if (input_enc != encoding){
      data[["word"]] <- iconv(data[["word"]], from = input_enc, to = encoding)
      Encoding(data[["word"]]) <- encoding
    }
    
    if (verbose) message("... writing token stream to disk")
    vrt_tmp_file <- tempfile()
    data.table::fwrite(
      data[,"word"], file = vrt_tmp_file,
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
    if (nrow(data) != polmineR::size(toupper(corpus)))
      stop("Length of character vector must be identical with size of corpus - not TRUE")
    
    if (p_attribute %in% polmineR::p_attributes(toupper(corpus)))
      stop("pAttribute already exists")
    
    if (any(grepl("^\\s*<.*?$", data[[pAttribute]])))
      warning("there is markup in the character vector - cwb-encode will issue warnings")
    
    # adjust encoding, if necessary
    input_enc <- get_encoding(data[[p_attribute]])
    if (input_enc != encoding){
      data[[p_attribute]] <- iconv(data[[p_attribute]], from = input_enc, to = encoding)
      Encoding(data[[p_attribute]]) <- encoding
    }
    
    if (verbose) message("... writing vector to disk for p-attribute ", p_attribute)
    vrt_tmp_file <- tempfile()
    data.table::fwrite(
      data[, p_attribute, with = FALSE], file = vrt_tmp_file,
      col.names = FALSE, quote = FALSE, showProgress = interactive()
    )
    
    if (verbose) message("... calling cwb-encode")
    p_attrs_old <- polmineR::RegistryFile$new(toupper(corpus))$getPAttributes() # for checking later if anything is missing
    cwb_encode_cmd_vec <- c(
      "cwb-encode",
      "-d", data_dir, # directory with indexed corpus files
      "-f", vrt_tmp_file,
      "-R", registry_file,
      "-p", "-", "-P", p_attribute,
      "-c", encoding
    )
    # cwb-encode may drop attributes from registry file apart from the newly encoded one ... 
    missing_attrs <- p_attrs_old[!p_attrs_old %in% polmineR::RegistryFile$new(corpus)$getPAttributes()]
    for (attr in missing_attrs) polmineR::RegistryFile$new(corpus)$addPAttribute(attr)
  }
  
  cwb_encode_cmd <- paste0(cwb_encode_cmd_vec, collapse = " ")
  
  system(cwb_encode_cmd)
  
  if (make) cwb_make(corpus = corpus, registry_dir = registry_dir, verbose = verbose)
  polmineR::use(dir = registry_dir)
}

