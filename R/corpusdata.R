#' Encode CWB corpus based on corpusdata.
#' 
#' \code{encode_structural_attribute} will add a structural attribute to a
#' corpus from a data.table with three columns: The left corpus position, the
#' right corpus position and the value of a s-attribute that will be encoded.
#' 
#' \code{corpusdata_add_corpus_positions} will add columns cpos_left and
#' cpos_right to table with structural attributes. A precondition is that a
#' column 'id' is present in tables 'tokenstream' and 'metadata'."
#' 
#' \code{corpusdata_tokenize} will tokenize the tokenstream in ...
#' 
#' @param x an object of class \code{corpusdata}
#' @param make logical, whether to run \code{cwb-make}
#' @param encoding encoding/charst of the CWB corpus 
#' @param registry_dir corpus registry, the directory where registry files are stored
#' @param corpus the name of the CWB corpus
#' @param p_attributes bla bla
#' @param s_attributes columns of .Object that will be encoded as structural attributes
#' @param data_dir directory where to create directory for indexed corpus files
#' @param method either "R" or "CWB"
#' @param verbose logical, whether to be verbose
#' @importFrom data.table uniqueN setkeyv fread fwrite setorderv
#' @export corpusdata_encode
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom data.table rbindlist .GRP .SD :=
#' @rdname corpusdata
#' @examples 
#' library(RcppCWB)
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
#'   ids <- cl_cpos2id(corpus = "REUTERS", cpos = row[1]:row[2], p_attribute = "word", registry = registry_rcppcwb)
#'   cl_id2str(corpus = "REUTERS", id = ids, p_attribute = "word", registry = registry_rcppcwb)
#' })
#' tokenstream <- rbindlist(lapply(1:length(tokens), function(i) data.table(id = i, word = tokens[[i]])))
#' tokenstream[["cpos"]] <- 1:nrow(tokenstream)
#' cdata <- list(tokenstream = tokenstream, metadata = s_attrs_places)
#' class(cdata) <- "corpusdata"
#' 
#' tmpdir <- tempdir()
#' if (.Platform$OS.type == "windows") tmpdir <- normalizePath(tmpdir, winslash = "/")
#' dir.create (registry_tmp <- file.path(tmpdir, "registry"))
#' dir.create(data_dir_tmp <- file.path(tmpdir, "data_dir"))
#' 
#' corpusdata_encode(
#'   x = cdata, corpus = "REUTERS", encoding = "utf8",
#'   p_attributes = "word", s_attributes = "places",
#'   registry_dir = registry_tmp,
#'   data_dir = data_dir_tmp, method = "R"
#' )
corpusdata_encode <- function(x, corpus, p_attributes = "word", s_attributes = NULL, encoding, registry_dir = Sys.getenv("CORPUS_REGISTRY"), data_dir = NULL, method = c("R", "CWB"), verbose = TRUE, make = FALSE){
  
  if (file.exists(registry_dir))
    if (file.info(registry_dir)[["isdir"]] != TRUE)
      stop("registry_dir is not a directory")
  registry_file <- file.path(registry_dir, tolower(corpus))
  if (file.exists(registry_file)){
    message("corpus already exists")
    data_dir <- registry_file_parse(tolower(corpus))[["home"]]
  } else {
    message(sprintf("Creating new corpus '%s'", corpus))
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
  
  if (verbose) message("... encoding p-attribute 'word'")
  p_attribute_encode(
    token_stream = x[["tokenstream"]][["word"]], corpus = corpus, encoding = encoding,
    registry_dir = registry_dir, data_dir = data_dir, method = method, verbose = verbose
    )
  
  # add other pAttributes than 'word'
  if (length(p_attributes > 1)){
    for (new_attribute in p_attributes[which(p_attributes != "word")]){
      if (verbose) message(sprintf("... encoding p-attribute '%s'", new_attribute))
      p_attribute_encode(
        token_stream = x[["tokenstream"]][[new_attribute]], corpus = corpus, encoding = encoding,
        registry_dir = registry_dir, data_dir = data_dir, method = method, verbose = verbose
        )
    }
  }
  
  for (s_attr in s_attributes){
    if (verbose) message(sprintf("... encoding s-attribute '%s'", s_attr))
    s_attribute_encode(
      values = x[["metadata"]][[s_attr]], corpus = corpus,
      s_attribute = s_attr,
      region_matrix = as.matrix(x[["metadata"]][,c("cpos_left", "cpos_right")]),
      data_dir = data_dir, registry_dir = registry_dir, encoding = encoding,
      method = method, verbose = verbose
      )
  }
}



#' @export corpusdata_add_corpus_positions
#' @export corpusdata_encode
#' @rdname corpusdata
corpusdata_add_corpus_positions <- function(x, verbose = TRUE){
  
  if (!"id" %in% colnames(x[["metadata"]])) stop("id column required")
  x[["tokenstream"]][["cpos"]] <- 0L:(nrow(x[["tokenstream"]]) - 1L)
  
  if (verbose) message("... adding corpus positions to table 'metadata'")
  grpn <- uniqueN(x[["tokenstream"]][["id"]])
  if (interactive()) pb <- txtProgressBar(min = 0, max = grpn, style = 3)
  cpos_dt <- x[["tokenstream"]][
    ,{
      if (interactive()) setTxtProgressBar(pb, .GRP);
      list(cpos_left = min(.SD[["cpos"]]), cpos_right = max(.SD[["cpos"]]))
    }, by = "id"
    ]
  if (interactive()) close(pb)
  
  setkeyv(cpos_dt, cols = "id")
  setkeyv(x[["metadata"]], cols = "id")
  x[["metadata"]] <- x[["metadata"]][cpos_dt]
  
  x
}

#' @export corpusdata_tokenize
#' @importFrom data.table setnames
#' @rdname corpusdata
corpusdata_tokenize <- function(x, verbose = TRUE){
  if (requireNamespace("tokenizers", quietly = TRUE)){
    x[["tokenstream"]] <- x[["text"]][,{tokenizers::tokenize_words(.SD[["text"]], lowercase = FALSE)}, by = "id"]
  } else {
    x[["tokenstream"]] <- x[["text"]][,{strsplit(.SD[["text"]], split = "(\\s|[\\.:;\\?!])")[[1]]}, by = "id"]
  }
  setnames(x[["tokenstream"]], old = "V1", new = "word")
  corpusdata_add_corpus_positions(x, verbose = verbose)
}

