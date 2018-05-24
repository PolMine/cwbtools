#' Encode Structural Attribute.
#' 
#' Pure R implementation to generate structural attributes.
#' @param values a character vector with the values of the structural attribute
#' @param data_dir the data directory where to write the files
#' @param s_attribute atomic character vector, the name of the structural attribute
#' @param region_matrix two-column matrix with corpus positions
#' @param corpus the CWB corpus
#' @param method either 'R' or 'CWB'
#' @param encoding encoding of the data
#' @param registry_dir registry directory
#' @param verbose logicalds
#' @rdname s_attribute
#' @export s_attribute_encode
s_attribute_encode <- function(values, data_dir, s_attribute, corpus, region_matrix, method = c("R", "CWB"), registry_dir = Sys.getenv("CORPUS_REGISTRY"), encoding, verbose = TRUE){
  if (method == "R"){
    avs_file <- file.path(data_dir, paste(s_attribute, "avs", sep = ".")) # attribute values
    avx_file <- file.path(data_dir, paste(s_attribute, "avx", sep = ".")) # attribute value index
    rng_file <- file.path(data_dir, paste(s_attribute, "rng", sep = ".")) # ranges
    
    # generate and write attrib.avs
    if (!is.character(values)) values <- as.character(values)
    values_unique <- unique(values)
    if (encoding == "latin1"){
      values_hex_list <- iconv(x = values_unique, from = "UTF-8", to = toupper(encoding), toRaw = TRUE)
    } else {
      values_hex_list <- lapply(values_unique, charToRaw)
    }
    values_hex_list <- lapply(values_hex_list, function(x) c(x, as.raw(0)))
    values_hex_vec <- unlist(values_hex_list)
    writeBin(object = values_hex_vec, con = avs_file)

    # generate and write attrib.avx
    offset <- cumsum(sapply(values_hex_list, length))
    offset <- c(0L, offset[1:(length(offset) - 1L)])
    avx_matrix <- matrix(
      c(0L:(length(values) - 1L), offset[match(values, values_unique)]),
      ncol = 2, byrow = FALSE
    )
    avx_vector <- as.integer(t(avx_matrix))
    writeBin(object = avx_vector, con = avx_file, size = 4L, endian = "big")
    
    # generate and write attrib.rng
    region_vector <- as.vector(t(region_matrix))
    writeBin(object = region_vector, con = rng_file, size = 4L, endian = "big")
    return( TRUE )
  } else if (method == "CWB"){
    
    tab <- data.table(region_matrix, s_attribute = values)
    setorderv(tab, cols = "cpos_left", order = 1L)
    
    # adjust encoding, if necessary
    input_enc <- get_encoding(as.character(tab[["s_attribute"]]))
    if (input_enc != encoding){
      tab[["s_attribute"]] <- iconv(tab[["s_attribute"]], from = input_enc, to = encoding)
      Encoding(tab[["s_attribute"]]) <- encoding
    }
    
    tmp_file <- tempfile()
    data.table::fwrite(x = tab, file = tmp_file, quote = FALSE, sep = "\t", col.names = FALSE)
    
    if (verbose) message(sprintf("... running 'cwb-s-encode' to add structural annotation for attribute '%s'", s_attribute))
    cmd <- c(
      file.path(cwb_get_bindir(), "cwb-s-encode"),
      "-d", data_dir,
      "-f", tmp_file,
      "-V", s_attribute
    )
    
    system(paste(cmd, collapse = " "))
    
    regdata <- registry_file_parse(tolower(corpus), registry_dir = registry_dir)
    if (!s_attribute %in% regdata[["s_attributes"]]){
      if (verbose) message(sprintf("... adding s-attribute '%s' to registry", s_attribute))
      regdata[["s_attributes"]] <- c(regdata[["s_attributes"]], s_attribute)
      registry_file_write(regdata, corpus = tolower(corpus), registry_dir = registry_dir)
    }
  }
}
