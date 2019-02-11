#' Read, process and write data on structural attributes.
#' 
#' @details In addition to using CWB functionality, the \code{s_attribute_encode}
#' function includes a pure R implementation to add or modify structural attributes
#' of an existing CWB corpus.
#' 
#' If the corpus has been loaded/used before,
#' a new s-attribute may not be available unless \code{RcppCWB::cl_delete_corpus}
#' has been called. Use the argument \code{delete} for calling this function.
#' @param values A character vector with the values of the structural attribute.
#' @param data_dir The data directory where to write the files.
#' @param s_attribute Atomic character vector, the name of the structural attribute.
#' @param region_matrix two-column matrix with corpus positions
#' @param corpus the CWB corpus
#' @param method either 'R' or 'CWB'
#' @param encoding encoding of the data
#' @param registry_dir registry directory
#' @param delete Logical, whether a call to \code{RcppCWB::cl_delete_corpus} is performed.
#' @param verbose logicalds
#' @export s_attribute_encode
#' @importFrom RcppCWB cl_delete_corpus
#' @seealso To decode a structural attribute, see \code{\link[RcppCWB]{s_attribute_decode}}.
#' @examples
#' require("RcppCWB")
#' registry_tmp <- file.path(normalizePath(tempdir(), winslash = "/"), "cwb", "registry", fsep = "/")
#' data_dir_tmp <- file.path(
#'   normalizePath(tempdir(), winslash = "/"),
#'   "cwb", "indexed_corpora", "reuters", fsep = "/"
#' )
#' 
#' corpus_copy(
#'   corpus = "REUTERS",
#'   registry_dir = system.file(package = "RcppCWB", "extdata", "cwb", "registry"),
#'   data_dir = system.file(package = "RcppCWB", "extdata", "cwb", "indexed_corpora", "reuters"),
#'   registry_dir_new = registry_tmp,
#'   data_dir_new = data_dir_tmp
#' )
#' 
#' no_strucs <- cl_attribute_size(
#'   corpus = "REUTERS",
#'   attribute = "id", attribute_type = "s",
#'   registry = registry_tmp
#' )
#' cpos_list <- lapply(
#'   0L:(no_strucs - 1L),
#'   function(i)
#'     cl_struc2cpos(corpus = "REUTERS", struc = i, s_attribute = "id", registry = registry_tmp)
#' )
#' cpos_matrix <- do.call(rbind, cpos_list)
#' 
#' s_attribute_encode(
#'   values = as.character(1L:nrow(cpos_matrix)),
#'   data_dir = data_dir_tmp,
#'   s_attribute = "foo",
#'   corpus = "REUTERS",
#'   region_matrix = cpos_matrix,
#'   method = "R",
#'   registry_dir = registry_tmp,
#'   encoding = "latin1",
#'   verbose = TRUE,
#'   delete = TRUE
#' )
#' 
#' cl_struc2str(
#'   "REUTERS", struc = 0L:(nrow(cpos_matrix) - 1L), s_attribute = "foo", registry = registry_tmp
#' )
#' 
#' unlink(registry_tmp, recursive = TRUE)
#' unlink(data_dir_tmp, recursive = TRUE)
#' @rdname s_attribute
s_attribute_encode <- function(values, data_dir, s_attribute, corpus, region_matrix, method = c("R", "CWB"), registry_dir = Sys.getenv("CORPUS_REGISTRY"), encoding, delete = FALSE, verbose = TRUE){
  stopifnot(
    class(region_matrix) == "matrix",
    ncol(region_matrix) == 2,
    is.character(values),
    length(values) == nrow(region_matrix)
    )
  if (class(as.vector(region_matrix)) != "integer"){
    region_matrix <- matrix(data = as.integer(as.vector(region_matrix)), ncol = 2)
  }
  if (method == "R"){
    avs_file <- file.path(data_dir, paste(s_attribute, "avs", sep = "."), fsep = "/") # attribute values
    avx_file <- file.path(data_dir, paste(s_attribute, "avx", sep = "."), fsep = "/") # attribute value index
    rng_file <- file.path(data_dir, paste(s_attribute, "rng", sep = "."), fsep = "/") # ranges
    
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
      ncol = 2L, byrow = FALSE
    )
    avx_vector <- as.integer(t(avx_matrix))
    writeBin(object = avx_vector, con = avx_file, size = 4L, endian = "big")
    
    # generate and write attrib.rng
    region_vector <- as.vector(t(region_matrix))
    writeBin(object = region_vector, con = rng_file, size = 4L, endian = "big")
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
      file.path(cwb_get_bindir(), "cwb-s-encode", fsep = "/"),
      "-d", data_dir,
      "-f", tmp_file,
      "-V", s_attribute
    )
    
    system(paste(cmd, collapse = " "))
    
  }
  regdata <- registry_file_parse(tolower(corpus), registry_dir = registry_dir)
  if (!s_attribute %in% regdata[["s_attributes"]]){
    if (verbose) message(sprintf("... adding s-attribute '%s' to registry", s_attribute))
    regdata[["s_attributes"]] <- c(regdata[["s_attributes"]], s_attribute)
    registry_file_write(regdata, corpus = tolower(corpus), registry_dir = registry_dir)
  }
  if (delete) cl_delete_corpus(corpus = toupper(corpus), registry = registry_dir)
  invisible( NULL )
}


#' @details \code{s_attribute_recode} will recode the values in the avs-file and change
#' the attribute value index in the avx file. The rng-file remains unchanged. The registry
#' file remains unchanged, and it is highly recommended to consider \code{s_attribute_recode}
#' as a helper for \code{corpus_recode} that will recode all s-attributes, all p-attributes,
#' and will reset the encoding in the registry file.
#' @param from Character string describing the current encoding of the attribute.
#' @param to Character string describing the target encoding of the attribute.
#' @export s_attribute_recode
#' @rdname s_attribute
s_attribute_recode <- function(data_dir, s_attribute, from = c("UTF-8", "latin1"), to = c("UTF-8", "latin1")){
  
  s_attr_files <- s_attribute_files(data_dir = data_dir, s_attribute = s_attribute)
  
  # read, recode and write values of s-attribute
  
  attribute_values <- readBin(
    con = s_attr_files[["avs"]],
    what = character(),
    n = file.info(s_attr_files[["avs"]])$size
    )
  Encoding(attribute_values) <- from

  values_hex_list <- iconv(x = attribute_values, from = toupper(from), to = toupper(to), toRaw = TRUE)
  values_hex_list <- lapply(values_hex_list, function(x) c(x, as.raw(0)))
  values_hex_vec <- unlist(values_hex_list)
  
  writeBin(object = values_hex_vec, con = s_attr_files[["avs"]])
  
  # generate and write attrib.avx
  
  avx_vector <- readBin(
    con = s_attr_files[["avx"]],
    what = integer(), size = 4L,
    n = file.info(s_attr_files[["avx"]])$size,
    endian = "big"
    )
  avx_dt <- as.data.table(matrix(data = avx_vector, ncol = 2, byrow = TRUE))
  colnames(avx_dt) <- c("struc", "offset_old")
  avx_dt[["offset_id"]] <- match(
    avx_dt[["offset_old"]],
    sort.int(unique(avx_dt[["offset_old"]]), decreasing = FALSE)
    )
  
  offset_new <- cumsum(sapply(values_hex_list, length))
  offset_new <- c(0L, offset_new[1L:(length(offset_new) - 1L)])
  avx_dt[["offset_new"]] <- offset_new[ avx_dt[["offset_id"]] ]

  avx_matrix_new <- as.matrix(avx_dt[, c("struc", "offset_new")])
  avx_vector <- as.integer(t(avx_matrix_new))
  writeBin(object = avx_vector, con = s_attr_files[["avx"]], size = 4L, endian = "big")
  
  invisible( NULL )
}


#' @details \code{s_attribute_files} will return a named character vector with
#'   the data files (extensions: "avs", "avx", "rng") in the directory indicated
#'   by \code{data_dir} for the structural attribute \code{s_attribute}.
#' @export s_attribute_files
#' @rdname s_attribute
s_attribute_files <- function(s_attribute, data_dir){
  sapply(
    c("avs", "avx", "rng"),
    function(fileext) file.path(data_dir, paste(s_attribute, fileext, sep = "."), fsep = "/")
  )
}


#' @details \code{s_attribute_get_values} is equivalent to performing the CL
#'   function cl_struc2id for all strucs of a structural attribute. It is a
#'   "pure R" operation that is faster than using CL, as it processes entire
#'   files for the s-attribute directly. The return value is a \code{character}
#'   vector with all string values for the s-attribute.
#' @examples
#' avs <- s_attribute_get_values(
#'   s_attribute = "id",
#'   data_dir = system.file(package = "RcppCWB", "extdata", "cwb", "indexed_corpora", "reuters")
#' )
#' @export s_attribute_get_values
#' @rdname s_attribute
s_attribute_get_values <- function(s_attribute, data_dir){
  
  avs_file <- sprintf("%s/%s.avs", data_dir, s_attribute)
  avs_size <- file.info(avs_file)$size
  avs <- readBin(con = avs_file, what = character(), n = avs_size)
  
  offset <- cumsum(sapply(lapply(avs, charToRaw), length) + 1L)
  offset <- c(0L, offset[1L:(length(offset) - 1L)])
  
  
  avx_file <- sprintf("%s/%s.avx", data_dir, s_attribute)
  avx_size <- file.info(avx_file)$size
  avx_vector <- readBin(con = avx_file, what = integer(), size = 4L, n = avx_size, endian = "big")
  avx_matrix <- matrix(data = avx_vector, ncol = 2, byrow = TRUE)
  
  avs[match(avx_matrix[,2], offset)]
}

#' @details \code{s_attribute_get_regions} will return a two-column integer
#'   matrix with regions for the strucs of a given s-attribute. Left corpus
#'   positions are in the first column, right corpus positions in the second
#'   column. The result is equivalent to calling RcppCWB::get_region_matrix for
#'   all strucs of a s-attribute, but may be somewhat faster. It is a "pure R"
#'   function which is fast as it processes files entirely and directly.
#' @examples
#' rng <- s_attribute_get_regions(
#'   s_attribute = "id",
#'   data_dir = system.file(package = "RcppCWB", "extdata", "cwb", "indexed_corpora", "reuters")
#' )
#' @export s_attribute_get_regions
#' @rdname s_attribute
s_attribute_get_regions <- function(s_attribute, data_dir){
  rng_file <- sprintf("%s/%s.rng", data_dir, s_attribute)
  rng_size <- file.info(rng_file)$size
  rng_vector <- readBin(con = rng_file, what = integer(), size = 4L, n = rng_size, endian = "big")
  matrix(data = rng_vector, ncol = 2, byrow = TRUE)
}


#' @details \code{s_attribute_merge} combines two tables with regions for
#'   s-attributes checking for intersections that may cause problems. The
#'   heuristic is to keep all non-intersecting annotations and those annotations
#'   that define the same region in object \code{x} and object \code{y}.
#'   Annotations of \code{x} and \code{y} which overlap uncleanly, i.e. without
#'   an identity of the left and the right corpus position ("cpos_left" /
#'   "cpos_right") are dropped. The scenario for using the function is to decode
#'   a s-attribute (using \code{s_attribute_decode}), mix in an additional
#'   annotation, and to re-encode the enhanced s-attribute (using
#'   \code{s_attribute_encode).
#' @param x Data defining a first s-attribute, a \code{data.table} (or an object
#'   coercible to a \code{data.table}) with three columns ("cpos_left",
#'   "cpos_right", "value").
#' @param y Data defining a second s-attribute, a \code{data.table} (or an
#'   object coercible to a \code{data.table})with three columns ("cpos_left",
#'   "cpos_right", "value").
#' @export s_attribute_merge 
#' @examples
#' x <- data.frame(
#'   cpos_left =  c(1L, 5L, 10L, 20L, 25L),
#'   cpos_right = c(2L, 5L, 12L, 21L, 27L),
#'   value = c("ORG", "LOC", "ORG", "PERS", "ORG"),
#'   stringsAsFactors = FALSE
#' )
#' y <- data.frame(
#'   cpos_left =  c(5, 11, 20, 25L, 30L),
#'   cpos_right = c(5, 12, 22, 27L, 33L),
#'   value = c("LOC", "ORG", "ORG", "ORG", "ORG"),
#'   stringsAsFactors = FALSE
#' )
#' s_attribute_merge(x,y)
#' @rdname s_attribute
#' @importFrom data.table is.data.table dcast.data.table
s_attribute_merge <- function(x, y){
  if (!is.data.table(x)) x <- as.data.table(x)
  if (!is.data.table(y)) y <- as.data.table(y)
  x[, "s_attr" := "x"][, "region_no" := seq_len(nrow(x))]
  y[, "s_attr" := "y"][, "region_no" := seq_len(nrow(y))]
  dt <- rbindlist(lapply(
    list(x, y),
    function(dt){
      dt[,
         {list(cpos = seq.int(from = .SD[["cpos_left"]], to = .SD[["cpos_right"]]))},
         by = "region_no"
        ][, "s_attr" := dt[["s_attr"]][1]]
    }
  ))

  overlaps <- dcast.data.table(data = dt, formula = cpos ~ s_attr, value.var = "region_no")
  
  # now we get the regions that overlap
  regions <- overlaps[!is.na(x)][!is.na(y)][,{.SD[1L]}, by = c("x", "y")]
  left_identical <- x[regions[["x"]]][["cpos_left"]] == y[regions[["y"]]][["cpos_left"]]
  right_identical <- x[regions[["x"]]][["cpos_right"]] == y[regions[["y"]]][["cpos_right"]]
  keep <- ifelse(left_identical == right_identical, TRUE, FALSE)
  regions_to_drop_x <- regions[!keep][["x"]]
  regions_to_drop_y <- regions[["y"]]
  if (length(regions_to_drop_x) > 0) x <- x[-regions_to_drop_x]
  if (length(regions_to_drop_y) > 0) y <- y[-regions_to_drop_y]
  retval <- rbindlist(list(x,y))
  setorderv(retval, cols = "cpos_left", order = 1L)
  retval[, "s_attr" := NULL][, "region_no" := NULL]
  as.data.frame(retval)
}


#' @details Function \code{s_attribute_delete} is not yet implemented.
#' @export s_attribute_delete
#' @rdname s_attribute
s_attribute_delete <- function(corpus, s_attribute){
  stop("function 's_attribute_delete' is not yet implemented")
}