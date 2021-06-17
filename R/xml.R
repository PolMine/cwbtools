.get_s_attr_regions <- function(data_dir, s_attr){
  rng_file <- path(data_dir, paste(s_attr, "rng", sep = "."))
  rng_file_size <- file.info(rng_file)[["size"]]
  con <- file(rng_file, open = "rb")
  rng <- readBin(con, what = integer(), size = 4L, n = rng_file_size / 4L, endian = "big")
  close(con)
  matrix(rng, ncol = 2, byrow = TRUE)
}


#' @details Evaluate whether s_attribute `descendent` is a child of s_attribute `ancestor`.
#' @importFrom fs path
#' @examples 
#' \dontrun{
#' s_attribute_is_descendent("s", "date", corpus = "BT19")
#' s_attribute_is_descendent("s", "date", corpus = "BT19", sample = 100)
#' }
s_attribute_is_descendent <- function(descendent, ancestor, corpus, registry = Sys.getenv("CORPUS_REGISTRY"), data_dir = cwbtools::registry_file_parse(corpus)[["home"]], sample = NULL){
  
  if (!is.null(sample)){
    sample <- as.integer(sample)
    if (is.na(sample)){
      warning(
        "Argument 'sample' is required to be an integer vector ",
        "or a numeric vector that can be coerced to integer vector. ",
        "Coercion not possible (yields NA)."
      )
    }
    n_descendents <- RcppCWB::cl_attribute_size(corpus = corpus, attribute = descendent, attribute_type = "s", registry = registry)
    struc_sample <- sample.int(n = n_descendents - 1L, size = sample)
    descendent_regions <- RcppCWB::get_region_matrix(corpus = corpus, s_attribute = descendent, strucs = struc_sample, registry = registry)
  } else {
    descendent_regions <- .get_s_attr_regions(data_dir = data_dir, s_attr = descendent)
  }
  
  s <- RcppCWB::cl_cpos2struc(corpus = corpus, s_attribute = ancestor, cpos = descendent_regions[,1])
  m <- RcppCWB::get_region_matrix(corpus = corpus, s_attribute = ancestor, strucs = s)

  if (all(descendent_regions[,1] >= m[,1]) && all(descendent_regions[,2] <= m[,2])) TRUE else FALSE
}

#' @examples
#' \dontrun{
#' s_attribute_is_parent("date", "s", )
#' }
s_attribute_is_ancestor <- function(x, y, corpus, registry = Sys.getenv("CORPUS_REGISTRY"), data_dir = cwbtools::registry_file_parse(corpus)[["home"]]){
  
  y_regions <- .get_s_attr_regions(data_dir = data_dir, s_attr = y)
  
  s <- RcppCWB::cl_cpos2struc(corpus = corpus, s_attribute = x, cpos = y_regions[,1])
  if (all(s < 0L)) return(FALSE)
  if (any(s < 0L)) return(NA)
  m <- RcppCWB::get_region_matrix(corpus = corpus, s_attribute = x, strucs = s)
  
  if (all(y_regions[,1] >= m[,1]) && all(y_regions[,2] <= m[,2])) TRUE else FALSE
}

s_attribute_is_sibling <- function(x = "s", y = "date", corpus = "BT19", registry = Sys.getenv("CORPUS_REGISTRY"), data_dir = cwbtools::registry_file_parse(corpus)[["home"]]){
  
  x_regions <- .get_s_attr_regions(s_attr = x, data_dir = data_dir)
  y_regions <- .get_s_attr_regions(s_attr = y, data_dir = data_dir)
  
  if (identical(x_regions, y_regions)) TRUE else FALSE
}

#' @importFrom fs path_ext_remove
s_attributes_structure <- function(corpus, registy =Sys.getenv("CORPUS_REGISTRY"), data_dir = cwbtools::registry_file_parse(corpus)[["home"]]){
  s_attrs <- fs::path_ext_remove(basename(Sys.glob(path(data_dir, "*.rng"))))
  fsizes <- sapply(
    s_attrs,
    function(s) file.info(path(data_dir, paste(s, "rng", sep = ".")))[["size"]]
  )
  fsizes <- fsizes[order(fsizes)]
  groups <- unname(lapply(split(fsizes, fsizes), names))
  
  m <- combn(1L:length(groups), 2L)
  rownames(m) <- c("a", "b")

  a_parent_of_b <- apply(m, 2, function(pair){
    s_attribute_is_parent(
      x = groups[[pair[1]]][[1]],
      y = groups[[pair[2]]][[1]],
      data_dir = data_dir, 
      registry = registry,
      corpus = corpus
    )
  })
}