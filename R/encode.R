#' Encode CWB Corpus.
#' 
#' @description
#' `r lifecycle::badge("experimental")`
#' 
#' @param x A `data.frame` or an object inheriting from `data.frame` (such as 
#'   `tibble`, `data.table`).
#' @param s_attributes A `list` of `data.frame` objects with columns 'cpos_left'
#'   and 'cpos_right' and columns with s-attributes, the names of which will 
#'   serve as names of s-attributes. It `s_attributes` is a `data.frame`, it will
#'   be coerced to a `list`.
#' @param properties A named `character` vector with corpus properties that will
#'   be added to the registry file describing the corpus. Names of the vector
#'   indicate a property (such as "version") and the values of the vector the
#'   values of a corpus property.
#' @param reload A logical value, whether to reload the corpus to make it
#'   immediatedly available.
#' @param ... Further arguments (unused).
#' @inheritParams p_attribute_encode
#' @rdname encode
#' @exportMethod encode
setGeneric("encode", function(x, ...) standardGeneric("encode"))

#' @examples
#' library(dplyr)
#' library(tidytext)
#' 
#' registry_tmp <- fs::path(tempdir(), "cwb_registry")
#' dir.create(registry_tmp)
#' 
#' tidydata <- quanteda::data_char_ukimmig2010 %>%
#'    as.data.frame() %>%
#'    as_tibble(rownames = "party") %>%
#'    rename(`text` = ".")
#'    
#' tokenstream <- tidydata %>%
#'    unnest_tokens(word, text, to_lower = FALSE, strip_punct = FALSE) %>%
#'    mutate(cpos = 0L:(nrow(.) - 1L))
#'    
#' metadata <- tokenstream %>% 
#'   group_by(party) %>% 
#'   summarise(cpos_left = min(cpos), cpos_right = max(cpos))
#' 
#' tokenstream %>%
#'   select(-cpos) %>%
#'   encode(
#'     corpus = "UKIMMIG2010",
#'     s_attributes = metadata,
#'     properties = c(lang = "en")
#'   )
#' @rdname encode
setMethod("encode", "data.frame", function(
    x,
    corpus,
    s_attributes = NULL,
    encoding = "utf8",
    registry_dir = fs::path(tempdir(), "cwb_registry"),
    data_dir = fs::path(tempdir(), "cwb_data_dir", tolower(corpus)),
    properties = c(),
    method = c("R", "CWB"),
    verbose = TRUE,
    compress = FALSE,
    reload = TRUE,
    quietly = TRUE
){
  if (!"word" %in% colnames(x)){
    cli_alert_danger("Column 'word' required but missing.")
    return(FALSE)
  }
  # add check on validity of p-attributes to be encoded
  
  if (inherits(s_attributes, "data.frame")) s_attributes <- list(s_attributes)
  for (i in seq_along(s_attributes)){
    if (!inherits(s_attributes[[i]], "data.frame")){
      cli_alert_danger("s_attribute is not a data.frame or an object inheriting from data.frame")
      return(FALSE)
    }
    if (!all(c("cpos_left", "cpos_right") %in% colnames(s_attributes[[i]]))){
      cli_alert_danger("columns 'cpos_left' and 'cpos_right' missing")
      return(FALSE)
    }
  }
  s_attr_names <- setdiff(
    unlist(lapply(s_attributes, colnames)),
    c("cpos_left", "cpos_right")
  )
  # add check on validity of s-attributes to be encoded

  if (verbose) cli_rule("Prepare encoding corpus {corpus}")
  
  # check registry file and registry directory ---------------------------------
  if (file.exists(registry_dir))
    if (file.info(registry_dir)[["isdir"]] != TRUE)
      stop("registry_dir is not a directory")
  if (verbose)
    cli_alert_info("registry directory: {.path {registry_dir}}")
  
  registry_file <- fs::path(registry_dir, tolower(corpus))
  
  if (file.exists(registry_file)){
    cli_alert_warning(
      "registry file for corpus {.val {corpus}} already exists"
    )
    corpus_remove(corpus = corpus, registry_dir = registry_dir)
  }
  
  if (!file.exists(data_dir)) dir.create(data_dir, recursive = TRUE)
  
  if (verbose) cli_alert_info("data directory: {.path {data_dir}}")
  
  # checking whether the encoding is ok should be part of p_attribute_encode()
  # and s_attribute_encode()
  if (!encoding %in% c("ascii", paste("latin", 1:9, sep = ""), "utf8")){
    stop(
      "encoding is required to be either ascii, latin1 to latin9, or utf8"
    )
  }
  if (verbose) cli_alert_info("encoding: {.val {encoding}}")
  
  if (length(method) > 1) method <- method[1]
  if (verbose)
    cli_alert_info(
      "method for encoding, indexing and compression: {.val {method}}"
    )
  
  if (verbose) cli_rule("encode p-attribute {.val word}")
  p_attribute_encode(
    token_stream = x[["word"]],
    corpus = corpus,
    encoding = encoding,
    registry_dir = registry_dir,
    data_dir = data_dir,
    method = method,
    verbose = verbose,
    compress = compress,
    quietly = quietly
  )
  
  # add other p-attributes than 'word'
  if (ncol(x) > 1L){
    for (p_attr in setdiff(colnames(x), "word")){
      if (verbose) cli_rule("encode p-attribute {.val {p_attr}}")
      p_attribute_encode(
        token_stream = x[[p_attr]],
        corpus = corpus, 
        p_attribute = p_attr,
        encoding = encoding,
        registry_dir = registry_dir,
        data_dir = data_dir,
        method = method,
        verbose = verbose,
        compress = compress,
        quietly = quietly
      )
    }
  }
  
  if (verbose) cli_rule("Encode s-attributes")
  for (i in seq_along(s_attributes)){
    s_attrs <- setdiff(
      colnames(s_attributes[[i]]),
      c("cpos_left", "cpos_right"))
    for (s_attr in s_attrs){
      if (verbose) cli_alert_info("encode s-attribute {.val {s_attr}}")
      s_attribute_encode(
        values = s_attributes[[i]][[s_attr]],
        corpus = corpus,
        s_attribute = s_attr,
        region_matrix = as.matrix(
          s_attributes[[i]][,c("cpos_left", "cpos_right")]
        ),
        data_dir = data_dir,
        registry_dir = registry_dir,
        encoding = encoding,
        method = method,
        verbose = verbose
      )
    }
  }
  
  if (verbose) cli_rule("Prepare and registry file")
  
  props <- c(charset = encoding)
  if (length(properties) > 0L){
    if (is.null(names(properties))){
      cli_alert_warning(paste0(
        "`properties` required to be a named character vector ",
        "(not adding properties)"
      ))
    } else {
      # TODO: check that names are unique, are non-ASCII characters allowed?
      if ("charset" %in% names(properties)){
        properties <- properties[-which(names(properties) == "charset")]
        cli_alert_warning(paste0(
          "drop property {.val charset} from input vector `properties` to avoid ",
          "re-defining it (using value of argument `encoding`: {.val {encoding}})"
        ))
      }
      props <- c(props, properties)
    }
  }
  
  if (verbose) cli_alert_info(paste0(
    "using corpus properties: ",
    "{col_blue({paste(paste(names(props), props, sep = ' = '), collapse = ' // ')})}"
  ))
  
  reg_data <- registry_data(
    name = toupper(corpus),
    id = tolower(corpus),
    home = path.expand(data_dir),
    properties = props, 
    p_attributes = colnames(x),
    s_attributes = s_attr_names
  )
  if (verbose) cli_progress_step("writing registry file")
  registry_file_write(
    data = reg_data,
    corpus = tolower(corpus),
    registry_dir = registry_dir
  )
  if (verbose) cli_progress_done()
  
  if (verbose) cli_alert_info("")
  
  if (verbose) cli_rule("Check result")
  
  if (isTRUE(reload))
    corpus_reload(corpus = corpus, registry = registry_dir, verbose = verbose)
  
  p_attrs <- corpus_p_attributes(corpus = corpus, registry = registry_dir)
  if (all(colnames(x) %in% p_attrs)){
    if (verbose) cli_alert_success("all p-attributes are available")
  } else {
    cli_alert_danger("not all p-attributes available")
  }
  
  s_attrs <- corpus_s_attributes(corpus = corpus, registry = registry_dir)
  if (all(s_attr_names %in% s_attrs)){
    if (verbose) cli_alert_success("all s-attributes are available")
  } else {
    cli_alert_danger("not all s-attributes available")
  }
  
  invisible(TRUE)
})

