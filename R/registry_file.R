#' Parse and create registry files.
#' 
#' A set of functions to parse, create and write registry files.
#' 
#' `registry_file_parse()` will return an object of class `registry_data`.
#' 
#' See the appendix to the 'Corpus Encoding Tutorial'
#' (https://cwb.sourceforge.io/files/CWB_Encoding_Tutorial.pdf), which includes an
#' explanation of the registry file format.
#' @param data A `registry_data` object.
#' @param corpus A CWB corpus indicated by a length-one `character` vector.
#' @param registry_dir Directory with registry files.
#' @param x An object of class \code{registry_data}.
#' @param ... further parameters
#' @param name Long descriptive name of corpus (\code{character} vector).
#' @param id Short name of corpus (\code{character} vector).
#' @param home Path with data directory for indexed corpus.
#' @param info A \code{character} vector containing path name of info file.
#' @param properties Named \code{character} vector with corpus properties,
#'   should at least include 'charset'.
#' @param p_attributes A \code{character} vector with positional attributes to
#'   declare.
#' @param s_attributes A \code{character} vector with structural attributes to
#'   declare.
#' @rdname registry_file
#' @importFrom stringi stri_match_all_regex
#' @export registry_file_parse
#' @examples 
#' regdata <- registry_file_parse(
#'   corpus = "REUTERS",
#'   registry_dir = system.file(package = "RcppCWB", "extdata", "cwb", "registry")
#'   )
registry_file_parse <- function(corpus, registry_dir = Sys.getenv("CORPUS_REGISTRY")){
  r <- readLines(fs::path(registry_dir, tolower(corpus)))
  regex_vec <- c(
    name = '^NAME\\s+"(.*?)"\\s*$',
    id = "^ID\\s+(.*?)\\s*$",
    home = '^HOME\\s+"?(.*?)"?\\s*$',
    info = '^INFO\\s+"?(.*?)"?\\s*$',
    p_attributes = "^ATTRIBUTE\\s+(.*?)$",
    s_attributes = "^STRUCTURE\\s+(.*?)(|\\s+.*?)$"
  )
  registry_data <- lapply(
    regex_vec,
    function(regex){
      matches <- stri_match_all_regex(str = r, pattern  = regex, omit_no_match = TRUE)
      do.call(rbind, matches)[,2]
    }
  )
  properties_raw <- stri_match_all_regex(
    r, pattern = '^##::\\s*(.*?)\\s*=\\s*"(.*?)".*?$',
    omit_no_match = TRUE
  )
  properties <- do.call(rbind, properties_raw)
  registry_data[["properties"]] <- setNames(properties[,3], properties[,2])
  class(registry_data) <- "registry_data"
  registry_data
}


#' @export
print.registry_data <- function(x, ...){
  cat("General Information\n")
  cat("-------------------\n")
  general <- c(
    "corpus name" = "name",
    "corpus id" = "id",
    "home directory" = "home",
    "info file" = "info",
    "charset / encoding" = "charset"
    )
  cat(
    sprintf(
      "%s:%s%s\n",
      names(general),
      sapply(20 - nchar(names(general)), function(x) paste(rep(" ", times = x), collapse = "")),
      sapply(general, function(n) x[[n]])
    ),
    sep = ""
  )
  
  cat("\nCorpus Properties\n")
  cat("-------------------\n")
  cat(
    sprintf(
      "%s:%s%s\n",
      names(x[["properties"]]),
      sapply(20 - nchar(names(x[["properties"]])), function(x) paste(rep(" ", times = x), collapse = "")),
      unname(x[["properties"]])
    ),
    sep = ""
  )
  
  
  cat("\nPositional Attributes\n")
  cat("---------------------\n")
  cat(paste(x[["s_attributes"]], collapse = " | "))
  
  cat("\n\nStructural Attributes\n")
  cat("---------------------\n")
  cat(paste0(x[["p_attributes"]], collapse = " | "))
}


#' @details \code{registry_file_compose} will turn an
#'   \code{registry_data}-object into a character vector with a registry file
#'   that can be written to disk.
#' @rdname registry_file
#' @export registry_file_compose
registry_file_compose <- function(x){
  
  if (!file.exists(x[["home"]]))
    warning("cannot confirm that data/home directory exists")
  if (.Platform$OS.type == "windows"){
    x[["home"]] <- sprintf('"%s"', path_tidy(x[["home"]]))
  } else {
    if (grepl("\\s+", x[["home"]])) x[["home"]] <- sprintf('"%s"', x[["home"]])
  }

  if (length(x[["info"]]) == 1L){
    if (.Platform$OS.type == "windows"){
      x[["info"]] <- sprintf('"%s"', path_tidy(x[["info"]]))
    } else {
      if (grepl("\\s+", x[["info"]])) x[["info"]] <- sprintf('"%s"', x[["info"]])
    }
  }
  
  c(
    "##",                                                                                                   
    sprintf("## registry entry for corpus %s", toupper(x[["id"]])),                                                                                
    "##",                                                                                                                  
    "",
    "# long descriptive name for the corpus",                                                                              
    sprintf("NAME \"%s\"", x[["name"]]),
    "# corpus ID (must be lowercase in registry!)",                                                                        
    sprintf("ID   %s", tolower(x[["id"]])),                                                                                                        
    "# path to binary data files",                                                                                         
    sprintf("HOME %s", x[["home"]]),
    if (length(x[["info"]]) == 1L)
      "# optional info file (displayed by \",info;\" command in CQP)"
    else
      NULL,
    if (length(x[["info"]]) == 1L) sprintf("INFO %s", x[["info"]]) else NULL,
    "",                                                                                                                 
    "# corpus properties provide additional information about the corpus:",                                                
    sprintf("##:: %s = \"%s\"", names(x[["properties"]]), unname(x[["properties"]])),
    "#========================================================================#",                                          
    "",
    "",
    "##",
    "## p-attributes (token annotations)",
    "##",
    "",
    paste0("ATTRIBUTE", " ", x[["p_attributes"]]),
    if (! is.null(x[["s_attributes"]])){
      c(
        "",
        "##",
        "## s-attributes",
        "##",
        "",
        if (length(x[["s_attributes"]]) > 0) paste0("STRUCTURE", " ", x[["s_attributes"]]) else character()
      )
    }
  )
}

#' @rdname registry_file
#' @export registry_data
registry_data <- function(name, id, home, info = fs::path(home, ".info"), properties = c(charset = "utf-8"), p_attributes, s_attributes = character()){
  y <- list(
    name = name, id = tolower(id), home = home, info = info,
    properties = properties, p_attributes = p_attributes, s_attributes = s_attributes
  )
  class(y) <- "registry_data"
  y
}


#' @details \code{registry_file_write} will compose a registry file from
#'   \code{data} and write it to disk.
#' @rdname registry_file
#' @export registry_file_write
registry_file_write <- function(data, corpus, registry_dir = Sys.getenv("CORPUS_REGISTRY"), ...){
  regfile <- registry_file_compose(x = data)
  writeLines(
    text = regfile,
    con = fs::path(registry_dir, tolower(corpus))
  )
  invisible(regfile)
}
