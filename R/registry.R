#' Generate and Write Registry File.
#' 
#' Generate a registry file describing a CWB indexed corpus.
#' \code{registry_file_make} will create a character vector with the content of
#' the registry file. \code{registry_file_write} will write also write this 
#' content as a registry file to directors \code{registry_dir}.
#'
#' A CWB indexed corpus needs to be described in a registry file. See the CWB
#' Corpus Encoding Tutorial, Appendix A (pp. 21f) for an example registry file
#' (\url{http://cwb.sourceforge.net/files/CWB_Encoding_Tutorial.pdf}).
#' 
#' In addition to the corpus properties explicitly declared, the corpus property
#' "drive_letter" will be added on Windows machines to handle the case that the 
#' data directory may be on another drive than the drive of the current working 
#' directory.
#' 
#' @param registry_dir the registry directory
#' @param corpus name of the corpus, will be used as ID (using tolower)
#' @param description long descriptive name for the corpus
#' @param data_dir path to binary data files
#' @param info_file optional info file, if NULL, we assume that file ".info.md" is in \code{data_dir}
#' @param corpus_properties named vector of corpus properties; properties
#'   \code{language} and \code{charset} need to be present
#' @param p_attributes positional attributes to be declared
#' @param s_attributes structural attributes to be declared
#' @param ... parameters that are passed into \code{registry_file_write}
#' @return the character vector of the registry file is returned invisibly
#' @rdname registry_file
#' @name registry file
#' @examples 
#' dir.create(temp_regdir <- tempfile())
#' temp_data_dir <- tempfile()
#' if (.Platform$OS.type == "windows"){
#'   temp_data_dir <- normalizePath(temp_data_dir, winslash = "/")
#' }
#' 
#' reuters_regfile <- registry_file_make(
#'   corpus = "REUTERS", description = "Reuters Example Corpus",
#'   data_dir = temp_data_dir, info_file = NULL,
#'   corpus_properties = c(language = "en", charset = "latin-1"),
#'   p_attributes = "word", s_attributes = c("places", "id")
#'   )
#' registry_file_write(
#'   registry_dir = temp_regdir, corpus = "REUTERS", description = "Reuters Example Corpus",
#'   data_dir = temp_data_dir, info_file = NULL,
#'   corpus_properties = c(language = "en", charset = "latin-1"),
#'   p_attributes = "word", s_attributes = c("places", "id")
#'   )
#' regfile <- readLines(file.path(temp_regdir, "reuters"))
NULL

#' @rdname registry_file
#' @export registry_file_write
registry_file_make <- function(
  registry_dir, corpus, description = "", data_dir, info_file = NULL,
  corpus_properties = c(language = "en", charset = "latin-1"),
  p_attributes = "word", s_attributes = NULL){
  
  data_dir_in <- data_dir
  if (.Platform$OS.type == "windows"){
    data_dir <- sprintf('"%s"', normalizePath(data_dir, winslash = "/"))
  } else {
    if (grepl("\\s", data_dir)) data_dir <- sprintf('"%s"', data_dir)
  }
  
  
  if (is.null(info_file)){
    info_file <- file.path(dirname(data_dir_in), ".info.md")
  }
  if (.Platform$OS.type == "windows"){
    info_file <- sprintf('"%s"', normalizePath(info_file, winslash = "/"))
  } else {
    if (grepl("\\s", info_file)) info_file <- sprintf('"%s"', info_file)
  }

  c(
    "##",                                                                                                   
    sprintf("## registry entry for corpus %s", toupper(corpus)),                                                                                
    "##",                                                                                                                  
    "",
    "# long descriptive name for the corpus",                                                                              
    sprintf("NAME \"%s\"", description),
    "# corpus ID (must be lowercase in registry!)",                                                                        
    sprintf("ID   %s", tolower(corpus)),                                                                                                        
    "# path to binary data files",                                                                                         
    sprintf("HOME %s", data_dir),
    "# optional info file (displayed by \",info;\" command in CQP)",                                                       
    sprintf("INFO %s", info_file),
    "",                                                                                                                 
    "# corpus properties provide additional information about the corpus:",                                                
    sprintf("##:: %s = \"%s\"", names(corpus_properties), unname(corpus_properties)),
    "#========================================================================#",                                          
    "",
    "",
    "##",
    "## p-attributes (token annotations)",
    "##",
    "",
    paste0("ATTRIBUTE", " ", p_attributes),
    if (! is.null(s_attributes)){
      c(
        "",
        "##",
        "## s-attributes",
        "##",
        "",
        paste0("STRUCTURE", " ", s_attributes)
      )
    }
  )
}

#' @rdname registry_file
#' @export registry_file_write
registry_file_write <- function(registry_dir, corpus, ...){
  regfile <- registry_file_make(corpus = corpus, ...)
  writeLines(
    text = regfile,
    con = file.path(registry_dir, tolower(corpus))
  )
  invisible(regfile)
}
