#' Get Encoding of Character Vector.
#' 
#' @param x a character vector
#' @param verbose logical, whether to output messages
#' @export get_encoding
#' @rdname get_encoding
#' @name get_encoding
#' @importFrom utils localeToCharset
get_encoding <- function(x, verbose = FALSE){
  enc <- unique(Encoding(x))
  if (length(enc) == 1){
    if (enc == "unknown"){
      locale <- localeToCharset()[1]
      if (verbose) message(sprintf("... encoding of the input vector is 'unknown', assuming it to be '%s'", locale))
      return( locale )
    } else {
      if (verbose) message("... encoding of the input vector is: ", enc)
      return(enc)
    }
  } else if (length(enc) == 2){
    if ("unknown" %in% enc){
      enc <- enc[-which(enc == "unknown")]
      if (verbose) message("... encoding of the input vector is: ", enc)
      return( enc )
    } else {
      stop("please check encoding of the input character vector - more than one encoding found")
    }
  } else {
    stop("please check encoding of the input character vector - more than one encoding found")
  }
}



#' Guess corpus directory with data directories for corpora
#' 
#' Apply heuristic to make a plausible suggestion for a corpus directory where
#' data directories for corpora reside.
#' 
#' First, directories in the parent directory of the registry directory that
#' contain "corpus" or "corpora" are suggested. If this does not yield a result,
#' the home directories stated in the registry files are evaluated. If there is
#' one unique parent directory of data directories (after removing temporary
#' directories and directories within packages), this directory is suggested.
#' 
#' @param registry_dir Path to the directory with registry files. If missing, an
#'   internal heuristic will try to get the value of environment variable
#'   CORPUS_REGISTRY.
#' @return Length-one \code{character} vector with path of suggested data directory, or
#'   \code{NULL} if the data directory cannot be plausible guessed.
#' @export cwb_corpus_dir
#' @rdname directories
cwb_corpus_dir <- function(registry_dir){
  
  if (missing(registry_dir)){
  }
  
  if (is.null(registry_dir)) return(NULL)

  # scenario 1: Parent directory of registry directory has a corpus directory
  cwb_dir <- dirname(getOption("polmineR.corpus_registry"))
  candidate_id <- grep("corp(us|ora)", list.files(file.path(cwb_dir), full.names = FALSE))
  candidate <- grep("corp(us|ora)", list.files(file.path(cwb_dir), full.names = TRUE), value = TRUE)[candidate_id]
  candidate <- candidate[file.info(candidate)[["isdir"]]]
  
  if (length(candidate) == 0L){
    data_dirs <- unname(sapply(
      list.files(registry_dir),
      function(corpus) registry_file_parse(corpus = corpus, registry_dir = registry_dir)[["home"]]
    ))
    
    # exclude temporary directories
    data_dirs <- data_dirs[!unname(sapply(data_dirs, function(data_dir) grepl("^Rtmp.*$", basename(dirname(data_dir)))))]
    
    is_in_libdir <- sapply(
      data_dirs,
      function(data_dir) sapply(.libPaths(), function(libdir) grepl(sprintf("^%s.*?$", libdir), data_dir))
    )
    data_dirs <- data_dirs[!is_in_libdir]
    corpus_dir <- unique(sapply(data_dirs, dirname))
    if (length(corpus_dir) != 1L){
      warning("Cannot suggest a corpus directory based on data directories as stated in registry files.")
      corpus_dir <- NULL
    }
  
  } else if (length(candidate) >= 2L){
    warning("There are at least two candidates for corpus directory in the parent directory of the registry directory.")
    corpus_dir <- NULL
  } else if (length(candidate) == 1L){
    corpus_dir <- candidate
  }
  
  corpus_dir
}

#' @rdname directories
#' @export cwb_registry_dir
cwb_registry_dir <- function(){
  if (isFALSE(is.null(getOption("polmineR.corpus_registry")))){
    # If polmineR has been loaded, the CORPUS_REGISTRY environment variable will
    # have been reset, and the system registry is stored in the options
    return(getOption("polmineR.corpus_registry"))
  } else {
    if (nchar(Sys.getenv("CORPUS_REGISTRY"))){
      return(Sys.getenv("CORPUS_REGISTRY"))
    } else {
      stop(
        "Argument 'registry_dir' is not given.",
        "Cannot guess registry directory as environment variable CORPUS_REGISTRY is not set."
      )
    }
    
  }
  NULL
}


#' @param prefix The base path that will be prefixed 
#' @param create A \code{logical} value, whether directories shall be created.
#' @param ask A \code{logical} value, whether to prompt user before creating
#'   directories.
#' @rdname directories
#' @export cwb_directories
#' @importFrom utils menu
cwb_directories <- function(prefix = "~/cwb", create = FALSE, ask = interactive()){
  corpus_dir <- cwb_corpus_dir()
  registry_dir <- cwb_registry_dir()
  if (is.null(registry_dir) && create) registry_dir <- cwb_registry_dir_create(prefix = prefix, ask = ask)
  if (is.null(corpus_dir) && create) corpus_dir <- cwb_corpus_dir_create(prefix = prefix, ask = ask)
  c(registry_dir = registry_dir, corpus_dir = corpus_dir)
}


#' @export cwb_registry_dir_create
#' @rdname directories
cwb_registry_dir_create <- function(prefix = "~/cwb", ask = interactive()){
  prefix <- path.expand(prefix)
  registry_dir <- file.path(prefix, "registry")
  if (file.exists(registry_dir)){
    msg <- sprintf(
      "The registry directory %s exists, but is not defined by the environment variable CORPUS_REGISTRY.",
      registry_dir
    )
    warning(msg)
  } else {
    msg <- sprintf("No registry directory available. Create directory '%s'?", registry_dir)
    answer <- utils::menu(choices = c("Yes", "no"), title = msg)
    if (answer == 1L) dir.create(registry_dir) else stop("Aborting.")
    use_corpus_registry_envvar()
  }
  registry_dir
}


#' @export cwb_corpus_dir_create
#' @rdname directories
cwb_corpus_dir_create <- function(prefix = "~/cwb", ask = interactive()){
  prefix <- path.expand(prefix)
  corpus_dir <- file.path(prefix, "indexed_corpora")
  if (file.exists(corpus_dir)){
    warning(sprintf("The corpus directory '%s' already exists.", corpus_dir)
    )
  } else {
    msg <- sprintf("No registry directory available. Create directory '%s'?", corpus_dir)
    answer <- utils::menu(choices = c("Yes", "no"), title = msg)
    if (answer == 1L) dir.create(corpus_dir)
  }
  corpus_dir
}

#' Set CORPUS_REGISTRY environment variable
#' 
#' @param path Path of the registry directory.
#' @export use_corpus_registry_envvar
#' @importFrom usethis edit_r_environ ui_todo ui_code_block
use_corpus_registry_envvar <- function (path){
  ui_todo(
    "Include this line in file {ui_value('.Renviron')} to make \\\n    {ui_field(path)} available in all interactive R sessions."
  )
  ui_code_block(sprintf('\n    CORPUS_REGISTRY="%s"\n    ', path))
  edit_r_environ("user")
}
