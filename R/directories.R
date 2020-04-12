#' @title Manage directories for indexed corpora
#' 
#' @description The Corpus Workbench (CWB) stores the binary files for
#'   structural and positional attributes in an individual 'data directory'
#'   (referred to by argument \code{data_dir}) for each corpus. The data
#'   directories will typically be subdirectories of a parent directory called
#'   'corpus directory' (argument \code{corpus_dir}). Irrespective of the
#'   location of the data directories, all corpora available on a machine are
#'   described by so-called (plain text) registry files stored in a so-called
#'   'registry directory' (referred to by argument \code{registry_dir}).  The
#'   functionality to manage theses directories is used as auxiliary
#'   functionality by higher-level functionality to download and install
#'   corpora.
#' @details \code{cwb_corpus_dir} will make a plausible suggestion for a corpus
#'   directory where data directories for corpora reside. The procedure requires
#'   that  the registry directory (argument \code{registry_dir}) is known. If
#'   the argument \code{registry_dir} is missing, the registry directory will be
#'   guessed by calling \code{cwb_registry_dir}. The heuristic to detect the
#'   corpus directory is as follows: First, directories in the parent directory
#'   of the registry directory that contain "corpus" or "corpora" are suggested.
#'   If this does not yield a result, the data directories stated in the
#'   registry files are evaluated. If there is one unique parent directory of
#'   data directories (after removing temporary directories and directories
#'   within packages), this unique directory is suggested. \code{cwb_corpus_dir}
#'   will return a length-one \code{character} vector with the path of the
#'   suggested corpus directory, or \code{NULL} if the heuristic does not yield
#'   a result.
#' @param registry_dir Path to the directory with registry files.
#' @param corpus_dir Path to the directory with data directories for corpora.
#' @export cwb_corpus_dir
#' @rdname directories
cwb_corpus_dir <- function(registry_dir){
  
  if (missing(registry_dir)) registry_dir <- cwb_registry_dir()
  
  if (is.null(registry_dir)) return(NULL)

  cwb_dir <- dirname(registry_dir)
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

#' @details \code{cwb_registry_dir} will return return the system registry
#'   directory. By default, the environment variable CORPUS_REGISTRY defines the
#'   system registry directory. If the polmineR-package is loaded, a temporary
#'   registry directory is used, replacing the system registry directory. In
#'   this case, \code{cwb_registry_dir} will retrieve the directory from the
#'   option 'polmineR.corpus_registry'. The return value is a length-one
#'   character vector or \code{NULL}, if no registry directory can be detected.
#' @rdname directories
#' @export cwb_registry_dir
cwb_registry_dir <- function(){
  if (isFALSE(is.null(getOption("polmineR.corpus_registry")))){
    # If polmineR has been loaded, the CORPUS_REGISTRY environment variable will
    # have been reset, and the system registry is stored in the options
    if (nchar(getOption("polmineR.corpus_registry")) > 0L){
      return(getOption("polmineR.corpus_registry"))
    } else {
      return(NULL)
    }
  } else {
    if (nchar(Sys.getenv("CORPUS_REGISTRY")) > 0L){
      return(Sys.getenv("CORPUS_REGISTRY"))
    } else {
      return(NULL)
    }
  }
}


#' @details \code{cwb_directories} will return a named character vector with the
#'   registry directory and the corpus directory.
#' @param prefix The base path that will be prefixed 
#' @param ask A \code{logical} value, whether to prompt user before creating
#'   directories.
#' @rdname directories
#' @export cwb_directories
#' @importFrom utils menu
#' @importFrom rstudioapi getSourceEditorContext
cwb_directories <- function(registry_dir = NULL, corpus_dir = NULL){
  registry_dir <- if (is.null(registry_dir)) cwb_registry_dir() else registry_dir
  c(
    registry_dir = registry_dir, 
    corpus_dir = if (is.null(corpus_dir)) cwb_corpus_dir(registry_dir) else corpus_dir
  )
}

#' @details \code{create_cwb_directories} will create a 'registry' and an
#'   'indexed_corpora' directory as subdirectories of the directory indicated by
#'   argument \code{prefix}. Argument \code{ask} indicates whether to create
#'   directories, and whether user feedback is asked for before creating the
#'   directories. The function returns a named character vector with the
#'   registry and the corpus directory.
#' @rdname directories
#' @export create_cwb_directories
create_cwb_directories <- function(prefix = "~/cwb", ask = interactive()){
  
  prefix <- path.expand(prefix)
  if (dir.exists(prefix)){
    message(
      sprintf("Using directory '%s' as parent directory for registry directory and the corpus directory. ", prefix),
      "(Directory already exists.)"
    )
  } else {
    if (ask){
      msg <- sprintf("Parent directory '%s' for registry directory and corpus directory is not yet available. Create it?", prefix)
      answer <- utils::menu(choices = c("Yes / continue", "No / abort"), title = msg)
      if (answer == 1L) dir.create(prefix, recursive = TRUE) else stop("Aborting.")
    } else {
      dir.create(prefix, recursive = TRUE)
    }
    if (dir.exists(prefix)){
      message(sprintf("... parent directory '%s' for registry directory and corpus directory has been created successfully.", prefix))
    } else {
      warning(sprintf("Parent directory '%s' for registry directory and corpus directory not found / created!", prefix))
    }
  }
  
  registry_dir <- file.path(prefix, "registry")
  if (file.exists(registry_dir)){
    msg <- sprintf(
      "The registry directory %s exists, but is not defined by the environment variable CORPUS_REGISTRY.",
      registry_dir
    )
    warning(msg)
  } else {
    if (ask){
      msg <- sprintf("Registry directory '%s' does not yet exist. Create it?", registry_dir)
      answer <- utils::menu(choices = c("Yes", "No"), title = msg)
      if (answer == 1L) dir.create(registry_dir) else stop("Aborting.")
      renviron_file <- use_corpus_registry_envvar(registry_dir = registry_dir)
      if (grepl("\\.Renviron$", rstudioapi::getSourceEditorContext()[["path"]])){
        message(
          "The '.Renviron' file is still open. Insert definition of CORPUS_REGISTRY environment variable",
          "as a line into the document, save and close the document to make the registry directory available accross sessions."
          )
        readline(prompt = "Hit any key to proceed.")
      }
      if (any(grepl("^CORPUS_REGISTRY\\s*=\\s*.*?$", readLines(renviron_file)))){
        message("Environment variable CORPUS_REGISTRY has been added successfully to .Renviron file.")
      } else {
        message(
          "Environment variable CORPUS_REGISTRY not defined in .Renviron file. You will have to set the ",
          sprintf('environmant variable by calling Sys.setenv("%s") to make corpora available.', registry_dir)
        )
      }
    } else {
      dir.create(registry_dir)
    }
    if (dir.exists(registry_dir)){
      message(sprintf("... registry directory '%s' has been created successfully.", registry_dir))
    } else {
      warning(sprintf("Designated registry directory '%s' not found / created!", registry_dir))
    }
    message("... environment variable CORPUS_REGISTRY set as: ", registry_dir)
    Sys.setenv(CORPUS_REGISTRY = registry_dir)
  }
  
  # Create corpus_dir --------------
  
  corpus_dir <- file.path(prefix, "indexed_corpora")
  if (file.exists(corpus_dir)){
    warning(sprintf("The corpus directory '%s' already exists.", corpus_dir)
    )
  } else {
    if (ask){
      msg <- sprintf("No corpus directory available. Create directory '%s'?", corpus_dir)
      answer <- utils::menu(choices = c("Yes", "No"), title = msg)
      if (answer == 1L) dir.create(corpus_dir, recursive = TRUE) else stop("Aborting.")
    } else {
      dir.create(corpus_dir)
    }
    if (dir.exists(corpus_dir)){
      message(sprintf("... corpus directory '%s' has been created successfully.", corpus_dir))
    } else {
      warning(sprintf("Designated corpus directory '%s' not found / created!", corpus_dir))
    }
    
    
  }
  c(registry_dir = registry_dir, corpus_dir = corpus_dir)
}





#' @details \code{use_corpus_registry_envvar} is an convenience function that
#'   will assist users to define the environment variable CORPUS_REGSITRY in the
#'   .Renviron-file, so that it will be available accross sessions.
#' @export use_corpus_registry_envvar
#' @importFrom usethis edit_r_environ ui_todo ui_code_block ui_value ui_field
#' @rdname directories
use_corpus_registry_envvar <- function (registry_dir){
  ui_todo(
    "Include this line in file {ui_value('.Renviron')} to make \\\n    {ui_field(registry_dir)} available in all interactive R sessions."
  )
  ui_code_block(sprintf('\n    CORPUS_REGISTRY="%s"\n    ', registry_dir))
  edit_r_environ("user")
}
