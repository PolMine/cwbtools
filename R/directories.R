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
cwb_directories <- function(registry_dir = NULL, corpus_dir = NULL){
  registry_dir <- if (is.null(registry_dir)) cwb_registry_dir() else registry_dir
  c(
    registry_dir = registry_dir, 
    corpus_dir = if (is.null(corpus_dir)) cwb_corpus_dir(registry_dir) else corpus_dir
  )
}

#' @param verbose A \code{logical} value, whether to output status messages.
#' @details \code{create_cwb_directories} will create a 'registry' and an
#'   'indexed_corpora' directory as subdirectories of the directory indicated by
#'   argument \code{prefix}. Argument \code{ask} indicates whether to create
#'   directories, and whether user feedback is asked for before creating the
#'   directories. The function returns a named character vector with the
#'   registry and the corpus directory.
#' @rdname directories
#' @export create_cwb_directories
#' @importFrom cli cat_rule
create_cwb_directories <- function(prefix = "~/cwb", ask = interactive(), verbose = TRUE){
  
  prefix <- path.expand(prefix)

  if (dir.exists(prefix)){
    if (file.access(prefix, mode = 2) != 0L){
      stop(sprintf("no write permissions for CWB data directory %s", prefix))
    }
    if (verbose){
      cli_rule("Create CWB directories")
      cli_alert_info(
        sprintf("Using existing directory {.path %s} as parent directory for registry directory and the corpus directory.", prefix),
        wrap = TRUE
      )
    }
  } else {
    if (file.access(dirname(prefix), mode = 2) != 0L){
      stop(sprintf("cannot create directory %s - no write permissions for parent directory %s", prefix, dirname(prefix)))
    }
    cli_rule("Create CWB directories")
    if (ask){
      answer <- menu(
        title = cli_text(sprintf("Create directory {.path %s} as parent directory for the registry directory and the corpus directory?", prefix)),
        choices = c("Yes!", "I want to choose another directory.", "Cancel (user abort).")
      )
      if (answer == 1L){
        dir.create(prefix, recursive = TRUE)
      } else if (answer == 2L){
        if (interactive()){
          if (rstudioapi::isAvailable()){
            prefix <- rstudioapi::selectDirectory()
          } else {
            again <- TRUE
            while (again){
              prefix <- readline(prompt = "Please enter valid path (hit ENTER without input to abort): ")
              if (nchar(prefix) == 0L) stop("user abort")
              if (file.exists(dirname(prefix))) again <- FALSE
            }
            dir.create(prefix)
          }
        } else {
          stop("not in interactive sessions - user cannot choose another directory.")
        }
      } else if (answer == 3L){
        stop("user abort")
      }
    } else {
      dir.create(prefix, recursive = TRUE)
    }
    if (dir.exists(prefix)){
      if (verbose) cli_alert_success(sprintf("parent directory {.path %s} for registry directory and corpus directory has been created", prefix))
    } else {
      if (verbose) cli_alert_danger(sprintf("parent directory {.path %s} for registry directory and corpus directory not found / created!", prefix))
    }
  }
  
  cwb_dirs <- c(
    registry_dir = file.path(prefix, "registry"),
    corpus_dir = file.path(prefix, "indexed_corpora")
  )

  for (dirtype in names(cwb_dirs)){
    what <- gsub("^(.*?)_dir$", "\\1", dirtype)
    if (file.exists(cwb_dirs[[dirtype]])){
      if (verbose) cli_alert_info(sprintf("%s directory {.path %s} already exists", what, cwb_dirs[[dirtype]]))
    } else {
      dir.create(cwb_dirs[[dirtype]])
      if (dir.exists(cwb_dirs[[dirtype]])){
        if (verbose) cli_alert_success(sprintf("%s directory {.path %s} has been created", what, cwb_dirs[[dirtype]]))
      } else {
        if (verbose) cli_alert_warning(sprintf("%s directory {.path %s} has not been created", what, cwb_dirs[[dirtype]]))
      }
    }
  }
  
  Sys.setenv(CORPUS_REGISTRY = cwb_dirs[["registry_dir"]])
  if (verbose){
    cli_alert_success(
      sprintf("environment variable {.envvar CORPUS_REGISTRY} set as: {.path %s}", cwb_dirs[["registry_dir"]])
    )
  }  
  
  cwb_dirs
}



#' @details \code{use_corpus_registry_envvar} is a convenience function that
#'   will assist users to define the environment variable CORPUS_REGSITRY in the
#'   .Renviron-file.  making it available across sessions. The function is
#'   intended to be used in an interactive R session. An error is thrown if this
#'   is not the case. The user will be prompted whether the cwbtools package
#'   shall take care of creating / modifying the .Renviron-file. If not,
#'   the file will be opened for manual modification with some instructions shown
#'   in the terminal.
#' @export use_corpus_registry_envvar
#' @rdname directories
use_corpus_registry_envvar <- function(registry_dir){
  
  if (isFALSE(interactive())){
    stop(
      "Defining the environment variable CORPUS_REGISTRY in the .Renviron file ",
      "is possible only when R is being used interactively."
    )
  }
  
  if (!dir.exists(registry_dir)){
    stop("directory provided by argument registry_dir does not exist.")
  }
  
  cli_rule("Define CORPUS_REGISTRY environmment variable in .Renviron file")
  user_input <- menu(
    choices = c("Yes", "No"),
    title = cli_text(
      "I want the {.pkg cwbtools} package to set the {.envvar CORPUS_REGISTRY} environment variable ", 
      "in the {.file .Renviron} file (see {.code ?Startup} for details}."
    )
  )
  
  if (user_input == 1L){
    
    if (nchar(Sys.getenv("R_ENVIRON_USER")) > 0L){
      renviron_file <- Sys.getenv("R_ENVIRON_USER")
      cli_alert_info(sprintf("using {.path .Renviron}-file defined in the environment variable {.envvar R_ENVIRON_USER}: {.path %s}", renviron_file))
    } else{
      renviron_file <- path.expand("~/.Renviron")
      cli_alert_info(sprintf("using default {.path .Renviron}-file: {.path %s}", renviron_file))
    }
    
    if (file.exists(renviron_file)){
      
      if (file.access(renviron_file, mode = 2) == 0L){
        cli_alert_info("the existing {.path .Renviron}-file exists and will be modified")
      } else {
        cli_alert_danger("you do not have write permissions for the {.path .Renviron} file - aborting")
        return(FALSE)
      }
      
      if (any(grepl("^\\s*CORPUS_REGISTRY\\s*=", readLines(renviron_file)))){
        cli_alert_danger(
          paste(
            "the environment variable {.envvar CORPUS_REGISTRY} is already defined in the existing",
            sprintf("{.path .Renviron} file ({.path %s}).", renviron_file),
            "The file will not be changed. Please modify the file manually."
          )
        )
        return(FALSE)
      }
      
      cat(sprintf('CORPUS_REGISTRY="%s"', registry_dir), file = renviron_file, append = TRUE)
      
    } else {
      cli_alert_info("the {.path .Renviron}-file does not yet exist and needs to be created")
      renviron_dirname <- dirname(renviron_file)
      if (file.access(renviron_dirname) != 0L){
        cli_alert_danger(sprintf("you do not have write permissions for the directory {.path %s}  - aborting", renviron_dirname))
      }
      writeLines(text = sprintf('CORPUS_REGISTRY="%s"', registry_dir), con = renviron_file)
    }
    
    if (any(grepl("^\\s*CORPUS_REGISTRY\\s*=", readLines(renviron_file)))){
      cli_alert_success(
        sprintf(
          "the environment  variable {.envvar CORPUS_REGISTRY} is now defined as {.path %s} in the {.file .Renviron}-file ({.path %s})",
          registry_dir, renviron_file
        )
      )
      return(TRUE)
    } else {
      cli_alert_warning(
        paste(
          "the definition of the {.envvar CORPUS_REGISTRY} has been written to the {.path .Renviron}-file,",
          "but cannot determine the success of the operation.",
          sprintf("Please check file {.path %s} manually ", renviron_file)
        )
      )
      return(FALSE)
    }

  } else if (user_input == 2L){
    if (!requireNamespace("usethis", quietly = TRUE)){
      stop("The package 'usethis' is required, but not available to modify the .Renviron file manually. ",
           'Install it by calling install.packages("usethis").'
      )
    }
    usethis::ui_todo(
      "Include the following line in file {usethis::ui_value('.Renviron')} to make \\\n    {usethis::ui_field(registry_dir)} available as environment variable CORPUS_REGISTRY in all interactive R sessions:"
    )
    usethis::ui_code_block(sprintf('\n    CORPUS_REGISTRY="%s"\n    ', registry_dir))
    usethis::edit_r_environ("user")
  }
}
