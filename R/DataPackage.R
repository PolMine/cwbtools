#' Class to manage packages with corpus data.
#' 
#' Putting CWB indexed corpora into R data packages is a convenient way to ship
#' and share corpora, and to keep documentation and supplementary functionality 
#' with the data. The \code{DataPackage}-class offers a set of methods to
#' perform standard tasks in a standardized fashion.
#' 
#' @field dir directory
#' 
#' @section Arguments:
#' \describe{
#'   \item{dir}{The directory of the data package.}
#'   \item{corpus}{A corpus described in a registry file in the CORPUS_REGISTRY directory.}
#'   \item{author}{The author of the package.}
#'   \item{pkg}{The package name (may not include special chars, no _).}
#'   \item{version}{The version number of the corpus.}
#'   \item{date}{The date of creation.}
#'   \item{maintainer}{Maintainer, R package style, either \code{character} vector or \code{person}.} 
#'   \item{description}{Short description of the data package.} 
#'   \item{license}{The license.}
#'   \item{verbose}{Logical, whether to be verbose.}
#' }
#' 
#' @section Methods:
#' \describe{
#'   \item{\code{initialize(dir)}}{Initialize a new object of class
#'   \code{DataPackage}.}
#'   \item{\code{create_directory_structure(verbose = TRUE)}}{Create the
#'   (sub)directories of the data package.}
#'   \item{\code{add_description(pkg = NULL, version = "0.0.1", date = NULL, 
#'   author = "", maintainer = "", description = "", license = "", verbose = 
#'   TRUE)}}{}
#'   \item{\code{add_corpus(corpus, registry = Sys.getenv("CORPUS_REGISTRY"),
#'   verbose = TRUE)}}{Add a corpus to the data package.} 
#'   \item{\code{add_license(license = "CC-BY-NC-SA", file = system.file(package
#'   = "ctk", "txt", "licenses", "CC_BY-NC-SA_3.0.txt"))}}{Add license information to
#'   the DESCRIPTION file, and move file LICENSE to top level directory of the package.} 
#'   \item{\code{add_configure_scripts()}}{Add standardized and tested configure
#'   scripts \code{configure} for Linux and macOS, and \code{configure.win} for
#'   Windows to the top level directory of the data package, and file
#'   \code{setpaths.R} to \code{tools} subdirectory. The configuration mechanism
#'   ensures that the data directory is specified correctly in the registry
#'   files during the installation of the data package.}
#'   \item{\code{add_gitattributes_file()}}{Add file \code{.gitattributes} to
#'   toplevel directory of the data package. The file specifies file endings for
#'   Git LFS (large file storage), i.e. binary files that cannot be diffed.}
#'   \item{\code{add_RbuildignoreFile()}}{} \item{\code{addGitignoreFile()}}{Add
#'   .gitignore file to the package.}
#' }
#' @examples 
#' y <- tempdir()
#' DP <- DataPackage$new(dir = y)
#' DP$create_directory_structure()
#' DP$add_description_file(pkg = "reuters", description = "Reuters data package")
#' DP$add_corpus("REUTERS", registry = system.file(package = "RcppCWB", "extdata", "cwb", "registry"))
#' DP$add_gitattributes_file()
#' DP$add_configure_scripts()
#' DP$add_license()
#' 
#' # exactly the same, with method chaining
#' y <- tempdir()
#' DP <- DataPackage$new(dir = y)$
#'   create_directory_structure()$
#'   add_description(pkg = "reuters", description = "Reuters data package")$
#'   add_corpus("REUTERS", registry = system.file(package = "RcppCWB", "extdata", "cwb", "registry"))$
#'   add_gitattributes_file()$
#'   add_configure_scripts()$
#'   add_license()
#' @rdname DataPackage
#' @export DataPackage
#' @importFrom R6 R6Class
DataPackage <- R6Class(
  
  "DataPackage",
  
  public = list(
    
    dir = NULL,
    
    initialize = function(dir){
      if (file.exists(dir) != TRUE) stop("directory does not exist")
      if (file.info(dir)$isdir != TRUE) stop("dir exists, but is not a directory")
      self$dir <- dir
      invisible(self)
    },
    
    create_directory_structure = function(verbose = TRUE){
      dirs_to_create <- c(
        "R",
        "man",
        "inst",
        file.path("inst", "extdata"),
        file.path("inst", "extdata", "cwb"),
        file.path("inst", "extdata", "cwb", "registry"),
        file.path("inst", "extdata", "cwb", "indexed_corpora")
      )
      for (dir in dirs_to_create) {
        new_dir <- file.path(self$dir, dir)
        if (!file.exists(new_dir)){
          if (verbose) message("... creating directory: ", new_dir)
          dir.create(new_dir)
        } else {
          if (verbose) message("... directory already exists:", new_dir)
        }
      }
      invisible(self)
    },
    
    add_description_file = function(pkg = NULL, version = "0.0.1", date = Sys.Date(), author = "", maintainer = NULL, description = "", license = "", verbose = TRUE){
      if (file.exists(file.path(self$dir, "DESCRIPTION"))){
        user_input <- readline(prompt = "DESCRIPTION file already exists. Proceed anyway (Y / any other key to abort)?")
        if (user_input != "Y"){
          message("file DESCRIPTION is not generated anew")
          return( invisible(self) )
        }
      }
      
      # sanity checks, to be extended
      stopifnot(
        class(author)[1] %in% c("character", "person"),
        length(date) == 1,
        is.character(date) || class(date)[1] 
      )
      
      if (verbose) message("... creating DESCRIPTION file")
      if (is.null(pkg)){
        pkg <- basename(self$dir)
        if (verbose) message("... package name not provided, using basename: ", pkg)
      }
      description_list <- c(
        Package = pkg,
        Type = "Package",
        Title = pkg,
        Version = version,
        Date = as.character(date),
        Author = paste0(as.character(author), collapse = "\n\t"),
        Maintainer = if (is.null(maintainer)) paste0(as.character(author), collapse = "\n\t") else paste0(as.character(maintainer), collapse = "\n\t"),
        Depends = "",
        LazyData = "yes",
        Description = description,
        License = license
      )
      desc <- paste0(names(description_list), ": ", unname(description_list))
      writeLines(desc, con = file.path(self$dir, "DESCRIPTION"))
      invisible(self)
    },
    
    add_corpus = function(corpus, registry = Sys.getenv("CORPUS_REGISTRY"), verbose = TRUE){
      # copy registry file
      file.copy(
        from = file.path(registry, tolower(corpus)),
        to = file.path(self$dir, "inst", "extdata", "cwb", "registry")
      )
      
      # copy files in dir for indexed corpora
      data_dir <- file.path(self$dir, "inst", "extdata", "cwb", "indexed_corpora")
      if (!file.exists(data_dir)) stop("data directory does not exist")
      target_dir <- file.path(data_dir, tolower(corpus))
      if (!file.exists(target_dir)){
        message("... directory for indexed corpus does not yet exist, creating: ", target_dir)
        dir.create(target_dir)
      }
      files_to_copy <- list.files(
        registry_file_parse(corpus, registry = registry)[["home"]],
        full.names = TRUE
        )
      for (x in files_to_copy){
        if (verbose) message("... copying file: ", x)
        file.copy(from = x, to = target_dir)
      }
      invisible(self)
    },
    
    add_license = function(license = "CC-BY-NC-SA", file = system.file(package = "cwbtools", "txt", "licenses", "CC_BY-NC-SA_3.0.txt")){
      description_file <- file.path(self$dir, "DESCRIPTION")
      if (!file.exists(description_file)){
        stop("The DESCRIPTION file does exist - please generate it first using the add_description method")
      }
      desc <- readLines(description_file)
      desc[grep("^Licen[cs]e", desc)] <- paste("License:", license, "| file LICENSE", sep = " ")
      writeLines(text = desc, con = file.path(self$dir, "DESCRIPTION"))
      file.copy(from = file, to = file.path(self$dir, "LICENSE"))
      invisible(self)
    },
    
    add_configure_scripts = function(){
      tool_dir <- file.path(self$dir, "tools")
      if (!file.exists(tool_dir)) dir.create(tool_dir)
      file.copy(
        from = system.file(package = "cwbtools", "Rscript", "setpaths.R"),
        to = file.path(tool_dir, "setpaths.R")
      )
      writeLines(
        text = c('#!/bin/bash', '${R_HOME}/bin/Rscript ./tools/setpaths.R --args "$R_PACKAGE_DIR"'),
        con = file.path(self$dir, "configure")
      )
      writeLines(
        text = c('#!/bin/sh', '${R_HOME}/bin${R_ARCH_BIN}/Rscript.exe ./tools/setpaths.R --args "$R_PACKAGE_DIR"'),
        con = file.path(self$dir, "configure.win")
      )
      invisible(self)
    },
    
    add_gitattributes_file = function(){
      extensions <- c(
        "RData", "cnt", "crc", "crx", "hcd", "huf", "syn",
        "lexicon", "idx", "srt", "rng", "avs", "avx"
      )
      writeLines(
        text = sprintf("*.%s filter=lfs diff=lfs merge=lfs -text", extensions),
        con = file.path(self$dir, ".gitattributes")
      )
      invisible(self)
    },
    
    add_Rbuildignore_file = function(){
      content <- c(
        "^data-raw$",
        "^data-raw/*$",
        "^_pkgdown\\.yml$"
      )
      writeLines(
        text = content,
        con = file.path(self$dir, ".Rbuildignore")
      )
      invisible(self)
    },
    
    add_gitignore_file = function(){
      content <- c("^.DS_Store$")
      writeLines(
        text = content,
        con = file.path(self$dir, ".gitignore")
      )
      invisible(self)
    }
  )
)