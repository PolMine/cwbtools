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
#'   \item{maintainer}{Maintainer, R package style.} 
#'   \item{description}{Short description of the data package.} 
#'   \item{license}{The license.}
#'   \item{verbose}{Logical, whether to be verbose.}
#' }
#' 
#' @section Methods:
#' \describe{
#'   \item{\code{initialize(dir)}}{Initialize a new object of class
#'   \code{DataPackage}.} \item{\code{createDirectoryStructure(verbose =
#'   TRUE)}}{Create the (sub)directories of the data package.} 
#'   \item{\code{addDescription(pkg = NULL, version = "0.0.1", date = NULL, 
#'   author = "", maintainer = "", description = "", license = "", verbose = 
#'   TRUE)}}{}
#'   \item{\code{addCorpus(corpus, registry = Sys.getenv("CORPUS_REGISTRY"),
#'   verbose = TRUE)}}{Add a corpus to the data package.} 
#'   \item{\code{addLicense(license = "CC-BY-NC-SA", file = system.file(package
#'   = "ctk", "txt", "licenses", "CC_BY-NC-SA_3.0.txt"))}}{Add license information to
#'   the DESCRIPTION file, and move file LICENSE to top level directory of the package.} 
#'   \item{\code{addConfigureScripts()}}{Add standardized and tested configure scripts \code{configure}
#'   for Linux and macOS, and \code{configure.win} for Windows to the top level directory of the 
#'   data package, and file \code{setpaths.R} to \code{tools} subdirectory. The configuration
#'   mechanism ensures that the data directory is specified correctly in the registry files
#'   during the installation of the data package.}
#'   \item{\code{addGitattributesFile()}}{Add file \code{.gitattributes} to toplevel directory
#'   of the data package. The file specifies file endings for Git LFS (large file storage), i.e. binary
#'   files that cannot be diffed.} 
#'   \item{\code{addRbuildignoreFile()}}{} \item{\code{addGitignoreFile()}}{Add
#'   .gitignore file to the package.}
#' }
#' @examples 
#' polmineR::use("polmineR")
#' 
#' y <- tempdir()
#' DP <- DataPackage$new(dir = y)
#' DP$createDirectoryStructure()
#' DP$addDescription()
#' DP$addCorpus("REUTERS")
#' DP$addGitattributesFile()
#' DP$addConfigureScripts()
#' DP$addLicense()
#' 
#' @rdname DataPackage
#' @export DataPackage
#' @importFrom polmineR RegistryFile
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
    
    createDirectoryStructure = function(verbose = TRUE){
      dirsToCreate <- c(
        "R",
        "man",
        "inst",
        file.path("inst", "extdata"),
        file.path("inst", "extdata", "cwb"),
        file.path("inst", "extdata", "cwb", "registry"),
        file.path("inst", "extdata", "cwb", "indexed_corpora")
      )
      for (dir in dirsToCreate) {
        newDir <- file.path(self$dir, dir)
        if (!file.exists(newDir)){
          if (verbose) message("... creating directory: ", newDir)
          dir.create(newDir)
        } 
      }
    },
    
    addDescription = function(pkg = NULL, version = "0.0.1", date = NULL, author = "", maintainer = "", description = "", license = "", verbose = TRUE){
      if (verbose) message("... creating DESCRIPTION file")
      if (is.null(pkg)){
        pkg <- basename(self$dir)
        if (verbose) message("... package name not provided, using extract from path: ", pkg)
      }
      description_list <- list(
        Package = pkg,
        Type = "Package",
        Title = pkg,
        Version = version,
        Date = if (is.null(date)) as.character(Sys.Date()) else date,
        Author = author,
        Maintainer = if (is.null(maintainer)) author else maintainer,
        Depends = "",
        LazyData = "yes",
        Description = description,
        License = license
      )
      description_char <- sapply(
        names(description_list),
        function(x) paste(x, description_list[[x]], sep = ": ")
      )
      writeLines(description_char, con = file.path(self$dir, "DESCRIPTION"))
    },
    
    addCorpus = function(corpus, registry = Sys.getenv("CORPUS_REGISTRY"), verbose = TRUE){
      # copy registry file
      file.copy(
        from = file.path(registry, tolower(corpus)),
        to = file.path(self$dir, "inst", "extdata", "cwb", "registry")
      )
      
      # copy files in dir for indexed corpora
      dataDir <- file.path(self$dir, "inst", "extdata", "cwb", "indexed_corpora")
      if (!file.exists(dataDir)) stop("data directory does not exist")
      targetDir <- file.path(dataDir, tolower(corpus))
      if (!file.exists(targetDir)){
        message("... directory for indexed corpus does not yet exist, creating: ", targetDir)
        dir.create(targetDir)
      }
      filesToCopy <- list.files(
        polmineR::RegistryFile$new(corpus, registry = registry)$getHome(),
        full.names = TRUE
        )
      dummy <- lapply(
        filesToCopy,
        function(x) {
          if (verbose) message("... file to copy: ", x)
          file.copy(from = x, to = targetDir)
        }
      )
    },
    
    addLicense = function(license = "CC-BY-NC-SA", file = system.file(package = "ctk", "txt", "licenses", "CC_BY-NC-SA_3.0.txt")){
      descriptionFile <- file.path(self$dir, "DESCRIPTION")
      if (!file.exists(descriptionFile)){
        stop("The DESCRIPTION file does exist - please generate it first using the addDescription method")
      }
      desc <- readLines(descriptionFile)
      licenseLine <- grep("^Licen[cs]e", desc)
      desc[licenseLine] <- paste("License:", license, "| file LICENSE", sep = " ")
      writeLines(text = desc, con = file.path(self$dir, "DESCRIPTION"))
      file.copy(from = file, to = file.path(self$dir, "LICENSE"))
      invisible(self)
    },
    
    addConfigureScripts = function(){
      toolDir <- file.path(self$dir, "tools")
      if (!file.exists(toolDir)) dir.create(toolDir)
      file.copy(
        from = system.file(package = "ctk", "Rscript", "setpaths.R"),
        to = file.path(toolDir, "setpaths.R")
      )
      writeLines(
        text = c('#!/bin/bash', '${R_HOME}/bin/Rscript ./tools/setpaths.R --args "$R_PACKAGE_DIR"'),
        con = file.path(self$dir, "configure")
      )
      writeLines(
        text = c('#!/bin/sh', '${R_HOME}/bin${R_ARCH_BIN}/Rscript.exe ./tools/setpaths.R --args "$R_PACKAGE_DIR"'),
        con = file.path(self$dir, "configure.win")
      )
    },
    
    addGitattributesFile = function(){
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
    
    addRbuildignoreFile = function(){
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
    
    addGitignoreFile = function(){
      content <- c("^.DS_Store$")
      writeLines(
        text = content,
        con = file.path(self$dir, ".gitignore")
      )
      invisible(self)
    }
  )
)