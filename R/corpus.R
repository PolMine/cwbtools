#' Install and Manage corpora. 
#' 
#' Utitlity functions for making the installation of indexed CWB corpora
#' wrapped into R data packages as easy as possible.
#' 
#' The function combines two steps necessary to install a CWB corpus wrapped into
#' a R data package. First, it calls \code{install.packages}, then it 
#' resets the path pointing to the directory with the indexed corpus
#' files in the registry file.
#' 
#' The corpus will be installed to the standard library directory for installing R packages
#' (\code{.libPaths()[1]}). Another location can be used by stating the param 'lib'
#' explicitly (see documentation for \code{\link{install.packages}}).
#' 
#' The function can also be used to install a corpus from a password protected repository. Further
#' parameters are handed over to install.packages, so you might add 
#' \code{method = "wget" extra = "--user donald --password duck"}.
#' 
#' See examples how to check whether the directory has been set correctly. 
#' 
#' An installed data package with a CWB corpus is assumed to include a directory
#' \code{/extdata/cwb/registry} for registry files and a directory
#' \code{/extdata/cwb/indexed_corpora} for the inexed corpus files.
#' 
#' @param old name of the (old) corpus
#' @param new name of the (new) corpus
#' @param pkgs names of data packages with corpora
#' @param repo URL of the repository 
#' @param lib directory for R packages, defaults to \code{.libPaths()[1]}; the path may not 
#' include a whitespace sign
#' @param verbose logical, whether to be verbose
#' @param registry_dir directory of registry
#' @param corpus a CWB corpus
#' @param ... further parameters that will be passed into \code{install.packages}
#' @name install.corpus
#' @seealso For managing registry files, see \code{\link{registry_file_parse}}
#' for switching to a packaged corpus. 
#' @examples
#' \donttest{
#' install.corpus("GermaParl")
#' # is equivalent to:
#' install.corpus("GermaParl", repo = "http://polmine.sowi.uni-due.de/packages")
#' RegistryFile(package = "GermaParl")$adjustHome()
#' # check the directory that has been set
#' RegistryFile$new(package = "GermaParl")$getHome()
#' }
#' @importFrom utils available.packages contrib.url install.packages
#' @importFrom utils installed.packages
#' @rdname corpus_utils
#' @export corpus_install
corpus_install <- function(pkgs, repo = "http://polmine.sowi.uni-due.de/packages", lib = .libPaths()[1], ...){
  for (package in pkgs){
    if (package %in% utils::available.packages(utils::contrib.url(repos = repo))){
      if (file.access(lib, "6") == -1){
        stop("You do not have write permissions for directory ", lib,
             ". Please run R with the required privileges, or provide another directory (param 'lib').")
      } else {
        install.packages(pkgs = package, repos = repo, lib = lib, ...)
        pkg_registry <- system.file(package = package, "extdata", "cwb", "registry")
        corpora <- list.files(pkg_registry)
        for (corpus in corpora){
          regdata <- registry_file_parse(corpus = corpus, registry_dir = pkg_registry)
          data_dir <- system.file(package = package, "extdata", "cwb", "indexed_corpora", corpus)
          if (regdata[["home"]] != data_dir){
            regdata[["home"]] <- data_dir
            registry_file_write(data = regdata, corpus = corpus, registry_dir = pkg_registry)
          }
        }
      }
    } else {
      stop("package ", package, " is not available")
    }
  }
}

#' @rdname corpus_utils
#' @export corpus_packages
corpus_packages <- function(){
  matrices <- lapply(
    .libPaths(),
    function(lib){
      vectors <- lapply(
        installed.packages(lib.loc = lib)[,"Package"],
        function(package){
          c(
            package = package,
            lib = lib,
            registry = system.file(package = package, "extdata", "cwb", "registry")
          )
        }
      )
      do.call(rbind, vectors)
    }
  )
  M <- data.table(do.call(rbind, matrices))
  M <- M[which(nchar(M[["registry"]]) > 0)]
  M
}

#' @rdname corpus_utils
#' @export corpus_copy
corpus_copy <- function(old, new, registry_dir = Sys.getenv("CORPUS_REGISTRY"), verbose = TRUE){
  stopifnot(tolower(old) %in% list.files(registry_dir))
  
  # copy data directory
  message("copying data directory")
  regdata <- registry_file_parse(corpus = old, registry_dir = registry_dir)
  data_dir_new <- file.path(dirname(regdata[["home"]]), tolower(new))
  if (file.exists(data_dir_new)){
    if (readline(prompt = "Data directory already exists. Proceed anyway? (type 'Y' to continue, anything else to abort)") != "Y")
      stop("Aborting the operation.")
  } else {
    dir.create(data_dir_new)
  }
  files_to_copy <- list.files(regdata[["home"]], full.names = TRUE)
  success <- pbapply::pblapply(
    files_to_copy,
    function(x) file.copy(from = x, to = data_dir_new, recursive = TRUE)
    )
  if (!all(unlist(success))){
    stop("copying the data directory failed")
  } else {
    message("... copying data directory succeeded")
  }
  
  # generate copy of registry file
  message("make copy of registry file")
  registry_file_new <- file.path(registry_file_new, tolower(new))
  if (file.exists(registry_file_new)){
    if (readline(prompt = "New registry file already exists. Proceed anyway? (type 'Y' to continue, anything else to abort)") != "Y"){
      file.remove(registry_file_new)
    } else {
      stop("Aborting the operation.")
    }
      
  }
  success <- file.copy(
    from = file.path(registry_dir, tolower(old)),
    to = file.path(registry_dir, tolower(new))
  )
  if (!success){
    stop("copying the registry file failed")
  } else {
    message("... copying registry file succeeded")
  }
  
  # modify the new registry file 
  message("updating new registry file")
  regdata <- registry_file_parse(corpus = new, registry_dir = registry_dir)
  regdata[["id"]] <- tolower(new)
  regdata[["home"]] <- data_dir_new
  registry_file_write(data = regdata, corpus = tolower(new), registry_dir = registry_dir)
  invisible(NULL)
}

#' @rdname corpus_utils
#' @export corpus_rename
corpus_rename <- function(old, new, registry_dir = Sys.getenv("CORPUS_REGISTRY"), verbose = TRUE){
  # check that old corpus exists
  stopifnot(tolower(old) %in% list.files(registry_dir))
  # check that new corpus does not yet exist
  if (tolower(new) %in% list.files(registry_dir)){
    stop("Corpus provided by 'new' already exists - do not overwrite an existing corpus")
  }
  
  # rename registry file
  message("renaming registry file")
  registry_old <- file.path(registry_dir, tolower(old))
  registry_new <- file.path(registry_dir, tolower(new))
  success <- file.rename(from = registry_old, to = registry_new)
  if (!success) stop("renaming the registry file failed")
  
  # rename data directory
  message("renaming data directory")
  regdata <- registry_file_parse(corpus = new, registry_dir = registry_dir)
  data_directory_old <- regdata[["home"]]
  data_directory_new <- file.path(dirname(data_directory_old), tolower(new))
  success <- file.rename(from = data_directory_old, to = data_directory_new)
  if (!success) stop("renaming the data directory failed")
  
  # modify and save registry file
  message("modifying and saving registry file")
  regdata[["home"]] <- data_directory_new
  regdata[["id"]] <- tolower(new)
  registry_file_write(data = regdata, corpus = tolower(new), registry_dir = registry_dir)
  invisible(NULL)
}

#' @rdname corpus_utils
#' @export corpus_remove
corpus_remove <- function(corpus, registry_dir = Sys.getenv("CORPUS_REGISTRY")){
  
  stopifnot(tolower(corpus) %in% list.files(registry_dir)) # check that corpus exists
  
  reg <- registry_file_parse(corpus = corpus, registry_dir = registry_dir)
  data_directory <- reg[["home"]]
  if (readline(prompt = sprintf("Are you sure you want to data files for corpus '%s'? ('Y' to continue, anything else to abort", corpus)) != "Y"){
    for (x in list.files(data_directory, full.names = TRUE)) file.remove(x)
    file.remove(data_directory)
  }
  if (readline(prompt = sprintf("Are you sure you want to relete the corpus '%s'? ('Y' to continue, anything else to abort)", corpus)) != "Y"){
    file.remove(file.path(registry_dir, tolower(x)))
  }
}

