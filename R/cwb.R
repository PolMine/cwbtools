#' Run cwb-make Command
#' 
#' @param corpus the CWB corpus
#' @param registry_dir the registry directory
#' @param verbose logical
#' @export cwb_make
cwb_make = function(corpus, registry_dir = Sys.getenv("CORPUS_REGISTRY"), verbose = TRUE){

  if (verbose) message("... running cwb-make")
  cwb_make_cmd <- paste0(c("cwb-make", "-V", toupper(corpus), "-r", registry_dir), collapse = " ")
  system(cwb_make_cmd)
  # polmineR::use(dir = registry_dir)
}