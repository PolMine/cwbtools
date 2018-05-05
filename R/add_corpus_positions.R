#' Add Corpus Positions to Metadata Table.
#'
#' Add columns cpos_left and cpos_right to table with structural attributes. A
#' precondition is that a column 'id' is present in tables 'tokenstream' and 'metadata'."
#' 
#' @param x a \code{corpusdata} object
#' @param verbose logical, whether to be talkative
#' @export add_corpus_positions
add_corpus_positions <- function(x, verbose = TRUE){

  if (!"id" %in% colnames(x[["metadata"]])) stop("id column required")
  x[["tokenstream"]][["cpos"]] <- 0L:(nrow(x[["tokenstream"]]) - 1L)
  
  if (verbose) message("... adding corpus positions to table 'metadata'")
  grpn <- uniqueN(x[["tokenstream"]][["id"]])
  if (interactive()) pb <- txtProgressBar(min = 0, max = grpn, style = 3)
  cpos_dt <- x[["tokenstream"]][
    ,{
      if (interactive()) setTxtProgressBar(pb, .GRP);
      list(cpos_left = min(.SD[["cpos"]]), cpos_right = max(.SD[["cpos"]]))
    }, by = "id"
    ]
  if (interactive()) close(pb)
  
  setkeyv(cpos_dt, cols = "id")
  setkeyv(x[["metadata"]], cols = "id")
  x[["metadata"]] <- x[["metadata"]][cpos_dt]
  
  x
}
