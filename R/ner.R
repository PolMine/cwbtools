#' Extract regions from NER annotations (CoNNL format).
#' 
#' @param x A \code{data.frame}, a \code{data.table}, or any other object that
#'   can be coerced to a \code{data.table}. The input table is expected to have
#'   the columns "token" and "ner", and "cpos".
#' @examples
#' x <- data.frame(
#'   token = c(
#'     "Die",
#'     "Bundeskanzlerin",
#'     "Angela",
#'     "Merkel",
#'     "spricht",
#'     "im",
#'     "Bundestag",
#'     "zur",
#'     "Lage",
#'     "der",
#'     "Nation",
#'     "."
#'   ),
#'   ne = c("O", "O", "B-PERS", "I-PERS", "O", "O", "B-ORG", "O", "O", "O", "O", "O"),
#'   stringsAsFactors = FALSE
#' )
#' x[["cpos"]] <- 100L:(100L + nrow(x) - 1L)
#' tab <- conll_get_regions(x)
#' @export conll_get_regions
conll_get_regions = function(x){
  # create a table with three coumns: "id", "start", "end"
  
  #### NEW
  
  dt <- as.data.table(x)
  anno_start <- grep("^B-.*$", dt[["ne"]])
  if (length(anno_start) == 0L) stop("Aborting - no named entity tags found.")
  anno_no <- as.integer(cut(1L:nrow(dt), right = FALSE, breaks = unique(c(anno_start, nrow(dt)))))
  anno_no <- ifelse(grepl("^(B-|I-).*$", dt[["ne"]]), anno_no, NA)
  dt[, "ne_no" := anno_no]
  annotations <- dt[!is.na(anno_no)][, {
    list(
      cpos_left = .SD[["cpos"]][1],
      cpos_right = .SD[["cpos"]][nrow(.SD)],
      ne_type = gsub("^(B|I)-(.*?)$", "\\2", .SD[["ne"]][1]),
      annotated = paste(.SD[["token"]], collapse = " ")
    )
    }, by = "ne_no"][, "ne_no" := NULL]

  # pos_list <- lapply(
  #   grep("B-", x[,"ne"]),
  #   function(start_position){
  #     start_tag <- x[start_position, "ne"]
  #     tag <- gsub("^B-", "", start_tag)
  #     follow_up_tag <- gsub("^B-", "I-", start_tag)
  #     end_position <- start_position + 1L 
  #     if (end_position > nrow(x)){
  #       # case: annotation starts with the last token
  #       return( data.table(tag = tag, row_start = start_position, row_end = start_position) )
  #     } else if (grepl(follow_up_tag, x[end_position, 2]) == FALSE){
  #       # case: only one token annotated
  #       return( data.table(tag = tag, row_start = start_position, row_end = start_position) )
  #     } else {
  #       while (TRUE){
  #         if (grepl(follow_up_tag, x[end_position, 2]) == FALSE){
  #           end_position <- end_position - 1L
  #           break
  #         } else {
  #           end_position <- end_position + 1L
  #         }
  #         if (end_position > nrow(x)){
  #           end_position <- nrow(x)
  #           break
  #         }
  #         
  #       }
  #       return( data.table(tag = tag, row_start = start_position, row_end = end_position) )
  #     }
  # })
  # y <- rbindlist(pos_list)
  # if ("cpos" %in% colnames(x)){
  #   y[, "cpos_left" := x[["cpos"]][y[["row_start"]]]]
  #   y[, "cpos_right" := x[["cpos"]][y[["row_end"]]]]
  #   y[, "row_start" := NULL]
  #   y[, "row_end" := NULL]
  #   setcolorder(y, neworder = c("cpos_left", "cpos_right", "tag"))
  #   
  # }
  annotations
}