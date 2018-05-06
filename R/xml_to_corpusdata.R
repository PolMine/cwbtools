#' Parse an XML/TEI file as corpusdata.
#' 
#' The procedure is inspired by the tidytext paradigm, but
#' uses a data.table for efficiency.
#' 
#' @param x a single filename, a character vector of filenames, or a directory with XML files
#' @param body an xpath expression defining the body of the xml document
#' @param meta a named character vector with xpath expressions
#' @param mc a numeric/integer value, number of cores to use
#' @export xml_to_corpusdata
#' @importFrom xml2 read_xml xml_attrs xml_find_all xml_find_first xml_name xml_parents xml_text
#' @importFrom data.table as.data.table
#' @importFrom pbapply pblapply
#' @importFrom stats setNames
#' @importFrom data.table data.table
xml_to_corpusdata <- function(x, body = "//body", omit, meta = NULL, mc = NULL){
  if (length(x) > 1){
    if (!all(file.exists(x))) stop("all files provided by x need to exist (not fulfilled)")
    data <- pbapply::pblapply(x, function(x) xml_to_corpusdata(x, body = body, meta = meta, mc = 1))
    text <- rbindlist(lapply(data, function(x) x[["text"]]))
    text[["id"]] <- 1L:nrow(text)
    metadata <- rbindlist(lapply(data, function(x) x[["metadata"]]), fill = TRUE)
    metadata[["id"]] <- 1L:nrow(metadata)
    return( list(text = text, metadata = metadata) )
  } else if (length(x) == 1){
    if (!file.exists(x)) stop("filename/directory x does not exist")
    if (file.info(x)[["isdir"]]){
      filenames <- list.files(x, full.names = TRUE)
      if (length(filenames) == 0) warning("directory x is empty")
      dt <- xml_to_corpusdata(x = filenames, body = body, meta = meta, mc = mc)
      return(dt)
    } else {
      doc <- xml2::read_xml(x)
      textnodes <- xml2::xml_find_all(doc, xpath = sprintf("%s//text()", body))
      .get_parents_attributes <- function(textnode){
        meta <- lapply(
          xml2::xml_parents(textnode),
          function(ancestor){
            sattrs <- xml2::xml_attrs(ancestor)
            if (length(sattrs) > 0){
              names(sattrs) <- paste(xml2::xml_name(ancestor), names(sattrs), sep = "_")
              return( sattrs )
            } else {
              return( setNames(TRUE, xml2::xml_name(ancestor)) )
            }
            
          }
        )
        data <- as.list(unlist(meta))
        as.data.table(data)
      }
      dt <- rbindlist(lapply(textnodes, .get_parents_attributes), fill = TRUE)
      if (!is.null(meta)){
        for (x in names(meta)){
          dt[, eval(x) := xml2::xml_text(xml2::xml_find_first(doc, meta[x])), with = TRUE]
        }
      }
      texttable <- data.table(id = 1L:length(textnodes), text = sapply(textnodes, xml2::xml_text))
      return( list(text = texttable, metadata = dt) )
    }
  }
}
