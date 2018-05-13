#' Get Encoding of Character Vector.
#' 
#' @param x a character vector
#' @param verbose logical, whether to output messages
#' @export get_encoding
#' @rdname get_encoding
#' @name get_encoding
#' @importFrom utils localeToCharset
get_encoding = function(x, verbose = FALSE){
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


