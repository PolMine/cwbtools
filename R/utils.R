#' Get Encoding of Character Vector.
#' 
#' @export get_encoding
#' @rdname get_encoding
#' @name get_encoding
get_encoding = function(x, verbose = TRUE){
  enc <- unique(Encoding(x))
  if (length(enc) == 1){
    if (enc == "unknown"){
      if (verbose) message("... encoding of the input vector is 'unknown', assuming it to be that of the locale")
      return( localeToCharset()[1] )
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