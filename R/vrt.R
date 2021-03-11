#' Consolidate vrt files for CWB import.
#' 
#' Files resulting from tagging/annotation may violate the requirements of the 
#' Corpus Workbench (CWB).  Consolidate the known issues the vrt files may cause.
#' 
#' Known issues resulting from annotating files (with the treetagger in particular)
#' are whitespace characters invalid for XML, XML elements at the end of a line
#' rather than in a seperate line, characters invalid for XML (such as ampersands),
#' inter alia.
#' 
#' Before doing respective corrections, the method tests whether there is any text at
#' all in the files. Empty files (files that contain nothing but XML tags) are dropped.
#' 
#' @param x a character vector providing a directory with vrt files
#' @param replacements a list of character vectors (length 2 each) with regular expressions / replacements
#' @export as.vrt
#' @rdname as.vrt
#' @name as.vrt
as.vrt <- function(x, replacements = list()){
  
  doc <- as.character(x)
  while(grepl("<.*?><", doc)) doc <- gsub("<(.*?)><", "<\\1>\n<", doc)

  vrt <- strsplit(doc, split = "\n")[[1]]
  
  # remove leading and trailing spaces
  vrt2 <- gsub("^\\s*(.*?)\\s*$", "\\1", vrt)
  
  # repair buggy lines
  vrt3 <- ifelse(
    grepl("#unknown#\t#unknown#", vrt2),
    c(
      gsub("^(.*?#unknown#)\t.*$", "\\1", vrt2, perl = TRUE),
      gsub("^.*\t#unknown#\t(#unknown#.*?)$", "\\1", vrt2, perl = TRUE)
    ),
    vrt2
  )
  
  # misplaced closing tags (at end of line)
  for (i in grep("^.+?</.*?>$", vrt3, perl = TRUE)){
    if (grepl("^.*?\\s.*?\\s.*?</.*?>$", vrt3[i])){
      # a line such as: 'professionell	ADJD	professionell</p>' / 'oder	KON	od</p>'
      vrt3[i] <- gsub("^(.*?\\s.*?\\s.*?)(</.*?>)$", "\\1\n\\2", vrt3[i])
    } else {
      # a line such as 'kommun</p>' or 'Dezember</p>'
      vrt3[i] <- gsub("^.*?(</.*?>)$", "\\1", vrt3[i])
    }
  }
  
  # repair wrong position of closing tags
  for (x in grep("#unknown#</p>", vrt3)){
    vrt3[x-1] <- paste(vrt3[x-1], "\t#unknown#", sep = "")
    vrt3[x] <- "</p>"
  }
  
  # remove empty tags
  self_closing_tags <- grep("^\\s*<.*?/>\\s*$", vrt3)
  misleading_tags <- grep('^[<>]\\s+.*$', vrt3)
  index_to_remove <- unique(c(self_closing_tags, misleading_tags))
  y <- if (length(index_to_remove) > 0) vrt3[-index_to_remove] else vrt3
  
  # run through replacements
  known_bugs <- list(
    c("\u00A0", " "), # incompatible with XML
    c("<unknown>", "#unknown#"), # incompatible with XML
    c("&", "&amp;"), # incompatible with XML
    c("\xC2\xA0", ""), # incompatible with XML
    c("^\u201E\\t[A-Z]+\\t#unknown#$", "'\t$(\t'"),
    c("\u200e", ""), # Left-to-Right Mark (invisible, hard to see, but disruptive)
    c("^``\\t.*?\\t``$", "'\t$(\t'"),
    c('^\\s*<\\s*$', ""),
    c("^(<.*?>)(<.*?>)$", "\\1\n\\2")
  )
  to_replace <- c(known_bugs, replacements)
  for (i in 1L:length(to_replace)) y <- gsub(to_replace[[i]][1], to_replace[[i]][2], y)

  empty_lines <- grep("^\\s*$", y)
  if (length(empty_lines) > 0L) y <- y[-empty_lines]
  y
}
