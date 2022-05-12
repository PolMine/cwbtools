#' Download corpus tarball from Zenodo
#' 
#' Download corpus tarball from Zenodo. Both freely available data and
#' restricted access is supported.
#' @return Filename of downloaded corpus, to serve as input (argument ``)
#' @examples
#' fname <- zenodo_get_tarball(url = "https://zenodo.org/record/3823245")
#' corpus_install(fname)
#' @importFrom curl curl new_handle handle_cookies curl_fetch_memory curl_download
#' @importFrom xml2 read_html xml_find_all xml_attr
zenodo_get_tarball <- function(url, destfile = tempfile(fileext = ".tar.gz"), verbose = TRUE, progress = TRUE){
  
  h <- new_handle()
  
  restricted <- grepl("\\?token=", url)
  if (verbose) message("... restricted", as.character(restricted))
  
  if (restricted){
    curl_fetch_memory(url = url, handle = h)
    page <- curl(url = strsplit(x = url, split = "\\?token=")[[1]][1], handle = h)
  } else {
    page <- curl(url = url)
  }
  
  website <- read_html(page)
  file_elems <- xml_find_all(website, xpath = "//a[@class='filename']")
  filenames <- sapply(
    strsplit(x = sapply(x, xml_attr, attr = "href"), split = "\\?download"),
    `[[`, 1L
  )
  tarball <- filenames[grep("\\.tar\\.gz", filenames)]
  if (length(tarball) > 1L) stop("more than one tarball could be downloaded")
  
  curl_download(
    url = fs::path("https://zenodo.org", tarball[[1]]),
    destfile = destfile,
    quiet = !progress, 
    handle = h
  )
  destfile
}


