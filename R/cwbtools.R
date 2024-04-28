#' cwbtools-package
#'
#' Tools to Create, Modify and Manage CWB Corpora.
#'
#' The \href{https://cwb.sourceforge.io/}{\emph{Corpus Workbench}} (CWB) offers
#' a classic approach for working with large, linguistically and structurally
#' annotated corpora. Its design ensures memory efficiency and makes running
#' queries fast (Evert and Hardie 2011). Technically, indexing and compressing
#' corpora as suggested by Witten et al. (1999) is the approach implemented by
#' the CWB (Christ 1994).
#' 
#' The C implementation of the CWB is mature and efficient. However, the
#' convenience and flexibility of traditional CWB command line tools is limited.
#' These tools are not portable across platforms, inhibiting the ideal of
#' reproducible research.
#'  
#' The 'cwbtools' package combines portable pure R tools to create indexed
#' corpus files and convenience wrappers for the original C implementation of
#' CWB as exposed by the
#' \href{https://CRAN.R-project.org/package=RcppCWB}{RcppCWB} package.
#' Additional functionality to add and modify annotations of corpora from within
#' R makes working with CWB indexed corpora much more flexible. "Pure R" workflows
#' to enrich corpora with annotations using standard NLP tools or generated
#' manually can be implemented seamlessly and conveniently.
#' 
#' The \emph{cwbtools} package is a companion of the
#' \href{https://CRAN.R-project.org/package=RcppCWB}{RcppCWB} and the
#' \href{https://CRAN.R-project.org/package=polmineR}{polmineR} package and is a
#' building block of an infrastructure to support the combination of
#' quantitative and qualitative approaches when working with textual data.
#'
#' @references Christ, Oliver (1994): "A Modular and Flexible Architecture for
#'   an Integrated Corpus Query System". \emph{Proceedings of COMPLEX'94},
#'   pp.23-32. \href{http://arxiv.org/pdf/cmp-lg/9408005}{(available online here)}
#' @references Evert, Stefan and Andrew Hardie (2011): "Twenty-first
#'   century Corpus Workbench: Updating a query architecture for the new
#'   millennium." In: \emph{Proceedings of the Corpus Linguistics 2011 conference},
#'   University of Birmingham, UK.
#'   \href{https://www.birmingham.ac.uk/documents/college-artslaw/corpus/conference-archives/2011/Paper-153.pdf}{(available online here)}
#' @references Witten, Ian H., Alistair Moffat and Timothy C. Bell (1999):
#'   \emph{Managing Gigabytes: Compressing and Indexing Documents and Images.}
#'   2nd edition. San Francisco et al.: Morgan Kaufmann.
#' @aliases cwbtools-package cwbtools
#' @docType package
#' @keywords package
#' @name cwbtools-package
#' @rdname cwbtools-package
#' @author Andreas Blaette
#' @import R6
#' @import methods
#' @importFrom lifecycle deprecate_warn
NULL

## usethis namespace: start
#' @importFrom lifecycle deprecate_soft
## usethis namespace: end
NULL

#' @noRd
#' @param x `character` vector with attribute names to check.
#' @importFrom cli cli_alert_danger
.check_attribute_name <- function(x){
  # To be able to use this check on p- and s-attributes alike, the check for
  # filename 'id.rng' (see issue #69) is part of s_attribute_encode()
  
  if (any(!grepl("^[a-z0-9_-]+$", x) | grepl("^[0-9].*$", x))){
    cli_alert_danger(
      text = paste(c(
        "Attribute names may only contain lowercase ASCII characters (a-z), ",
        "digits (0-9), -, and _. Names may not include non-ASCII or uppercase ",
        "letters or start with a digit."
      ))
      
    )
    return(FALSE)
  }
  return(TRUE)
}