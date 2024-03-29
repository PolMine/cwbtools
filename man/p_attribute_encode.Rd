% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/p_attribute.R
\name{p_attribute_encode}
\alias{p_attribute_encode}
\alias{p_attribute_recode}
\alias{p_attribute_rename}
\title{Encode Positional Attribute(s).}
\usage{
p_attribute_encode(
  token_stream,
  p_attribute = "word",
  registry_dir,
  corpus,
  data_dir,
  method = c("R", "CWB"),
  verbose = TRUE,
  quietly = FALSE,
  encoding = get_encoding(token_stream),
  compress = FALSE,
  reload = TRUE
)

p_attribute_recode(
  data_dir,
  p_attribute,
  from = c("UTF-8", "latin1"),
  to = c("UTF-8", "latin1")
)

p_attribute_rename(
  corpus,
  old,
  new,
  registry_dir,
  verbose = TRUE,
  dryrun = FALSE
)
}
\arguments{
\item{token_stream}{A \code{character} vector with the tokens of the corpus. The
maximum length is 2 147 483 647 (2^31 - 1); a warning is issued if this
threshold is exceeded. See the \href{https://cwb.sourceforge.io/files/CWB_Encoding_Tutorial.pdf}{CWB Encoding Tutorial} for
size limitations of corpora. May also be a file.}

\item{p_attribute}{The positional attribute to create - a \code{character} vector
containing only lowercase ASCII characters (a-z), digits (0-9), -, and _:
No non-ASCII or uppercase letters allowed. If method is "R", only one
positional attribute can be encoded at a time. If \code{method} is "CWB", more
than one p-attribute allowed.}

\item{registry_dir}{Registry directory.}

\item{corpus}{ID of the CWB corpus to create.}

\item{data_dir}{The data directory for the binary files of the corpus.}

\item{method}{Either 'CWB' or 'R', defaults to 'R'. See section 'Details'.}

\item{verbose}{A \code{logical} value, whether to output progress messages.}

\item{quietly}{A \code{logical} value passed into \code{RcppCWB::cwb_makeall()},
\code{RcppCWB::cwb_huffcode()} and \code{RcppCWB::cwb_compress_rdx} to control
verbosity of these functions.}

\item{encoding}{Encoding as defined in the charset corpus property of the
registry file for the corpus ('latin1' to 'latin9', and 'utf8').}

\item{compress}{A \code{logical} value, whether to run \code{RcppCWB::cwb_huffcode()}
and \code{RcppCWB::cwb_compress_rdx()} (method 'R'), or command line tools
\code{cwb-huffcode} and \code{cwb-compress-rdx} (method 'CWB'). Defaults to \code{FALSE}
as compression is not stable on Windows.}

\item{reload}{A \code{logical} value that defaults to \code{TRUE} to ensure that all
features are available.}

\item{from}{Character string describing the current encoding of the attribute.}

\item{to}{Character string describing the target encoding of the attribute.}

\item{old}{A \code{character} vector with p-attributes to be renamed.}

\item{new}{A \code{character} vector with new names of p-attributes. The vector
needs to have the same length as vector \code{old}.}

\item{dryrun}{A \code{logical} value, whether to suppress actual renaming
operation for inspecting output messages}
}
\value{
\code{TRUE} is returned invisibly, if encoding has been successful.
\code{FALSE} indicates an error has occurred.
}
\description{
Generate positional attribute from a character vector of
tokens (the token stream).
}
\details{
Four steps generate the binary CWB corpus data format for positional
attributes: (1) Encode the token stream of the corpus, (2) create index
files, (3) compress token stream and (4) compress index files. Whereas steps
1 and 2 are required to make a corpus work, steps 3 and 4 are optional yet
useful to reduce disk usage and improve performance. See the \href{https://cwb.sourceforge.io/files/CWB_Encoding_Tutorial.pdf}{CQP Corpus Encoding Tutorial}
(sections 2-4) for an explanation of the procedure.

\code{p_attribute_encode()} offers an R and a CWB implementation controlled by
argument \code{method}. When choosing method 'R', the token stream is encoded in
'pure R', then the C implementation of CWB functionality as exposed to R via
the RcppCWB package is used (functions \code{RcppCWB::cwb_makeall()} for indexing,
\code{RcppCWB::cwb_huffcode()} and \code{RcppCWB::cwb_compress_rdx()} for compression).
When choosing method 'CWB', the token stream is written to disk, then CWB
command line utilities 'cwb-encode', cwb-makeall', 'cwb-huffcode' and
'cwb-compress-rdx' are called using \code{system2()}. The CWB-method requires an
installation of the 'CWB'. The \code{cwb_install()} function will download and #
install the CWB command line tools within the package. The 'CWB'-method is
still supported as it is used in the test suite of the packaage. The
'R'-method is robust and is recommended.

\code{p_attribute_recode()} will recode the values in the avs-file and
change the attribute value index in the avx file. The rng-file remains
unchanged. The registry file remains unchanged, and it is highly
recommended to consider \code{s_attribute_recode()} as a helper for
\code{corpus_recode()} that will recode all s-attributes, all p-attributes, and
will reset the encoding in the registry file.

Function \code{p_attribute_rename()} can be used to rename a
positional attribute. Note that the corpus is not refreshed (unloaded,
re-loaded), so it may be necessary to restart R for changes to become
effective.
}
\examples{
# In this example, we follow a "pure R" approach. 
library(dplyr)

reu <- system.file(package = "RcppCWB", "extdata", "examples", "reuters.txt")
tokens <- readLines(reu)

# Create new (and empty) directory structure

registry_tmp <- fs::path(tempdir(), "registry")
data_dir_tmp <- fs::path(tempdir(), "data_dir", "reuters")

if (dir.exists(registry_tmp)) unlink(registry_tmp, recursive = TRUE)
if (dir.exists(data_dir_tmp)) unlink(data_dir_tmp, recursive = TRUE)

dir.create(registry_tmp)
dir.create(data_dir_tmp, recursive = TRUE)

# Encode token stream (without compression)

p_attribute_encode(
  corpus = "reuters",
  token_stream = tokens,
  p_attribute = "word",
  data_dir = data_dir_tmp,
  registry_dir = registry_tmp,
  method = "R",
  compress = FALSE,
  quietly = TRUE,
  encoding = "utf8"
)

# Augment registry file 

registry_file_parse(corpus = "REUTERS", registry_dir = registry_tmp) \%>\%
  registry_set_name("Reuters Sample Corpus") \%>\%
  registry_set_property("charset", "utf8") \%>\%
  registry_set_property("language", "en") \%>\%
  registry_set_property("build_date", as.character(Sys.Date())) \%>\%
  registry_file_write()

# Run query as a test

library(RcppCWB)

cqp_query(corpus = "REUTERS", query = '[]{3} "oil" []{3};')
regions <- cqp_dump_subcorpus(corpus = "REUTERS")

kwic <- apply(
  regions, 1,
  function(region){
    ids <- cl_cpos2id(
      "REUTERS",
      p_attribute = "word",
      registry = registry_tmp,
      cpos = region[1]:region[2]
    )
    words <- cl_id2str(
      corpus = "REUTERS",
      p_attribute = "word",
      registry = registry_tmp,
      id = ids
    )
    paste0(words, collapse = " ")
  }
)
kwic[1:10]
}
\author{
Christoph Leonhardt, Andreas Blaette
}
