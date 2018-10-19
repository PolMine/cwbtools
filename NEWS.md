# v0.0.11.9001
  * New functions `s_atttribute_get_regions()` and `s_attribute_get_values()`.

# v0.0.11
  * New functions `p_attribute_recode()`, `s_attribute_recode()`, and supplementary `s_attributed_files()`, and `corpus_recode()`.
  * Any call to `tempdir()` is now wrapped as `normalizePath(tempdir(), winslash = "/")` to avoid Problems on Windows, when different file seperators may be used.
  * When calling `file.path()`, the argument `fsep` is "/" to prevent confusion of file seperators.
  * A new function `corpus_copy()` is available to create a copy a corpus.
  * Working example for `s_attribute_encode`().
  * A call to `cl_delete_corpus()` from RcppCWB is added to `s_attribute_encode()`, so that newly added s-attributes can be used without restarting the R session.


# v0.0.10
  * Missing documentation written for fields of class CorpusData.
  * New fiels 'sentences' and 'named_entities' added to class CorpusData, as a basis
  for encoding annotation of sentences and named entities.

# v0.0.9
  * issue with parsing path correctly in registry_file_path when path is in inverted commas solved (adjusted regex)
  * issue with ALTREP vector for corpus positions resolved
  * layout of progress bars consistently using pbapply package
  * sanity checks for s_attribute_encode, ensure that region_matrix is integer matrix
  * s_attribute_encode when called with method = "R" will now add s_attribute to registry
  * s_attribute_encode will add structural attribute to registry when using R implementation, too
  * corpus_as_tarball-function added
  * install_corpus able to install from tarball
  * progress option for `CorpusData$import_xml()`-method
  * Minimal rework of progress bar in `CorpusData$add_corpus_positions()` (helper function .fn)
  * Three dots (...) are passed into `download.file()` by `install_corpus()`, if argument tarball is specified. This is a precondition for passing arguments to download password-protected corpora.

# v0.0.8
  * major bug removed when writing regions to disk (s_attribute_encode) with R
  * when creating/removing files in p_attribute_encode, only basenames of filenames are outputted
  * for CorpusData$encode(), an already existing corpus will be removed

# v0.0.7
  * bug removed in function pkg_create_cwb_dirs causing error when a directory already exists
  * new vignette 'europarl': sample workflow for putting indexed corpus into package
  * for $tokenize()-method of CorpusData: stricter requirement that chunkdata is data.table
  * progress bar for $tokenize()-method, when tokenizers package is used
  * tilde expansion for paths that are passed into p_attribute_encode
  * stri_detect_regex replacing grepl to speed things up in p_attribute_encode
  * awful workaround for coping with latin1 removed in p_attribute_encode
  * stip_punct = FALSE for $tokenize() method of CorpusData
  * purging the data for the CWB has been moved away from p_attribute_encode to a $purge()-method
    of CorpusData (to be performed on chunkdata) as a matter of efficiency.
  * continuous removal of objects and garbage collection in p_attribute_encode to be
    as parsimonious with memory as possible
  * checking of encoding in p_attribute_encode has been moved to $check_encoding() method in 
    CorpusData-class to keep necessity to copy around vectors (potentially exceeding memory)
    to a minimum.
  * additional parameters passed into tokenizers::tokenize_words by ...
  * writing hex for content of s_attributes to cope with encoding issues
  * values coerced to character

# v0.0.6
  * DataPackage class turned into pkg_*-functions
  * first version that passes all tests

# v0.0.5
  * undocumented

# v0.0.4
  * askYesNo function has been replaced by readlines(), to ensure compatibility with R versions < 3.5