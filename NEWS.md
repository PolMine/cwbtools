# v0.0.8.9004
  * issue with ALTREP vector for corpus positions resolved
  * layout of progress bars consistently using pbapply package
  * sanity checks for s_attribute_encode, ensure that region_matrix is integer matrix
  * s_attribute_encode when called with method = "R" will now add s_attribute to registry

# v0.0.8.9003
  * s_attribute_encode will add structural attribute to registry when using R implementation, too

# v0.0.8.9001
  * corpus_as_tarball-function added
  * install_corpus able to install from tarball

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