cwbtools 0.3.8.9001
===================

* `p_attribute_encode()` accepts multiple p-attributes if method is "CWB".

cwbtools 0.3.8
==============

* Package names, software names and API are wrapped in single quotes in the
DESCRIPTION files, to follow section 1.1.1 of 'Writing R extensions' #43.
* References in the description of the DESCRIPTION file have been standardized
#44.
* To meet CRAN requirements, any remaining usage of `install.packages()` has 
been removed from the package. Using argument `pkg` of `corpus_install()` will
install corpora found in a package as system corpora defined in the default
registry directory #46.
* The vignettes 'opennnlp.Rmd' and 'sentences.Rmd' have been removed from the
package; they are now part of the PolMine Cookbook repository at
`https://github.com/PolMine/cookbook`. Packages 'NLP' and 'openNLP' are no
longer suggested and the `install.packages()` call (though not evaluated) is
omitted. Part of the fix for #46.
* The `fs::path()` function replaces base R `file.path()` throughout to solidify
the generation of paths and to improve the readability of the code throughout.
* `p_attribute_encode()` checks that the `character` vector `token_stream` does
not exceed the CWB corpus size limit (2^31 - 1) #40.


cwbtools 0.3.7
==============

* `gparlsample_url_restricted` has been updated to replace a URL that has become
defunct.
* New auxiliary function `zenodo_get_tarballurl()` steps in for functionality of
the zen4R package temporarily not working #42. It is used internally by the
`corpus_install()` function.


cwbtools 0.3.6
==============

* Ensure that `zenodo_get_tarball()` fails gracefully if Zenodo is temporarily
not available.

cwbtools 0.3.5
==============

* New function `p_attribute_rename()`, corresponding to `s_attribute_rename()`.
* `p_attribute_encode()` will remove the [p_attr].corpus file as suggested my
cwb-makeall (if `compress` is `TRUE`).
* Assumptions about the statement of an info file in registry files are relaxed,
the line starting with "INFO" is not required.
* Internally, functionality from the `fs` package for a consistent handling of 
paths (such as `fs::path()`) is used more widely (#36).
* Assumptions about the definition of a version in the name of a corpus tarball 
are relaxed. If possible, the version is taken from the properties (i.e. the
registry file).
* New function `zenodo_get_tarball()` for downloading corpus tarballs from 
Zenodo. Restricted access can be handled too (personalized URL with token).
* Function `corpus_install()` has new argument `load` to control whether corpus
is loaded after installation.


cwbtools 0.3.4
==============

* The function `pkg_add_description()` is declared deprecated. To alert users,
functionality of the lifecycle package is used (#1).
* A new function `as.vrt()` will generate valid *.vrt files from `xml_document` input.
* Added Left-to-Right Mark / "\u200E" to signs that are cleaned. 
* Due to an inconsistency in the code of `cwb_corpus_dir()`, the function would falsely yield `NA` results if the CWB directory would contain more than two directories.
* To be able to recognize faulty directories, the registry directory and the corpus directory are reported by `cwb_corpus_dir()` and `cwb_registry_dir()`. Argument `verbose` can be used to suppress this output.
* The statement on 'LazyData' has been removed from the DESCRIPTION file to avoid a warning emerging with R-devel on CRAN check machines (#33).
* Executing the code in the vignette 'sentences.Rmd' is conditional on the availability of the sample corpus. If the corpus can not be installed from Zenodo, building the vignette will not fail (#35).
* The `corpus_install()` function will abort with a FALSE return value if the requested tarball is not available (#34).
* A new function `s_attribute_rename()` can be used to rename s-attributes.
* A new function `corpus_get_version()` will derive the corpus version number from the registry file and return a `numeric version` object (#16).
* A limitation of `writeBin()` to write long integer vectors has been overcome with R v4.0.0. A warning and a preliminary workaround to address this limitation when using `p_attribute_encode()` for corpora with more than 536870911 tokens can therefore be dropped. For large corpora, the function will check the R version and issue the recommendation to install $ v4.0.0 or higher, if the size limitation (536870911) is relevant (#28).
* In addition to the URL for downloading the CWB, `cwb_get_url()` will return the MD5 checksum of the compressed file as attribute 'md5'.
* The `cwb_install()` function will fail gracefully if downloading the CWB fails (returning `NULL`). A new argument md5 will trigger checking the MD5 sum of the downloaded file (if provided). The default value of `cwb_dir` is now a temporary directory.


cwbtools 0.3.3
==============

## NEW FEATURES

* Assumptions about the directory structure in a corpus tarball are somewhat relaxed: The name of the data directory may also be "data" (not just "indexed_corpora") and data files need not be necessarily in a subdirectory of the data directory. This makes downloading and installing the Europarl and the Dickens corpus possible.

## MINOR IMPROVEMENTS

* The dependency on the devtools package can be dropped as one consequence of removing the Europarl vignette.
* The dependency on the usethis package has been removed.
* The sentences-vignette is more robust by explicitly creating a temporary registry directory.

## BUX FIXES

* A unit test that involves calling `cwb_install()` is skipped on Solaris to ensure that Solaris CRAN tests will not fail: A CWB binary is not available for Solaris.

## DOCUMENTATION FIXES

* The vignette "europarl.Rmd" is dropped altogether: Putting corpora into packages is not the recommended approach any more.


cwbtools 0.3.2
==============

## NEW FEATURES

* It is now possible to install a corpus from S3 by stating a S3-URI as argument `tarball` of `corpus_install()`.
* A new argument `checksum` for the `corpus_install()` function introduces functionality to check the integrity of a downloaded corpus tarball. If the tarball is downloaded from Zenodo (by stating a DOI using argument `doi`), the md5 checksum included in the record's metadata is extracted internally and used for checking.
* A new vignette explains how an existing CWB corpus can be enhanced using openNLP.
* The function `corpus_copy()` will accept a new argument `remove`. If `TRUE` (the default value is `FALSE`), files that have been copied will be removed. Removing files is reasonable to handle disk space parsimonously if the source corpus is at a temporary location where nobody will miss it.

## MINOR IMPROVEMENTS

* The `corpus_install()` function will abort with a warning and return value `FALSE` rather than an error if the DOI is not offered by Zenodo.
* If `corpus_install()` is used to install a corpus from a tarball present locally, a somewhat confusing message suggested that the tarball was downloaded. This message is not shown any more.
* Extracting a corpus tarball present locally involved copying the tarball to a temporary location before extracting it. This step consuming more disk space than necessary (inefficient and potentially problematic with large corpora) is now omitted.
* The function `cwb_install()` now replaces an internally hardcoded argument `cwb_dir` with an argument `cwb_dir`; the function returns the directory where the CWB is installed rather than `NULL` value.
* The function `cwb_get_bindir()` now introduces an argument `bindir`.
* Argument `compress` of `p_attribute_encode(` now has default value `FALSE` (#29).
* Examples in documentation of `p_attribute_encode()` have been adapted so that GitHub Action unit test passes on Windows.
* A user abort if an existing corpus would be removed by installing the same version anew will not result in an error message any more, but in return value `FALSE` (#25).


## BUG FIXES

* To avoid an issue with a false negative issued by `RCurl::url.exists()`, this function has been replaced by `httr::http_error()` (#31).
* The `corpus_install()` function still showed some progress messages even when `verbose` was set as `FALSE` (argument not passed to `corpus_copy()`. Fixed.
* The code in the vignette on adding a sentence annotation was not executed when building the package and a bug in the code went unnoticed. Fixed (#17).
* The `get_encoding()` method would return `NA` if `localeToCharset()` fails to infer charset from locale. In this case, UTF-8 is assumed.


## DOCUMENTATION FIXES

* A misleading, deprecated example in a dontrun section of the general package documentation has been removed (#23). The vignette includes a working and tested example how to encode the REUTERS corpus.


cwbtools 0.3.1
==============

## NEW FEATURES

* The (weak) dependency on the polmineR package (it was in the 'Suggests:' section of the DESCRIPTION file) has been removed. Changes are purely internal (higher-level polmineR functions have been replaced by lower-level RcppCWB functions, some tests were re-written). Dropping the dependency has the advantage that there is a much clearer structure of dependencies now (RcppCWB -> cwbtools -> polmineR).


## MINOR IMPROVEMENTS

* A remaining CLI formatting issue has been removed from the user dialogue for modifying the .Renviron file.
* Unit tests used a test download of the United Nations General Assembly (UNGA) corpus from Zenodo. To reduce the time required for testing the package, a test download of the (much smaller) GermaParlSample copus is performed.


## BUG FIXES

* The `corpus_install()` function tried to ask for user feedback when not in an interactive session. The function now checks whether it is possible to ask for user feedback.
* Part of the output of the `cwbtools::create_cwb_directories()` function did show if `verbose` was `FALSE`. Fixed.



cwbtools 0.3.0
==============

## NEW FEATURES

* The `corpus_install()` gives much better and nicer reports on steps performed during 
corpus downloads. User dialogues have been reworked thoroughly to provide better user guidance.
* The `use_corpus_registry_envvar()` function is called by `corpus_install()` and will
amend the .Renviron file as appropriate if the user so desires.
* To resolve a DOI, the 'zen4R' package is used, to extract information on the whereabouts
of a corpus tarball efficiently from the Zenodo API.
* A `corpus_testload()` has been implemented to check whether a (newly installed) corpus
is accessible.
* Dependency on usethis-package has been dropped.


## MINOR IMPROVEMENTS

* Extracting the version number from the corpus tarball is somewhat more forgiving if the
version number does not start with "v".
* The registry file for a newly downloaded corpus is refreshed only if a temporary registry directory is used.
* To remedy the fairly common error that the path to the info file is not stated correctly in the registry file, a fallback mechanism will look up potential alternatives to an info file stated wrongly. 


## BUG FIXES

* The json string returned from Zenodo may include newline strings that are escaped such
that they cannot be processed by `jsonlite::fromJSON()`. The auxiliary function to get and 
process information from Zenodo now ensures that newline characters are escaped such that 
they can be processed.
* The `corpus_copy()` function did not set the path to the info file to the new data directory - corrected.
* The `corpus_install()` function failed when the `registry_dir` got a `NULL` value from the default call to `cwbtools::cwb_registry_dir()`. But if the directories are created, the registry directory is there. Fixed.
* Removed a bug (faulty assignment) that would prevent that the path of a registry file 
is handled correctly (i.e. wrapped in quotation marks) by `registry_file_compose()` when the 
path includes any whitespace characters.


## DOCUMENTATION FIXES

* A problem with updating the `curl` dependency of `cwbtools` that may arise when `devtools::install_github()` is used is addressed in an extended explanation in the README.md file how to install the development version of `cwbtools` using `remotes::install_github()` (#21).


cwbtools 0.2.0
==============

## NEW FEATURES

* The `install_corpus()` function has been reworked thoroughly. Using system directories
  for the registry and the corpus directory is now supported. This is a prerequisite that
  corpora can be installed outside of R packages Installing corpora within corpora is
  not allowed by CRAN.
* A set of new auxiliary functions (`cwb_directories()`, `cwb_registry_dir()`, 
  `cwb_corpus_dir()`) will get the whereabouts of the registry directory and  the corpus 
  directory.  In particular, they consider that the polmineR package may have generated a
  temporary corpus registry, resetting the CORPUS_REGISTRY environment variable.
* The `install_corpus()` function accepts an argument `doi` to provide a Document Object 
  Identifier (DOI). At this stage, the DOI is assumed to be awarded by [Zenodo](https://zenodo.org/). Information available at the Zenodo site will be resolved 
  to get the URL of a corpus tarball that can be downloaded. Upon installing a corpus
  from Zenodo, the DOI and the version number will be written as corpus properties into 
  the registry file.
* To avoid removing corpora accidentally, the `corpus_install()` function will ask the user
  for feedback if a corpus would be installed that is already present and that would be 
  deleted or overwritten.
* New auxiliary functions `create_cwb_directories` and `use_corpus_registry_envvar()`
  will assist users to create the required directory structure for CWB indexed corpora.


## MINOR IMPROVEMENTS

* The default value of the argument "repo" that defines the repository for packaged corpora 
  is now the drat repository of the PolMine GitHub account ("https://PolMine.github.io/drat/").


## DOCUMENTATION FIXES

* New R6 Roxygen documentation used for documenting the `CorpusData` class.
* A (preliminary) vignette has been added that explains how to add a sentence annotation 
  can be added to an existing indexed corpus.


cwbtools 0.1.2
==============

## BUG FIXES

  * Trying to remove the entire temporary session directory at the end of the package 
    vignettes caused problems to build the package documentation. A more limited 
    approach to clean up temporary files after build the vignettes will omit 
    this problem.


cwbtools 0.1.1
===============

## MINOR IMPROVEMENTS

  * The `pkg_add_corpus()` function will now create the cwb directories (registry and data directory) if necessary. Previously, these directories were required to exist before moving a corpus into a package, making it necessary to put dummy files into packages to keep R CMD build from issuing warnings and git from dropping these directories. Creating the directories on demand is a precondition for a CRAN release of data packages (#11).

## BUG FIXES

  * In the upcoming R version 4.0, the `matrix` class will inherit from class `array`. The new package version now takes into account that `length(class(matrix(1:4,2,2)))` will return the value 2.
 
## DOCUMENTATION FIXES

  * The NEWS file now follows the styleguide such that `pkgdown::build_site()` will generate a proper changelog page.




cwbtools 0.1.0
==============
  * updated vignette so that annex explains installation of CoreNLP v3.9.2 (2018-10-05)
  * New functions `s_atttribute_get_regions()` and `s_attribute_get_values()`.
  * In `corpus_install()`, using `download.file()` replaces `curl::curl_download()` for Windows because curl apparently is not able to process target filenames that include special characters.
  * For Windows machines, there is a check for non-ASCII characters in the file path. If TRUE, a path generated by a call to `shortPathName()` is used.
  * In the vignette, the registry is reset after creating the new corpora, to make the new corpus available.
  * A (preliminary) `decode()`-method will turn a `partition` into an `Annotation` object from the NLP package.
  * A new `conll_get_regions()`-function will turn an CoNLL-style annotated token stream into a table with regions that can be encoded using `s_attribute_encode()`.
  * A new function `s_attribute_merge()` will merge two `data.table` objects defining s-attributes, checking for overlaps.

cwbtools 0.0.11
===============

  * New functions `p_attribute_recode()`, `s_attribute_recode()`, and supplementary `s_attributed_files()`, and `corpus_recode()`.
  * Any call to `tempdir()` is now wrapped as `normalizePath(tempdir(), winslash = "/")` to avoid Problems on Windows, when different file separators may be used.
  * When calling `file.path()`, the argument `fsep` is "/" to prevent confusion of file seperators.
  * A new function `corpus_copy()` is available to create a copy a corpus.
  * Working example for `s_attribute_encode`().
  * A call to `cl_delete_corpus()` from RcppCWB is added to `s_attribute_encode()`, so that newly added s-attributes can be used without restarting the R session.
  * The `corpus_copy()` was defined (and documented) twice in a confusing manner. This is cleaned up.
  * Calls to `installed.packages()` were replaced to meet an advice of the CRAN team in the submission process.
  * 


cwbtools 0.0.10
===============

  * Missing documentation written for fields of class CorpusData.
  * New fields 'sentences' and 'named_entities' added to class CorpusData, as a basis
  for encoding annotation of sentences and named entities.


cwbtools 0.0.9
==============

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


cwbtools 0.0.8
==============

  * major bug removed when writing regions to disk (s_attribute_encode) with R
  * when creating/removing files in p_attribute_encode, only basenames of filenames are outputted
  * for CorpusData$encode(), an already existing corpus will be removed


cwbtools 0.0.7
==============

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


cwbtools 0.0.6
==============

  * DataPackage class turned into pkg_*-functions
  * first version that passes all tests


cwbtools 0.0.5
==============

  * undocumented


cwbtools 0.0.4
==============

  * askYesNo function has been replaced by readlines(), to ensure compatibility with R versions < 3.5
