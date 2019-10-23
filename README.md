[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Travis-CI Build Status](https://api.travis-ci.org/PolMine/cwbtools.svg?branch=dev)](https://travis-ci.org/PolMine/cwbtools)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/PolMine/cwbtools?branch=dev&svg=true)](https://ci.appveyor.com/project/PolMine/cwbtools)
[![codecov](https://codecov.io/gh/PolMine/cwbtools/branch/dev/graph/badge.svg)](https://codecov.io/gh/PolMine/cwbtools/branch/dev)


# Tools to Create, Modify and Manage Corpora for the Corpus Workbench (CWB)

The [Corpus Workbench (CWB)](http://cwb.sourceforge.net/) is a classic indexing and query engine to efficiently work with large, linguistically annotated corpora. The *cwbtools* package offers a set of tools to conveniently create, modify and manage CWB indexed corpora from within R. It complements R packages that use the CWB as a backend for text mining with R, namely the [RcppCWB](https://CRAN.R-project.org/package=RcppCWB) package for low-level access to CWB indexed corpora, and [polmineR](https://CRAN.R-project.org/package=polmineR) as a toolset to implement common text mining workflows.


## Installation

The package is available via CRAN and can be installed as follows on Windows, macOS and Linux.

```{r}
install.packages("cwbtools")
```

To install the development version of the package, use the installation mechanism offered by the [devtools](https://CRAN.R-project.org/package=devtools) package. On Windows, an installation of [Rtools](https://cran.r-project.org/bin/windows/Rtools/) may be necessary.

```{r}
if (!"devtools" %in% installed.packages()[,"Package"]) install.packages("devtools")
devtools::install_github("PolMine/cwbtools")
```

## Acknowledgements

The CWB is a classical indexing and query engine. Its character as an open source project is of great value for the community working with corpora. The enduring effort of the developers of the CWB is gratefully acknowledged!
