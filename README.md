[![License](https://img.shields.io/aur/license/yaourt.svg)](http://www.gnu.org/licenses/gpl-3.0.html)
[![Travis-CI Build Status](https://api.travis-ci.org/PolMine/cwbtools.svg?branch=dev)](https://travis-ci.org/PolMine/cwbtools)
[![codecov](https://codecov.io/gh/PolMine/cwbtools/branch/dev/graph/badge.svg)](https://codecov.io/gh/PolMine/cwbtools/branch/dev)


# Tools to Create, Manipulate and Manage Corpora for the Corpus Workbench (CWB)

The Corpus Workbench (CWB) is a indexing and query engine to efficiently work with large, linguistically annotated corpora. The 'cwbtools' package offers a set of tools to create, manipulate and manage CWB indexed corpora from within R in a convenient fashion. It complements packages that use the CWB as a backend for text mining with R, namely the packages 'rcqp' and 'RcppCWB' for low-level access to CWB indexed corpora, and 'polmineR' as a toolset to implement common text mining workflows.


## Installation

The package is 'GitHub only' package so far. The most convenient way to install it will be to use an installation mechanism offered by the devtools package. The procedure is the same for Windows, Linux, and macOS. On Windows, having [Rtools](https://cran.r-project.org/bin/windows/Rtools/) installed on your system may be necessary to use the full functionality of 'devtools'.

First, check that devtools is installed ...

```{r}
if (!"devtools" %in% installed.packages()[,"Package"]) install.packages("devtools")
```

Then install the cwbtools package.

```{r}
devtools::install_github("PolMine/cwbtools")
```

## Acknowledgements

The CWB is a classical indexing and query engine. Its character as an open source project is of great value. The enduring effort of the CWB developers is gratefully acknowledged.

