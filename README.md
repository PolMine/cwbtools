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

To install the development version of the package, use the installation mechanism offered by the [remotes](https://cran.r-project.org/package=remotes) package. On Windows, an installation of [Rtools](https://cran.r-project.org/bin/windows/Rtools/) may be necessary.

```{r}
# Make sure the remotes package is present
if (!"remotes" %in% installed.packages()[,"Package"]) install.packages("remotes")
Sys.setenv(R_REMOTES_STANDALONE = "true")
remotes::install_github("PolMine/cwbtools", ref = "dev", force = TRUE)
```

**Explanatory note:**

The default approach to install the development version `cwbtools` from GitHub would be `devtools::install_github("PolMine/cwbtools", ref = "dev")`. However, the concurrent dependency of `devtools` and of `cwbtools` on the `curl` package may cause nerve-wrecking problems if `curl` can be updated: If a newer version of `curl` is available, the user will be prompted whether this update is desired. Most users will agree. However, this update will fail because `curl` is loaded by `devtools`, and parts of the `curl` package cannot be deleted/updated (the dynamic library that is loaded). 

To avoid having to perform manual updates in the correct order, using the original `install_github()` function of the `remotes` package is recommended. When setting the environment variable `R_REMOTES_STANDALONE` to `true`, the `remotes` package will rely on a minimal set of additional packages. The aforementioned situation that may make the installation of `cwbtools` difficult for most users is omitted.


## Acknowledgements

The CWB is a classical indexing and query engine. Its character as an open source project is of great value for the community working with corpora. The enduring effort of the developers of the CWB is gratefully acknowledged!
