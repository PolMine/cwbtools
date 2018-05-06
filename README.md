[![License](https://img.shields.io/aur/license/yaourt.svg)](http://www.gnu.org/licenses/gpl-3.0.html)
[![Travis-CI Build Status](https://api.travis-ci.org/PolMine/cwbtools.svg?branch=master)](https://travis-ci.org/PolMine/cwbtools)
[![codecov](https://codecov.io/gh/PolMine/cwbtools/branch/master/graph/badge.svg)](https://codecov.io/gh/PolMine/cwbtools/branch/master)


# Rcpp bindings for the Corpus Workbench (CWB)

The package exposes core functions of the corpus library (CL) of the Corpus Worbench (CWB) by way of Rcpp wrappers. Furthermore, the packages includes Rcpp versions of performance critical operations. The main purpose of the package at this stage is to serve as an interface to the CWB for the package 'polmineR'.

There is a huge intellectual debt to the developers of the R-package 'rcqp', Bernard Desgraupes and Sylvain Loiseau. The reason for this new package is, that its approach Rcpp decreases the pains to make it portable and run on Windows systems.


### Installation on Windows

Pre-compiled binaries of the package 'RcppCWB' can be obtained from CRAN.

```{r}
install.packages("RcppCWB")
```

If you want to compile RcppCWB yourself, having [Rtools](https://cran.r-project.org/bin/windows/Rtools/) installed on your system is necessary. Using the mechanism offered by the devtools package, you can install RcppCWB.

```{r}
if (!"devtools" %in% installed.packages()[,"Package"]) install.packages("devtools")
devtools::install_github("PolMine/RcppCWB")
```

During the installation, cross-compiled versions of the corpus library (CL) are downloaded, to fulfill the condition that libcl.a needs to present on your system to compile the C++ code in the package against it. No further installations are necessary.


## Installation on Ubuntu

The C++ code included in the package is compiled against the corpus library (CL), a core C library of the CWB. If the CWB is not yet present, pre-compiled binaries of the CWB are downloaded during the installation process.

The pcre and glib libraries also need to be present. You can install them by running the following command from the shell.

```{sh}
sudo apt-get install libglib2.0-dev libssl-dev libcurl4-openssl-dev
```

Then open R. The easiest way to install RcppCWB is to install it from GitHub using the mechanism offered by the devtools package.

```{r}
install.packages(pkgs = c("Rcpp", "knitr", "testthat"))
if (!"devtools" %in% installed.packages()[,"Package"]) install.packages("devtools")
devtools::install_github("PolMine/RcppCWB")
```

## Installation on Debian

From the shell, install dependencies required to compile the C code of the Corpus Workbench.

```{sh}
apt-get install libglib2.0-dev bison flex
```

Then follow the instructions for the Ubuntu installation.

## Installation on MacOS

The C++ code included in the package is compiled against the corpus library (CL), a core C library of the CWB. If the CWB is not yet present, pre-compiled binaries of the CWB are downloaded during the installation process.

The pcre and glib libraries also need to be present. You can install them by running the following command from the shell. We recommend using 'Homebrew'. To install Homebrew, follow the instructions on the [Homebrew Homepage](https://brew.sh/index_de.html). It may be necessary to also install [Xcode](https://developer.apple.com/xcode/) and [XQuartz](https://www.xquartz.org).

The following commands then need to be executed from a terminal window. They will install the C libraries that the rcqp package relies on:

```{sh}
brew -v install pkg-config
brew -v install glib --universal
brew -v install pcre --universal
brew -v install readline
```

Then open R. The easiest way to install RcppCWB is to install it from GitHub using the mechanism offered by the devtools package.

```{r}
if (!"devtools" %in% installed.packages()[,"Package"]) install.packages("devtools")
devtools::install_github("PolMine/RcppCWB")
```
