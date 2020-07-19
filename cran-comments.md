## General remarks

The dependency on the polmineR package has been dropped. As pointed out to me by Brian Ripley, polmineR was not used conditionally the orderly way. Removing the dependency altogether (using lower-level functions and alternative tests) was my preferred solution to reduce the number of dependencies.


## Test environments

* local OS X install, R 4.0.2
* Ubuntu 14.04 (on travis-ci), R 4.0.0
* Windows/AppVeyor, R 4.0.2 Patched
* win-builder (devel and release, R. 4.0.2)
* Fedora (R-Hub) R-devel, clang, gfortran

## R CMD check results

There were no ERRORs, WARNINGs or NOTEs on the Linux / macOS / Windows environments I used. 

An ERROR reported at the CRAN package check results page for cwbtools results from 
a failure to compile the RcppCWB dependency on Fedora 32 (with GCC 10). I maintain the
RcppCWB package and I have submitted a new RcppCWB version yesterday that fixes the issue.


## Downstream dependencies

The 'GermaParl' package depends on cwbtools. I do not see any issues when checking GermaParl
with the new cwbtools version.

