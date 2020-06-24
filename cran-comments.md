## General remarks

The new version brings improvements of the user interface: Reworked user 
dialogues and  with more telling status messages. This  results in two 
new dependencies (cli, rstudioapi), but there is no tricky issue I could 
anticipate.

## Test environments

* local OS X install, R 4.0.0
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

