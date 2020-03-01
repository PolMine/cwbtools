## General remarks

Package version 0.1.1 has just been released a few day ago. A somewhat embarrassing
line of code in the package vignette tried to remove the whole temporary session
directory, causing warnings on a few machines (R-devel-linux, R-release-linux,
R-patched-linux). The code in the vignettes has been adapted to remove this issue. 

Thanks to Kurt Hornik for helping me to understand the problem.


## Test environments

* local OS X install, R 3.6.1
* Ubuntu 14.04 (on travis-ci), R 3.6.1
* Ubuntu 14.04 (project server), R 3.4.3
* win-builder (devel and release), R. 3.6.1
* Windows/AppVeyor, R 3.6.1 Patched
* Debian Linux, R-devel, GCC


## R CMD check results

There were no ERRORs, WARNINGs or NOTEs on the Linux / macOS environments I used. 


## Downstream dependencies

Checking not necessary at this time.

