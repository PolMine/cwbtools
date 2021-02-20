## General remarks

This version addresses an error on Solaris and a warning on Fedora that emerged with v0.3.2, see https://cran.r-project.org/web/checks/check_results_cwbtools.html. 


## Test environments

* local OS X install, R 4.0.2
* GitHub Actions (Ubuntu 20.04 release and devel, macOS, Windows) R 4.0.4
* win-builder (devel and release, R. 4.0.4)
* Debian (docker image, R-devel)
* Solaris on R-hub (Oracle Solaris 10, x86, 32 bit, R-release, Oracle Developer Studio 12.6)
* Fedora, R-devel (docker image).

## R CMD check results

There were no ERRORs, WARNINGs or NOTEs on the Linux / macOS / Windows environments I used. 


## Downstream dependencies

I do not see any issues when checking GermaParl with the new cwbtools version.

