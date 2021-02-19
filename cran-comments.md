## General remarks

This release solves issues with the robustness of download mechanisms for corpora (function `corpus_install()`). An issue with the GermaParl package on Windows (see https://cran.r-project.org/web/checks/check_results_GermaParl.html) will disappear with this release. 


## Test environments

* local OS X install, R 4.0.2
* GitHub Actions (Ubuntu 20.04 release and devel, macOS, Windows) R 4.0.4
* win-builder (devel and release, R. 4.0.4)
* Debian (docker image, R-devel)
* Solaris on R-hub (Oracle Solaris 10, x86, 32 bit, R-release, Oracle Developer Studio 12.6)

## R CMD check results

There were no ERRORs, WARNINGs or NOTEs on the Linux / macOS / Windows environments I used. 


## Downstream dependencies

I do not see any issues when checking GermaParl with the new cwbtools version.

