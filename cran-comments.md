## General remarks

`zenodo_get_tarball()`, a new function for downloading restricted data from a repository is the main  new feature of this release. Example for
this function are wrapped into \donttest{}, but I do not suppress the unit test
on CRAN, hoping that it will not run too long.

## Test environments

* local macOS install, R 4.1.3
* GitHub Actions (Ubuntu 20.04, macOS, Windows) R 4.2.0
* win-builder (devel and release, R. 4.2.0)

## R CMD check results

There were no ERRORs, WARNINGs or NOTEs on the Linux / macOS / Windows environments I used. 


## Downstream dependencies

I do not see any issues when checking GermaParl with the new cwbtools version.

