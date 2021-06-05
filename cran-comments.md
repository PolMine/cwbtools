## General remarks

The package was removed from CRAN and archived because I failed to fix an issue in time: There is one remaining scenario when `corpus_install()` does not abort gracefully if a ressource cannot be downloaded. 
This has been fixed.

In addition I removed the 'LazyData' statement that is unnecessary because there is not sample data within the package. R-devel started to show a respective NOTE.

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

