## General remarks

The package has been archived because there was a policy violation (on internet access) I failed to fix in time: There has been a scenario when `corpus_install()` does not abort gracefully if a ressource cannot be downloaded. 
This has been fixed.

In addition I removed the 'LazyData' statement that is unnecessary because there is not sample data within the package. R-devel started to show a respective NOTE.

## Test environments

* local macOS install, R 4.1.0
* GitHub Actions (Ubuntu 20.04, macOS, Windows) R 4.1.0 and R-devel (Ubuntu 20.04)
* win-builder (devel and release, R. 4.1.0)
* Debian (docker image, R-devel)

## R CMD check results

There were no ERRORs, WARNINGs or NOTEs on the Linux / macOS / Windows environments I used. 


## Downstream dependencies

I do not see any issues when checking GermaParl with the new cwbtools version.

