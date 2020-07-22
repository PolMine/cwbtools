## General remarks

The dependency on the polmineR package has been dropped. As pointed out to me by Brian Ripley, polmineR was not used conditionally the orderly way. Removing the dependency altogether (using lower-level functions and alternative tests) was my preferred solution to reduce the number of dependencies.


## Test environments

* local OS X install, R 4.0.2
* Ubuntu 14.04 (on travis-ci), R 4.0.0
* Windows/AppVeyor, R 4.0.2 Patched
* win-builder (devel and release, R. 4.0.2)
* Debian (docker image, R-devel)

## R CMD check results

There were no ERRORs, WARNINGs or NOTEs on the Linux / macOS / Windows environments I used. 


## Downstream dependencies

The 'polmineR' package depends on cwbtools. I do not see any issues when checking GermaParl
with the new cwbtools version.

