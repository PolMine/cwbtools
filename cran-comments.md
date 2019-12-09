## General remarks

This is a maintenance release, following a notification by Email by Kurt Hornik that the in the upcoming R version (4.0), the 'matrix' class will inherit from the 'array' class, thus causing a error when running for instance: class(matrix(1 : 4, 2, 2)) == "matrix"

I modified the code such that it will comply with the upcoming changes in R-devel / R 4.0. 

I am grateful for the foresight of the CRAN team and the precise advice I received!


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

