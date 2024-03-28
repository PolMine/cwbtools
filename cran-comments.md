## General remarks

Maintanence release, fixes issue with `zenodo_get_tarball()` reported on CRAN check machine.


## Test environments

* local macOS install, R 4.3.2
* GitHub Actions (Ubuntu, macOS, Windows) R 4.3.2
* win-builder (devel, release 4.3.2, oldrel)

## R CMD check results

On R winbuilder I have seen a NOTE on possibly misspelled words in the DESCRIPTION
file ("CWB" and "Hardie"), but that is correct.

There were no ERRORs or WARNINGs on the Linux / macOS / Windows environments I used. 


## Downstream dependencies

None at this stage.

