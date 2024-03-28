## General remarks

Maintanence release, fixes issue with `zenodo_get_tarball()` reported on CRAN check machine.


## Test environments

* local macOS install, R 4.3.3
* GitHub Actions (Ubuntu, macOS, Windows) R 4.3.4
* win-builder (devel, oldrel, 4.4.0)

## R CMD check results

On R winbuilder I have seen a NOTE on possibly misspelled words in the DESCRIPTION
file ("CWB" and "Hardie"), but that is correct.

There were no ERRORs or WARNINGs on the Linux / macOS / Windows environments I used. 


## Downstream dependencies

None at this stage.

