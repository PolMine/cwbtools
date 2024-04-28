## General remarks

Not all packages listed in 'Suggests' were used conditionally as required (pkg passes tests using _R_CHECK_DEPENDS_ONLY_=true).

Fixes an issue I did not address in time, so cwbtools has been archived. My apologies!


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

