## General remarks

The package has been archived as I failed to fix bugs in time. The main issue
was a changed interface for downloads from Zenodo. I revert to use functionality
from the zen4R package, and implemented a set of further fixes.


## Test environments

* local macOS install, R 4.3.2
* GitHub Actions (Ubuntu, macOS, Windows) R 4.3.2
* win-builder (devel and release 4.3.2)

## R CMD check results

On R winbuilder I see a NOTE on possibly misspelled words in the DESCRIPTION
file ("CWB" and "Hardie"), but that is correct.

There were no ERRORs or WARNINGs on the Linux / macOS / Windows environments I used. 


## Downstream dependencies

None at this stage.

