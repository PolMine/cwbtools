## General remarks

This is the third initial submit of cwbtools (v0.1.0. I am reallygrateful 
for your  scrutiny and advice. Martina Schmirl alerted me on two matters ...


### issue 1

My usage of single quotes in the DESCRIPTION was not in line with standards. I corrected this to the
best of my knowledge ("Pure R workflows" without single quotes / abbreviation NLP, which 
does not refer to software is omitted).


### issue 2

Martina observed: "You have examples for unexported functions which cannot run in this way.
Please either add packagename::: to the function calls in the examples,
omit these examples or export these functions in as.Annotation.Rd"

Reconsidering the function, I dropped it and its documentation from the package
entirely, because I realized that there is already a better implementation
in the polmineR package anyway. 

Thanks for your feedback and care!


## Test environments

* local OS X install, R 3.6.1
* Ubuntu 14.04 (on travis-ci), R 3.6.1
* Ubuntu 14.04 (project server), R 3.4.3
* win-builder (devel and release), R. 3.6.1
* Windows/AppVeyor, R 3.5.0


## R CMD check results

There were no ERRORs, WARNINGs or NOTEs on the Linux / macOS environments I used. 


## Downstream dependencies

Checking not necessary at this time.

