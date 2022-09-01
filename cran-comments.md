## General remarks

The package has been removed from CRAN in July due to an ERROR resulting from a
broken link for test data.

Submission of v0.3.7 offered a workaround for functionality of the zen4R package 
that is currently not working, but it could not be released due to shortcomings.
This release addresses feedback by Beni Altmann. 

This is how I address the feedback:

(a) Write package names, software names and API names in single quotes in title
and description: Implemented (see DESCRIPTION file).

(b) Write references in the description of the DESCRIPTION file in
the form authors (year) <doi:...>: Implemented.

(c) Ensure that functions do not write anywhere but the temporary session
directory: I checked any path I could find. To improve the readability of the 
code, I moved to `fs::path()` throughout.

(d) Please do not install packages in your functions, examples or vignette: I
replaced a remaining call of `install.packages()`, changing the functionality of
`corpus_install()`. I also moved two vignettes ('sentences.Rmd' and
'opennlp.Rmd') with unexecuted sample code to a cookbook/recipe repository,
which is more reasonable anyway.

I hope it is now easier to see that there is nothing remaining that violates
CRAN policies. 

Thanks a lot for your scrutiny and support! 

Andreas 


## Test environments

* local macOS install, R 4.1.3
* GitHub Actions (Ubuntu 20.04, macOS, Windows) R 4.2.1
* win-builder (devel and release, R. 4.2.1)

## R CMD check results

On R winbuilder I see a NOTE on possibly misspelled words in the DESCRIPTION
file ("CWB" and "Hardie"), but that is correct.

There were no ERRORs or WARNINGs on the Linux / macOS / Windows environments I used. 


## Downstream dependencies

None at this stage.

