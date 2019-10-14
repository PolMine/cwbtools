## General remarks

The 'cwbtools' package concentrates and consolidates a set of functions and
classes that were previously included in the 'polmineR' and the 'RcppCWB'
package, or that were scattered across packages (I wrote) that were only
available via GitHub.

I received some advice from Swetlana Herbrandt following my first submission
of the package detailed in a somewhet abbreviated fashion below. See my replies
on the changes I made to comply.

_Issue 1_: [...] please add a few more details about the functionality of this
package. If there are references describing the (theoretical background of)
methods in your package, please add these in the Description field of
your DESCRIPTION file [...]

**Reply:** The Description field of the DESCRIPTION file has been reworked and
expanded. The entry in the manual for the whole package is an extended version 
of this description, with further references.

_Issue 2_: Please do not change the working directory in your functions. If you
really have to, please ensure with an immediate call of on.exit() that
the the old working directory is reset.

**Reply:** I fully understand the point. The working directory is reset in the file 'corpus.R'
before a call of `tar()`. The tarball that needs to be prepared needs to maintain
the directory structure in the (temporary) archive_dir. I cannot find a way
how to do this without setting the working directory temporarily to the archive_dir.
Using the argument `files` of `tar()` will not have the desired result (i.e. not
maintain the required directory structure). Following your advice, there is the following 
call of `on.exit()`. So this is the relevant code now:

```r
old_wd <- getwd()
on.exit(setwd(path.expand(old_wd)))
setwd(archive_dir)
tar(tarfile = tarfile, compression = "gzip")
setwd(path.expand(old_wd))
```

Hope this is ok.

_Issue 3_: You are using installed.packages(): "This needs to read several files per
installed package, which will be slow on Windows and on some network-mounted
file systems. It will be slow when thousands of packages are installed, so do
not use it to find out if a named package is installed (use find.package or
system.file) nor to find out if a package is usable (call requireNamespace or
require and check the return value) nor to find details of a small number of
packages (use packageDescription)." [installed.packages() help page]

**Reply:** I replaced all uses of `installed.packages()` to find out whether a package
is installed by `system.file()`. The auxiliary function `corpus_packages()` 
explores the directory structure of all packages whether the directory structure
meets certain requirements. To speed things up, I rely on a combination of
`list.files()` and `system.file()` now, to keep the number of files evaluated to a
minimum. I fully understand that this may be slow on some systems. As it is an auxiliary
function at the margins of the package, it could be removed. But for the very
same reason I thought I would leave it in the package if you do not object.
For the time being, I added a warning on performance to the documentation.

_Issue 4_: Please ensure that you do not install software in your examples, tests
or vignettes and rather write or return a message what and how to install.

**Reply:** The `cwb_install()` function had been wrapped in \dontrun{} sections and not been 
executed in the version of cwbtools I submitted. To make it more explicit when and
how you might want to call `cwb_install(), I added an explaining comment and removed
the code wrapped in \dontrun{}.

Thanks a lot for your scrutiny and for your advice!


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

