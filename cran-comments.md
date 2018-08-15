## Resubmission
This is a resubmission. In this version I have:

* added references for the methodology in the description file

* replaced \dontrun{} with \donttest{} for example
chunks that take >5 sec to run.

* fixed yaml in deconvolve.R in attempt to fix 'unexecutable' error
in man/deconvolve.Rd file.

## Test environments
* OS X (on travis-ci) - release and oldrelease
* ubuntu 14.04 (on travis-ci) - devel, release, and oldrelease
* win-builder - devel, release, and oldrelease

## R CMD check results
There were no ERRORs or WARNINGs.

NOTE
Maintainer: 'Saras Windecker <saras.windecker@gmail.com>'
New submission

## Downstream dependencies
I have used devtools::revdep_check() and no ERRORs or WARNINGs were found.
