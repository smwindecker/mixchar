## Resubmission
This is a resubmission. In this version I have:

* added references for the methodology in the description file

* replaced \dontrun{} with \donttest{} for example
chunks that take >5 sec to run, as requested.

* fixed yaml in deconvolve.R in attempt to fix 'unexecutable' error
in man/deconvolve.Rd file.

## Test environments
* OS X (on travis-ci) - release and oldrelease
* ubuntu 14.04 (on travis-ci) - devel, release, and oldrelease
* win-builder - devel, release, and oldrelease

## R CMD check results
There were no ERRORs or WARNINGs.

2 NOTES
Maintainer: 'Saras Windecker <saras.windecker@gmail.com>'
New submission

Examples with CPU or elapsed time > 5s
                    user system elapsed
deconvolve        19.240  0.753  20.251
component_weights  9.987  0.562  10.682
model_parameters   9.775  0.509  10.423
model_fit          9.691  0.428  10.234

## Downstream dependencies
I have used devtools::revdep_check() and no ERRORs or WARNINGs were found.
