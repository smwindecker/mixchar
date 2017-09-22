[![Build Status](https://travis-ci.org/smwindecker/deconvolve.svg?branch=master)](https://travis-ci.org/smwindecker/deconvolve)
[![codecov.io](https://codecov.io/gh/smwindecker/deconvolve/branch/master/graph/badge.svg)](https://codecov.io/gh/smwindecker/deconvolve)

# deconvolve
Repository for the R package 'deconvolve'

This package was designed so that there would be a reproducible method for deconvolving derivative thergravimetric data into pseudo-components. 

Thermogravimetric Analysis (TGA) is a method to estimate biomass fractions. After pyrolysing dried samples in the machine, a derivative curve is calculated. This curve is deconvolved using the three-part Fraser-Suzuki mixture model. 

It is designed for anyone to use, but will only work, for now, for material that needs to be deconvolved into three portions corresponding to hemicellulose, cellulose, and lignin. 

