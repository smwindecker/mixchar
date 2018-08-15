<img src = "man/figures/logo.png" height="80" width="350" />
============================================================

[![Build
Status](https://travis-ci.com/smwindecker/mixchar.svg?branch=master)](https://travis-ci.org/smwindecker/mixchar)
[![codecov](https://codecov.io/gh/smwindecker/mixchar/branch/master/graph/badge.svg)](https://codecov.io/gh/smwindecker/mixchar)
[![repo
status](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![DOI](https://zenodo.org/badge/103010631.svg)](https://zenodo.org/badge/latestdoi/103010631)

Why deconvolution?
==================

Plant cell wall biomass is composed of a range of different types of
carbon. Proportions of primary carbon types are useful for estimating
kinetic decay parameters or for calculation of intrinsic plant traits.
Traditional methods for calculation of these components involve wet
chemistry methods that can be monetarily and environmentally costly.
Thermogravimetric analysis is an alternative method, already in use in
the biofuel field, that involves pyrolysing dry, ground plant litter and
estimating components from resulting mass decay peaks. Since different
carbon types break down relatively independently during different
temperature phases, we can separate the multi-peaked rate of mass loss
curve into constituent parts using a mixture model. This package
conducts this peak separation analysis in a open-source and reproducible
way using R. This methodology has been tested on a range of plant litter
composed primarily of soluble carbohydrates, hemicellulose, cellulose,
and lignin.

Installation
============

You can install `mixchar` from [GitHub](https://github.com/) with:

``` r
install.packages("devtools")
devtools::install_github("smwindecker/mixchar")
```

Below we will show a basic implementation of the package. For a detailed
step by step workflow, see the [workflow vignette on the
website](http://smwindecker.github.io/mixchar/articles/mixchar.html).

For a detailed discussion of the methodology, please see the
[methodology vignette on the
website](http://smwindecker.github.io/mixchar/articles/Background.html).

Basic use
=========

Data from thermogravimetric analysis is usually exported in one of two
forms: as mass loss or mass remaining (mg) by temperature. An example
dataset that contains mass loss data for the species *Juncus amabilis*
is included in the package:

``` r
library(mixchar)
head(juncus)
#>   temp_C mass_loss
#> 1 31.453 -0.000931
#> 2 31.452 -0.001340
#> 3 31.450 -0.001350
#> 4 31.450 -0.001660
#> 5 31.450 -0.001680
#> 6 31.450 -0.001800
```

We can use the function `process()` to take the derivative of this data,
resulting in rate of mass loss over temperature data. To do so you need
to specify the dataset, the initial mass of sample, the name of the
temperature data column, and the name of your mass loss or mass data
column. If you have mass loss data you can specify with `mass_loss`
argument, if it’s mass remaining data you can use the `mass` argument.
You only need to provide one. The function defaults to temperature data
in Celsius, but you can also indicate the data is provided in Kelvin, by
adding `temp_units = 'K'`.

``` r
tmp <- process(juncus, 
               init_mass = 18.96, 
               temp = 'temp_C', 
               mass_loss = 'mass_loss')
```

The default plot function for the derivative data shows you both the
mass with temperature curve and the derivative rate of mass loss curve.

Processed data then needs to be deconvolved into its constituent parts.
The deconvolve function takes care of this step, by cropping the
derivative data to exclude dehydration and then running the
Fraser-Suzuki mixture model and estimating individual peak parameters
and weights.

``` r
output <- deconvolve(tmp)
```

Although most biomass samples have only three main components
(corresponding to hemicellulose, cellulose, and lignin), some have a
second hemicellulose peak in the low temperature range. The function
will decide whether three or four peaks are best, but you can override
it by modifying the `n_peaks` argument. The function also has built in
starting values for the nonlinear optimisation. If you’d like to modify
those or the upper and lower bounds for the estimates, you can also do
so with the `start_vec`, `lower_vec`, and `upper_vec` arguments to
`deconvolve()`.

The `deconvolve()` function results in a variety of outputs. You can use
a accessor functions to look at these. `component_weights()` will
display the weights of each carbon component, `rate_data()` will show
you the modified dataset used for fitting, `model_fit()` will show you
the model fit, and `temp_bounds()` will print the temperature values at
which the data were cropped for analysis. You can also plot the
resulting output using the default plotting function, which can be in
black and white or in colour.

Contribution
============

This is still a work in progress! If you see any mistakes, or find that
the code is not functioning well on your data, let us know by logging a
bug on the [issues
page](http://www.github.com/smwindecker/mixchar/issues).

Acknowledgements
================

Thank you to Nick Tierney and David Wilkinson for valuable feedback
during development. Thanks to the Holsworth Wildlife Reseach Endowment &
The Ecological Society of Australia for support on this project.
