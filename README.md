<img src="logo/mixchar_logo.png" width="100%" />

[![Build Status](https://travis-ci.com/smwindecker/mixchar.svg?branch=master)](https://travis-ci.org/smwindecker/mixchar) [![codecov](https://codecov.io/gh/smwindecker/mixchar/branch/master/graph/badge.svg)](https://codecov.io/gh/smwindecker/mixchar) [![repo status](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)

Why deconvolution?
------------------

Ecologists are often interested in determining the composition of plant cell wall biomass, primarily hemicellulose, cellulose, and lignin carbon types. Proportions of these carbon types are useful for estimating kinetic decay parameters or for calculation of intrinsic plant traits. Traditional methods for calculation of these components involve wet chemistry methods that can be monetarily and environmentally costly. Thermogravimetric analysis (TGA) is an alternative method, already used in the biofuel field, to approximate these carbon compounds from mass loss data obtained by heating a biomass sample (in a *N*<sub>2</sub> environment, termed pyrolysis).

To estimate carbon components from this mass loss data we have to take the derivative, to find the rate of mass loss across temperature, and then use a mixture model to separate the multi-peaked derivative curve into constituent parts. This process is called deconvolution, and is typically conducted in proprietary peak-fitting softwares, which, while effective, are not open source. This package seeks to provide a platform for reproducible deconvolution using R. This methodology has been tested on a range of plant litter composed primarily of soluble carbohydrates, hemicellulose, cellulose, and lignin. For more details (link paper here).

<hr>
Installation
------------

You can install `mixchar` from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("smwindecker/mixchar", build_vignettes = TRUE)
```

Basic use
---------

Data from thermogravimetric analysis is usually exported as mass loss with temperature. An example dataset for the species *Juncus amabilis* is included in the package:

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

We can use the function `process()` to take the derivative of this mass loss data, resulting in rate of mass loss over temperature data. To do so you simply need to specify the dataset, identify which columns contain temperature and mass loss data, and provide a value for the starting mass of the sample. The function defaults to temperature data in Celsius, but you can also modify to indicate the data is provided in Kelvin, by adding `temp_type = 'K'`.

``` r
tmp <- process(juncus, temp = 'temp_C', mass = 'mass_loss', init_mass = 18.96)
tmp
#> Derivative thermogravimetry data (DTG) calculated for
#>  768 datapoints from 31.5 to 798.52 degrees C.
```

The default plot function for the derivative data shows you both the original mass loss curve and the derivative rate of mass loss curve.

Processed data then simply needs to be deconvolved into its constituent parts. The deconvolve function takes care of this step, by cropping the derivative data to exclude dehydration and then running the Fraser-Suzuki mixture model and estimating individual curve parameters and weights.

``` r
output <- deconvolve(tmp)
```

Although most biomass samples have only three main components (corresponding to hemicellulose, cellulose, and lignin), some have a second hemicellulose curve in the low temperature range. The function will decide whether three or four curves are best, but you can override it by modifying the `n_curves` argument. The function also has built in starting values for the nonlinear optimisation. If you'd like to modify those or the upper and lower bounds for the estimates, you can also do so with the `start_vec`, `lower_vec`, and `upper_vec` arguments to `deconvolve()`.

The `deconvolve()` function results in a variety of outputs. You can use a variety of accessor functions to look at these. `component_weights()` will display the weights of each carbon component, `rate_data()` will show you the modified dataset used for fitting, `model_fit()` will show you the model fit, and `temp_bounds()` will print the temperature values at which the data were cropped for analysis. You can also plot the resulting output using the default plotting function.

You can see the full worked example in the vignette, which you can access on the or in R:

``` r
vignette('mixchar')
#> Warning: vignette 'mixchar' not found
```

This is still a work in progress! If you see any mistakes, or find that the code is not functioning well on your data, let us know by logging a bug on the [issues page](http://www.github.com/smwindecker/mixchar/issues).

Acknowledgements
----------------

Thank you to Nick Tierney for valuable feedback during development. Thanks to the Holsworth Wildlife Reseach Endowment & The Ecological Society of Australia for support on this project.
