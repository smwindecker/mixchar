[![Build Status](https://travis-ci.org/smwindecker/deconvolve.svg?branch=master)](https://travis-ci.org/smwindecker/deconvolve)
[![codecov](https://codecov.io/gh/smwindecker/deconvolve/branch/master/graph/badge.svg)](https://codecov.io/gh/smwindecker/deconvolve)

# deconvolve
Repository for the R package 'deconvolve'

This package provides an easy and straightforward way to quantify proportions of lignocellulosic biomass components (typically considered hemicellulose, cellulose, and lignin) from the mass loss data generated during thermogravimetric analysis (TGA). 

Proportions of the main carbon types in litter can be used as functional traits to understand ecosystem processes such as litter decomposition, or to aid in calculation of kinetic decay constants. Thermogravimetric analysis is the process of pyrolysing dry, ground plant litter, and results in mass loss data across a temperature range. Since different carbon types break down during different temperature phases, we can separate the multi-peaked rate of mass loss curve into independent decay phases and estimate proportions of each carbon type by integrating under their respective individual decay curve. This package conducts this analysis in an open-source and reproducible fashion. 

Data from thermogravimetric analysis is usually exported as mass loss with temperature. An example dataset for the species *Juncus usitatus* is included in the package:
```{r}
library(deconvolve)
data(juncus)
```

We can then use the function `process()` to take the derivative of this mass loss data, resulting in rate of mass loss over temperature data. To do so you simply need to specify the dataset, identify which columns contain temperature and mass loss data, and provide a value for the starting mass of the sample. The function defaults to temperature data in Celsius, but you can also modify to indicate the data is provided in Kelvin, by adding `temp_type = 'K`. 
```{r}
tmp <- process(juncus, temp_col = 'temp_C', massloss_col = 'mass_loss', init_mass = 16.85)
```

The default plot function for the derivative data shows you both the original mass loss curve and the derivative rate of mass loss curve. 
```{r}
plot(tmp)
```

Processed data then simply needs to be deconvolved into its constituent parts. The deconvolve function takes care of this step, by cropping the derivative data to exclude dehydration and then running the Fraser-Suzuki mixture model and estimating individual curve parameters and weights. 
```{r}
output <- deconvolve(tmp)
```

Although most biomass samples have only three main components (corresponding to hemicellulose, cellulose, and lignin), some have a second hemicellulose curve in the low temperature range. The function will decide whether three or four curves are best, but you override it by modifying the `ncurves` argument. The function also has built in starting values for the nonlinear optimisation. If you'd like to modify those or the upper and lower bounds for the estimates, you can also do so with the `start_vec`, `lower_vec`, and `upper_vec` arguments to `deconvolve()`. You can plot the resulting output using the default plotting function. 
```{r}
plot(output)
```

Thanks to the Holsworth Wildlife Reseach Endowment & The Ecological Society of Australia for support on this project. Please get in touch or log an issue if you are having any trouble. 

