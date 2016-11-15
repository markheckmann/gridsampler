# gridsampler

[![Build Status](https://travis-ci.org/markheckmann/gridsampler.svg?branch=master)](https://travis-ci.org/markheckmann/gridsampler)

### Introduction

A common approach for evaluating multiple repertory grids is to sort the elicited constructs into categories using content analysis (e.g. Green, 2004; Jankowicz, 2004). Sometimes the question arises what sample size would be needed in order to achieve a minimum saturation of a certain number of constructs per category. The gridsampler GUI allows to run simulations in order to answer these type of questions.

An introduction to its usage can be found on [GitHub pages](http://markheckmann.github.io/gridsampler/). Please report any bugs [here](https://github.com/markheckmann/gridsampler/issues).

### Installation 

You can install the software from CRAN:

```r
install.packages("gridsampler")
```

or from [GitHub](https://github.com/markheckmann/gridsampler):

```r
if (!("devtools" %in% installed.packages())) {
  install.packages("devtools")
} else {
  devtools::install_github("markheckmann/gridsampler")
}
```

To run it, simply type this:

```r
gridsampler()

# Or run it in a browser window (recommended):
gridsampler(launch.browser = "TRUE")
```

### Citation

If you use gridsampler in your publications, you can cite it as follows. 
 
Heckmann, M . & Burk, L. (2016). gridsampler: A simulation tool to determine the required sample size for repertory grid studies. R package version 0.5. *Zenodo*. doi:10.5281/zenodo.61067

[![DOI](https://zenodo.org/badge/doi/10.5281/zenodo.61067.svg)](http://dx.doi.org/10.5281/zenodo.61067)


### References

+ Green, B. (2004). Personal construct psychology and content analysis. *Personal Construct Theory & Practice, 1*(3), 82–91.
+ Jankowicz, D. (2004). *The easy guide to repertory grids*. Chichester, England: John Wiley & Sons.
