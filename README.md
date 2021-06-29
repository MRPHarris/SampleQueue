
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SampleQueue

<p align="center">
<img src="man/figures/SampleQ logo 1.png" height="150px" />
</p>
<!-- badges: start -->
<!-- badges: end -->

The **SampleQueue** package is a collection of functions and scripts
designed to act as a workflow for the Horiba Aqualog
Spectrofluorometer’s sample queuing system. Aqualog sample queuing
offers an attractive alternative to standard single-sample analysis and
processing tasks, automatically processing and correcting sample outputs
during rapid, back-to-back analysis. Moreover, sample queuing avoids
some of the classic pitfalls of the Aqualog’s capability - namely, host
computer processing slowdown as the user’s project folder grows in size.

Unfortunately, the sample queue only outputs files with a restrictive
file naming convention - a basic collation of three
words/suffixes/prefixes, along with sequential digits. This package
provides a workaround, and allows the user to compile more complicated
runs using the sample queueing function.

In a basic workflow, the user compiles a ‘run sheet’ - a basic
spreadsheet containing the predicted sample queue names, and the user’s
own desired file names. Once the analysis is complete, the user
processes the imported sample queue files using the run sheet, exporting
the processed files (Absorbance data, Processed Excitation-Emission
Matrices, Workbook files) with their ‘real’ names.

After running a **SampleQueue** workflow, the user should be left with a
collection of appropriately named and catalogued files that can then be
indexed and interrogated with the existing R fluorescence analysis
framework provided by the
[eemR](https://cran.r-project.org/web/packages/eemR/index.html),
[staRdom](https://github.com/MatthiasPucher/staRdom) and
[EEM](https://CRAN.R-project.org/package=EEM) packages.

This package is a work in progress. At the moment, I only support ABS
(absorbance), PEM (processed EEMs) and .ogw (workbook) files as part of
the workflow and functions.

If you have any questions or comments, please don’t hesitate to get in
touch.

## Installation

To get access to the functions in **SampleQueue**, simply use the
**devtools** package to install the package from github.

``` r
devtools::install_github("MRPHarris/SampleQueue")
```

## References

Massicotte, P. (2019). eemR: Tools for Pre-Processing
Emission-Excitation-Matrix (EEM) Fluorescence Data. R package version
1.0.1. <https://CRAN.R-project.org/package=eemR>

Murphy, K. R., Stedmon, C. A., Graeber, D., & Bro, R. (2013).
Fluorescence spectroscopy and multi-way techniques. PARAFAC. *Analytical
Methods*, *5*, 6557–6566. doi:
[10.1039/C3AY41160E](https://doi.org/10.1039/C3AY41160E)

Pucher, M., Wünsch, U., Weigelhofer, G., Murphy, K., Hein, T., &
Graeber, D. (2019). staRdom: Versatile Software for Analyzing
Spectroscopic Data of Dissolved Organic Matter in R. *Water*, *11*,
2366. doi: [10.3390/w11112366](https://doi.org/10.3390/w11112366)

Trivittayasil, V. (2016). EEM: Read and Preprocess Fluorescence
Excitation-Emission Matrix (EEM) Data. R package version 1.1.1.
<https://CRAN.R-project.org/package=EEM>
