
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SampleQueue

<p align="center">
<img src="man/figures/SampleQ logo 1.png" height="150px" />
</p>
<!-- badges: start -->
<!-- badges: end -->

The **SampleQueue** package is a streamlined set of functions designed
to act as a file processing framework for the Horiba Aqualog
spectrofluorometer’s sample queuing system. Sample queuing potentially
offers an attractive alternative to standard single-sample analysis and
processing tasks, automatically processing and correcting sample outputs
during rapid, back-to-back analysis. Moreover, sample queuing avoids
some of the classic pitfalls of the Aqualog’s capability - namely, host
computer processing slowdown as the user’s project folder grows in size.

Unfortunately, the sample queue only outputs files with a restrictive
file naming convention - a basic collation of three
words/suffixes/prefixes, along with sequential digits
(e.g. Example001Sample0001). This package provides a workaround, and
allows the user to compile more complicated runs using the sample
queueing function in a fashion that is similar to other scientific
systems.

In a simple workflow, the user compiles a ‘run sheet’ - a basic table
containing the predicted sample queue names, the user’s own desired file
names, and one of a number of row-file ‘types’. Once the analysis is
complete, the user processes the imported sample queue files using the
run sheet, exporting the processed files (Absorbance data, Processed
Excitation-Emission Matrices, Workbook files) with their ‘real’ names.
With correct set-up, this is performed by the single function
`process_sample_queue()`. Setting up the folder framework within the
desired directory is done using `create_queue_folders()`.

After running a **SampleQueue** workflow, the user should be left with a
collection of appropriately named and catalogued files that can then be
indexed and interrogated with the existing R fluorescence analysis
framework provided chiefly by the
[eemR](https://cran.r-project.org/web/packages/eemR/index.html),
[staRdom](https://github.com/MatthiasPucher/staRdom) and
[EEM](https://CRAN.R-project.org/package=EEM) packages.

If you have any questions or comments, please don’t hesitate to get in
touch.

## Supported file and data types

When running samples with sample queue, the user exports to a specified
directory. The following file types are extracted from this directory by
the **SampleQueue** package.

### .txt files

Text files are collated together using the `generate_logfile()`
function. This operates by identifying all the text files in the
specified folder, importing them with `readLines()`, then combining them
together into a ‘log file’. The run sheet is also appended. Typically
.txt files in the imported folder will be limited to the
Aqualog-produced log (AqualogSampleQLog.txt), but any user-generated
.txt files will also be included provided they are in the same folder as
all the other data files.

### .opj project files

The standard way of using the Aqualog software is to work in a project
file, which features the .opj file extension. Any and all project files
in the imported folder are copied (renamed) to the relevant folder in
the export directory, with the imported folder name appended to the
project file name if it wasn’t already.

### .ogw workbook files

If the user has chosen to export workbook files with their data whilst
running the Aqualog sample queue, their chosen folder will be populated
with .ogw workbook files for each sample/blank. These files can be
loaded back into the Aqualog software and track all processing steps for
each file. As with the ASCII .dat files, each .ogw file is copied
(renamed) to a suitable export folder matching the sample type.

### ASCII .dat files

The current live package supports the ABS (absorbance data) and PEM
(sample-blank processed XYY) ASCII .dat data types. The others (e.g. %
transmission PCT, blank XYY BEM) will be added in a subsequent release.

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
