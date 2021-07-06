
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SampleQueue

<p align="center">
<img src="man/figures/sample queue logo v1.png" height="150px" />
</p>
<!-- badges: start -->
<!-- badges: end -->

The **SampleQueue** package is a streamlined set of functions that act
as a file processing framework for the Horiba Aqualog
spectrofluorometer’s sample queuing (“SampleQ”) system. During SampleQ,
the Aqualog software automatically processes and corrects sample data
based upon pre-set parameters, rapidly performing tasks that would
typically take far longer to do for individual samples (such as
correction for inner filter effects and Rayleigh masking).

SampleQ also avoids some of the classic pitfalls of the Aqualog’s
capability, including host computer processing slowdown as the user’s
project folder grows in size.

Unfortunately, the SampleQ system only outputs files using a restrictive
file naming convention, comprising a basic combination of
prefixes/suffixes and sequential digits (e.g. Example001Sample0001 or
Example001Blank).

This package provides a workaround, allowing the user to compile complex
runs using SampleQ via the creation of a ‘run sheet’, in a fashion that
should be familiar to users of other scientific instruments. Even using
run setups incorporating multiple blanks, standards and replicates, the
combination of SampleQ and the **SampleQueue** package allows for sample
throughput exceeding 100 samples per day under ideal circumstances
(minimal sample pre-treatment, etc.).

## Using the SampleQueue package

The basic logic comprising the **SampleQueue** package workflow is
illustrated below.

<p align="center">
<img src="man/figures/workflow example v1.png" height="850px" />
</p>

Setting up the folder framework used by the **SampleQueue** package is
performed by choosing a parent directory and running
`create_queue_folders()`.

In a standard workflow, the user then compiles a ‘run sheet’. A run
sheet is comprised of a basic table containing the predicted sample
queue names, the user’s own desired file names, and one of a number of
supported row-file ‘types’ (e.g. ‘sample’, “mqblank”, “sqblank”,
“standard”, “replicate”). An example is shown below. Column names must
match those shown in the example in order for the package to work. The
“Real\_Names” column can include any combination of characters or digits
reflecting your desired file naming convention. The checklist column is
optional - I use it whilst running samples on the Aqualog to ensure
there is no chance of a mix-up during the course of an analysis.

``` r
data_example <- read.table(file = "data/run_sheet_example.txt", sep = "\t", header = TRUE)
knitr::kable(data_example)
```

| Order | Remaining | SampleQ\_Name         | Real\_Name           | Type      | Checklist |
|------:|----------:|:----------------------|:---------------------|:----------|:----------|
|     1 |        10 | Example0001Blank      | swc\_sqblank\_050721 | sqblank   | NA        |
|     2 |         9 | Example0001Sample0001 | mqblank\_050721a     | mqblank   | NA        |
|     3 |         8 | Example0001Sample0002 | mqblank\_050721b     | mqblank   | NA        |
|     4 |         7 | Example0001Sample0003 | Sample\_1            | sample    | NA        |
|     5 |         6 | Example0001Sample0004 | Sample\_2            | sample    | NA        |
|     6 |         5 | Example0001Sample0005 | Sample\_3            | sample    | NA        |
|     7 |         4 | Example0001Sample0006 | Sample\_4            | sample    | NA        |
|     8 |         3 | Example0001Sample0007 | Sample\_5            | sample    | NA        |
|     9 |         2 | Example0001Sample0018 | StandardA\_050721a   | standard  | NA        |
|    10 |         1 | Example0001Sample0019 | Sample\_1\_re        | replicate | NA        |
|    11 |         0 | Example0001Sample0020 | mqblank\_050721c     | mqblank   | NA        |

Once analysis using SampleQ on the Aqualog is completed, the user
collates the files together in a folder with any project (.opj) and text
files (.txt) into a single folder, and moves this folder to the
**SampleQueue** import directory.

After this, running the function `process_sample_queue()` with the
appropriate parameter inputs (correct run sheet, etc.) will
automatically rename and copy all the files to the appropriate folders
within the **SampleQueue** export folder.

After running a **SampleQueue** workflow, the user should be left with a
collection of appropriately named and catalogued files that can then be
indexed and interrogated with the existing R fluorescence analysis
framework provided chiefly by the
[eemR](https://cran.r-project.org/web/packages/eemR/index.html),
[staRdom](https://github.com/MatthiasPucher/staRdom) and
[EEM](https://CRAN.R-project.org/package=EEM) packages.

If you have any questions or comments, please don’t hesitate to get in
touch.

This package is a work in progress. **Always back up your data!**

## Supported file and data types

The following file types are extracted from the designated folder by the
**SampleQueue** package.

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
the export directory using `transfer_project_files()`, with the imported
folder name appended to the project file name if it wasn’t already.

### .ogw workbook files

If the user has chosen to export workbook files with their data whilst
running the Aqualog sample queue, their chosen folder will be populated
with .ogw workbook files for each sample/blank. These files can be
loaded back into the Aqualog software to track all processing steps for
each file. As with the ASCII .dat files, each .ogw file is copied
(renamed) to a suitable export folder matching the sample type.

### ASCII .dat files

Depending on user choices during the SampleQ setup process on the
Aqualog, a number of different ASCII file types will be exported by the
system for each sample during analysis. All ASCII files have the .dat
file extension. The current version of the **SampleQueue** package
supports the ABS (absorbance data) and PEM (sample-blank processed XYY)
ASCII data types. The others (e.g. % transmission PCT, blank XYY BEM)
will be added in a subsequent release.

## Installation

To get access to the functions in **SampleQueue**, simply use the
**devtools** package to install the package from github.

``` r
devtools::install_github("MRPHarris/SampleQueue")
```

## Planned revisions

-   update error checking to provide more useful information in the
    event of (1) file copy failure, and (2) project file transfers

-   add handling ability for all ASCII data types.

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
