
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SampleQueue

<p align="center">
<img src="man/figures/sample queue logo v1.png" height="150px" />
</p>
<!-- badges: start -->
<!-- badges: end -->

The **SampleQueue** package is a streamlined file processing/handling
framework for the Horiba
[Aqualog](https://www.horiba.com/en_en/products/detail/action/show/Product/aqualog-water-treatment-plant-analyzer-1578/)
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
Example001Blank). This makes managing outputs extremely cumbersome.

This package provides a workaround, allowing the user to compile complex
runs using SampleQ via the creation of a ‘run sheet’, in a fashion that
should be familiar to users of other scientific instruments. Even using
run setups incorporating multiple blanks, standards and replicates, the
combination of SampleQ and **SampleQueue** allows for rapid sample
throughput, followed by easy management of the resulting data files.

##### A quick note on ASCII data types

At present **SampleQueue** supports the Processed
Excitation-Emission-Matrix (PEM) and Absorbance (ABS) ASCII .dat file
types. See ‘Supported file and data types’ below. Support for more file
types will be added shortly.

## Using the SampleQueue package

The basic logic comprising the **SampleQueue** workflow is illustrated
below.

<p align="center">
<img src="man/figures/workflow example v1.png" height="850px" />
</p>

Setting up the folder framework used by **SampleQueue** is performed by
choosing a parent directory and running `create_queue_folders()`. This
will fill that folder with all the sub-directories used by the package.

In a standard workflow, the user then compiles a ‘run sheet’. A run
sheet is comprised of a basic table containing a given run’s sample
queue names (i.e. the naming convention that will be used by the Aqualog
during analysis), the user’s own desired file names, and one of a number
of supported row-file ‘types’ (e.g. ‘sample’, “mqblank”, “sqblank”,
“standard”, “replicate”). An example is shown below.

``` r
data_example <- readRDS(file = "data/run_sheet_example.rds")
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

Column names must match those shown in the example in order for the
package to work. The “Real\_Names” column can include any combination of
characters or digits reflecting your desired file naming convention. The
‘types’ determine categorisation and file sorting - for example, files
associated with run sheet rows marked ‘standard’ will be sorted and sent
to the standards folder. The checklist column is optional - I use it
whilst running samples on the Aqualog to ensure there is no chance of a
mix-up during the course of an analysis.

Once analysis using SampleQ on the Aqualog is completed, the user
collates the files together in a single folder along with any project
(.opj) and text files (.txt). This folder should then be placed into to
the **SampleQueue** import directory.

After this, running the function `process_sample_queue()` with the
appropriate parameter inputs (correct run sheet, etc.) will
automatically rename and copy all the files to the appropriate folders
within the **SampleQueue** export folder. An option is included for
milli-q blank subtraction.

After running a **SampleQueue** workflow, the user should be left with a
collection of appropriately named and sorted files that can then be
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
together into a ‘log file’. The run sheet is also appended during this
process. Typically .txt files in the imported folder will be limited to
the Aqualog-produced log (AqualogSampleQLog.txt), but any user-generated
.txt files will also be included provided they are in the same folder as
all the other data files. I typically include a small text file that
contains my notes about the run and the calibration values used at the
time of run, including the normalisation value (e.g. RU norm factor, QSU
norm factor).

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
(renamed in line with the run sheet) to an export folder matching the
sample type.

### ASCII .dat files

Depending on user choices during the SampleQ setup process on the
Aqualog, a number of different ASCII file types will be exported by the
system for each sample during analysis. All ASCII files have the .dat
file extension. The current version of **SampleQueue** supports the ABS
(absorbance data) and PEM (sample-blank processed XYY) ASCII data types.
The others (e.g. % transmission PCT, blank XYY BEM) will be added in a
subsequent release.

## Installation

To get access to the functions in **SampleQueue**, simply use the
**devtools** package to install the package from github.

``` r
devtools::install_github("MRPHarris/SampleQueue")
```

**SampleQueue** has a number of package dependencies. These extend from
string handling (e.g. stringr) to a number of packages required for
blank subtraction
([eemR](https://cran.r-project.org/web/packages/eemR/index.html),
[staRdom](https://github.com/MatthiasPucher/staRdom), and my own package
[eemUtils](https://github.com/MRPHarris/eemUtils)). Check the
[DESCRIPTION](DESCRIPTION) file for a list of the dependencies. They
should all (hopefully) be fetched automatically when you install and
load **SampleQueue**.

## Planned revisions

-   update error checking to provide more useful information in the
    event of (1) file copy failure, and (2) project file transfers

-   add handling for all ASCII data types.

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
