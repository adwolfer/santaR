
# santaR
[![R-CMD-check](https://github.com/adwolfer/santaR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/adwolfer/santaR/actions/workflows/R-CMD-check.yaml)
[![R-CMD-check-devel](https://github.com/adwolfer/santaR/actions/workflows/R-CMD-check-devel.yaml/badge.svg)](https://github.com/adwolfer/santaR/actions/workflows/R-CMD-check-devel.yaml)
[![codecov](https://codecov.io/gh/adwolfer/santaR/branch/master/graph/badge.svg)](https://codecov.io/gh/adwolfer/santaR/branch/master)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/santaR)](https://cran.r-project.org/package=santaR)

Interactive package for *Short AsyNchronous Time-series Analysis*
(SANTA), implemented in `R` and `Shiny`

## Overview


[**santaR**](https://adwolfer.github.io/santaR/) is an R package that implements functions for analysis of short asynchronous time-series analysis.

`santaR` can deal with challenges not simultaneously addressed by current time-series statistical methods:
 - missing observations
 - asynchronous sampling
 - measurement error
 - low number of time points (*e.g. 4 to 10*)
 - high number of variables
 - biological variability
 - nonlinearity
 
The reference versions of `santaR` is available on [CRAN](https://cran.r-project.org/package=santaR).
Active development and issue tracking take place on the [github page](https://github.com/adwolfer/santaR/tree/master), while an overview of the package, vignettes and documentation are available on the [supporting website](https://adwolfer.github.io/santaR/).

To address the challenges of time-series in Systems Biology, `santaR` (*Short AsyNchronous Time-series
Analysis*) provides a Functional Data Analysis (*FDA*) approach -*where
the fundamental units of analysis are curves representing each
individual across time*-, in a graphical and automated pipeline for
robust analysis of short time-series studies.

Analytes levels are descriptive of the underlying biological state and
evolve smoothly through time. For a single analyte, the time trajectory
of each individual is described with a smooth curve estimated by
smoothing splines. For a group of individuals, a curve representing the
group mean trajectory is also calculated. These individual and group
mean curves become the new observational unit for subsequent data
analysis, that is, the estimation of the intra-class variability and the
identification of trajectories significantly altered between groups.

Designed initially for metabolomic, `santaR` is also suited for other
Systems Biology disciplines. Implemented in `R` and `Shiny`, `santaR` is
developed as a complete and easy-to-use statistical software package,
which enables command line and GUI analysis, with fast and parallel
automated analysis and reporting. Comprehensive plotting options as well
as automated summaries allow clear identification of significantly
altered analytes for non-specialist users.

![](man/figures/santaR-approach.jpg)

## Installation

Install the CRAN release of `santaR` with:

``` r
install.packages("santaR")
```

The development version can be obtained from GitHub:

``` r
# Install devtools
if(!require("devtools")) install.packages("devtools")
devtools::install_github("adwolfer/santaR", ref="master")
```

If the dependency [pcaMethods](https://www.bioconductor.org/packages/release/bioc/html/pcaMethods.html) is not successfully installed, it can be installed from `Bioconductor`:
 
``` r
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("pcaMethods")
```

## Usage

To get started `santaR`’s graphical user interface implements all the
functions for short asynchronous time-series analysis:

``` r
library(santaR)

santaR_start_GUI(browser = TRUE)
#  To exit press ESC in the command line
```

The graphical user interface is divided in 4 sections, corresponding to
the main steps of analysis:

**Import**, **DF search**, **Analysis** and **Export**:

  - The **Import** tab manages input data in comma separated value
    (*csv*) format or as an *RData* file containing a previous analysis.
    Once data is imported the **DF search** and **Analysis** tabs become
    available.
  - **DF search** implements the tools for the selection of an optimal
    number of degrees of freedom (*df*).
  - With the data imported and a pertinent *df* selected, **Analysis**
    regroups the interface to visualise and identify variables
    significantly altered over time. A plotting interface enables the
    interactive visualisation of the raw data points, individual
    trajectories, group mean curves and confidence bands for all
    variables, which subsequently can be saved. Finally, if inter-group
    differential trajectories have been characterised, all significance
    testing results (with correction for multiple testing) are presented
    in interactive tables.
  - The **Export** tab manages the saving of results and automated
    reporting. Fitted data can be saved as an *RData* file for future
    analysis or reproduction of results. *csv* tables containing
    significance testing results can also be generated and summary plot
    for each significantly altered variable saved for rapid evaluation.

![](man/figures/README-example-1.png)

## Vignettes and Demo data

More information is available in the graphical user interface as well as
in the following vignettes:

  - [Getting Started with
    santaR](https://adwolfer.github.io/santaR/articles/getting-started.html)
  - [How to prepare input data for
    santaR](https://adwolfer.github.io/santaR/articles/prepare-input-data.html)
  - [santaR Graphical user interface use](https://adwolfer.github.io/santaR/articles/santaR-GUI.html)
  - [santaR Theoretical
    Background](https://adwolfer.github.io/santaR/articles/theoretical-background.html)
  - [Automated command line
    analysis](https://adwolfer.github.io/santaR/articles/automated-command-line.html)
  - [Plotting
    options](https://adwolfer.github.io/santaR/articles/plotting-options.html)
  - [Selecting an optimal number of degrees of
    freedom](https://adwolfer.github.io/santaR/articles/selecting-optimal-df.html)
  - [Advanced command line
    options](https://adwolfer.github.io/santaR/articles/advanced-command-line-functions.html)

A dataset containing the concentrations of 22 mediators of inflammation
over an episode of acute inflammation is also available. The mediators
have been measured at 7 time-points on 8 subjects, concentration values
have been unit-variance scaled for each variable. A subset of the data
is presented below:

``` r
## Metadata
acuteInflammation$meta
```

| time |  ind   | group  |
| :--: | :----: | :----: |
|  4   | ind\_6 | Group2 |
|  4   | ind\_7 | Group1 |
|  4   | ind\_8 | Group2 |
|  8   | ind\_1 | Group1 |
|  8   | ind\_2 | Group2 |
|  8   | ind\_3 | Group1 |

``` r
## Data
acuteInflammation$data
```

|  var\_1  |  var\_2   |  var\_3   |  var\_4  |
| :------: | :-------: | :-------: | :------: |
|  2.668   |   2.464   |   1.365   |  1.743   |
| \-0.3002 |  0.05366  |  0.4509   | 0.01572  |
|  3.777   |   2.543   |   1.858   |  2.213   |
| \-0.3275 |  0.1564   |   0.585   | 0.03299  |
|  0.708   |  0.4893   | \-0.08219 |  0.9345  |
| \-0.4101 | \-0.03727 | \-0.2914  | \-0.7239 |

## Other tips

The GUI is to be prefered to understand the methodology, select the best
parameters on a subset of the data before running the command line, or
to visually explore results.

If a very high number of variables is to be processed, `santaR`’s
command line functions are more efficient, as they can be integrated in
scripts and the reporting automated.

## Copyright

`santaR` is licensed under the
[GPLv3](https://choosealicense.com/licenses/gpl-3.0/)

As a summary, the GPLv3 license requires attribution, inclusion of
copyright and license information, disclosure of source code and
changes. Derivative work must be available under the same terms.

© Arnaud Wolfer (2021)
