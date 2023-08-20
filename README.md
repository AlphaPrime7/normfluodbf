
<!-- README.md is generated from README.Rmd. Please edit that file -->

# normfluodbf

<!-- badges: start -->
<!-- badges: end -->

The goal of normfluodbf is to normalize fluorescence data obtained from
liposome flux assay experiments via the FLUOstar microplate reader.
Input or source file is a .dbf file that is not ready for analysis
without processing through normfluodbf. There is a ShinyApp for this as
well.

## Installation

You can install the development version of normfluodbf from
[GitHub](https://github.com/AlphaPrime7/normfluodbf) with:

``` r
# install.packages("devtools")
devtools::install_github("AlphaPrime7/normfluodbf")
```

## Example
Original versions of the program entailed two-steps meaning using the foreign package to read the dbf file as seen in the data folder then applying normfluodbf to normalize the data. This version focused on achieving normalized data in a single step because WHY NOT?

This is an example of a one step solution to converting a .dbf file into a normalized file:

``` r
library(normfluodbf)
## normalized_data <- normfluodbf("liposomes_214.dbf")
```
