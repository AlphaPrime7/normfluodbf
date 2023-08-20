Tingwei Adeck
August 20, 2023

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

This is a basic example which shows you how to solve a common problem:

``` r
library(normfluodbf)
## normalized_data <- normfluodbf("liposomes_214.dbf")
```

## R packages used

- tidyr
- data.table
- foreign

# References

(Dowle and Srinivasan 2023) (R Core Team 2022) (Wickham et al. 2019)

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-datatable" class="csl-entry">

Dowle, Matt, and Arun Srinivasan. 2023.
*<span class="nocase">data.table</span>: Extension of
“<span class="nocase">data.frame</span>”*.
<https://CRAN.R-project.org/package=data.table>.

</div>

<div id="ref-foreign" class="csl-entry">

R Core Team. 2022. *<span class="nocase">foreign</span>: Read Data
Stored by “Minitab,” “S,” “SAS,” “SPSS,” “Stata,” “Systat,” “Weka,”
“<span class="nocase">dBase</span>,” ...*
<https://CRAN.R-project.org/package=foreign>.

</div>

<div id="ref-tidyverse" class="csl-entry">

Wickham, Hadley, Mara Averick, Jennifer Bryan, Winston Chang, Lucy
D’Agostino McGowan, Romain François, Garrett Grolemund, et al. 2019.
“Welcome to the <span class="nocase">tidyverse</span>.” *Journal of Open
Source Software* 4 (43): 1686. <https://doi.org/10.21105/joss.01686>.

</div>

</div>
