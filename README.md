Tingwei Adeck
August 21, 2023

<!-- README.md is generated from README.Rmd. Please edit that file -->

# normfluodbf <img src="man/figures/logo.png" align="right" width="180" />

[![Project
status](https://www.repostatus.org/badges/latest/concept.svg)](https://github.com/AlphaPrime7/normfluodbf/commits)
[![Project
Status](https://www.repostatus.org/badges/latest/active.svg)](https://github.com/AlphaPrime7/normfluodbf/commits)
[![Project
Status](https://www.repostatus.org/badges/latest/wip.svg)](https://github.com/AlphaPrime7/normfluodbf/commits)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![saythanks](https://img.shields.io/badge/say-thanks-ff69b4.svg)](https://saythanks.io/to/GuangchuangYu)
[![R-CMD-check](https://github.com/AlphaPrime7/normfluodbf/actions/workflows/devel-check.yaml/badge.svg)](https://github.com/AlphaPrime7/normfluodbf/actions/workflows/devel-check.yaml)
[![license](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
[![Maintenance](https://img.shields.io/badge/Maintained%3F-yes-green.svg)](https://GitHub.com/Nelson-Gon/mde/graphs/commit-activity)

The goal of
[`{normfluodbf}`](https://github.com/AlphaPrime7/normfluodbf) is to
normalize fluorescence data obtained from liposome flux assay
experiments via the FLUOstar microplate reader. Input or source file is
a .dbf file that is not ready for analysis without processing through
{normfluodbf}. There is a ShinyApp for this as well.

## ↓ Installation

You can install the development version of normfluodbf from
[`{normfluodbf}`](https://github.com/AlphaPrime7/normfluodbf) with:

``` r
install.packages("devtools")
devtools::install_github("AlphaPrime7/normfluodbf")
pak::pak("AlphaPrime7/normfluodbf")

#Once the package makes CRAN
install.packages("normfluodbf")
library(normfluodbf)
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(normfluodbf)
liposomes_214 <- system.file("extdata", "liposomes_214.dbf", package = "normfluodbf")
normalized_data <- normfluodbf(liposomes_214)
```

## R packages used

- tidyr
- data.table
- foreign
- tibble

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
