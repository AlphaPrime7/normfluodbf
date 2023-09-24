Tingwei Adeck
September 23, 2023

<!-- README.md is generated from README.Rmd. Please edit that file -->

# normfluodbf <img src="man/figures/logo.png" align="right" width="180"/>

[![Project
status](https://www.repostatus.org/badges/latest/concept.svg)](https://github.com/AlphaPrime7/normfluodbf/commits)
[![Project
Status](https://www.repostatus.org/badges/latest/active.svg)](https://github.com/AlphaPrime7/normfluodbf/commits)
[![Project
Status](https://www.repostatus.org/badges/latest/wip.svg)](https://github.com/AlphaPrime7/normfluodbf/commits)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![saythanks](https://img.shields.io/badge/say-thanks-ff69b4.svg)](https://github.com/AlphaPrime7/normfluodbf)
[![test-coverage](https://github.com/AlphaPrime7/normfluodbf/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/AlphaPrime7/normfluodbf/actions/workflows/test-coverage.yaml)
[![R-CMD-check](https://github.com/AlphaPrime7/normfluodbf/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/AlphaPrime7/normfluodbf/actions/workflows/R-CMD-check.yaml)
[![pages-build-deployment](https://github.com/AlphaPrime7/normfluodbf/actions/workflows/pages/pages-build-deployment/badge.svg)](https://github.com/AlphaPrime7/normfluodbf/actions/workflows/pages/pages-build-deployment)
[![license](https://img.shields.io/badge/MIT-License?label=license)](https://mit-license.org/)
[![Maintenance](https://img.shields.io/badge/Maintained%3F-yes-green.svg)](https://github.com/AlphaPrime7/normfluodbf/graphs/commit-activity)
[![CRAN
status](https://www.r-pkg.org/badges/version/normfluodbf)](https://CRAN.R-project.org/package=normfluodbf)
[![](http://cranlogs.r-pkg.org/badges/grand-total/normfluodbf?color=yellow)](https://cran.r-project.org/package=normfluodbf)
[![](http://cranlogs.r-pkg.org/badges/last-month/normfluodbf?color=green)](https://cran.r-project.org/package=normfluodbf)
[![](http://cranlogs.r-pkg.org/badges/last-week/normfluodbf?color=yellow)](https://cran.r-project.org/package=normfluodbf)

The goal of
[`{normfluodbf}`](https://github.com/AlphaPrime7/normfluodbf) is to
normalize fluorescence data obtained from liposome flux assay
experiments via the FLUOstar micro plate reader. The Input file is a
.dbf file that requires cleaning and normalization using
[`{normfluodbf}`](https://github.com/AlphaPrime7/normfluodbf), before
insightful analysis can be conducted.

## ↓ Installation

The development version of
[`{normfluodbf}`](https://github.com/AlphaPrime7/normfluodbf) can be
installed as illustrated below:

``` r
install.packages("devtools")
devtools::install_github("AlphaPrime7/normfluodbf")
pak::pak("AlphaPrime7/normfluodbf")

#Once the package makes CRAN
install.packages("normfluodbf")
library(normfluodbf)
```

## Example

This is a basic example which illustrates package usage:

``` r
library(normfluodbf)
liposomes_214 <- system.file("extdata", "liposomes_214.dbf", package = "normfluodbf")
normalized_data <- normfluodbf(liposomes_214)
```

## R packages Imported

- tidyr
- data.table
- foreign
- tibble

Visit
[`{my page}`](https://alphaprime7.github.io/normfluodbf/articles/normfluodbf.html)
for details on the concept behind the project.

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
