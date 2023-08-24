Tingwei Adeck August 24, 2023

<!-- README.md is generated from README.Rmd. Please edit that file -->

# normfluodbf <img src="man/figures/logo.png" align="right" width="180"/>

[![Project status](https://www.repostatus.org/badges/latest/concept.svg)](https://github.com/AlphaPrime7/normfluodbf/commits) [![Project Status](https://www.repostatus.org/badges/latest/active.svg)](https://github.com/AlphaPrime7/normfluodbf/commits) [![Project Status](https://www.repostatus.org/badges/latest/wip.svg)](https://github.com/AlphaPrime7/normfluodbf/commits) [![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html) [![saythanks](https://img.shields.io/badge/say-thanks-ff69b4.svg)](https://github.com/AlphaPrime7/normfluodbf) [![test-coverage](https://github.com/AlphaPrime7/normfluodbf/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/AlphaPrime7/normfluodbf/actions/workflows/test-coverage.yaml) [![R-CMD-check](https://github.com/AlphaPrime7/normfluodbf/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/AlphaPrime7/normfluodbf/actions/workflows/R-CMD-check.yaml) [![license](https://img.shields.io/badge/MIT-License?label=license)](https://www.gnu.org/licenses/gpl-3.0.en.html) [![Maintenance](https://img.shields.io/badge/Maintained%3F-yes-green.svg)](https://github.com/AlphaPrime7/normfluodbf/graphs/commit-activity)

The goal of [`{normfluodbf}`](https://github.com/AlphaPrime7/normfluodbf) is to normalize fluorescence data obtained from liposome flux assay experiments via the FLUOstar micro plate reader. The Input file is a .dbf file that requires cleaning and normalization using [`{normfluodbf}`](https://github.com/AlphaPrime7/normfluodbf), before insightful analysis can be conducted.

## ↓ Installation

The development version of [`{normfluodbf}`](https://github.com/AlphaPrime7/normfluodbf) can be installed as illustrated below:

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

-   tidyr
-   data.table
-   foreign
-   tibble

Visit [`{my page}`](https://alphaprime7.github.io/normfluodbf/articles/normfluodbf.html) for details on the concept behind the project.

# References

(Dowle and Srinivasan 2023) (R Core Team 2022) (Wickham et al. 2019)

::: {#refs .references .csl-bib-body .hanging-indent}
::: {#ref-datatable .csl-entry}
Dowle, Matt, and Arun Srinivasan. 2023. [*data.table*]{.nocase}*: Extension of "[data.frame]{.nocase}"*. <https://CRAN.R-project.org/package=data.table>.
:::

::: {#ref-foreign .csl-entry}
R Core Team. 2022. [*foreign*]{.nocase}*: Read Data Stored by "Minitab," "S," "SAS," "SPSS," "Stata," "Systat," "Weka," "[dBase]{.nocase}," ...* <https://CRAN.R-project.org/package=foreign>.
:::

::: {#ref-tidyverse .csl-entry}
Wickham, Hadley, Mara Averick, Jennifer Bryan, Winston Chang, Lucy D'Agostino McGowan, Romain François, Garrett Grolemund, et al. 2019. "Welcome to the [tidyverse]{.nocase}." *Journal of Open Source Software* 4 (43): 1686. <https://doi.org/10.21105/joss.01686>.
:::
:::
