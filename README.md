# teal.modules.clinical

<!-- start badges -->
[![CRAN Version](https://www.r-pkg.org/badges/version/teal.modules.clinical?color=green)](https://cran.r-project.org/package=teal.modules.clinical)
[![Total Downloads](http://cranlogs.r-pkg.org/badges/grand-total/teal.modules.clinical?color=green)](https://cran.r-project.org/package=teal.modules.clinical)
[![Last Month Downloads](http://cranlogs.r-pkg.org/badges/last-month/teal.modules.clinical?color=green)](https://cran.r-project.org/package=teal.modules.clinical)
[![Last Week Downloads](http://cranlogs.r-pkg.org/badges/last-week/teal.modules.clinical?color=green)](https://cran.r-project.org/package=teal.modules.clinical)

[![Check 🛠](https://github.com/insightsengineering/teal.modules.clinical/actions/workflows/check.yaml/badge.svg)](https://insightsengineering.github.io/teal.modules.clinical/main/unit-test-report/) [![Docs 📚](https://github.com/insightsengineering/teal.modules.clinical/actions/workflows/docs.yaml/badge.svg)](https://insightsengineering.github.io/teal.modules.clinical/)

![GitHub forks](https://img.shields.io/github/forks/insightsengineering/teal.modules.clinical?style=social)
![GitHub repo stars](https://img.shields.io/github/stars/insightsengineering/teal.modules.clinical?style=social)

![GitHub commit activity](https://img.shields.io/github/commit-activity/m/insightsengineering/teal.modules.clinical)
![GitHub contributors](https://img.shields.io/github/contributors/insightsengineering/teal.modules.clinical)
![GitHub last commit](https://img.shields.io/github/last-commit/insightsengineering/teal.modules.clinical)
![GitHub pull requests](https://img.shields.io/github/issues-pr/insightsengineering/teal.modules.clinical)
![GitHub repo size](https://img.shields.io/github/repo-size/insightsengineering/teal.modules.clinical)
![GitHub language count](https://img.shields.io/github/languages/count/insightsengineering/teal.modules.clinical)
[![Project Status: Active -- The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Current Version](https://img.shields.io/github/r-package/v/insightsengineering/teal.modules.clinical/main?color=purple&label=package%20version)](https://github.com/insightsengineering/teal.modules.clinical/tree/main)
[![Open Issues](https://img.shields.io/github/issues-raw/insightsengineering/teal.modules.clinical?color=red&label=open%20issues)](https://github.com/insightsengineering/teal.modules.clinical/issues?q=is%3Aissue+is%3Aopen+sort%3Aupdated-desc)
<!-- end badges -->

This package contains a set of standard `teal` modules to be used with `CDISC` data in order to generate many of the standard outputs used in clinical trials.

These modules include, but are not limited to:

<!-- markdownlint-disable MD007 MD030 -->
-   Data visualizations:
    -   Forest plots (`tm_g_forest_rsp()`/`tm_g_forest_tte()`)
    -   Line plots (`tm_g_lineplot()`)
    -   Kaplan-Meier plots (`tm_g_km()`)
    -   ...
-   Statistical model fits:
    -   MMRM (`tm_a_mmrm()`)
    -   Logistic regression (`tm_t_logistic()`)
    -   Cox regression (`tm_t_coxreg()`)
    -   ...
-   Summary tables:
    -   Unique patients (`tm_t_summary()`)
    -   Exposure across patients (`tm_t_exposure()`)
    -   Change from baseline for parameters (`tm_t_summary_by()`)
    -   ...
-   Patient-level profile modules:
    -   Table of basic information about chosen patient (`tm_t_pp_basic_info()`)
    -   Plot of patient vitals over time (`tm_g_pp_vitals()`)
    -   General timeline for individual patients (`tm_g_pp_patient_timeline()`)
    -   ...

<!-- markdownlint-enable MD007 MD030 -->

Most modules in the package are implemented using functions from the R package [`tern`](https://insightsengineering.github.io/tern/) in order to produce their output.

Please see the [Teal Gallery](https://insightsengineering.github.io/teal.gallery/) and [TLG Catalog](https://insightsengineering.github.io/tlg-catalog/) for examples of `shiny` apps created using modules from this package.

## Installation

```r
install.packages('teal.modules.clinical')
```

Alternatively, you might want to use the development version.

```r
# install.packages("pak")
pak::pak("insightsengineering/teal.modules.clinical")
```

## Usage

To understand how to use this package, please refer to the [Getting Started](https://insightsengineering.github.io/teal.modules.clinical/latest-tag/articles/teal-modules-clinical.html) article, which provides multiple examples of code implementation.

## Getting help

If you encounter a bug or have a feature request, please file an issue. For questions, discussions, and staying up to date, please use the `teal` channel in the [`pharmaverse` slack workspace](https://pharmaverse.slack.com).

## Acknowledgment

This package is the result of the joint efforts of many developers and stakeholders. We would like to thank everyone who has contributed so far!

## Stargazers and Forkers

### Stargazers over time

[![Stargazers over time](https://starchart.cc/insightsengineering/teal.modules.clinical.svg)](https://starchart.cc/insightsengineering/teal.modules.clinical)

### Stargazers

[![Stargazers repo roster for](https://reporoster.com/stars/insightsengineering/teal.modules.clinical)](https://github.com/insightsengineering/teal.modules.clinical/stargazers)

### Forkers

[![Forkers repo roster for](https://reporoster.com/forks/insightsengineering/teal.modules.clinical)](https://github.com/insightsengineering/teal.modules.clinical/network/members)
