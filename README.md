# teal.modules.clinical

<!-- start badges -->
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
    -   forest plots (`tm_g_forest_rsp`/`tm_g_forest_tte`)
    -   line plots (`tm_g_lineplot`)
    -   `Kaplan-Meier` plots (`tm_g_km`)
    -   ...
-   Statistical model fits:
    -   `MMRM` (`tm_a_mmrm`)
    -   logistic regression (`tm_t_logistic`)
    -   Cox regression (`tm_t_coxreg`)
    -   ...
-   Summary tables:
    -   unique patients (`tm_t_summary`)
    -   exposure across patients (`tm_t_exposure`)
    -   change from baseline for parameters (`tm_t_summary_by`)
    -   ...
-   Patient-level profile modules:
    -   table of basic information about chosen patient (`tm_t_pp_basic_info`)
    -   plot of patient vitals over time (`tm_g_pp_vitals`)
    -   general timeline for individual patients (`tm_g_pp_patient_timeline`)
    -   ...

<!-- markdownlint-enable MD007 MD030 -->

Most of the modules in `teal.modules.clinical` use functions from the R package [tern](https://insightsengineering.github.io/tern/) in order to produce their output.

Please see [teal gallery](https://insightsengineering.github.io/teal.gallery/main/) and [TLG Catalog](https://insightsengineering.github.io/tlg-catalog/) for examples of `teal` apps created using modules from this package.

## Installation

From July 2023 `insightsengineering` packages are available on [r-universe](https://r-universe.dev/).

```r
# stable versions
install.packages('teal.modules.clinical', repos = c('https://insightsengineering.r-universe.dev', 'https://cloud.r-project.org'))

# beta versions
install.packages('teal.modules.clinical', repos = c('https://pharmaverse.r-universe.dev', 'https://cloud.r-project.org'))
```

See package vignettes `browseVignettes(package = "teal.modules.clinical")` for usage of this package.

## Acknowledgment

This package is the result of the joint efforts of many developers and stakeholders. We would like to thank everyone who has contributed so far!

## Stargazers and Forkers

### Stargazers over time

[![Stargazers over time](https://starchart.cc/insightsengineering/teal.modules.clinical.svg)](https://starchart.cc/insightsengineering/teal.modules.clinical)

### Stargazers

[![Stargazers repo roster for](https://reporoster.com/stars/insightsengineering/teal.modules.clinical)](https://github.com/insightsengineering/teal.modules.clinical/stargazers)

### Forkers

[![Forkers repo roster for](https://reporoster.com/forks/insightsengineering/teal.modules.clinical)](https://github.com/insightsengineering/teal.modules.clinical/network/members)
