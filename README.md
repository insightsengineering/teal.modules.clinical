# teal.modules.clinical

<!-- start badges -->
[![Check 🛠](https://github.com/insightsengineering//actions/workflows/check.yaml/badge.svg)](https://github.com/insightsengineering//actions/workflows/check.yaml)
[![Docs 📚](https://github.com/insightsengineering//actions/workflows/docs.yaml/badge.svg)](https://insightsengineering.github.io//)
[![Code Coverage 📔](https://raw.githubusercontent.com/insightsengineering//_xml_coverage_reports/data/main/badge.svg)](https://raw.githubusercontent.com/insightsengineering//_xml_coverage_reports/data/main/coverage.xml)

![GitHub forks](https://img.shields.io/github/forks/insightsengineering/?style=social)
![GitHub Repo stars](https://img.shields.io/github/stars/insightsengineering/?style=social)

![GitHub commit activity](https://img.shields.io/github/commit-activity/m/insightsengineering/)
![GitHub contributors](https://img.shields.io/github/contributors/insightsengineering/)
![GitHub last commit](https://img.shields.io/github/last-commit/insightsengineering/)
![GitHub pull requests](https://img.shields.io/github/issues-pr/insightsengineering/)
![GitHub repo size](https://img.shields.io/github/repo-size/insightsengineering/)
![GitHub language count](https://img.shields.io/github/languages/count/insightsengineering/)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Current Version](https://img.shields.io/github/r-package/v/insightsengineering//main?color=purple\&label=package%20version)](https://github.com/insightsengineering//tree/main)
[![Open Issues](https://img.shields.io/github/issues-raw/insightsengineering/?color=red\&label=open%20issues)](https://github.com/insightsengineering//issues?q=is%3Aissue+is%3Aopen+sort%3Aupdated-desc)
<!-- end badges -->

# teal.modules.clinical

This package contains a set of standard `teal` modules to be used with CDISC data in order to generate many of the standard outputs used in clinical trials.

These modules include, but are not limited to:
<!-- markdownlint-disable MD007 MD030 -->
-    data visualizations:
     -    forest plots
     -    line plots
     -    Kaplan-Meier plots
     -    ...
-    statistical model fits:
     -    MMRM
     -    logistic regression
     -    Cox regression
     -    ...
-    summary tables:
     -    unique patients
     -    exposure across patients
     -    change from baseline for parameters
     -    ...
-    patient-level profile modules
     -    `tm_t_pp_basic_info`: table of basic information about chosen patient
     -    `tm_g_pp_vitals`: plot of patient vitals over time
     -    `tm_g_pp_patient_timeline`: general timeline for individual patients
     -    ...
<!-- markdownlint-enable MD007 MD030 -->
Most of the modules in `teal.modules.clinical` use functions from the R package [tern](https://insightsengineering.github.io/tern/) in order to produce their output.

## Installation

For releases from August 2022 it is recommended that you [create and use a Github PAT](https://docs.github.com/en/github/authenticating-to-github/keeping-your-account-and-data-secure/creating-a-personal-access-token) to install the latest version of this package. Once you have the PAT, run the following:

```r
Sys.setenv(GITHUB_PAT = "your_access_token_here")
if (!require("remotes")) install.packages("remotes")
remotes::install_github("insightsengineering/teal.modules.clinical@*release")
```

You might need to manually install all of the package dependencies before installing this package as without
the `dependencies = FALSE` argument to `install_github` it may produce an error.

A stable release of all `NEST` packages from June 2022 is also available [here](https://github.com/insightsengineering/depository#readme).

See package vignettes `browseVignettes(package = "teal.modules.clinical")` for usage of this package.

## Acknowledgment

This package is the result of the joint efforts of many developers and stakeholders. We would like to thank everyone who has contributed so far!

## Stargazers and Forkers

### Stargazers over time

[![Stargazers over time](https://starchart.cc/insightsengineering/teal.modules.clinical.svg)](https://starchart.cc/insightsengineering/teal.modules.clinical)

### Stargazers

[![Stargazers repo roster for @insightsengineering/teal.modules.clinical](https://reporoster.com/stars/insightsengineering/teal.modules.clinical)](https://github.com/insightsengineering/teal.modules.clinical/stargazers)

### Forkers

[![Forkers repo roster for @insightsengineering/teal.modules.clinical](https://reporoster.com/forks/insightsengineering/teal.modules.clinical)](https://github.com/insightsengineering/teal.modules.clinical/network/members)
