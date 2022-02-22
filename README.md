# teal.modules.clinical

This package contains a set of standard `teal` modules to be used with CDISC data in order to generate many of the standard outputs used in clinical trials.

These modules include, but are not limited to:

- time-to-event endpoints:
  * `tm_t_tte`: time-to-event table
  * `tm_t_coxreg`: Cox regression
  * `tm_g_km`: Kaplan-Meier plot
  * ...
- data models for other endpoint types:
  * `tm_a_mmrm`: MMRM model fits
  * `tm_t_ancova`: ANCOVA model
  * `tm_t_logistic`: logistic regression
  * ...
- adverse event summaries:
  * `tm_t_events`: adverse events by term
  * `tm_t_events_summary`: adverse events summary table
  * `tm_t_events_by_grade`: adverse events by grade summary table
  * ...
- patient-level profile modules
  * `tm_t_pp_basic_info`: table of basic information about chosen patient
  * `tm_g_pp_vitals`: plot of patient vitals over time
  * `tm_g_pp_patient_timeline`: general timeline for individual patients
  * ...

Most of the modules in `teal.modules.clinical` use functions from the R package [tern](https://github.com/insightsengineering/tern) in order to produce their output.

## Installation

This repository requires a personal access token to install see here [creating and using PAT](https://docs.github.com/en/github/authenticating-to-github/keeping-your-account-and-data-secure/creating-a-personal-access-token). Once this is set up, to install the latest released version of the package run:

```r
Sys.setenv(GITHUB_PAT = "your_access_token_here")
if (!require("devtools")) install.packages("devtools")
devtools::install_github("insightsengineering/teal.modules.clinical@*release", dependencies = FALSE)
```

You might need to manually install all of the package dependencies before installing this package as without
the `dependencies = FALSE` argument to `install_github` it may produce an error.

See package vignettes `browseVignettes(package = "teal.modules.clinical")` for usage of this package.
