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
Most of the modules in `teal.modules.clinical` use functions from the R package [tern](https://github.com/insightsengineering/tern) in order to produce their output.

## Installation

It is recommended that you [create and use a Github PAT](https://docs.github.com/en/github/authenticating-to-github/keeping-your-account-and-data-secure/creating-a-personal-access-token) to install the latest version of this package. Once you have the PAT, run the following:

```r
Sys.setenv(GITHUB_PAT = "your_access_token_here")
if (!require("devtools")) install.packages("devtools")
devtools::install_github("insightsengineering/teal.modules.clinical@*release", dependencies = FALSE)
```

You might need to manually install all of the package dependencies before installing this package as without
the `dependencies = FALSE` argument to `install_github` it may produce an error.

See package vignettes `browseVignettes(package = "teal.modules.clinical")` for usage of this package.

## Acknowledgment

This package is a result of a joint efforts by many developers and stakeholders. We would like to thank everyone who contributed so far!