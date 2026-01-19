# Control Function for Time-To-Event teal Module

Controls the arguments for Cox regression and survival analysis results.

## Usage

``` r
control_tte(
  surv_time = list(conf_level = 0.95, conf_type = "plain", quantiles = c(0.25, 0.75)),
  coxph = list(pval_method = "log-rank", ties = "efron", conf_level = 0.95),
  surv_timepoint = tern::control_surv_timepoint(conf_level = 0.95, conf_type = c("plain",
    "none", "log", "log-log"))
)
```

## Arguments

- surv_time:

  (`list`)  
  control parameters for `survfit` model. See
  [`tern::control_surv_time()`](https://insightsengineering.github.io/tern/latest-tag/reference/control_surv_time.html)
  for details.

- coxph:

  (`list`)  
  control parameters for Cox-PH model. See
  [`tern::control_coxph()`](https://insightsengineering.github.io/tern/latest-tag/reference/control_coxph.html)
  for details.

- surv_timepoint:

  (`list`)  
  control parameters for `survfit` model at time point. See
  [`tern::control_surv_timepoint()`](https://insightsengineering.github.io/tern/latest-tag/reference/control_surv_timepoint.html)
  for details.

## See also

[`template_tte()`](https://insightsengineering.github.io/teal.modules.clinical/reference/template_tte.md),
[`tm_t_tte()`](https://insightsengineering.github.io/teal.modules.clinical/reference/tm_t_tte.md)
