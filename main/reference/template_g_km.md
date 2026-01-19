# Template: Kaplan-Meier Plot

Creates a valid expression to generate a Kaplan-Meier plot.

## Usage

``` r
template_g_km(
  dataname = "ANL",
  arm_var = "ARM",
  ref_arm = NULL,
  comp_arm = NULL,
  compare_arm = FALSE,
  combine_comp_arms = FALSE,
  aval_var = "AVAL",
  cnsr_var = "CNSR",
  xticks = NULL,
  strata_var = NULL,
  time_points = NULL,
  facet_var = "SEX",
  font_size = 11,
  conf_level = 0.95,
  conf_type = "plain",
  ties = "efron",
  xlab = "Survival time",
  time_unit_var = "AVALU",
  yval = "Survival",
  ylim = NULL,
  pval_method = "log-rank",
  annot_surv_med = TRUE,
  annot_coxph = TRUE,
  control_annot_surv_med = control_surv_med_annot(),
  control_annot_coxph = tern::control_coxph_annot(x = 0.27, y = 0.35, w = 0.3),
  legend_pos = NULL,
  rel_height_plot = 0.8,
  ci_ribbon = FALSE,
  title = "KM Plot"
)
```

## Arguments

- dataname:

  (`character`)  
  analysis data used in teal module.

- arm_var:

  (`character`)  
  variable names that can be used as `arm_var`.

- ref_arm:

  (`character`)  
  the level of reference arm in case of arm comparison.

- comp_arm:

  (`character`)  
  the level of comparison arm in case of arm comparison.

- compare_arm:

  (`logical`)  
  triggers the comparison between study arms.

- combine_comp_arms:

  (`logical`)  
  triggers the combination of comparison arms.

- aval_var:

  (`character`)  
  name of the analysis value variable.

- cnsr_var:

  (`character`)  
  name of the censoring variable.

- xticks:

  (`numeric` or `NULL`)  
  numeric vector of tick positions or a single number with spacing
  between ticks on the x-axis. If `NULL` (default),
  [`labeling::extended()`](https://rdrr.io/pkg/labeling/man/extended.html)
  is used to determine optimal tick positions on the x-axis.

- strata_var:

  (`character`)  
  names of the variables for stratified analysis.

- time_points:

  (`character`)  
  time points that can be used in
  [`tern::surv_timepoint()`](https://insightsengineering.github.io/tern/latest-tag/reference/survival_timepoint.html).

- facet_var:

  (`character`)  
  name of the variable to use to facet the plot.

- font_size:

  (`numeric`)  
  font size value.

- conf_level:

  (`numeric`)  
  value for the confidence level within the range of (0, 1).

- conf_type:

  (`string`)  
  confidence interval type for median survival time CI. Options are
  "plain" (default), "log", "log-log".

- ties:

  (`string`)  
  among `exact` (equivalent to `DISCRETE` in SAS), `efron` and
  `breslow`, see
  [`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html).
  Note: there is no equivalent of SAS `EXACT` method in R.

- xlab:

  (`string`)  
  x-axis label.

- time_unit_var:

  (`character`)  
  name of the variable representing time units.

- yval:

  (`string`)  
  type of plot, to be plotted on the y-axis. Options are `Survival`
  (default) and `Failure` probability.

- ylim:

  (`numeric(2)`)  
  vector containing lower and upper limits for the y-axis, respectively.
  If `NULL` (default), the default scale range is used.

- pval_method:

  (`string`)  
  the method used for estimation of p.values; `wald` (default) or
  `likelihood`.

- annot_surv_med:

  (`flag`)  
  compute and add the annotation table on the Kaplan-Meier curve
  estimating the median survival time per group.

- annot_coxph:

  (`flag`)  
  whether to add the annotation table from a
  [`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html)
  model.

- control_annot_surv_med:

  (`list`)  
  parameters to control the position and size of the annotation table
  added to the plot when `annot_surv_med = TRUE`, specified using the
  [`control_surv_med_annot()`](https://insightsengineering.github.io/tern/latest-tag/reference/control_annot.html)
  function. Parameter options are: `x`, `y`, `w`, `h`, and `fill`. See
  [`control_surv_med_annot()`](https://insightsengineering.github.io/tern/latest-tag/reference/control_annot.html)
  for details.

- control_annot_coxph:

  (`list`)  
  parameters to control the position and size of the annotation table
  added to the plot when `annot_coxph = TRUE`, specified using the
  [`control_coxph_annot()`](https://insightsengineering.github.io/tern/latest-tag/reference/control_annot.html)
  function. Parameter options are: `x`, `y`, `w`, `h`, `fill`, and
  `ref_lbls`. See
  [`control_coxph_annot()`](https://insightsengineering.github.io/tern/latest-tag/reference/control_annot.html)
  for details.

- legend_pos:

  (`numeric(2)` or `NULL`)  
  vector containing x- and y-coordinates, respectively, for the legend
  position relative to the KM plot area. If `NULL` (default), the legend
  is positioned in the bottom right corner of the plot, or the middle
  right of the plot if needed to prevent overlapping.

- rel_height_plot:

  (`proportion`)  
  proportion of total figure height to allocate to the Kaplan-Meier
  plot. Relative height of patients at risk table is then
  `1 - rel_height_plot`. If `annot_at_risk = FALSE` or `as_list = TRUE`,
  this parameter is ignored.

- ci_ribbon:

  (`flag`)  
  whether the confidence interval should be drawn around the
  Kaplan-Meier curve.

- title:

  (`character`)  
  title of the output.

## Value

a `list` of expressions to generate a table or plot object.

## See also

[`tm_g_km()`](https://insightsengineering.github.io/teal.modules.clinical/reference/tm_g_km.md)
