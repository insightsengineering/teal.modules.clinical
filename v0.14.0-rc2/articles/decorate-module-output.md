# Decorate Module Output

## Introduction

The outputs produced by `teal` modules, like graphs or tables, are
created by the module developer and look a certain way. It is hard to
design an output that will satisfy every possible user, so the form of
the output should be considered a default value that can be customized.
Here we describe the concept of *decoration*, enabling the app developer
to tailor outputs to their specific requirements without rewriting the
original module code.

The decoration process is build upon transformation procedures,
introduced in `teal`. While `transformators` are meant to edit module’s
input, decorators are meant to adjust the module’s output. To
distinguish the difference, modules in `teal.modules.clinical` have 2
separate parameters: `transformators` and `decorators`.

To get a complete understanding refer the following vignettes:

- Transforming the input data in [this
  vignette](https://insightsengineering.github.io/teal/latest-tag/articles/transform-input-data.html).
- Transforming module output in [this
  vignette](https://insightsengineering.github.io/teal/latest-tag/articles/transform-module-output.html).

## Outputs that can be decorated

It is important to note which output objects from a given module can be
decorated. The module function documentation’s *Decorating Module*
section has this information.

You can also refer the table shown below to know which module outputs
can be decorated.

| Module | Outputs (Class) |
|----|----|
| `tm_a_gee` | table (ElementaryTable) |
| `tm_a_mmrm` | lsmeans_table (TableTree), lsmeans_plot (ggplot), covariance_table (ElementaryTable), fixed_effects_table (ElementaryTable), diagnostic_table (ElementaryTable), diagnostic_plot (ggplot) |
| `tm_g_barchart_simple` | plot (ggplot) |
| `tm_g_ci` | plot (ggplot) |
| `tm_g_forest_rsp` | plot (ggplot) |
| `tm_g_forest_tte` | plot (ggplot) |
| `tm_g_ipp` | plot (ggplot) |
| `tm_g_km` | plot (ggplot) |
| `tm_g_lineplot` | plot (ggplot) |
| `tm_g_pp_adverse_events` | table (datatables), plot (ggplot) |
| `tm_g_pp_patient_timeline` | plot (ggplot) |
| `tm_g_pp_therapy` | plot (ggplot), table (datatables) |
| `tm_g_pp_vitals` | plot (ggplot) |
| `tm_t_abnormality` | table (TableTree) |
| `tm_t_abnormality_by_worst_grade` | table (TableTree) |
| `tm_t_ancova` | table (TableTree) |
| `tm_t_binary_outcome` | table (TableTree) |
| `tm_t_coxreg` | table (TableTree) |
| `tm_t_events` | table (TableTree) |
| `tm_t_events_by_grade` | table (TableTree) |
| `tm_t_events_patyear` | table (ElementaryTable) |
| `tm_t_events_summary` | table (TableTree) |
| `tm_t_exposure` | table (ElementaryTable) |
| `tm_t_logistic` | table (TableTree) |
| `tm_t_mult_events` | table (TableTree) |
| `tm_t_pp_basic_info` | table (datatables) |
| `tm_t_pp_laboratory` | table (datatables) |
| `tm_t_pp_medical_history` | table (TableTree) |
| `tm_t_pp_prior_medication` | table (datatables) |
| `tm_t_shift_by_arm` | table (TableTree) |
| `tm_t_shift_by_arm_by_worst` | table (TableTree) |
| `tm_t_shift_by_grade` | table (TableTree) |
| `tm_t_smq` | table (TableTree) |
| `tm_t_summary` | table (TableTree) |
| `tm_t_summary_by` | table (TableTree) |
| `tm_t_tte` | table (TableTree) |

Also, note that there are three different types of objects that can be
decorated:

1.  `listing_df`, `ElementaryTable`, `TableTree`
2.  `ggplot`
3.  `datatables`

*Tip:* A general tip before trying to decorate the output from the
module is to copy the reproducible code and running them in a separate R
session to quickly iterate the decoration you want.

## Decorating `listing_df`, `ElementaryTable`, `TableTree`

Here’s an example to showcase how you can edit an output of class
`listing_df`, `ElementaryTable`, or `TableTree`. All these classes are
extension of objects created using `rtables` and can be modified with
the help of `rtables` modifiers like
[`rtables::insert_rrow`](https://insightsengineering.github.io/rtables/latest-tag/reference/insert_rrow.html).

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`teal.modules.clinical`](https://insightsengineering.github.io/teal.modules.clinical/)`)`\
\
`data`` ``<-`` `[`within`](https://rdrr.io/r/base/with.html)`(`[`teal_data`](https://insightsengineering.github.io/teal.data/latest-tag/reference/teal_data.html)`(``)``, ``{`\
`  `[`library`](https://rdrr.io/r/base/library.html)`(`[`dplyr`](https://dplyr.tidyverse.org)`)`\
`  ``ADSL`` ``<-`` ``tmc_ex_adsl`` ``|>`\
`    `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(`\
`      ITTFL ``=`` `[`factor`](https://rdrr.io/r/base/factor.html)`(``"Y"``)`` ``|>`` `[`with_label`](https://pharmaverse.github.io/formatters/latest-tag/reference/with_label.html)`(``"Intent-To-Treat Population Flag"``)`\
`    ``)`` ``|>`\
`    `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``DTHFL ``=`` `[`case_when`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)`(``!`[`is.na`](https://rdrr.io/r/base/NA.html)`(``DTHDT``)`` ``~`` ``"Y"``, ``TRUE`` ``~`` ``""``)`` ``|>`` `[`with_label`](https://pharmaverse.github.io/formatters/latest-tag/reference/with_label.html)`(``"Subject Death Flag"``)``)`\
\
\
`  ``ADLB`` ``<-`` ``tmc_ex_adlb`` ``|>`\
`    `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``AVISIT ``=`` ``forcats``::`[`fct_reorder`](https://forcats.tidyverse.org/reference/fct_reorder.html)`(``AVISIT``, ``AVISITN``, ``min``)``)`` ``|>`\
`    `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(`\
`      ONTRTFL ``=`` `[`case_when`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)`(`\
`        ``AVISIT`` `[`%in%`](https://rdrr.io/r/base/match.html)` `[`c`](https://rdrr.io/r/base/c.html)`(``"SCREENING"``, ``"BASELINE"``)`` ``~`` ``""``,`\
`        ``TRUE`` ``~`` ``"Y"`\
`      ``)`` ``|>`` `[`with_label`](https://pharmaverse.github.io/formatters/latest-tag/reference/with_label.html)`(``"On Treatment Record Flag"``)`\
`    ``)`\
`}``)`\
[`join_keys`](https://insightsengineering.github.io/teal.data/latest-tag/reference/join_keys.html)`(``data``)`` ``<-`` ``default_cdisc_join_keys``[`[`names`](https://rdrr.io/r/base/names.html)`(``data``)``]`\
\
`insert_rrow_decorator`` ``<-`` ``function``(``default_caption`` ``=`` ``"I am a good new row"``)`` ``{`\
`  ``teal_transform_module``(`\
`    label ``=`` ``"New row"``,`\
`    ui ``=`` ``function``(``id``)`` ``{`\
`      ``shiny``::`[`textInput`](https://rdrr.io/pkg/shiny/man/textInput.html)`(``shiny``::`[`NS`](https://rdrr.io/pkg/shiny/man/NS.html)`(``id``, ``"new_row"``)``, ``"New row"``, value ``=`` ``default_caption``)`\
`    ``}``,`\
`    server ``=`` ``function``(``id``, ``data``)`` ``{`\
`      ``moduleServer``(``id``, ``function``(``input``, ``output``, ``session``)`` ``{`\
`        ``reactive``(``{`\
`          `[`data`](https://rdrr.io/r/utils/data.html)`(``)`` ``|>`\
`            `[`within`](https://rdrr.io/r/base/with.html)`(`\
`              ``{`\
`                ``table`` ``<-`` ``rtables``::`[`insert_rrow`](https://insightsengineering.github.io/rtables/latest-tag/reference/insert_rrow.html)`(``table``, ``rtables``::`[`rrow`](https://insightsengineering.github.io/rtables/latest-tag/reference/rrow.html)`(``new_row``)``)`\
`              ``}``,`\
`              new_row ``=`` ``input``$``new_row`\
`            ``)`\
`        ``}``)`\
`      ``}``)`\
`    ``}`\
`  ``)`\
`}`\
\
`app`` ``<-`` ``init``(`\
`  data ``=`` ``data``,`\
`  modules ``=`` ``modules``(`\
`    `[`tm_t_abnormality`](https://insightsengineering.github.io/teal.modules.clinical/reference/tm_t_abnormality.md)`(`\
`      label ``=`` ``"tm_t_abnormality"``,`\
`      dataname ``=`` ``"ADLB"``,`\
`      arm_var ``=`` ``variables``(`\
`        choices ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"ARM"``, ``"ARMCD"``)``,`\
`        selected ``=`` ``"ARM"`\
`      ``)``,`\
`      add_total ``=`` ``FALSE``,`\
`      by_vars ``=`` ``variables``(`\
`        choices ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"LBCAT"``, ``"PARAM"``, ``"AVISIT"``)``,`\
`        selected ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"LBCAT"``, ``"PARAM"``)``,`\
`        multiple ``=`` ``TRUE``,`\
`        ordered ``=`` ``TRUE`\
`      ``)``,`\
`      baseline_var ``=`` ``variables``(`\
`        choices ``=`` ``"BNRIND"``,`\
`        selected ``=`` ``"BNRIND"``,`\
`        fixed ``=`` ``TRUE`\
`      ``)``,`\
`      grade ``=`` ``variables``(`\
`        choices ``=`` ``"ANRIND"``,`\
`        selected ``=`` ``"ANRIND"``,`\
`        fixed ``=`` ``TRUE`\
`      ``)``,`\
`      abnormal ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``low ``=`` ``"LOW"``, high ``=`` ``"HIGH"``)``,`\
`      exclude_base_abn ``=`` ``FALSE``,`\
`      decorators ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``table ``=`` ``insert_rrow_decorator``(``"I am a good new row"``)``)`\
`    ``)`\
`  ``)`\
`)`\
\
`if`` ``(`[`interactive`](https://rdrr.io/r/base/interactive.html)`(``)``)`` ``{`\
`  ``shinyApp``(``app``$``ui``, ``app``$``server``)`\
`}`

## Decorating `ggplot`

Here’s an example to showcase how you can edit an output of class
`ggplot`. You can extend them using `ggplot2` functions.

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`teal.modules.clinical`](https://insightsengineering.github.io/teal.modules.clinical/)`)`\
\
`data`` ``<-`` `[`teal_data`](https://insightsengineering.github.io/teal.data/latest-tag/reference/teal_data.html)`(``join_keys ``=`` ``default_cdisc_join_keys``[`[`c`](https://rdrr.io/r/base/c.html)`(``"ADSL"``, ``"ADRS"``)``]``)`\
`data`` ``<-`` `[`within`](https://rdrr.io/r/base/with.html)`(``data``, ``{`\
`  `[`require`](https://rdrr.io/r/base/library.html)`(`[`nestcolor`](https://insightsengineering.github.io/nestcolor/)`)`\
`  ``ADSL`` ``<-`` ``rADSL`\
`  ``ADTTE`` ``<-`` ``tmc_ex_adtte`\
`}``)`\
[`join_keys`](https://insightsengineering.github.io/teal.data/latest-tag/reference/join_keys.html)`(``data``)`` ``<-`` ``default_cdisc_join_keys``[`[`names`](https://rdrr.io/r/base/names.html)`(``data``)``]`\
\
`ADTTE`` ``<-`` ``data``[[``"ADTTE"``]``]`\
\
`ggplot_caption_decorator`` ``<-`` ``function``(``default_caption`` ``=`` ``"I am a good decorator"``)`` ``{`\
`  ``teal_transform_module``(`\
`    label ``=`` ``"Caption"``,`\
`    ui ``=`` ``function``(``id``)`` ``{`\
`      ``shiny``::`[`textInput`](https://rdrr.io/pkg/shiny/man/textInput.html)`(``shiny``::`[`NS`](https://rdrr.io/pkg/shiny/man/NS.html)`(``id``, ``"title"``)``, ``"Plot Title"``, value ``=`` ``default_caption``)`\
`    ``}``,`\
`    server ``=`` ``function``(``id``, ``data``)`` ``{`\
`      ``moduleServer``(``id``, ``function``(``input``, ``output``, ``session``)`` ``{`\
`        ``reactive``(``{`\
`          `[`data`](https://rdrr.io/r/utils/data.html)`(``)`` ``|>`\
`            `[`within`](https://rdrr.io/r/base/with.html)`(`\
`              ``{`\
`                ``plot`` ``<-`` ``plot`` ``+`\
`                  ``ggplot2``::`[`ggtitle`](https://ggplot2.tidyverse.org/reference/labs.html)`(``title``)`` ``+`\
`                  ``cowplot``::`[`theme_cowplot`](https://wilkelab.org/cowplot/reference/theme_cowplot.html)`(``)`\
`              ``}``,`\
`              title ``=`` ``input``$``title`\
`            ``)`\
`        ``}``)`\
`      ``}``)`\
`    ``}`\
`  ``)`\
`}`\
\
`app`` ``<-`` ``init``(`\
`  data ``=`` ``data``,`\
`  modules ``=`` ``modules``(`\
`    `[`tm_g_km`](https://insightsengineering.github.io/teal.modules.clinical/reference/tm_g_km.md)`(`\
`      label ``=`` ``"tm_g_km"``,`\
`      dataname ``=`` ``"ADTTE"``,`\
`      parentname ``=`` ``"ADSL"``,`\
`      arm_var ``=`` ``variables``(`\
`        choices ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"ARM"``, ``"ARMCD"``, ``"ACTARMCD"``)``,`\
`        selected ``=`` ``"ARM"``,`\
`        multiple ``=`` ``FALSE`\
`      ``)``,`\
`      paramcd ``=`` ``picks``(`\
`        ``datasets``(``"ADTTE"``)``,`\
`        ``variables``(``"PARAMCD"``, fixed ``=`` ``TRUE``)``,`\
`        ``values``(`\
`          choices ``=`` `[`unique`](https://rdrr.io/r/base/unique.html)`(``ADTTE``$``PARAMCD``)``,`\
`          selected ``=`` ``"OS"``,`\
`          multiple ``=`` ``FALSE`\
`        ``)`\
`      ``)``,`\
`      arm_ref_comp ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(`\
`        ACTARMCD ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``ref ``=`` ``"ARM B"``, comp ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"ARM A"``, ``"ARM C"``)``)``,`\
`        ARM ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``ref ``=`` ``"B: Placebo"``, comp ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"A: Drug X"``, ``"C: Combination"``)``)`\
`      ``)``,`\
`      strata_var ``=`` ``variables``(`\
`        choices ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"SEX"``, ``"BMRKR2"``)``,`\
`        selected ``=`` ``"SEX"``,`\
`        multiple ``=`` ``FALSE`\
`      ``)``,`\
`      facet_var ``=`` ``variables``(`\
`        choices ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"SEX"``, ``"BMRKR2"``)``,`\
`        selected ``=`` ``NULL``,`\
`        multiple ``=`` ``FALSE`\
`      ``)``,`\
`      decorators ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``plot ``=`` ``ggplot_caption_decorator``(``)``)`\
`    ``)`\
`  ``)`\
`)`

    ## Warning in FALSE: variables has eager choices (character) while datasets has
    ## dynamic choices. It is not guaranteed that explicitly defined choices will be a
    ## subset of data selected in a previous element.

\
`if`` ``(`[`interactive`](https://rdrr.io/r/base/interactive.html)`(``)``)`` ``{`\
`  ``shinyApp``(``app``$``ui``, ``app``$``server``)`\
`}`

## Decorating `datatables`

Here’s an example to showcase how you can edit an output of class
`datatables`. Please refer the [helper
functions](https://rstudio.github.io/DT/functions.html) of the `DT`
package to learn more about extending the `datatables` objects.

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`teal.modules.clinical`](https://insightsengineering.github.io/teal.modules.clinical/)`)`\
\
`data`` ``<-`` `[`teal_data`](https://insightsengineering.github.io/teal.data/latest-tag/reference/teal_data.html)`(``join_keys ``=`` ``default_cdisc_join_keys``[`[`c`](https://rdrr.io/r/base/c.html)`(``"ADSL"``, ``"ADRS"``)``]``)`\
`data`` ``<-`` `[`within`](https://rdrr.io/r/base/with.html)`(``data``, ``{`\
`  ``ADSL`` ``<-`` ``rADSL`\
`  ``ADLB`` ``<-`` ``tmc_ex_adlb`` ``|>`\
`    `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``AVISIT ``=`` ``forcats``::`[`fct_reorder`](https://forcats.tidyverse.org/reference/fct_reorder.html)`(``AVISIT``, ``AVISITN``, ``min``)``)`` ``|>`\
`    `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(`\
`      ONTRTFL ``=`` `[`case_when`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)`(`\
`        ``AVISIT`` `[`%in%`](https://rdrr.io/r/base/match.html)` `[`c`](https://rdrr.io/r/base/c.html)`(``"SCREENING"``, ``"BASELINE"``)`` ``~`` ``""``,`\
`        ``TRUE`` ``~`` ``"Y"`\
`      ``)`` ``|>`` `[`with_label`](https://pharmaverse.github.io/formatters/latest-tag/reference/with_label.html)`(``"On Treatment Record Flag"``)`\
`    ``)`\
`}``)`\
[`join_keys`](https://insightsengineering.github.io/teal.data/latest-tag/reference/join_keys.html)`(``data``)`` ``<-`` ``default_cdisc_join_keys``[`[`names`](https://rdrr.io/r/base/names.html)`(``data``)``]`\
\
`dt_table_decorator`` ``<-`` ``function``(``color1`` ``=`` ``"pink"``, ``color2`` ``=`` ``"lightblue"``)`` ``{`\
`  ``teal_transform_module``(`\
`    label ``=`` ``"Table color"``,`\
`    ui ``=`` ``function``(``id``)`` ``{`\
`      ``selectInput``(`\
`        ``NS``(``id``, ``"color"``)``,`\
`        ``"Table Color"``,`\
`        choices ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"white"``, ``color1``, ``color2``)``,`\
`        selected ``=`` ``"Default"`\
`      ``)`\
`    ``}``,`\
`    server ``=`` ``function``(``id``, ``data``)`` ``{`\
`      ``moduleServer``(``id``, ``function``(``input``, ``output``, ``session``)`` ``{`\
`        ``reactive``(``{`\
`          `[`data`](https://rdrr.io/r/utils/data.html)`(``)`` ``|>`` `[`within`](https://rdrr.io/r/base/with.html)`(`\
`            ``{`\
`              ``table`` ``<-`` ``DT``::`[`formatStyle`](https://rdrr.io/pkg/DT/man/formatCurrency.html)`(`\
`                ``table``,`\
`                columns ``=`` `[`attr`](https://rdrr.io/r/base/attr.html)`(``table``$``x``, ``"colnames"``)``[``-``1``]``,`\
`                target ``=`` ``"row"``,`\
`                backgroundColor ``=`` ``color`\
`              ``)`\
`            ``}``,`\
`            color ``=`` ``input``$``color`\
`          ``)`\
`        ``}``)`\
`      ``}``)`\
`    ``}`\
`  ``)`\
`}`\
\
`app`` ``<-`` ``init``(`\
`  data ``=`` ``data``,`\
`  modules ``=`` ``modules``(`\
`    `[`tm_t_pp_laboratory`](https://insightsengineering.github.io/teal.modules.clinical/reference/tm_t_pp_laboratory.md)`(`\
`      label ``=`` ``"tm_t_pp_laboratory"``,`\
`      dataname ``=`` ``"ADLB"``,`\
`      patient_col ``=`` ``"USUBJID"``,`\
`      paramcd ``=`` ``variables``(``"PARAMCD"``, fixed ``=`` ``TRUE``)``,`\
`      param ``=`` ``variables``(``"PARAM"``, fixed ``=`` ``TRUE``)``,`\
`      time_points ``=`` ``variables``(``"ADY"``, fixed ``=`` ``TRUE``)``,`\
`      anrind ``=`` ``variables``(``"ANRIND"``, fixed ``=`` ``TRUE``)``,`\
`      aval_var ``=`` ``variables``(``"AVAL"``, fixed ``=`` ``TRUE``)``,`\
`      avalu_var ``=`` ``variables``(``"AVALU"``, fixed ``=`` ``TRUE``)``,`\
`      decorators ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``table ``=`` ``dt_table_decorator``(``)``)`\
`    ``)`\
`  ``)`\
`)`

    ## Warning: The `decorators` argument of `tm_t_pp_laboratory()` is deprecated as of
    ## teal.modules.clinical 0.11.0.
    ## ℹ Decorators functionality was removed from this module. The `decorators`
    ##   argument will be ignored.
    ## This warning is displayed once per session.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

\
`if`` ``(`[`interactive`](https://rdrr.io/r/base/interactive.html)`(``)``)`` ``{`\
`  ``shinyApp``(``app``$``ui``, ``app``$``server``)`\
`}`
