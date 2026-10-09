# Example Data Generation

## Generating minimal data to test `teal.modules.clinical`

The following script is used to create and save cached synthetic `CDISC`
data to the `data/` directory to use in examples and tests in the
`teal.modules.clinical` package. This script/vignette was initialized by
Emily de la Rua in `tern`.

*Disclaimer*: this vignette concerns mainly the development of minimal
and stable test data and it is kept internal for feature tracking.

## Setup & Helper Functions

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`dplyr`](https://dplyr.tidyverse.org)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`teal.data`](https://insightsengineering.github.io/teal.data/)`)`\
\
`study_duration_secs`` ``<-`` ``lubridate``::`[`seconds`](https://lubridate.tidyverse.org/reference/period.html)`(``lubridate``::`[`years`](https://lubridate.tidyverse.org/reference/period.html)`(``2``)``)`\
\
`sample_fct`` ``<-`` ``function``(``x``, ``N``, ``...``)`` ``{`` `\
`  ``checkmate``::`[`assert_number`](https://mllg.github.io/checkmate/reference/checkNumber.html)`(``N``)`\
`  `[`factor`](https://rdrr.io/r/base/factor.html)`(`[`sample`](https://rdrr.io/r/base/sample.html)`(``x``, ``N``, replace ``=`` ``TRUE``, ``...``)``, levels ``=`` ``if`` ``(`[`is.factor`](https://rdrr.io/r/base/factor.html)`(``x``)``)`` `[`levels`](https://rdrr.io/r/base/levels.html)`(``x``)`` ``else`` ``x``)`\
`}`\
\
`retain`` ``<-`` ``function``(``df``, ``value_var``, ``event``, ``outside`` ``=`` ``NA``)`` ``{`\
`  ``indices`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``1``, `[`which`](https://rdrr.io/r/base/which.html)`(``event`` ``==`` ``TRUE``)``, `[`nrow`](https://rdrr.io/r/base/nrow.html)`(``df``)`` ``+`` ``1``)`\
`  ``values`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``outside``, ``value_var``[``event`` ``==`` ``TRUE``]``)`\
`  `[`rep`](https://rdrr.io/r/base/rep.html)`(``values``, `[`diff`](https://rdrr.io/r/base/diff.html)`(``indices``)``)`\
`}`\
\
`relvar_init`` ``<-`` ``function``(``relvar1``, ``relvar2``)`` ``{`\
`  ``if`` ``(`[`length`](https://rdrr.io/r/base/length.html)`(``relvar1``)`` ``!=`` `[`length`](https://rdrr.io/r/base/length.html)`(``relvar2``)``)`` ``{`\
`    `[`message`](https://rdrr.io/r/base/message.html)`(`[`simpleError`](https://rdrr.io/r/base/conditions.html)`(`\
`      ``"The argument value length of relvar1 and relvar2 differ. They must contain the same number of elements."`\
`    ``)``)`\
`    `[`return`](https://rdrr.io/r/base/function.html)`(``NA``)`\
`  ``}`\
`  ``List``(``"relvar1"`` ``=`` ``relvar1``, ``"relvar2"`` ``=`` ``relvar2``)`\
`}`\
\
`rel_var`` ``<-`` ``function``(``df`` ``=`` ``NULL``, ``var_name`` ``=`` ``NULL``, ``var_values`` ``=`` ``NULL``, ``related_var`` ``=`` ``NULL``)`` ``{`\
`  ``if`` ``(`[`is.null`](https://rdrr.io/r/base/NULL.html)`(``df``)``)`` ``{`\
`    `[`message`](https://rdrr.io/r/base/message.html)`(``"Missing data frame argument value."``)`\
`    ``NA`\
`  ``}`` ``else`` ``{`\
`    ``n_relvar1`` ``<-`` `[`length`](https://rdrr.io/r/base/length.html)`(`[`unique`](https://rdrr.io/r/base/unique.html)`(``df``[``, ``related_var``, drop ``=`` ``TRUE``]``)``)`\
`    ``n_relvar2`` ``<-`` `[`length`](https://rdrr.io/r/base/length.html)`(``var_values``)`\
`    ``if`` ``(``n_relvar1`` ``!=`` ``n_relvar2``)`` ``{`\
`      `[`message`](https://rdrr.io/r/base/message.html)`(`[`paste`](https://rdrr.io/r/base/paste.html)`(``"Unequal vector lengths for"``, ``related_var``, ``"and"``, ``var_name``)``)`\
`      ``NA`\
`    ``}`` ``else`` ``{`\
`      ``relvar1`` ``<-`` `[`unique`](https://rdrr.io/r/base/unique.html)`(``df``[``, ``related_var``, drop ``=`` ``TRUE``]``)`\
`      ``relvar2_values`` ``<-`` `[`rep`](https://rdrr.io/r/base/rep.html)`(``NA``, `[`nrow`](https://rdrr.io/r/base/nrow.html)`(``df``)``)`\
`      ``for`` ``(``r`` ``in`` `[`seq_along`](https://rdrr.io/r/base/seq.html)`(``relvar1``)``)`` ``{`\
`        ``matched`` ``<-`` `[`which`](https://rdrr.io/r/base/which.html)`(``df``[``, ``related_var``, drop ``=`` ``TRUE``]`` ``==`` ``relvar1``[``r``]``)`\
`        ``relvar2_values``[``matched``]`` ``<-`` ``var_values``[``r``]`\
`      ``}`\
`      ``relvar2_values`\
`    ``}`\
`  ``}`\
`}`\
\
`visit_schedule`` ``<-`` ``function``(``visit_format`` ``=`` ``"WEEK"``,`\
`                           ``n_assessments`` ``=`` ``10L``,`\
`                           ``n_days`` ``=`` ``5L``)`` ``{`\
`  ``if`` ``(``!``(`[`toupper`](https://rdrr.io/r/base/chartr.html)`(``visit_format``)`` `[`%in%`](https://rdrr.io/r/base/match.html)` `[`c`](https://rdrr.io/r/base/c.html)`(``"WEEK"``, ``"CYCLE"``)``)``)`` ``{`\
`    `[`message`](https://rdrr.io/r/base/message.html)`(``"Visit format value must either be: WEEK or CYCLE"``)`\
`    `[`return`](https://rdrr.io/r/base/function.html)`(``NA``)`\
`  ``}`\
`  ``if`` ``(`[`toupper`](https://rdrr.io/r/base/chartr.html)`(``visit_format``)`` ``==`` ``"WEEK"``)`` ``{`\
`    ``assessments`` ``<-`` ``1``:``n_assessments`\
`    ``assessments_ord`` ``<-`` ``-``1``:``n_assessments`\
`    ``visit_values`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``"SCREENING"``, ``"BASELINE"``, `[`paste`](https://rdrr.io/r/base/paste.html)`(`[`toupper`](https://rdrr.io/r/base/chartr.html)`(``visit_format``)``, ``assessments``, ``"DAY"``, ``(``assessments`` ``*`` ``7``)`` ``+`` ``1``)``)`\
`  ``}`` ``else`` ``if`` ``(`[`toupper`](https://rdrr.io/r/base/chartr.html)`(``visit_format``)`` ``==`` ``"CYCLE"``)`` ``{`\
`    ``cycles`` ``<-`` `[`sort`](https://rdrr.io/r/base/sort.html)`(`[`rep`](https://rdrr.io/r/base/rep.html)`(``1``:``n_assessments``, times ``=`` ``1``, each ``=`` ``n_days``)``)`\
`    ``days`` ``<-`` `[`rep`](https://rdrr.io/r/base/rep.html)`(`[`seq`](https://rdrr.io/r/base/seq.html)`(``1``:``n_days``)``, times ``=`` ``n_assessments``, each ``=`` ``1``)`\
`    ``assessments_ord`` ``<-`` ``0``:``(``n_assessments`` ``*`` ``n_days``)`\
`    ``visit_values`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``"SCREENING"``, `[`paste`](https://rdrr.io/r/base/paste.html)`(`[`toupper`](https://rdrr.io/r/base/chartr.html)`(``visit_format``)``, ``cycles``, ``"DAY"``, ``days``)``)`\
`  ``}`\
`  ``visit_values`` ``<-`` ``stats``::`[`reorder`](https://rdrr.io/r/stats/reorder.factor.html)`(`[`factor`](https://rdrr.io/r/base/factor.html)`(``visit_values``)``, ``assessments_ord``)`\
`}`\
\
`rtpois`` ``<-`` ``function``(``n``, ``lambda``)`` ``stats``::`[`qpois`](https://rdrr.io/r/stats/Poisson.html)`(``stats``::`[`runif`](https://rdrr.io/r/stats/Uniform.html)`(``n``, ``stats``::`[`dpois`](https://rdrr.io/r/stats/Poisson.html)`(``0``, ``lambda``)``, ``1``)``, ``lambda``)`\
\
`rtexp`` ``<-`` ``function``(``n``, ``rate``, ``l`` ``=`` ``NULL``, ``r`` ``=`` ``NULL``)`` ``{`\
`  ``if`` ``(``!`[`is.null`](https://rdrr.io/r/base/NULL.html)`(``l``)``)`` ``{`\
`    ``l`` ``-`` `[`log`](https://rdrr.io/r/base/Log.html)`(``1`` ``-`` ``stats``::`[`runif`](https://rdrr.io/r/stats/Uniform.html)`(``n``)``)`` ``/`` ``rate`\
`  ``}`` ``else`` ``if`` ``(``!`[`is.null`](https://rdrr.io/r/base/NULL.html)`(``r``)``)`` ``{`\
`    ``-`[`log`](https://rdrr.io/r/base/Log.html)`(``1`` ``-`` ``stats``::`[`runif`](https://rdrr.io/r/stats/Uniform.html)`(``n``)`` ``*`` ``(``1`` ``-`` `[`exp`](https://rdrr.io/r/base/Log.html)`(``-``r`` ``*`` ``rate``)``)``)`` ``/`` ``rate`\
`  ``}`` ``else`` ``{`\
`    ``stats``::`[`rexp`](https://rdrr.io/r/stats/Exponential.html)`(``n``, ``rate``)`\
`  ``}`\
`}`\
\
`str_extract`` ``<-`` ``function``(``string``, ``pattern``)`` `[`regmatches`](https://rdrr.io/r/base/regmatches.html)`(``string``, `[`gregexpr`](https://rdrr.io/r/base/grep.html)`(``pattern``, ``string``)``)`\
\
`with_label`` ``<-`` ``function``(``x``, ``label``)`` ``{`\
`  `[`attr`](https://rdrr.io/r/base/attr.html)`(``x``, ``"label"``)`` ``<-`` `[`as.vector`](https://rdrr.io/r/base/vector.html)`(``label``)`\
`  ``x`\
`}`\
\
`common_var_labels`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(`\
`  USUBJID ``=`` ``"Unique Subject Identifier"``,`\
`  STUDYID ``=`` ``"Study Identifier"``,`\
`  PARAM ``=`` ``"Parameter"``,`\
`  PARAMCD ``=`` ``"Parameter Code"``,`\
`  AVISIT ``=`` ``"Analysis Visit"``,`\
`  AVISITN ``=`` ``"Analysis Visit (N)"``,`\
`  AVAL ``=`` ``"Analysis Value"``,`\
`  AVALU ``=`` ``"Analysis Value Unit"``,`\
`  AVALC ``=`` ``"Character Result/Finding"``,`\
`  BASE ``=`` ``"Baseline Value"``,`\
`  BASE2 ``=`` ``"Screening Value"``,`\
`  ABLFL ``=`` ``"Baseline Record Flag"``,`\
`  ABLFL2 ``=`` ``"Screening Record Flag"``,`\
`  CHG ``=`` ``"Absolute Change from Baseline"``,`\
`  PCHG ``=`` ``"Percentage Change from Baseline"``,`\
`  ANRIND ``=`` ``"Analysis Reference Range Indicator"``,`\
`  BNRIND ``=`` ``"Baseline Reference Range Indicator"``,`\
`  ANRLO ``=`` ``"Analysis Normal Range Lower Limit"``,`\
`  ANRHI ``=`` ``"Analysis Normal Range Upper Limit"``,`\
`  CNSR ``=`` ``"Censor"``,`\
`  ADTM ``=`` ``"Analysis Datetime"``,`\
`  ADY ``=`` ``"Analysis Relative Day"``,`\
`  ASTDY ``=`` ``"Analysis Start Relative Day"``,`\
`  AENDY ``=`` ``"Analysis End Relative Day"``,`\
`  ASTDTM ``=`` ``"Analysis Start Datetime"``,`\
`  AENDTM ``=`` ``"Analysis End Datetime"``,`\
`  VISITDY ``=`` ``"Planned Study Day of Visit"``,`\
`  EVNTDESC ``=`` ``"Event or Censoring Description"``,`\
`  CNSDTDSC ``=`` ``"Censor Date Description"``,`\
`  BASETYPE ``=`` ``"Baseline Type"``,`\
`  DTYPE ``=`` ``"Derivation Type"``,`\
`  ONTRTFL ``=`` ``"On Treatment Record Flag"``,`\
`  WORS01FL ``=`` ``"Worst Observation in Window Flag 01"``,`\
`  WORS02FL ``=`` ``"Worst Post-Baseline Observation"`\
`)`

## `ADSL`

\
`generate_adsl`` ``<-`` ``function``(``N`` ``=`` ``200``)`` ``{`` `\
`  `[`set.seed`](https://rdrr.io/r/base/Random.html)`(``1``)`\
`  ``sys_dtm`` ``<-`` ``lubridate``::`[`fast_strptime`](https://lubridate.tidyverse.org/reference/parse_date_time.html)`(``"20/2/2019 11:16:16.683"``, ``"%d/%m/%Y %H:%M:%OS"``, tz ``=`` ``"UTC"``)`\
`  ``country_site_prob`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``.5``, ``.121``, ``.077``, ``.077``, ``.075``, ``.052``, ``.046``, ``.025``, ``.014``, ``.003``)`\
\
`  ``adsl`` ``<-`` ``tibble``::`[`tibble`](https://tibble.tidyverse.org/reference/tibble.html)`(`\
`    STUDYID ``=`` `[`rep`](https://rdrr.io/r/base/rep.html)`(``"AB12345"``, ``N``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `[`with_label`](https://pharmaverse.github.io/formatters/latest-tag/reference/with_label.html)`(``"Study Identifier"``)``,`\
`    COUNTRY ``=`` ``sample_fct``(`\
`      `[`c`](https://rdrr.io/r/base/c.html)`(``"CHN"``, ``"USA"``, ``"BRA"``, ``"PAK"``, ``"NGA"``, ``"RUS"``, ``"JPN"``, ``"GBR"``, ``"CAN"``, ``"CHE"``)``,`\
`      ``N``,`\
`      prob ``=`` ``country_site_prob`\
`    ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `[`with_label`](https://pharmaverse.github.io/formatters/latest-tag/reference/with_label.html)`(``"Country"``)``,`\
`    SITEID ``=`` ``sample_fct``(``1``:``20``, ``N``, prob ``=`` `[`rep`](https://rdrr.io/r/base/rep.html)`(``country_site_prob``, times ``=`` ``2``)``)``,`\
`    SUBJID ``=`` `[`paste`](https://rdrr.io/r/base/paste.html)`(``"id"``, `[`seq_len`](https://rdrr.io/r/base/seq.html)`(``N``)``, sep ``=`` ``"-"``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `[`with_label`](https://pharmaverse.github.io/formatters/latest-tag/reference/with_label.html)`(``"Subject Identifier for the Study"``)``,`\
`    AGE ``=`` ``(`[`sapply`](https://rdrr.io/r/base/lapply.html)`(``stats``::`[`rchisq`](https://rdrr.io/r/stats/Chisquare.html)`(``N``, df ``=`` ``5``, ncp ``=`` ``10``)``, ``max``, ``0``)`` ``+`` ``20``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `[`with_label`](https://pharmaverse.github.io/formatters/latest-tag/reference/with_label.html)`(``"Age"``)``,`\
`    SEX ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"F"``, ``"M"``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` ``sample_fct``(``N``, prob ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``.52``, ``.48``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `[`with_label`](https://pharmaverse.github.io/formatters/latest-tag/reference/with_label.html)`(``"Sex"``)``,`\
`    ARMCD ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"ARM A"``, ``"ARM B"``, ``"ARM C"``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` ``sample_fct``(``N``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `[`with_label`](https://pharmaverse.github.io/formatters/latest-tag/reference/with_label.html)`(``"Planned Arm Code"``)``,`\
`    ARM ``=`` ``dplyr``::`[`recode`](https://dplyr.tidyverse.org/reference/recode.html)`(`\
`      ``.data``$``ARMCD``,`\
`      ``"ARM A"`` ``=`` ``"A: Drug X"``, ``"ARM B"`` ``=`` ``"B: Placebo"``, ``"ARM C"`` ``=`` ``"C: Combination"`\
`    ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `[`with_label`](https://pharmaverse.github.io/formatters/latest-tag/reference/with_label.html)`(``"Description of Planned Arm"``)``,`\
`    ACTARMCD ``=`` ``.data``$``ARMCD`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `[`with_label`](https://pharmaverse.github.io/formatters/latest-tag/reference/with_label.html)`(``"Actual Arm Code"``)``,`\
`    ACTARM ``=`` ``.data``$``ARM`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `[`with_label`](https://pharmaverse.github.io/formatters/latest-tag/reference/with_label.html)`(``"Description of Actual Arm"``)``,`\
`    RACE ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(`\
`      ``"ASIAN"``, ``"BLACK OR AFRICAN AMERICAN"``, ``"WHITE"``, ``"AMERICAN INDIAN OR ALASKA NATIVE"``,`\
`      ``"MULTIPLE"``, ``"NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER"``, ``"OTHER"``, ``"UNKNOWN"`\
`    ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`      ``sample_fct``(``N``, prob ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``.55``, ``.23``, ``.16``, ``.05``, ``.004``, ``.003``, ``.002``, ``.002``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`      `[`with_label`](https://pharmaverse.github.io/formatters/latest-tag/reference/with_label.html)`(``"Race"``)``,`\
`    TRTSDTM ``=`` ``sys_dtm`` ``+`` `[`sample`](https://rdrr.io/r/base/sample.html)`(`[`seq`](https://rdrr.io/r/base/seq.html)`(``0``, ``study_duration_secs``)``, size ``=`` ``N``, replace ``=`` ``TRUE``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`      `[`with_label`](https://pharmaverse.github.io/formatters/latest-tag/reference/with_label.html)`(``"Datetime of First Exposure to Treatment"``)``,`\
`    TRTEDTM ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``TRTSDTM`` ``+`` ``study_duration_secs``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`      `[`with_label`](https://pharmaverse.github.io/formatters/latest-tag/reference/with_label.html)`(``"Datetime of Last Exposure to Treatment"``)``,`\
`    EOSDY ``=`` `[`ceiling`](https://rdrr.io/r/base/Round.html)`(`[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(`[`difftime`](https://rdrr.io/r/base/difftime.html)`(``TRTEDTM``, ``TRTSDTM``, units ``=`` ``"days"``)``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`      `[`with_label`](https://pharmaverse.github.io/formatters/latest-tag/reference/with_label.html)`(``"End of Study Relative Day"``)``,`\
`    EOSDT ``=`` ``lubridate``::`[`date`](https://lubridate.tidyverse.org/reference/date.html)`(``TRTEDTM``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `[`with_label`](https://pharmaverse.github.io/formatters/latest-tag/reference/with_label.html)`(``"End of Study Date"``)``,`\
`    STRATA1 ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"A"``, ``"B"``, ``"C"``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` ``sample_fct``(``N``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `[`with_label`](https://pharmaverse.github.io/formatters/latest-tag/reference/with_label.html)`(``"Stratification Factor 1"``)``,`\
`    STRATA2 ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"S1"``, ``"S2"``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` ``sample_fct``(``N``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `[`with_label`](https://pharmaverse.github.io/formatters/latest-tag/reference/with_label.html)`(``"Stratification Factor 2"``)``,`\
`    BMRKR1 ``=`` ``stats``::`[`rchisq`](https://rdrr.io/r/stats/Chisquare.html)`(``N``, ``6``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `[`with_label`](https://pharmaverse.github.io/formatters/latest-tag/reference/with_label.html)`(``"Continuous Level Biomarker 1"``)``,`\
`    BMRKR2 ``=`` ``sample_fct``(`[`c`](https://rdrr.io/r/base/c.html)`(``"LOW"``, ``"MEDIUM"``, ``"HIGH"``)``, ``N``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `[`with_label`](https://pharmaverse.github.io/formatters/latest-tag/reference/with_label.html)`(``"Continuous Level Biomarker 2"``)`\
`  ``)`\
\
`  ``# associate sites with countries and regions`\
`  ``adsl`` ``<-`` ``adsl`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(`\
`      SITEID ``=`` `[`paste0`](https://rdrr.io/r/base/paste.html)`(``.data``$``COUNTRY``, ``"-"``, ``.data``$``SITEID``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `[`with_label`](https://pharmaverse.github.io/formatters/latest-tag/reference/with_label.html)`(``"Study Site Identifier"``)``,`\
`      REGION1 ``=`` `[`factor`](https://rdrr.io/r/base/factor.html)`(``dplyr``::`[`case_when`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)`(`\
`        ``COUNTRY`` `[`%in%`](https://rdrr.io/r/base/match.html)` `[`c`](https://rdrr.io/r/base/c.html)`(``"NGA"``)`` ``~`` ``"Africa"``,`\
`        ``COUNTRY`` `[`%in%`](https://rdrr.io/r/base/match.html)` `[`c`](https://rdrr.io/r/base/c.html)`(``"CHN"``, ``"JPN"``, ``"PAK"``)`` ``~`` ``"Asia"``,`\
`        ``COUNTRY`` `[`%in%`](https://rdrr.io/r/base/match.html)` `[`c`](https://rdrr.io/r/base/c.html)`(``"RUS"``)`` ``~`` ``"Eurasia"``,`\
`        ``COUNTRY`` `[`%in%`](https://rdrr.io/r/base/match.html)` `[`c`](https://rdrr.io/r/base/c.html)`(``"GBR"``)`` ``~`` ``"Europe"``,`\
`        ``COUNTRY`` `[`%in%`](https://rdrr.io/r/base/match.html)` `[`c`](https://rdrr.io/r/base/c.html)`(``"CAN"``, ``"USA"``)`` ``~`` ``"North America"``,`\
`        ``COUNTRY`` `[`%in%`](https://rdrr.io/r/base/match.html)` `[`c`](https://rdrr.io/r/base/c.html)`(``"BRA"``)`` ``~`` ``"South America"``,`\
`        ``TRUE`` ``~`` `[`as.character`](https://rdrr.io/r/base/character.html)`(``NA``)`\
`      ``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `[`with_label`](https://pharmaverse.github.io/formatters/latest-tag/reference/with_label.html)`(``"Geographic Region 1"``)``,`\
`      SAFFL ``=`` `[`factor`](https://rdrr.io/r/base/factor.html)`(``"Y"``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `[`with_label`](https://pharmaverse.github.io/formatters/latest-tag/reference/with_label.html)`(``"Safety Population Flag"``)`\
`    ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(`\
`      USUBJID ``=`` `[`paste`](https://rdrr.io/r/base/paste.html)`(``.data``$``STUDYID``, ``.data``$``SITEID``, ``.data``$``SUBJID``, sep ``=`` ``"-"``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`        `[`with_label`](https://pharmaverse.github.io/formatters/latest-tag/reference/with_label.html)`(``"Unique Subject Identifier"``)`\
`    ``)`\
\
`  ``# disposition related variables`\
`  ``# using probability of 1 for the "DEATH" level to ensure at least one death record exists`\
`  ``l_dcsreas`` ``<-`` `[`list`](https://rdrr.io/r/base/list.html)`(`\
`    choices ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(`\
`      ``"ADVERSE EVENT"``, ``"DEATH"``, ``"LACK OF EFFICACY"``, ``"PHYSICIAN DECISION"``,`\
`      ``"PROTOCOL VIOLATION"``, ``"WITHDRAWAL BY PARENT/GUARDIAN"``, ``"WITHDRAWAL BY SUBJECT"`\
`    ``)``,`\
`    prob ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``.2``, ``1``, ``.1``, ``.1``, ``.2``, ``.1``, ``.1``)`\
`  ``)`\
`  ``l_dthcat_other`` ``<-`` `[`list`](https://rdrr.io/r/base/list.html)`(`\
`    choices ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(`\
`      ``"Post-study reporting of death"``, ``"LOST TO FOLLOW UP"``, ``"MISSING"``, ``"SUICIDE"``, ``"UNKNOWN"`\
`    ``)``,`\
`    prob ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``.1``, ``.3``, ``.3``, ``.2``, ``.1``)`\
`  ``)`\
\
`  ``adsl`` ``<-`` ``adsl`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(`\
`      EOSSTT ``=`` ``dplyr``::`[`case_when`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)`(`\
`        ``EOSDY`` ``==`` `[`max`](https://rdrr.io/r/base/Extremes.html)`(``EOSDY``, na.rm ``=`` ``TRUE``)`` ``~`` ``"COMPLETED"``,`\
`        ``EOSDY`` ``<`` `[`max`](https://rdrr.io/r/base/Extremes.html)`(``EOSDY``, na.rm ``=`` ``TRUE``)`` ``~`` ``"DISCONTINUED"``,`\
`        `[`is.na`](https://rdrr.io/r/base/NA.html)`(``TRTEDTM``)`` ``~`` ``"ONGOING"`\
`      ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `[`with_label`](https://pharmaverse.github.io/formatters/latest-tag/reference/with_label.html)`(``"End of Study Status"``)`\
`    ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(`\
`      EOTSTT ``=`` ``.data``$``EOSSTT`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `[`with_label`](https://pharmaverse.github.io/formatters/latest-tag/reference/with_label.html)`(``"End of Treatment Status"``)`\
`    ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(`\
`      DCSREAS ``=`` `[`ifelse`](https://rdrr.io/r/base/ifelse.html)`(`\
`        ``.data``$``EOSSTT`` ``==`` ``"DISCONTINUED"``,`\
`        `[`sample`](https://rdrr.io/r/base/sample.html)`(``x ``=`` ``l_dcsreas``$``choices``, size ``=`` ``N``, replace ``=`` ``TRUE``, prob ``=`` ``l_dcsreas``$``prob``)``,`\
`        `[`as.character`](https://rdrr.io/r/base/character.html)`(``NA``)`\
`      ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `[`with_label`](https://pharmaverse.github.io/formatters/latest-tag/reference/with_label.html)`(``"Reason for Discontinuation from Study"``)`\
`    ``)`\
\
`  ``tmc_ex_adsl`` ``<-`` ``adsl`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``DTHDT ``=`` ``dplyr``::`[`case_when`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)`(`\
`      ``DCSREAS`` ``==`` ``"DEATH"`` ``~`` ``lubridate``::`[`date`](https://lubridate.tidyverse.org/reference/date.html)`(``TRTEDTM`` ``+`` ``lubridate``::`[`days`](https://lubridate.tidyverse.org/reference/period.html)`(`[`sample`](https://rdrr.io/r/base/sample.html)`(``0``:``50``, size ``=`` ``N``, replace ``=`` ``TRUE``)``)``)`\
`    ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `[`with_label`](https://pharmaverse.github.io/formatters/latest-tag/reference/with_label.html)`(``"Date of Death"``)``)`\
\
`  `[`save`](https://rdrr.io/r/base/save.html)`(``tmc_ex_adsl``, file ``=`` ``"data/tmc_ex_adsl.rda"``, compress ``=`` ``"xz"``)`\
`}`

## `ADAE`

\
`generate_adae`` ``<-`` ``function``(``adsl`` ``=`` ``tmc_ex_adsl``,`\
`                          ``max_n_aes`` ``=`` ``5``)`` ``{`\
`  `[`set.seed`](https://rdrr.io/r/base/Random.html)`(``1``)`\
`  ``lookup_ae`` ``<-`` ``tibble``::`[`tribble`](https://tibble.tidyverse.org/reference/tribble.html)`(`\
`    ``~``AEBODSYS``, ``~``AELLT``, ``~``AEDECOD``, ``~``AEHLT``, ``~``AEHLGT``, ``~``AETOXGR``, ``~``AESOC``, ``~``AESER``, ``~``AEREL``,`\
`    ``"cl A.1"``, ``"llt A.1.1.1.1"``, ``"dcd A.1.1.1.1"``, ``"hlt A.1.1.1"``, ``"hlgt A.1.1"``, ``"1"``, ``"cl A"``, ``"N"``, ``"N"``,`\
`    ``"cl A.1"``, ``"llt A.1.1.1.2"``, ``"dcd A.1.1.1.2"``, ``"hlt A.1.1.1"``, ``"hlgt A.1.1"``, ``"2"``, ``"cl A"``, ``"Y"``, ``"N"``,`\
`    ``"cl B.1"``, ``"llt B.1.1.1.1"``, ``"dcd B.1.1.1.1"``, ``"hlt B.1.1.1"``, ``"hlgt B.1.1"``, ``"5"``, ``"cl B"``, ``"Y"``, ``"Y"``,`\
`    ``"cl B.2"``, ``"llt B.2.1.2.1"``, ``"dcd B.2.1.2.1"``, ``"hlt B.2.1.2"``, ``"hlgt B.2.1"``, ``"3"``, ``"cl B"``, ``"N"``, ``"N"``,`\
`    ``"cl B.2"``, ``"llt B.2.2.3.1"``, ``"dcd B.2.2.3.1"``, ``"hlt B.2.2.3"``, ``"hlgt B.2.2"``, ``"1"``, ``"cl B"``, ``"Y"``, ``"N"``,`\
`    ``"cl C.1"``, ``"llt C.1.1.1.3"``, ``"dcd C.1.1.1.3"``, ``"hlt C.1.1.1"``, ``"hlgt C.1.1"``, ``"4"``, ``"cl C"``, ``"N"``, ``"Y"``,`\
`    ``"cl C.2"``, ``"llt C.2.1.2.1"``, ``"dcd C.2.1.2.1"``, ``"hlt C.2.1.2"``, ``"hlgt C.2.1"``, ``"2"``, ``"cl C"``, ``"N"``, ``"Y"``,`\
`    ``"cl D.1"``, ``"llt D.1.1.1.1"``, ``"dcd D.1.1.1.1"``, ``"hlt D.1.1.1"``, ``"hlgt D.1.1"``, ``"5"``, ``"cl D"``, ``"Y"``, ``"Y"``,`\
`    ``"cl D.1"``, ``"llt D.1.1.4.2"``, ``"dcd D.1.1.4.2"``, ``"hlt D.1.1.4"``, ``"hlgt D.1.1"``, ``"3"``, ``"cl D"``, ``"N"``, ``"N"``,`\
`    ``"cl D.2"``, ``"llt D.2.1.5.3"``, ``"dcd D.2.1.5.3"``, ``"hlt D.2.1.5"``, ``"hlgt D.2.1"``, ``"1"``, ``"cl D"``, ``"N"``, ``"Y"`\
`  ``)`\
\
`  ``aag`` ``<-`` ``utils``::`[`read.table`](https://rdrr.io/r/utils/read.table.html)`(`\
`    sep ``=`` ``","``, header ``=`` ``TRUE``,`\
`    text ``=`` `[`paste`](https://rdrr.io/r/base/paste.html)`(`\
`      ``"NAMVAR,SRCVAR,GRPTYPE,REFNAME,REFTERM,SCOPE"``,`\
`      ``"CQ01NAM,AEDECOD,CUSTOM,D.2.1.5.3/A.1.1.1.1 aesi,dcd D.2.1.5.3,"``,`\
`      ``"CQ01NAM,AEDECOD,CUSTOM,D.2.1.5.3/A.1.1.1.1 aesi,dcd A.1.1.1.1,"``,`\
`      ``"SMQ01NAM,AEDECOD,SMQ,C.1.1.1.3/B.2.2.3.1 aesi,dcd C.1.1.1.3,BROAD"``,`\
`      ``"SMQ01NAM,AEDECOD,SMQ,C.1.1.1.3/B.2.2.3.1 aesi,dcd B.2.2.3.1,BROAD"``,`\
`      ``"SMQ02NAM,AEDECOD,SMQ,Y.9.9.9.9/Z.9.9.9.9 aesi,dcd Y.9.9.9.9,NARROW"``,`\
`      ``"SMQ02NAM,AEDECOD,SMQ,Y.9.9.9.9/Z.9.9.9.9 aesi,dcd Z.9.9.9.9,NARROW"``,`\
`      sep ``=`` ``"\n"`\
`    ``)``, stringsAsFactors ``=`` ``FALSE`\
`  ``)`\
\
`  ``adae`` ``<-`` `[`Map`](https://rdrr.io/r/base/funprog.html)`(`\
`    ``function``(``id``, ``sid``)`` ``{`\
`      ``n_aes`` ``<-`` `[`sample`](https://rdrr.io/r/base/sample.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``0``, `[`seq_len`](https://rdrr.io/r/base/seq.html)`(``max_n_aes``)``)``, ``1``)`\
`      ``i`` ``<-`` `[`sample`](https://rdrr.io/r/base/sample.html)`(`[`seq_len`](https://rdrr.io/r/base/seq.html)`(`[`nrow`](https://rdrr.io/r/base/nrow.html)`(``lookup_ae``)``)``, ``n_aes``, ``TRUE``)`\
`      ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(`\
`        ``lookup_ae``[``i``, ``]``,`\
`        USUBJID ``=`` ``id``,`\
`        STUDYID ``=`` ``sid`\
`      ``)`\
`    ``}``,`\
`    ``adsl``$``USUBJID``,`\
`    ``adsl``$``STUDYID`\
`  ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    `[`Reduce`](https://rdrr.io/r/base/funprog.html)`(``rbind``, ``.``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``` `[` ```(`[`c`](https://rdrr.io/r/base/c.html)`(``10``, ``11``, ``1``, ``2``, ``3``, ``4``, ``5``, ``6``, ``7``, ``8``, ``9``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(`\
`      AETERM ``=`` `[`gsub`](https://rdrr.io/r/base/grep.html)`(``"dcd"``, ``"trm"``, ``.data``$``AEDECOD``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `[`with_label`](https://pharmaverse.github.io/formatters/latest-tag/reference/with_label.html)`(``"Reported Term for the Adverse Event"``)``,`\
`      AESEV ``=`` ``dplyr``::`[`case_when`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)`(`\
`        ``AETOXGR`` ``==`` ``1`` ``~`` ``"MILD"``,`\
`        ``AETOXGR`` `[`%in%`](https://rdrr.io/r/base/match.html)` `[`c`](https://rdrr.io/r/base/c.html)`(``2``, ``3``)`` ``~`` ``"MODERATE"``,`\
`        ``AETOXGR`` `[`%in%`](https://rdrr.io/r/base/match.html)` `[`c`](https://rdrr.io/r/base/c.html)`(``4``, ``5``)`` ``~`` ``"SEVERE"`\
`      ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `[`with_label`](https://pharmaverse.github.io/formatters/latest-tag/reference/with_label.html)`(``"Severity/Intensity"``)`\
`    ``)`\
\
`  ``# merge adsl to be able to add AE date and study day variables`\
`  ``adae`` ``<-`` ``dplyr``::`[`inner_join`](https://dplyr.tidyverse.org/reference/mutate-joins.html)`(``adae``, ``adsl``, by ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"STUDYID"``, ``"USUBJID"``)``, multiple ``=`` ``"all"``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`rowwise`](https://dplyr.tidyverse.org/reference/rowwise.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``TRTENDT ``=`` ``lubridate``::`[`date`](https://lubridate.tidyverse.org/reference/date.html)`(``dplyr``::`[`case_when`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)`(`\
`      `[`is.na`](https://rdrr.io/r/base/NA.html)`(``TRTEDTM``)`` ``~`` ``lubridate``::`[`floor_date`](https://lubridate.tidyverse.org/reference/round_date.html)`(``lubridate``::`[`date`](https://lubridate.tidyverse.org/reference/date.html)`(``TRTSDTM``)`` ``+`` ``study_duration_secs``, unit ``=`` ``"day"``)``,`\
`      ``TRUE`` ``~`` ``TRTEDTM`\
`    ``)``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``ASTDTM ``=`` `[`sample`](https://rdrr.io/r/base/sample.html)`(`\
`      `[`seq`](https://rdrr.io/r/base/seq.html)`(``lubridate``::`[`as_datetime`](https://lubridate.tidyverse.org/reference/as_date.html)`(``TRTSDTM``)``, ``lubridate``::`[`as_datetime`](https://lubridate.tidyverse.org/reference/as_date.html)`(``TRTENDT``)``, by ``=`` ``"day"``)``,`\
`      size ``=`` ``1`\
`    ``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``ASTDY ``=`` `[`ceiling`](https://rdrr.io/r/base/Round.html)`(`[`difftime`](https://rdrr.io/r/base/difftime.html)`(``ASTDTM``, ``TRTSDTM``, units ``=`` ``"days"``)``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``# add 1 to end of range incase both values passed to sample() are the same`\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``AENDTM ``=`` `[`sample`](https://rdrr.io/r/base/sample.html)`(`\
`      `[`seq`](https://rdrr.io/r/base/seq.html)`(``lubridate``::`[`as_datetime`](https://lubridate.tidyverse.org/reference/as_date.html)`(``ASTDTM``)``, ``lubridate``::`[`as_datetime`](https://lubridate.tidyverse.org/reference/as_date.html)`(``TRTENDT`` ``+`` ``1``)``, by ``=`` ``"day"``)``,`\
`      size ``=`` ``1`\
`    ``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``AENDY ``=`` `[`ceiling`](https://rdrr.io/r/base/Round.html)`(`[`difftime`](https://rdrr.io/r/base/difftime.html)`(``AENDTM``, ``TRTSDTM``, units ``=`` ``"days"``)``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``LDOSEDTM ``=`` ``dplyr``::`[`case_when`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)`(`\
`      ``TRTSDTM`` ``<`` ``ASTDTM`` ``~`` ``lubridate``::`[`as_datetime`](https://lubridate.tidyverse.org/reference/as_date.html)`(``stats``::`[`runif`](https://rdrr.io/r/stats/Uniform.html)`(``1``, ``TRTSDTM``, ``ASTDTM``)``)``,`\
`      ``TRUE`` ``~`` ``ASTDTM`\
`    ``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`select`](https://dplyr.tidyverse.org/reference/select.html)`(``-``TRTENDT``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`ungroup`](https://dplyr.tidyverse.org/reference/group_by.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`arrange`](https://dplyr.tidyverse.org/reference/arrange.html)`(``.data``$``STUDYID``, ``.data``$``USUBJID``, ``.data``$``ASTDTM``, ``.data``$``AETERM``)`\
\
`  ``adae`` ``<-`` ``adae`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(``.data``$``USUBJID``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``AESEQ ``=`` `[`seq_len`](https://rdrr.io/r/base/seq.html)`(``dplyr``::`[`n`](https://dplyr.tidyverse.org/reference/context.html)`(``)``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`ungroup`](https://dplyr.tidyverse.org/reference/group_by.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`arrange`](https://dplyr.tidyverse.org/reference/arrange.html)`(`\
`      ``.data``$``STUDYID``,`\
`      ``.data``$``USUBJID``,`\
`      ``.data``$``ASTDTM``,`\
`      ``.data``$``AETERM``,`\
`      ``.data``$``AESEQ`\
`    ``)`\
\
`  ``outcomes`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(`\
`    ``"UNKNOWN"``,`\
`    ``"NOT RECOVERED/NOT RESOLVED"``,`\
`    ``"RECOVERED/RESOLVED WITH SEQUELAE"``,`\
`    ``"RECOVERING/RESOLVING"``,`\
`    ``"RECOVERED/RESOLVED"`\
`  ``)`\
\
`  ``adae`` ``<-`` ``adae`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(`\
`      AEOUT ``=`` `[`factor`](https://rdrr.io/r/base/factor.html)`(`[`ifelse`](https://rdrr.io/r/base/ifelse.html)`(`\
`        ``.data``$``AETOXGR`` ``==`` ``"5"``,`\
`        ``"FATAL"``,`\
`        `[`as.character`](https://rdrr.io/r/base/character.html)`(``sample_fct``(``outcomes``, `[`nrow`](https://rdrr.io/r/base/nrow.html)`(``adae``)``, prob ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0.1``, ``0.2``, ``0.1``, ``0.3``, ``0.3``)``)``)`\
`      ``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `[`with_label`](https://pharmaverse.github.io/formatters/latest-tag/reference/with_label.html)`(``"Outcome of Adverse Event"``)``,`\
`      TRTEMFL ``=`` `[`ifelse`](https://rdrr.io/r/base/ifelse.html)`(``.data``$``ASTDTM`` ``>=`` ``.data``$``TRTSDTM``, ``"Y"``, ``""``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`        `[`with_label`](https://pharmaverse.github.io/formatters/latest-tag/reference/with_label.html)`(``"Treatment Emergent Analysis Flag"``)`\
`    ``)`\
\
`  ``l_aag`` ``<-`` `[`split`](https://rdrr.io/r/base/split.html)`(``aag``, `[`interaction`](https://rdrr.io/r/base/interaction.html)`(``aag``$``NAMVAR``, ``aag``$``SRCVAR``, ``aag``$``GRPTYPE``, drop ``=`` ``TRUE``)``)`\
\
`  ``# Create aesi flags`\
`  ``l_aesi`` ``<-`` `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``l_aag``, ``function``(``d_adag``, ``d_adae``)`` ``{`\
`    `[`names`](https://rdrr.io/r/base/names.html)`(``d_adag``)``[`[`names`](https://rdrr.io/r/base/names.html)`(``d_adag``)`` ``==`` ``"REFTERM"``]`` ``<-`` ``d_adag``$``SRCVAR``[``1``]`\
`    `[`names`](https://rdrr.io/r/base/names.html)`(``d_adag``)``[`[`names`](https://rdrr.io/r/base/names.html)`(``d_adag``)`` ``==`` ``"REFNAME"``]`` ``<-`` ``d_adag``$``NAMVAR``[``1``]`\
\
`    ``if`` ``(``d_adag``$``GRPTYPE``[``1``]`` ``==`` ``"CUSTOM"``)`` ``{`\
`      ``d_adag`` ``<-`` ``d_adag``[``-`[`which`](https://rdrr.io/r/base/which.html)`(`[`names`](https://rdrr.io/r/base/names.html)`(``d_adag``)`` ``==`` ``"SCOPE"``)``]`\
`    ``}`` ``else`` ``if`` ``(``d_adag``$``GRPTYPE``[``1``]`` ``==`` ``"SMQ"``)`` ``{`\
`      `[`names`](https://rdrr.io/r/base/names.html)`(``d_adag``)``[`[`names`](https://rdrr.io/r/base/names.html)`(``d_adag``)`` ``==`` ``"SCOPE"``]`` ``<-`` `[`paste0`](https://rdrr.io/r/base/paste.html)`(`[`substr`](https://rdrr.io/r/base/substr.html)`(``d_adag``$``NAMVAR``[``1``]``, ``1``, ``5``)``, ``"SC"``)`\
`    ``}`\
\
`    ``d_adag`` ``<-`` ``d_adag``[``-`[`which`](https://rdrr.io/r/base/which.html)`(`[`names`](https://rdrr.io/r/base/names.html)`(``d_adag``)`` `[`%in%`](https://rdrr.io/r/base/match.html)` `[`c`](https://rdrr.io/r/base/c.html)`(``"NAMVAR"``, ``"SRCVAR"``, ``"GRPTYPE"``)``)``]`\
`    ``d_new`` ``<-`` ``dplyr``::`[`left_join`](https://dplyr.tidyverse.org/reference/mutate-joins.html)`(``x ``=`` ``d_adae``, y ``=`` ``d_adag``, by ``=`` `[`intersect`](https://generics.r-lib.org/reference/setops.html)`(`[`names`](https://rdrr.io/r/base/names.html)`(``d_adae``)``, `[`names`](https://rdrr.io/r/base/names.html)`(``d_adag``)``)``)`\
`    ``d_new``[``, ``dplyr``::`[`setdiff`](https://generics.r-lib.org/reference/setops.html)`(`[`names`](https://rdrr.io/r/base/names.html)`(``d_new``)``, `[`names`](https://rdrr.io/r/base/names.html)`(``d_adae``)``)``, drop ``=`` ``FALSE``]`\
`  ``}``, ``adae``)`\
`  ``adae`` ``<-`` ``dplyr``::`[`bind_cols`](https://dplyr.tidyverse.org/reference/bind_cols.html)`(``adae``, ``l_aesi``)`\
\
`  ``actions`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(`\
`    ``"DOSE RATE REDUCED"``,`\
`    ``"UNKNOWN"``,`\
`    ``"NOT APPLICABLE"``,`\
`    ``"DRUG INTERRUPTED"``,`\
`    ``"DRUG WITHDRAWN"``,`\
`    ``"DOSE INCREASED"``,`\
`    ``"DOSE NOT CHANGED"``,`\
`    ``"DOSE REDUCED"``,`\
`    ``"NOT EVALUABLE"`\
`  ``)`\
\
`  ``tmc_ex_adae`` ``<-`` ``adae`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(`\
`      AEACN ``=`` `[`factor`](https://rdrr.io/r/base/factor.html)`(`[`ifelse`](https://rdrr.io/r/base/ifelse.html)`(`\
`        ``.data``$``AETOXGR`` ``==`` ``"5"``,`\
`        ``"NOT EVALUABLE"``,`\
`        `[`as.character`](https://rdrr.io/r/base/character.html)`(``sample_fct``(``actions``, `[`nrow`](https://rdrr.io/r/base/nrow.html)`(``adae``)``, prob ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0.05``, ``0.05``, ``0.05``, ``0.01``, ``0.05``, ``0.1``, ``0.45``, ``0.1``, ``0.05``)``)``)`\
`      ``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `[`with_label`](https://pharmaverse.github.io/formatters/latest-tag/reference/with_label.html)`(``"Action Taken With Study Treatment"``)`\
`    ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    `[`col_relabel`](https://insightsengineering.github.io/teal.data/latest-tag/reference/col_labels.html)`(`\
`      AEBODSYS ``=`` ``"Body System or Organ Class"``,`\
`      AELLT ``=`` ``"Lowest Level Term"``,`\
`      AEDECOD ``=`` ``"Dictionary-Derived Term"``,`\
`      AEHLT ``=`` ``"High Level Term"``,`\
`      AEHLGT ``=`` ``"High Level Group Term"``,`\
`      AETOXGR ``=`` ``"Analysis Toxicity Grade"``,`\
`      AESOC ``=`` ``"Primary System Organ Class"``,`\
`      AESER ``=`` ``"Serious Event"``,`\
`      AEREL ``=`` ``"Analysis Causality"``,`\
`      AESEQ ``=`` ``"Sponsor-Defined Identifier"``,`\
`      LDOSEDTM ``=`` ``"End Time/Time of Last Dose"``,`\
`      CQ01NAM ``=`` ``"CQ 01 Reference Name"``,`\
`      SMQ01NAM ``=`` ``"SMQ 01 Reference Name"``,`\
`      SMQ01SC ``=`` ``"SMQ 01 Scope"``,`\
`      SMQ02NAM ``=`` ``"SMQ 02 Reference Name"``,`\
`      SMQ02SC ``=`` ``"SMQ 02 Scope"`\
`    ``)`\
\
`  ``i_lbls`` ``<-`` `[`sapply`](https://rdrr.io/r/base/lapply.html)`(`\
`    `[`names`](https://rdrr.io/r/base/names.html)`(`[`col_labels`](https://insightsengineering.github.io/teal.data/latest-tag/reference/col_labels.html)`(``tmc_ex_adae``)``[`[`is.na`](https://rdrr.io/r/base/NA.html)`(`[`col_labels`](https://insightsengineering.github.io/teal.data/latest-tag/reference/col_labels.html)`(``tmc_ex_adae``)``)``]``)``, ``function``(``x``)`` `[`which`](https://rdrr.io/r/base/which.html)`(`[`names`](https://rdrr.io/r/base/names.html)`(``common_var_labels``)`` ``==`` ``x``)`\
`  ``)`\
`  `[`col_labels`](https://insightsengineering.github.io/teal.data/latest-tag/reference/col_labels.html)`(``tmc_ex_adae``[`[`names`](https://rdrr.io/r/base/names.html)`(``i_lbls``)``]``)`` ``<-`` ``common_var_labels``[``i_lbls``]`\
\
`  `[`save`](https://rdrr.io/r/base/save.html)`(``tmc_ex_adae``, file ``=`` ``"data/tmc_ex_adae.rda"``, compress ``=`` ``"xz"``)`\
`}`

## `ADAETTE`

\
`generate_adaette`` ``<-`` ``function``(``adsl`` ``=`` ``tmc_ex_adsl``)`` ``{`\
`  `[`set.seed`](https://rdrr.io/r/base/Random.html)`(``1``)`\
`  ``lookup_adaette`` ``<-`` ``tibble``::`[`tribble`](https://tibble.tidyverse.org/reference/tribble.html)`(`\
`    ``~``ARM``, ``~``CATCD``, ``~``CAT``, ``~``LAMBDA``, ``~``CNSR_P``,`\
`    ``"ARM A"``, ``"1"``, ``"any adverse event"``, ``1`` ``/`` ``80``, ``0.4``,`\
`    ``"ARM B"``, ``"1"``, ``"any adverse event"``, ``1`` ``/`` ``100``, ``0.2``,`\
`    ``"ARM C"``, ``"1"``, ``"any adverse event"``, ``1`` ``/`` ``60``, ``0.42``,`\
`    ``"ARM A"``, ``"2"``, ``"any serious adverse event"``, ``1`` ``/`` ``100``, ``0.3``,`\
`    ``"ARM B"``, ``"2"``, ``"any serious adverse event"``, ``1`` ``/`` ``150``, ``0.1``,`\
`    ``"ARM C"``, ``"2"``, ``"any serious adverse event"``, ``1`` ``/`` ``80``, ``0.32``,`\
`    ``"ARM A"``, ``"3"``, ``"a grade 3-5 adverse event"``, ``1`` ``/`` ``80``, ``0.2``,`\
`    ``"ARM B"``, ``"3"``, ``"a grade 3-5 adverse event"``, ``1`` ``/`` ``100``, ``0.08``,`\
`    ``"ARM C"``, ``"3"``, ``"a grade 3-5 adverse event"``, ``1`` ``/`` ``60``, ``0.23`\
`  ``)`\
`  ``evntdescr_sel`` ``<-`` ``"Preferred Term"`\
`  ``cnsdtdscr_sel`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(`\
`    ``"Clinical Cut Off"``,`\
`    ``"Completion or Discontinuation"``,`\
`    ``"End of AE Reporting Period"`\
`  ``)`\
\
`  ``random_patient_data`` ``<-`` ``function``(``patient_info``)`` ``{`\
`    ``startdt`` ``<-`` ``lubridate``::`[`date`](https://lubridate.tidyverse.org/reference/date.html)`(``patient_info``$``TRTSDTM``)`\
`    ``trtedtm`` ``<-`` ``lubridate``::`[`floor_date`](https://lubridate.tidyverse.org/reference/round_date.html)`(``dplyr``::`[`case_when`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)`(`\
`      `[`is.na`](https://rdrr.io/r/base/NA.html)`(``patient_info``$``TRTEDTM``)`` ``~`` ``lubridate``::`[`date`](https://lubridate.tidyverse.org/reference/date.html)`(``patient_info``$``TRTSDTM``)`` ``+`` ``study_duration_secs``,`\
`      ``TRUE`` ``~`` ``lubridate``::`[`date`](https://lubridate.tidyverse.org/reference/date.html)`(``patient_info``$``TRTEDTM``)`\
`    ``)``, unit ``=`` ``"day"``)`\
`    ``enddts`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``patient_info``$``EOSDT``, ``lubridate``::`[`date`](https://lubridate.tidyverse.org/reference/date.html)`(``trtedtm``)``)`\
`    ``enddts_min_index`` ``<-`` `[`which.min`](https://rdrr.io/r/base/which.min.html)`(``enddts``)`\
`    ``adt`` ``<-`` ``enddts``[``enddts_min_index``]`\
`    ``adtm`` ``<-`` ``lubridate``::`[`as_datetime`](https://lubridate.tidyverse.org/reference/as_date.html)`(``adt``)`\
`    ``ady`` ``<-`` `[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(``adt`` ``-`` ``startdt`` ``+`` ``1``)`\
`    `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(`\
`      ARM ``=`` ``patient_info``$``ARM``,`\
`      STUDYID ``=`` ``patient_info``$``STUDYID``,`\
`      SITEID ``=`` ``patient_info``$``SITEID``,`\
`      USUBJID ``=`` ``patient_info``$``USUBJID``,`\
`      PARAMCD ``=`` ``"AEREPTTE"``,`\
`      PARAM ``=`` ``"Time to end of AE reporting period"``,`\
`      CNSR ``=`` ``0``,`\
`      AVAL ``=`` ``lubridate``::`[`days`](https://lubridate.tidyverse.org/reference/period.html)`(``ady``)`` ``/`` ``lubridate``::`[`years`](https://lubridate.tidyverse.org/reference/period.html)`(``1``)``,`\
`      AVALU ``=`` ``"YEARS"``,`\
`      EVNTDESC ``=`` `[`ifelse`](https://rdrr.io/r/base/ifelse.html)`(``enddts_min_index`` ``==`` ``1``, ``"Completion or Discontinuation"``, ``"End of AE Reporting Period"``)``,`\
`      CNSDTDSC ``=`` ``NA``,`\
`      ADTM ``=`` ``adtm``,`\
`      ADY ``=`` ``ady``,`\
`      stringsAsFactors ``=`` ``FALSE`\
`    ``)`\
`  ``}`\
\
`  ``paramcd_hy`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``"HYSTTEUL"``, ``"HYSTTEBL"``)`\
`  ``param_hy`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``"Time to Hy's Law Elevation in relation to ULN"``, ``"Time to Hy's Law Elevation in relation to Baseline"``)`\
`  ``param_init_list`` ``<-`` ``relvar_init``(``param_hy``, ``paramcd_hy``)`\
`  ``adsl_hy`` ``<-`` ``dplyr``::`[`select`](https://dplyr.tidyverse.org/reference/select.html)`(``adsl``, ``"STUDYID"``, ``"USUBJID"``, ``"TRTSDTM"``, ``"SITEID"``, ``"ARM"``)`\
`  ``adaette_hy`` ``<-`` `[`expand.grid`](https://rdrr.io/r/base/expand.grid.html)`(`\
`    STUDYID ``=`` `[`unique`](https://rdrr.io/r/base/unique.html)`(``adsl``$``STUDYID``)``,`\
`    USUBJID ``=`` ``adsl``$``USUBJID``,`\
`    PARAM ``=`` `[`as.factor`](https://rdrr.io/r/base/factor.html)`(``param_init_list``$``relvar1``)``,`\
`    stringsAsFactors ``=`` ``FALSE`\
`  ``)`\
\
`  ``adaette_hy`` ``<-`` ``dplyr``::`[`left_join`](https://dplyr.tidyverse.org/reference/mutate-joins.html)`(``adaette_hy``, ``adsl_hy``, by ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"STUDYID"``, ``"USUBJID"``)``, multiple ``=`` ``"all"``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(`\
`      PARAMCD ``=`` `[`factor`](https://rdrr.io/r/base/factor.html)`(``rel_var``(`\
`        df ``=`` `[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html)`(``adaette_hy``)``,`\
`        var_values ``=`` ``param_init_list``$``relvar2``,`\
`        related_var ``=`` ``"PARAM"`\
`      ``)``)`\
`    ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(`\
`      CNSR ``=`` `[`sample`](https://rdrr.io/r/base/sample.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``0``, ``1``)``, prob ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0.1``, ``0.9``)``, size ``=`` ``dplyr``::`[`n`](https://dplyr.tidyverse.org/reference/context.html)`(``)``, replace ``=`` ``TRUE``)``,`\
`      EVNTDESC ``=`` ``dplyr``::`[`if_else`](https://dplyr.tidyverse.org/reference/if_else.html)`(`\
`        ``.data``$``CNSR`` ``==`` ``0``,`\
`        ``"First Post-Baseline Raised ALT or AST Elevation Result"``,`\
`        ``NA_character_`\
`      ``)``,`\
`      CNSDTDSC ``=`` ``dplyr``::`[`if_else`](https://dplyr.tidyverse.org/reference/if_else.html)`(``.data``$``CNSR`` ``==`` ``0``, ``NA_character_``,`\
`        `[`sample`](https://rdrr.io/r/base/sample.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``"Last Post-Baseline ALT or AST Result"``, ``"Treatment Start"``)``,`\
`          prob ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0.9``, ``0.1``)``,`\
`          size ``=`` ``dplyr``::`[`n`](https://dplyr.tidyverse.org/reference/context.html)`(``)``, replace ``=`` ``TRUE`\
`        ``)`\
`      ``)`\
`    ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`rowwise`](https://dplyr.tidyverse.org/reference/rowwise.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``ADTM ``=`` ``dplyr``::`[`case_when`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)`(`\
`      ``CNSDTDSC`` ``==`` ``"Treatment Start"`` ``~`` ``TRTSDTM``,`\
`      ``TRUE`` ``~`` ``TRTSDTM`` ``+`` `[`sample`](https://rdrr.io/r/base/sample.html)`(`[`seq`](https://rdrr.io/r/base/seq.html)`(``0``, ``study_duration_secs``)``, size ``=`` ``dplyr``::`[`n`](https://dplyr.tidyverse.org/reference/context.html)`(``)``, replace ``=`` ``TRUE``)`\
`    ``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(`\
`      ADY_int ``=`` ``lubridate``::`[`date`](https://lubridate.tidyverse.org/reference/date.html)`(``ADTM``)`` ``-`` ``lubridate``::`[`date`](https://lubridate.tidyverse.org/reference/date.html)`(``TRTSDTM``)`` ``+`` ``1``,`\
`      ADY ``=`` `[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(``ADY_int``)``,`\
`      AVAL ``=`` ``lubridate``::`[`days`](https://lubridate.tidyverse.org/reference/period.html)`(``ADY_int``)`` ``/`` ``lubridate``::`[`weeks`](https://lubridate.tidyverse.org/reference/period.html)`(``1``)``,`\
`      AVALU ``=`` ``"WEEKS"`\
`    ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`select`](https://dplyr.tidyverse.org/reference/select.html)`(``-``TRTSDTM``, ``-``ADY_int``)`\
\
`  ``random_ae_data`` ``<-`` ``function``(``lookup_info``, ``patient_info``, ``patient_data``)`` ``{`\
`    ``cnsr`` ``<-`` `[`sample`](https://rdrr.io/r/base/sample.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``0``, ``1``)``, ``1``, prob ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``1`` ``-`` ``lookup_info``$``CNSR_P``, ``lookup_info``$``CNSR_P``)``)`\
`    ``ae_rep_tte`` ``<-`` ``patient_data``$``AVAL``[``patient_data``$``PARAMCD`` ``==`` ``"AEREPTTE"``]`\
`    `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(`\
`      ARM ``=`` `[`rep`](https://rdrr.io/r/base/rep.html)`(``patient_data``$``ARM``, ``2``)``,`\
`      STUDYID ``=`` `[`rep`](https://rdrr.io/r/base/rep.html)`(``patient_data``$``STUDYID``, ``2``)``,`\
`      SITEID ``=`` `[`rep`](https://rdrr.io/r/base/rep.html)`(``patient_data``$``SITEID``, ``2``)``,`\
`      USUBJID ``=`` `[`rep`](https://rdrr.io/r/base/rep.html)`(``patient_data``$``USUBJID``, ``2``)``,`\
`      PARAMCD ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(`\
`        `[`paste0`](https://rdrr.io/r/base/paste.html)`(``"AETTE"``, ``lookup_info``$``CATCD``)``,`\
`        `[`paste0`](https://rdrr.io/r/base/paste.html)`(``"AETOT"``, ``lookup_info``$``CATCD``)`\
`      ``)``,`\
`      PARAM ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(`\
`        `[`paste`](https://rdrr.io/r/base/paste.html)`(``"Time to first occurrence of"``, ``lookup_info``$``CAT``)``,`\
`        `[`paste`](https://rdrr.io/r/base/paste.html)`(``"Number of occurrences of"``, ``lookup_info``$``CAT``)`\
`      ``)``,`\
`      CNSR ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``cnsr``, ``NA``)``,`\
`      AVAL ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(`\
`        `[`ifelse`](https://rdrr.io/r/base/ifelse.html)`(``cnsr`` ``==`` ``1``, ``ae_rep_tte``, ``rtexp``(``1``, ``lookup_info``$``LAMBDA`` ``*`` ``365.25``, r ``=`` ``ae_rep_tte``)``)``,`\
`        `[`ifelse`](https://rdrr.io/r/base/ifelse.html)`(``cnsr`` ``==`` ``1``, ``0``, ``rtpois``(``1``, ``lookup_info``$``LAMBDA`` ``*`` ``365.25``)``)`\
`      ``)``,`\
`      AVALU ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"YEARS"``, ``NA``)``,`\
`      EVNTDESC ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(`[`ifelse`](https://rdrr.io/r/base/ifelse.html)`(``cnsr`` ``==`` ``0``, `[`sample`](https://rdrr.io/r/base/sample.html)`(``evntdescr_sel``, ``1``)``, ``""``)``, ``NA``)``,`\
`      CNSDTDSC ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(`[`ifelse`](https://rdrr.io/r/base/ifelse.html)`(``cnsr`` ``==`` ``1``, `[`sample`](https://rdrr.io/r/base/sample.html)`(``cnsdtdscr_sel``, ``1``)``, ``""``)``, ``NA``)``,`\
`      stringsAsFactors ``=`` ``FALSE`\
`    ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(`\
`      ADY ``=`` ``dplyr``::`[`if_else`](https://dplyr.tidyverse.org/reference/if_else.html)`(`[`is.na`](https://rdrr.io/r/base/NA.html)`(``AVALU``)``, ``NA_real_``, `[`ceiling`](https://rdrr.io/r/base/Round.html)`(`[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(``lubridate``::`[`dyears`](https://lubridate.tidyverse.org/reference/duration.html)`(``AVAL``)``, ``"days"``)``)``)``,`\
`      ADTM ``=`` ``dplyr``::`[`if_else`](https://dplyr.tidyverse.org/reference/if_else.html)`(`\
`        `[`is.na`](https://rdrr.io/r/base/NA.html)`(``AVALU``)``,`\
`        ``lubridate``::`[`as_datetime`](https://lubridate.tidyverse.org/reference/as_date.html)`(``NA``)``,`\
`        ``patient_info``$``TRTSDTM`` ``+`` ``lubridate``::`[`days`](https://lubridate.tidyverse.org/reference/period.html)`(``ADY``)`\
`      ``)`\
`    ``)`\
`  ``}`\
\
`  ``adaette`` ``<-`` `[`split`](https://rdrr.io/r/base/split.html)`(``adsl``, ``adsl``$``USUBJID``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``function``(``patient_info``)`` ``{`\
`      ``patient_data`` ``<-`` ``random_patient_data``(``patient_info``)`\
`      ``lookup_arm`` ``<-`` ``lookup_adaette`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`        ``dplyr``::`[`filter`](https://dplyr.tidyverse.org/reference/filter.html)`(``.data``$``ARM`` ``==`` `[`as.character`](https://rdrr.io/r/base/character.html)`(``patient_info``$``ARMCD``)``)`\
`      ``ae_data`` ``<-`` `[`split`](https://rdrr.io/r/base/split.html)`(``lookup_arm``, ``lookup_arm``$``CATCD``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`        `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``random_ae_data``, patient_data ``=`` ``patient_data``, patient_info ``=`` ``patient_info``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`        `[`Reduce`](https://rdrr.io/r/base/funprog.html)`(``rbind``, ``.``)`\
`      ``dplyr``::`[`bind_rows`](https://dplyr.tidyverse.org/reference/bind_rows.html)`(``patient_data``, ``ae_data``)`\
`    ``}``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    `[`Reduce`](https://rdrr.io/r/base/funprog.html)`(``rbind``, ``.``)`\
`  ``adaette`` ``<-`` `[`rbind`](https://insightsengineering.github.io/rtables/latest-tag/reference/rbind.html)`(``adaette``, ``adaette_hy``)`\
\
`  ``tmc_ex_adaette`` ``<-`` ``adsl`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`inner_join`](https://dplyr.tidyverse.org/reference/mutate-joins.html)`(`\
`      ``dplyr``::`[`select`](https://dplyr.tidyverse.org/reference/select.html)`(``adaette``, ``-``"SITEID"``, ``-``"ARM"``)``,`\
`      by ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"STUDYID"``, ``"USUBJID"``)``,`\
`      multiple ``=`` ``"all"`\
`    ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(``.data``$``USUBJID``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`arrange`](https://dplyr.tidyverse.org/reference/arrange.html)`(``.data``$``ADTM``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``PARAM ``=`` `[`as.factor`](https://rdrr.io/r/base/factor.html)`(``.data``$``PARAM``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``PARAMCD ``=`` `[`as.factor`](https://rdrr.io/r/base/factor.html)`(``.data``$``PARAMCD``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`ungroup`](https://dplyr.tidyverse.org/reference/group_by.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`arrange`](https://dplyr.tidyverse.org/reference/arrange.html)`(`\
`      ``.data``$``STUDYID``,`\
`      ``.data``$``USUBJID``,`\
`      ``.data``$``PARAMCD``,`\
`      ``.data``$``ADTM`\
`    ``)`\
\
`  ``i_lbls`` ``<-`` `[`sapply`](https://rdrr.io/r/base/lapply.html)`(`\
`    `[`names`](https://rdrr.io/r/base/names.html)`(`[`col_labels`](https://insightsengineering.github.io/teal.data/latest-tag/reference/col_labels.html)`(``tmc_ex_adaette``)``[`[`is.na`](https://rdrr.io/r/base/NA.html)`(`[`col_labels`](https://insightsengineering.github.io/teal.data/latest-tag/reference/col_labels.html)`(``tmc_ex_adaette``)``)``]``)``,`\
`    ``function``(``x``)`` `[`which`](https://rdrr.io/r/base/which.html)`(`[`names`](https://rdrr.io/r/base/names.html)`(``common_var_labels``)`` ``==`` ``x``)`\
`  ``)`\
`  `[`col_labels`](https://insightsengineering.github.io/teal.data/latest-tag/reference/col_labels.html)`(``tmc_ex_adaette``[`[`names`](https://rdrr.io/r/base/names.html)`(``i_lbls``)``]``)`` ``<-`` ``common_var_labels``[``i_lbls``]`\
\
`  `[`save`](https://rdrr.io/r/base/save.html)`(``tmc_ex_adaette``, file ``=`` ``"data/tmc_ex_adaette.rda"``, compress ``=`` ``"xz"``)`\
`}`

## `ADCM`

\
`generate_adcm`` ``<-`` ``function``(``adsl`` ``=`` ``tmc_ex_adsl``,`\
`                          ``max_n_cms`` ``=`` ``5L``)`` ``{`\
`  `[`set.seed`](https://rdrr.io/r/base/Random.html)`(``1``)`\
`  ``lookup_cm`` ``<-`` ``tibble``::`[`tribble`](https://tibble.tidyverse.org/reference/tribble.html)`(`\
`    ``~``CMCLAS``, ``~``CMDECOD``, ``~``ATIREL``,`\
`    ``"medcl A"``, ``"medname A_1/3"``, ``"PRIOR"``,`\
`    ``"medcl A"``, ``"medname A_2/3"``, ``"CONCOMITANT"``,`\
`    ``"medcl A"``, ``"medname A_3/3"``, ``"CONCOMITANT"``,`\
`    ``"medcl B"``, ``"medname B_1/4"``, ``"CONCOMITANT"``,`\
`    ``"medcl B"``, ``"medname B_2/4"``, ``"PRIOR"``,`\
`    ``"medcl B"``, ``"medname B_3/4"``, ``"PRIOR"``,`\
`    ``"medcl B"``, ``"medname B_4/4"``, ``"CONCOMITANT"``,`\
`    ``"medcl C"``, ``"medname C_1/2"``, ``"CONCOMITANT"``,`\
`    ``"medcl C"``, ``"medname C_2/2"``, ``"CONCOMITANT"`\
`  ``)`\
\
`  ``adcm`` ``<-`` `[`Map`](https://rdrr.io/r/base/funprog.html)`(``function``(``id``, ``sid``)`` ``{`\
`    ``n_cms`` ``<-`` `[`sample`](https://rdrr.io/r/base/sample.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``0``, `[`seq_len`](https://rdrr.io/r/base/seq.html)`(``max_n_cms``)``)``, ``1``)`\
`    ``i`` ``<-`` `[`sample`](https://rdrr.io/r/base/sample.html)`(`[`seq_len`](https://rdrr.io/r/base/seq.html)`(`[`nrow`](https://rdrr.io/r/base/nrow.html)`(``lookup_cm``)``)``, ``n_cms``, ``TRUE``)`\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(`\
`      ``lookup_cm``[``i``, ``]``,`\
`      USUBJID ``=`` ``id``,`\
`      STUDYID ``=`` ``sid`\
`    ``)`\
`  ``}``, ``adsl``$``USUBJID``, ``adsl``$``STUDYID``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    `[`Reduce`](https://rdrr.io/r/base/funprog.html)`(``rbind``, ``.``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``` `[` ```(`[`c`](https://rdrr.io/r/base/c.html)`(``4``, ``5``, ``1``, ``2``, ``3``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``CMCAT ``=`` ``.data``$``CMCLAS`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `[`with_label`](https://pharmaverse.github.io/formatters/latest-tag/reference/with_label.html)`(``"Category for Medication"``)``)`\
\
`  ``# merge adsl to be able to add CM date and study day variables`\
`  ``adcm`` ``<-`` ``dplyr``::`[`inner_join`](https://dplyr.tidyverse.org/reference/mutate-joins.html)`(`\
`    ``adcm``,`\
`    ``adsl``,`\
`    by ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"STUDYID"``, ``"USUBJID"``)``,`\
`    multiple ``=`` ``"all"`\
`  ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`rowwise`](https://dplyr.tidyverse.org/reference/rowwise.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``TRTENDT ``=`` ``lubridate``::`[`date`](https://lubridate.tidyverse.org/reference/date.html)`(``dplyr``::`[`case_when`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)`(`\
`      `[`is.na`](https://rdrr.io/r/base/NA.html)`(``TRTEDTM``)`` ``~`` ``lubridate``::`[`floor_date`](https://lubridate.tidyverse.org/reference/round_date.html)`(``lubridate``::`[`date`](https://lubridate.tidyverse.org/reference/date.html)`(``TRTSDTM``)`` ``+`` ``study_duration_secs``, unit ``=`` ``"day"``)``,`\
`      ``TRUE`` ``~`` ``TRTEDTM`\
`    ``)``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``ASTDTM ``=`` `[`sample`](https://rdrr.io/r/base/sample.html)`(`\
`      `[`seq`](https://rdrr.io/r/base/seq.html)`(``lubridate``::`[`as_datetime`](https://lubridate.tidyverse.org/reference/as_date.html)`(``TRTSDTM``)``, ``lubridate``::`[`as_datetime`](https://lubridate.tidyverse.org/reference/as_date.html)`(``TRTENDT``)``, by ``=`` ``"day"``)``,`\
`      size ``=`` ``1`\
`    ``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``ASTDY ``=`` `[`ceiling`](https://rdrr.io/r/base/Round.html)`(`[`difftime`](https://rdrr.io/r/base/difftime.html)`(``ASTDTM``, ``TRTSDTM``, units ``=`` ``"days"``)``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``# add 1 to end of range incase both values passed to sample() are the same`\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``AENDTM ``=`` `[`sample`](https://rdrr.io/r/base/sample.html)`(`\
`      `[`seq`](https://rdrr.io/r/base/seq.html)`(``lubridate``::`[`as_datetime`](https://lubridate.tidyverse.org/reference/as_date.html)`(``ASTDTM``)``, ``lubridate``::`[`as_datetime`](https://lubridate.tidyverse.org/reference/as_date.html)`(``TRTENDT`` ``+`` ``1``)``, by ``=`` ``"day"``)``,`\
`      size ``=`` ``1`\
`    ``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``AENDY ``=`` `[`ceiling`](https://rdrr.io/r/base/Round.html)`(`[`difftime`](https://rdrr.io/r/base/difftime.html)`(``AENDTM``, ``TRTSDTM``, units ``=`` ``"days"``)``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`select`](https://dplyr.tidyverse.org/reference/select.html)`(``-``TRTENDT``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`ungroup`](https://dplyr.tidyverse.org/reference/group_by.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`arrange`](https://dplyr.tidyverse.org/reference/arrange.html)`(``STUDYID``, ``USUBJID``, ``ASTDTM``)`\
\
`  ``tmc_ex_adcm`` ``<-`` ``adcm`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(``.data``$``USUBJID``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``CMSEQ ``=`` `[`seq_len`](https://rdrr.io/r/base/seq.html)`(``dplyr``::`[`n`](https://dplyr.tidyverse.org/reference/context.html)`(``)``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`ungroup`](https://dplyr.tidyverse.org/reference/group_by.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`arrange`](https://dplyr.tidyverse.org/reference/arrange.html)`(``.data``$``STUDYID``, ``.data``$``USUBJID``, ``.data``$``ASTDTM``, ``.data``$``CMSEQ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(`\
`      ATC1 ``=`` `[`paste`](https://rdrr.io/r/base/paste.html)`(``"ATCCLAS1"``, `[`substr`](https://rdrr.io/r/base/substr.html)`(``.data``$``CMDECOD``, ``9``, ``9``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `[`with_label`](https://pharmaverse.github.io/formatters/latest-tag/reference/with_label.html)`(``"ATC Level 1 Text"``)``,`\
`      ATC2 ``=`` `[`paste`](https://rdrr.io/r/base/paste.html)`(``"ATCCLAS2"``, `[`substr`](https://rdrr.io/r/base/substr.html)`(``.data``$``CMDECOD``, ``9``, ``9``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `[`with_label`](https://pharmaverse.github.io/formatters/latest-tag/reference/with_label.html)`(``"ATC Level 2 Text"``)``,`\
`      ATC3 ``=`` `[`paste`](https://rdrr.io/r/base/paste.html)`(``"ATCCLAS3"``, `[`substr`](https://rdrr.io/r/base/substr.html)`(``.data``$``CMDECOD``, ``9``, ``9``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `[`with_label`](https://pharmaverse.github.io/formatters/latest-tag/reference/with_label.html)`(``"ATC Level 3 Text"``)``,`\
`      ATC4 ``=`` `[`paste`](https://rdrr.io/r/base/paste.html)`(``"ATCCLAS4"``, `[`substr`](https://rdrr.io/r/base/substr.html)`(``.data``$``CMDECOD``, ``9``, ``9``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `[`with_label`](https://pharmaverse.github.io/formatters/latest-tag/reference/with_label.html)`(``"ATC Level 4 Text"``)`\
`    ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(`\
`      CMINDC ``=`` `[`sample`](https://rdrr.io/r/base/sample.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(`\
`        ``"Nausea"``, ``"Hypertension"``, ``"Urticaria"``, ``"Fever"``,`\
`        ``"Asthma"``, ``"Infection"``, ``"Diabete"``, ``"Diarrhea"``, ``"Pneumonia"`\
`      ``)``, ``dplyr``::`[`n`](https://dplyr.tidyverse.org/reference/context.html)`(``)``, replace ``=`` ``TRUE``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `[`with_label`](https://pharmaverse.github.io/formatters/latest-tag/reference/with_label.html)`(``"Indication"``)``,`\
`      CMDOSE ``=`` `[`sample`](https://rdrr.io/r/base/sample.html)`(``1``:``99``, ``dplyr``::`[`n`](https://dplyr.tidyverse.org/reference/context.html)`(``)``, replace ``=`` ``TRUE``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `[`with_label`](https://pharmaverse.github.io/formatters/latest-tag/reference/with_label.html)`(``"Dose per Administration"``)``,`\
`      CMTRT ``=`` `[`substr`](https://rdrr.io/r/base/substr.html)`(``.data``$``CMDECOD``, ``9``, ``13``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `[`with_label`](https://pharmaverse.github.io/formatters/latest-tag/reference/with_label.html)`(``"Reported Name of Drug, Med, or Therapy"``)``,`\
`      CMDOSU ``=`` `[`sample`](https://rdrr.io/r/base/sample.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(`\
`        ``"ug/mL"``, ``"ug/kg/day"``, ``"%"``, ``"uL"``, ``"DROP"``,`\
`        ``"umol/L"``, ``"mg"``, ``"mg/breath"``, ``"ug"`\
`      ``)``, ``dplyr``::`[`n`](https://dplyr.tidyverse.org/reference/context.html)`(``)``, replace ``=`` ``TRUE``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `[`with_label`](https://pharmaverse.github.io/formatters/latest-tag/reference/with_label.html)`(``"Dose Units"``)`\
`    ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(`\
`      CMROUTE ``=`` `[`sample`](https://rdrr.io/r/base/sample.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(`\
`        ``"INTRAVENOUS"``, ``"ORAL"``, ``"NASAL"``,`\
`        ``"INTRAMUSCULAR"``, ``"SUBCUTANEOUS"``, ``"INHALED"``, ``"RECTAL"``, ``"UNKNOWN"`\
`      ``)``, ``dplyr``::`[`n`](https://dplyr.tidyverse.org/reference/context.html)`(``)``, replace ``=`` ``TRUE``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `[`with_label`](https://pharmaverse.github.io/formatters/latest-tag/reference/with_label.html)`(``"Route of Administration"``)``,`\
`      CMDOSFRQ ``=`` `[`sample`](https://rdrr.io/r/base/sample.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(`\
`        ``"Q4W"``, ``"QN"``, ``"Q4H"``, ``"UNKNOWN"``, ``"TWICE"``,`\
`        ``"Q4H"``, ``"QD"``, ``"TID"``, ``"4 TIMES PER MONTH"`\
`      ``)``, ``dplyr``::`[`n`](https://dplyr.tidyverse.org/reference/context.html)`(``)``, replace ``=`` ``TRUE``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `[`with_label`](https://pharmaverse.github.io/formatters/latest-tag/reference/with_label.html)`(``"Dosing Frequency per Interval"``)`\
`    ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    `[`col_relabel`](https://insightsengineering.github.io/teal.data/latest-tag/reference/col_labels.html)`(`\
`      CMCLAS ``=`` ``"Medication Class"``,`\
`      CMDECOD ``=`` ``"Standardized Medication Name"``,`\
`      ATIREL ``=`` ``"Time Relation of Medication"``,`\
`      CMSEQ ``=`` ``"Sponsor-Defined Identifier"`\
`    ``)`\
\
`  ``i_lbls`` ``<-`` `[`sapply`](https://rdrr.io/r/base/lapply.html)`(`\
`    `[`names`](https://rdrr.io/r/base/names.html)`(`[`col_labels`](https://insightsengineering.github.io/teal.data/latest-tag/reference/col_labels.html)`(``tmc_ex_adcm``)``[`[`is.na`](https://rdrr.io/r/base/NA.html)`(`[`col_labels`](https://insightsengineering.github.io/teal.data/latest-tag/reference/col_labels.html)`(``tmc_ex_adcm``)``)``]``)``, ``function``(``x``)`` `[`which`](https://rdrr.io/r/base/which.html)`(`[`names`](https://rdrr.io/r/base/names.html)`(``common_var_labels``)`` ``==`` ``x``)`\
`  ``)`\
`  `[`col_labels`](https://insightsengineering.github.io/teal.data/latest-tag/reference/col_labels.html)`(``tmc_ex_adcm``[`[`names`](https://rdrr.io/r/base/names.html)`(``i_lbls``)``]``)`` ``<-`` ``common_var_labels``[``i_lbls``]`\
\
`  `[`save`](https://rdrr.io/r/base/save.html)`(``tmc_ex_adcm``, file ``=`` ``"data/tmc_ex_adcm.rda"``, compress ``=`` ``"xz"``)`\
`}`

## `ADEG`

\
`generate_adeg`` ``<-`` ``function``(``adsl`` ``=`` ``tmc_ex_adsl``,`\
`                          ``n_assessments`` ``=`` ``3L``,`\
`                          ``n_days`` ``=`` ``3L``,`\
`                          ``max_n_eg`` ``=`` ``3L``)`` ``{`\
`  `[`set.seed`](https://rdrr.io/r/base/Random.html)`(``1``)`\
`  ``param`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``"QT Duration"``, ``"RR Duration"``, ``"Heart Rate"``, ``"ECG Interpretation"``)`\
`  ``paramcd`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``"QT"``, ``"RR"``, ``"HR"``, ``"ECGINTP"``)`\
`  ``paramu`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``"msec"``, ``"msec"``, ``"beats/min"``, ``""``)`\
`  ``visit_format`` ``<-`` ``"WEEK"`\
\
`  ``param_init_list`` ``<-`` ``relvar_init``(``param``, ``paramcd``)`\
`  ``unit_init_list`` ``<-`` ``relvar_init``(``param``, ``paramu``)`\
\
`  ``adeg`` ``<-`` `[`expand.grid`](https://rdrr.io/r/base/expand.grid.html)`(`\
`    STUDYID ``=`` `[`unique`](https://rdrr.io/r/base/unique.html)`(``adsl``$``STUDYID``)``,`\
`    USUBJID ``=`` ``adsl``$``USUBJID``,`\
`    PARAM ``=`` `[`as.factor`](https://rdrr.io/r/base/factor.html)`(``param_init_list``$``relvar1``)``,`\
`    AVISIT ``=`` ``visit_schedule``(``visit_format ``=`` ``visit_format``, n_assessments ``=`` ``n_assessments``, n_days ``=`` ``n_days``)``,`\
`    stringsAsFactors ``=`` ``FALSE`\
`  ``)`\
\
`  ``adeg``$``PARAMCD`` ``<-`` `[`as.factor`](https://rdrr.io/r/base/factor.html)`(``rel_var``(`\
`    df ``=`` ``adeg``,`\
`    var_name ``=`` ``"PARAMCD"``,`\
`    var_values ``=`` ``param_init_list``$``relvar2``,`\
`    related_var ``=`` ``"PARAM"`\
`  ``)``)`\
\
`  ``adeg`` ``<-`` ``adeg`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``AVAL ``=`` ``dplyr``::`[`case_when`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)`(`\
`    ``.data``$``PARAMCD`` ``==`` ``"QT"`` ``~`` ``stats``::`[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(`[`nrow`](https://rdrr.io/r/base/nrow.html)`(``adeg``)``, mean ``=`` ``350``, sd ``=`` ``100``)``,`\
`    ``.data``$``PARAMCD`` ``==`` ``"RR"`` ``~`` ``stats``::`[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(`[`nrow`](https://rdrr.io/r/base/nrow.html)`(``adeg``)``, mean ``=`` ``1050``, sd ``=`` ``300``)``,`\
`    ``.data``$``PARAMCD`` ``==`` ``"HR"`` ``~`` ``stats``::`[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(`[`nrow`](https://rdrr.io/r/base/nrow.html)`(``adeg``)``, mean ``=`` ``70``, sd ``=`` ``20``)``,`\
`    ``.data``$``PARAMCD`` ``==`` ``"ECGINTP"`` ``~`` ``NA_real_`\
`  ``)``)`\
\
`  ``adeg`` ``<-`` ``adeg`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``AVISITN ``=`` ``dplyr``::`[`case_when`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)`(`\
`    ``AVISIT`` ``==`` ``"SCREENING"`` ``~`` ``-``1``,`\
`    ``AVISIT`` ``==`` ``"BASELINE"`` ``~`` ``0``,`\
`    ``(`[`grepl`](https://rdrr.io/r/base/grep.html)`(``"^WEEK"``, ``AVISIT``)`` ``|`` `[`grepl`](https://rdrr.io/r/base/grep.html)`(``"^CYCLE"``, ``AVISIT``)``)`` ``~`` `[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(``AVISIT``)`` ``-`` ``2``,`\
`    ``TRUE`` ``~`` ``NA_real_`\
`  ``)``)`\
\
`  ``adeg``$``AVALU`` ``<-`` `[`as.factor`](https://rdrr.io/r/base/factor.html)`(``rel_var``(`\
`    df ``=`` ``adeg``,`\
`    var_name ``=`` ``"AVALU"``,`\
`    var_values ``=`` ``unit_init_list``$``relvar2``,`\
`    related_var ``=`` ``"PARAM"`\
`  ``)``)`\
\
`  ``adeg`` ``<-`` ``adeg``[`[`order`](https://rdrr.io/r/base/order.html)`(``adeg``$``STUDYID``, ``adeg``$``USUBJID``, ``adeg``$``PARAMCD``, ``adeg``$``AVISITN``)``, ``]`\
`  ``adeg`` ``<-`` `[`Reduce`](https://rdrr.io/r/base/funprog.html)`(``rbind``, `[`lapply`](https://rdrr.io/r/base/lapply.html)`(`[`split`](https://rdrr.io/r/base/split.html)`(``adeg``, ``adeg``$``USUBJID``)``, ``function``(``x``)`` ``{`\
`    ``x``$``STUDYID`` ``<-`` ``adsl``$``STUDYID``[`[`which`](https://rdrr.io/r/base/which.html)`(``adsl``$``USUBJID`` ``==`` ``x``$``USUBJID``[``1``]``)``]`\
`    ``x``$``ABLFL`` ``<-`` `[`ifelse`](https://rdrr.io/r/base/ifelse.html)`(`[`toupper`](https://rdrr.io/r/base/chartr.html)`(``visit_format``)`` ``==`` ``"WEEK"`` ``&`` ``x``$``AVISIT`` ``==`` ``"BASELINE"``,`\
`      ``"Y"``,`\
`      `[`ifelse`](https://rdrr.io/r/base/ifelse.html)`(`[`toupper`](https://rdrr.io/r/base/chartr.html)`(``visit_format``)`` ``==`` ``"CYCLE"`` ``&`` ``x``$``AVISIT`` ``==`` ``"CYCLE 1 DAY 1"``, ``"Y"``, ``""``)`\
`    ``)`\
`    ``x`\
`  ``}``)``)`\
\
`  ``adeg``$``BASE`` ``<-`` `[`ifelse`](https://rdrr.io/r/base/ifelse.html)`(``adeg``$``AVISITN`` ``>=`` ``0``, ``retain``(``adeg``, ``adeg``$``AVAL``, ``adeg``$``ABLFL`` ``==`` ``"Y"``)``, ``adeg``$``AVAL``)`\
`  ``adeg`` ``<-`` ``adeg`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``ANRLO ``=`` ``dplyr``::`[`case_when`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)`(`\
`      ``.data``$``PARAMCD`` ``==`` ``"QT"`` ``~`` ``200``,`\
`      ``.data``$``PARAMCD`` ``==`` ``"RR"`` ``~`` ``600``,`\
`      ``.data``$``PARAMCD`` ``==`` ``"HR"`` ``~`` ``40``,`\
`      ``.data``$``PARAMCD`` ``==`` ``"ECGINTP"`` ``~`` ``NA_real_`\
`    ``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``ANRHI ``=`` ``dplyr``::`[`case_when`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)`(`\
`      ``.data``$``PARAMCD`` ``==`` ``"QT"`` ``~`` ``500``,`\
`      ``.data``$``PARAMCD`` ``==`` ``"RR"`` ``~`` ``1500``,`\
`      ``.data``$``PARAMCD`` ``==`` ``"HR"`` ``~`` ``100``,`\
`      ``.data``$``PARAMCD`` ``==`` ``"ECGINTP"`` ``~`` ``NA_real_`\
`    ``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``ANRIND ``=`` `[`factor`](https://rdrr.io/r/base/factor.html)`(``dplyr``::`[`case_when`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)`(`\
`      ``.data``$``AVAL`` ``<`` ``.data``$``ANRLO`` ``~`` ``"LOW"``,`\
`      ``.data``$``AVAL`` ``>=`` ``.data``$``ANRLO`` ``&`` ``.data``$``AVAL`` ``<=`` ``.data``$``ANRHI`` ``~`` ``"NORMAL"``,`\
`      ``.data``$``AVAL`` ``>`` ``.data``$``ANRHI`` ``~`` ``"HIGH"`\
`    ``)``)``)`\
\
`  ``adeg`` ``<-`` ``adeg`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``CHG ``=`` `[`ifelse`](https://rdrr.io/r/base/ifelse.html)`(``.data``$``AVISITN`` ``>`` ``0``, ``.data``$``AVAL`` ``-`` ``.data``$``BASE``, ``NA``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``PCHG ``=`` `[`ifelse`](https://rdrr.io/r/base/ifelse.html)`(``.data``$``AVISITN`` ``>`` ``0``, ``100`` ``*`` ``(``.data``$``CHG`` ``/`` ``.data``$``BASE``)``, ``NA``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``BASETYPE ``=`` ``"LAST"``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(``.data``$``USUBJID``, ``.data``$``PARAMCD``, ``.data``$``BASETYPE``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``BNRIND ``=`` ``.data``$``ANRIND``[``.data``$``ABLFL`` ``==`` ``"Y"``]``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`ungroup`](https://dplyr.tidyverse.org/reference/group_by.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``DTYPE ``=`` ``NA``)`\
\
`  ``adeg``$``ANRIND`` ``<-`` `[`factor`](https://rdrr.io/r/base/factor.html)`(``adeg``$``ANRIND``, levels ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"LOW"``, ``"NORMAL"``, ``"HIGH"``)``)`\
`  ``adeg``$``BNRIND`` ``<-`` `[`factor`](https://rdrr.io/r/base/factor.html)`(``adeg``$``BNRIND``, levels ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"LOW"``, ``"NORMAL"``, ``"HIGH"``)``)`\
\
`  ``adeg`` ``<-`` ``dplyr``::`[`inner_join`](https://dplyr.tidyverse.org/reference/mutate-joins.html)`(`\
`    ``adsl``,`\
`    ``adeg``,`\
`    by ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"STUDYID"``, ``"USUBJID"``)``,`\
`    multiple ``=`` ``"all"`\
`  ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`rowwise`](https://dplyr.tidyverse.org/reference/rowwise.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``TRTENDT ``=`` ``lubridate``::`[`date`](https://lubridate.tidyverse.org/reference/date.html)`(``dplyr``::`[`case_when`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)`(`\
`      `[`is.na`](https://rdrr.io/r/base/NA.html)`(``TRTEDTM``)`` ``~`` ``lubridate``::`[`floor_date`](https://lubridate.tidyverse.org/reference/round_date.html)`(``lubridate``::`[`date`](https://lubridate.tidyverse.org/reference/date.html)`(``TRTSDTM``)`` ``+`` ``study_duration_secs``, unit ``=`` ``"day"``)``,`\
`      ``TRUE`` ``~`` ``TRTEDTM`\
`    ``)``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`ungroup`](https://dplyr.tidyverse.org/reference/group_by.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(``USUBJID``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`arrange`](https://dplyr.tidyverse.org/reference/arrange.html)`(``USUBJID``, ``AVISITN``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``ADTM ``=`` `[`rep`](https://rdrr.io/r/base/rep.html)`(`\
`      `[`sort`](https://rdrr.io/r/base/sort.html)`(`[`sample`](https://rdrr.io/r/base/sample.html)`(`\
`        `[`seq`](https://rdrr.io/r/base/seq.html)`(``lubridate``::`[`as_datetime`](https://lubridate.tidyverse.org/reference/as_date.html)`(``TRTSDTM``[``1``]``)``, ``lubridate``::`[`as_datetime`](https://lubridate.tidyverse.org/reference/as_date.html)`(``TRTENDT``[``1``]``)``, by ``=`` ``"day"``)``,`\
`        size ``=`` `[`nlevels`](https://rdrr.io/r/base/nlevels.html)`(``AVISIT``)`\
`      ``)``)``,`\
`      each ``=`` `[`n`](https://dplyr.tidyverse.org/reference/context.html)`(``)`` ``/`` `[`nlevels`](https://rdrr.io/r/base/nlevels.html)`(``AVISIT``)`\
`    ``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`ungroup`](https://dplyr.tidyverse.org/reference/group_by.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`select`](https://dplyr.tidyverse.org/reference/select.html)`(``-``TRTENDT``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`ungroup`](https://dplyr.tidyverse.org/reference/group_by.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`arrange`](https://dplyr.tidyverse.org/reference/arrange.html)`(``.data``$``STUDYID``, ``.data``$``USUBJID``, ``.data``$``ADTM``)`\
\
`  ``adeg`` ``<-`` ``adeg`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(``.data``$``USUBJID``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`ungroup`](https://dplyr.tidyverse.org/reference/group_by.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`arrange`](https://dplyr.tidyverse.org/reference/arrange.html)`(`\
`      ``.data``$``STUDYID``,`\
`      ``.data``$``USUBJID``,`\
`      ``.data``$``PARAMCD``,`\
`      ``.data``$``BASETYPE``,`\
`      ``.data``$``AVISITN``,`\
`      ``.data``$``DTYPE``,`\
`      ``.data``$``ADTM`\
`    ``)`\
\
`  ``adeg`` ``<-`` ``adeg`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``ONTRTFL ``=`` `[`factor`](https://rdrr.io/r/base/factor.html)`(``dplyr``::`[`case_when`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)`(`\
`      `[`is.na`](https://rdrr.io/r/base/NA.html)`(``.data``$``TRTSDTM``)`` ``~`` ``""``,`\
`      `[`is.na`](https://rdrr.io/r/base/NA.html)`(``.data``$``ADTM``)`` ``~`` ``"Y"``,`\
`      ``(``.data``$``ADTM`` ``<`` ``.data``$``TRTSDTM``)`` ``~`` ``""``,`\
`      ``(``.data``$``ADTM`` ``>`` ``.data``$``TRTEDTM``)`` ``~`` ``""``,`\
`      ``TRUE`` ``~`` ``"Y"`\
`    ``)``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``AVALC ``=`` `[`ifelse`](https://rdrr.io/r/base/ifelse.html)`(`\
`      ``.data``$``PARAMCD`` ``==`` ``"ECGINTP"``,`\
`      `[`as.character`](https://rdrr.io/r/base/character.html)`(``sample_fct``(`[`c`](https://rdrr.io/r/base/c.html)`(``"ABNORMAL"``, ``"NORMAL"``)``, `[`nrow`](https://rdrr.io/r/base/nrow.html)`(``adeg``)``, prob ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0.25``, ``0.75``)``)``)``,`\
`      `[`as.character`](https://rdrr.io/r/base/character.html)`(``.data``$``AVAL``)`\
`    ``)``)`\
\
`  ``adeg`` ``<-`` ``adeg`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``row_check ``=`` `[`seq_len`](https://rdrr.io/r/base/seq.html)`(`[`nrow`](https://rdrr.io/r/base/nrow.html)`(``adeg``)``)``)`\
`  ``get_groups`` ``<-`` ``function``(``data``, ``minimum``)`` ``{`\
`    ``data`` ``<-`` ``data`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`      ``dplyr``::`[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(``.data``$``USUBJID``, ``.data``$``PARAMCD``, ``.data``$``BASETYPE``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`      ``dplyr``::`[`arrange`](https://dplyr.tidyverse.org/reference/arrange.html)`(``.data``$``ADTM``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`      ``dplyr``::`[`filter`](https://dplyr.tidyverse.org/reference/filter.html)`(`\
`        ``(``.data``$``AVISIT`` ``!=`` ``"BASELINE"`` ``&`` ``.data``$``AVISIT`` ``!=`` ``"SCREENING"``)`` ``&`\
`          ``(``.data``$``ONTRTFL`` ``==`` ``"Y"`` ``|`` ``.data``$``ADTM`` ``<=`` ``.data``$``TRTSDTM``)`\
`      ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`      ``{`\
`        ``if`` ``(``minimum`` ``==`` ``TRUE``)`` ``{`\
`          ``dplyr``::`[`filter`](https://dplyr.tidyverse.org/reference/filter.html)`(``.``, ``.data``$``AVAL`` ``==`` `[`min`](https://rdrr.io/r/base/Extremes.html)`(``.data``$``AVAL``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`            ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``.``, DTYPE ``=`` ``"MINIMUM"``, AVISIT ``=`` ``"POST-BASELINE MINIMUM"``)`\
`        ``}`` ``else`` ``{`\
`          ``dplyr``::`[`filter`](https://dplyr.tidyverse.org/reference/filter.html)`(``.``, ``.data``$``AVAL`` ``==`` `[`max`](https://rdrr.io/r/base/Extremes.html)`(``.data``$``AVAL``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`            ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``.``, DTYPE ``=`` ``"MAXIMUM"``, AVISIT ``=`` ``"POST-BASELINE MAXIMUM"``)`\
`        ``}`\
`      ``}`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`      ``dplyr``::`[`slice`](https://dplyr.tidyverse.org/reference/slice.html)`(``1``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`      ``dplyr``::`[`ungroup`](https://dplyr.tidyverse.org/reference/group_by.html)`(``)`\
`    ``data`\
`  ``}`\
\
`  ``lbls`` ``<-`` `[`col_labels`](https://insightsengineering.github.io/teal.data/latest-tag/reference/col_labels.html)`(``adeg``)`\
`  ``adeg`` ``<-`` `[`rbind`](https://insightsengineering.github.io/rtables/latest-tag/reference/rbind.html)`(``adeg``, ``get_groups``(``adeg``, ``TRUE``)``, ``get_groups``(``adeg``, ``FALSE``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`arrange`](https://dplyr.tidyverse.org/reference/arrange.html)`(``.data``$``row_check``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(``.data``$``USUBJID``, ``.data``$``PARAMCD``, ``.data``$``BASETYPE``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`arrange`](https://dplyr.tidyverse.org/reference/arrange.html)`(``.data``$``AVISIT``, .by_group ``=`` ``TRUE``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`ungroup`](https://dplyr.tidyverse.org/reference/group_by.html)`(``)`\
`  `[`col_labels`](https://insightsengineering.github.io/teal.data/latest-tag/reference/col_labels.html)`(``adeg``)`` ``<-`` ``lbls`\
\
`  ``adeg`` ``<-`` ``adeg``[``, ``-`[`which`](https://rdrr.io/r/base/which.html)`(`[`names`](https://rdrr.io/r/base/names.html)`(``adeg``)`` `[`%in%`](https://rdrr.io/r/base/match.html)` `[`c`](https://rdrr.io/r/base/c.html)`(``"row_check"``)``)``]`\
`  ``flag_variables`` ``<-`` ``function``(``data``, ``worst_obs``)`` ``{`\
`    ``data_compare`` ``<-`` ``data`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`      ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``row_check ``=`` `[`seq_len`](https://rdrr.io/r/base/seq.html)`(`[`nrow`](https://rdrr.io/r/base/nrow.html)`(``data``)``)``)`\
`    ``data`` ``<-`` ``data_compare`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`      ``{`\
`        ``if`` ``(``worst_obs`` ``==`` ``FALSE``)`` ``{`\
`          ``dplyr``::`[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(``.``, ``.data``$``USUBJID``, ``.data``$``PARAMCD``, ``.data``$``BASETYPE``, ``.data``$``AVISIT``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`            ``dplyr``::`[`arrange`](https://dplyr.tidyverse.org/reference/arrange.html)`(``.``, ``.data``$``ADTM``)`\
`        ``}`` ``else`` ``{`\
`          ``dplyr``::`[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(``.``, ``.data``$``USUBJID``, ``.data``$``PARAMCD``, ``.data``$``BASETYPE``)`\
`        ``}`\
`      ``}`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`      ``dplyr``::`[`filter`](https://dplyr.tidyverse.org/reference/filter.html)`(`\
`        ``.data``$``AVISITN`` ``>`` ``0`` ``&`` ``(``.data``$``ONTRTFL`` ``==`` ``"Y"`` ``|`` ``.data``$``ADTM`` ``<=`` ``.data``$``TRTSDTM``)`` ``&`\
`          `[`is.na`](https://rdrr.io/r/base/NA.html)`(``.data``$``DTYPE``)`\
`      ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`      ``{`\
`        ``if`` ``(``worst_obs`` ``==`` ``TRUE``)`` ``{`\
`          ``dplyr``::`[`arrange`](https://dplyr.tidyverse.org/reference/arrange.html)`(``.``, ``.data``$``AVALC``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` ``dplyr``::`[`filter`](https://dplyr.tidyverse.org/reference/filter.html)`(``.``, `[`ifelse`](https://rdrr.io/r/base/ifelse.html)`(`\
`            ``.data``$``PARAMCD`` ``==`` ``"ECGINTP"``,`\
`            `[`ifelse`](https://rdrr.io/r/base/ifelse.html)`(``.data``$``AVALC`` ``==`` ``"ABNORMAL"``, ``.data``$``AVALC`` ``==`` ``"ABNORMAL"``, ``.data``$``AVALC`` ``==`` ``"NORMAL"``)``,`\
`            ``.data``$``AVAL`` ``==`` `[`min`](https://rdrr.io/r/base/Extremes.html)`(``.data``$``AVAL``)`\
`          ``)``)`\
`        ``}`` ``else`` ``{`\
`          ``dplyr``::`[`filter`](https://dplyr.tidyverse.org/reference/filter.html)`(``.``, `[`ifelse`](https://rdrr.io/r/base/ifelse.html)`(`\
`            ``.data``$``PARAMCD`` ``==`` ``"ECGINTP"``,`\
`            ``.data``$``AVALC`` ``==`` ``"ABNORMAL"`` ``|`` ``.data``$``AVALC`` ``==`` ``"NORMAL"``,`\
`            ``.data``$``AVAL`` ``==`` `[`min`](https://rdrr.io/r/base/Extremes.html)`(``.data``$``AVAL``)`\
`          ``)``)`\
`        ``}`\
`      ``}`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`      ``dplyr``::`[`slice`](https://dplyr.tidyverse.org/reference/slice.html)`(``1``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`      ``{`\
`        ``if`` ``(``worst_obs`` ``==`` ``TRUE``)`` ``{`\
`          ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``.``, new_var ``=`` ``dplyr``::`[`case_when`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)`(`\
`            ``(``.data``$``AVALC`` ``==`` ``"ABNORMAL"`` ``|`` ``.data``$``AVALC`` ``==`` ``"NORMAL"``)`` ``~`` ``"Y"``,`\
`            ``(``!`[`is.na`](https://rdrr.io/r/base/NA.html)`(``.data``$``AVAL``)`` ``&`` `[`is.na`](https://rdrr.io/r/base/NA.html)`(``.data``$``DTYPE``)``)`` ``~`` ``"Y"``,`\
`            ``TRUE`` ``~`` ``""`\
`          ``)``)`\
`        ``}`` ``else`` ``{`\
`          ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``.``, new_var ``=`` ``dplyr``::`[`case_when`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)`(`\
`            ``(``.data``$``AVALC`` ``==`` ``"ABNORMAL"`` ``|`` ``.data``$``AVALC`` ``==`` ``"NORMAL"``)`` ``~`` ``"Y"``,`\
`            ``(``!`[`is.na`](https://rdrr.io/r/base/NA.html)`(``.data``$``AVAL``)`` ``&`` `[`is.na`](https://rdrr.io/r/base/NA.html)`(``.data``$``DTYPE``)``)`` ``~`` ``"Y"``,`\
`            ``TRUE`` ``~`` ``""`\
`          ``)``)`\
`        ``}`\
`      ``}`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`      ``dplyr``::`[`ungroup`](https://dplyr.tidyverse.org/reference/group_by.html)`(``)`\
\
`    ``data_compare``$``new_var`` ``<-`` `[`ifelse`](https://rdrr.io/r/base/ifelse.html)`(``data_compare``$``row_check`` `[`%in%`](https://rdrr.io/r/base/match.html)` ``data``$``row_check``, ``"Y"``, ``""``)`\
`    ``data_compare`` ``<-`` ``data_compare``[``, ``-`[`which`](https://rdrr.io/r/base/which.html)`(`[`names`](https://rdrr.io/r/base/names.html)`(``data_compare``)`` `[`%in%`](https://rdrr.io/r/base/match.html)` `[`c`](https://rdrr.io/r/base/c.html)`(``"row_check"``)``)``]`\
\
`    ``data_compare`\
`  ``}`\
`  ``adeg`` ``<-`` ``flag_variables``(``adeg``, ``FALSE``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` ``dplyr``::`[`rename`](https://dplyr.tidyverse.org/reference/rename.html)`(``WORS01FL ``=`` ``"new_var"``)`\
`  ``adeg`` ``<-`` ``flag_variables``(``adeg``, ``TRUE``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` ``dplyr``::`[`rename`](https://dplyr.tidyverse.org/reference/rename.html)`(``WORS02FL ``=`` ``"new_var"``)`\
\
`  ``tmc_ex_adeg`` ``<-`` ``adeg`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(``.data``$``USUBJID``, ``.data``$``PARAMCD``, ``.data``$``BASETYPE``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``BASEC ``=`` `[`ifelse`](https://rdrr.io/r/base/ifelse.html)`(`\
`      ``.data``$``PARAMCD`` ``==`` ``"ECGINTP"``,`\
`      ``.data``$``AVALC``[``.data``$``AVISIT`` ``==`` ``"BASELINE"``]``,`\
`      `[`as.character`](https://rdrr.io/r/base/character.html)`(``.data``$``BASE``)`\
`    ``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`ungroup`](https://dplyr.tidyverse.org/reference/group_by.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    `[`col_relabel`](https://insightsengineering.github.io/teal.data/latest-tag/reference/col_labels.html)`(``BASEC ``=`` ``"Baseline Character Value"``)`\
\
`  ``i_lbls`` ``<-`` `[`sapply`](https://rdrr.io/r/base/lapply.html)`(`\
`    `[`names`](https://rdrr.io/r/base/names.html)`(`[`col_labels`](https://insightsengineering.github.io/teal.data/latest-tag/reference/col_labels.html)`(``tmc_ex_adeg``)``[`[`is.na`](https://rdrr.io/r/base/NA.html)`(`[`col_labels`](https://insightsengineering.github.io/teal.data/latest-tag/reference/col_labels.html)`(``tmc_ex_adeg``)``)``]``)``, ``function``(``x``)`` `[`which`](https://rdrr.io/r/base/which.html)`(`[`names`](https://rdrr.io/r/base/names.html)`(``common_var_labels``)`` ``==`` ``x``)`\
`  ``)`\
`  `[`col_labels`](https://insightsengineering.github.io/teal.data/latest-tag/reference/col_labels.html)`(``tmc_ex_adeg``[`[`names`](https://rdrr.io/r/base/names.html)`(``i_lbls``)``]``)`` ``<-`` ``common_var_labels``[``i_lbls``]`\
\
`  `[`save`](https://rdrr.io/r/base/save.html)`(``tmc_ex_adeg``, file ``=`` ``"data/tmc_ex_adeg.rda"``, compress ``=`` ``"xz"``)`\
`}`

## `ADEX`

\
`generate_adex`` ``<-`` ``function``(``adsl`` ``=`` ``tmc_ex_adsl``,`\
`                          ``n_assessments`` ``=`` ``3L``,`\
`                          ``n_days`` ``=`` ``3L``,`\
`                          ``max_n_exs`` ``=`` ``3L``)`` ``{`\
`  `[`set.seed`](https://rdrr.io/r/base/Random.html)`(``1``)`\
`  ``param`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(`\
`    ``"Dose administered during constant dosing interval"``,`\
`    ``"Number of doses administered during constant dosing interval"``,`\
`    ``"Total dose administered"``,`\
`    ``"Total number of doses administered"`\
`  ``)`\
`  ``paramcd`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``"DOSE"``, ``"NDOSE"``, ``"TDOSE"``, ``"TNDOSE"``)`\
`  ``paramu`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``"mg"``, ``" "``, ``"mg"``, ``" "``)`\
`  ``parcat1`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``"INDIVIDUAL"``, ``"OVERALL"``)`\
`  ``parcat2`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``"Drug A"``, ``"Drug B"``)`\
`  ``visit_format`` ``<-`` ``"WEEK"`\
\
`  ``param_init_list`` ``<-`` ``relvar_init``(``param``, ``paramcd``)`\
`  ``unit_init_list`` ``<-`` ``relvar_init``(``param``, ``paramu``)`\
\
`  ``adex`` ``<-`` `[`expand.grid`](https://rdrr.io/r/base/expand.grid.html)`(`\
`    STUDYID ``=`` `[`unique`](https://rdrr.io/r/base/unique.html)`(``adsl``$``STUDYID``)``,`\
`    USUBJID ``=`` ``adsl``$``USUBJID``,`\
`    PARAM ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(`\
`      `[`rep`](https://rdrr.io/r/base/rep.html)`(`\
`        ``param_init_list``$``relvar1``[``1``]``,`\
`        `[`length`](https://rdrr.io/r/base/length.html)`(`[`levels`](https://rdrr.io/r/base/levels.html)`(``visit_schedule``(``visit_format ``=`` ``visit_format``, n_assessments ``=`` ``n_assessments``, n_days ``=`` ``n_days``)``)``)`\
`      ``)``,`\
`      `[`rep`](https://rdrr.io/r/base/rep.html)`(`\
`        ``param_init_list``$``relvar1``[``2``]``,`\
`        `[`length`](https://rdrr.io/r/base/length.html)`(`[`levels`](https://rdrr.io/r/base/levels.html)`(``visit_schedule``(``visit_format ``=`` ``visit_format``, n_assessments ``=`` ``n_assessments``, n_days ``=`` ``n_days``)``)``)`\
`      ``)``,`\
`      ``param_init_list``$``relvar1``[``3``:``4``]`\
`    ``)``,`\
`    stringsAsFactors ``=`` ``FALSE`\
`  ``)`\
\
`  ``adex``$``PARAMCD`` ``<-`` `[`as.factor`](https://rdrr.io/r/base/factor.html)`(``rel_var``(`\
`    df ``=`` ``adex``,`\
`    var_name ``=`` ``"PARAMCD"``,`\
`    var_values ``=`` ``param_init_list``$``relvar2``,`\
`    related_var ``=`` ``"PARAM"`\
`  ``)``)`\
\
`  ``adex``$``AVALU`` ``<-`` `[`as.factor`](https://rdrr.io/r/base/factor.html)`(``rel_var``(`\
`    df ``=`` ``adex``,`\
`    var_name ``=`` ``"AVALU"``,`\
`    var_values ``=`` ``unit_init_list``$``relvar2``,`\
`    related_var ``=`` ``"PARAM"`\
`  ``)``)`\
\
`  ``adex`` ``<-`` ``adex`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(``.data``$``USUBJID``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``PARCAT_ind ``=`` `[`sample`](https://rdrr.io/r/base/sample.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``1``, ``2``)``, size ``=`` ``1``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``PARCAT2 ``=`` `[`ifelse`](https://rdrr.io/r/base/ifelse.html)`(``.data``$``PARCAT_ind`` ``==`` ``1``, ``parcat2``[``1``]``, ``parcat2``[``2``]``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`select`](https://dplyr.tidyverse.org/reference/select.html)`(``-``"PARCAT_ind"``)`\
\
`  ``adex`` ``<-`` ``adex`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``PARCAT1 ``=`` ``dplyr``::`[`case_when`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)`(`\
`    ``(``.data``$``PARAMCD`` ``==`` ``"TNDOSE"`` ``|`` ``.data``$``PARAMCD`` ``==`` ``"TDOSE"``)`` ``~`` ``"OVERALL"``,`\
`    ``.data``$``PARAMCD`` ``==`` ``"DOSE"`` ``|`` ``.data``$``PARAMCD`` ``==`` ``"NDOSE"`` ``~`` ``"INDIVIDUAL"`\
`  ``)``)`\
\
`  ``adex_visit`` ``<-`` ``adex`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`filter`](https://dplyr.tidyverse.org/reference/filter.html)`(``.data``$``PARAMCD`` ``==`` ``"DOSE"`` ``|`` ``.data``$``PARAMCD`` ``==`` ``"NDOSE"``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(`\
`      AVISIT ``=`` `[`rep`](https://rdrr.io/r/base/rep.html)`(``visit_schedule``(``visit_format ``=`` ``visit_format``, n_assessments ``=`` ``n_assessments``, n_days ``=`` ``n_days``)``, ``2``)`\
`    ``)`\
\
`  ``adex`` ``<-`` ``dplyr``::`[`left_join`](https://dplyr.tidyverse.org/reference/mutate-joins.html)`(`\
`    ``adex`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`      ``dplyr``::`[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(`\
`        ``.data``$``USUBJID``,`\
`        ``.data``$``STUDYID``,`\
`        ``.data``$``PARAM``,`\
`        ``.data``$``PARAMCD``,`\
`        ``.data``$``AVALU``,`\
`        ``.data``$``PARCAT1``,`\
`        ``.data``$``PARCAT2`\
`      ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`      ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``id ``=`` ``dplyr``::`[`row_number`](https://dplyr.tidyverse.org/reference/row_number.html)`(``)``)``,`\
`    ``adex_visit`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`      ``dplyr``::`[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(`\
`        ``.data``$``USUBJID``,`\
`        ``.data``$``STUDYID``,`\
`        ``.data``$``PARAM``,`\
`        ``.data``$``PARAMCD``,`\
`        ``.data``$``AVALU``,`\
`        ``.data``$``PARCAT1``,`\
`        ``.data``$``PARCAT2`\
`      ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`      ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``id ``=`` ``dplyr``::`[`row_number`](https://dplyr.tidyverse.org/reference/row_number.html)`(``)``)``,`\
`    by ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"USUBJID"``, ``"STUDYID"``, ``"PARCAT1"``, ``"PARCAT2"``, ``"id"``, ``"PARAMCD"``, ``"PARAM"``, ``"AVALU"``)`\
`  ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`select`](https://dplyr.tidyverse.org/reference/select.html)`(``-``"id"``)`\
\
`  ``adex`` ``<-`` ``adex`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``AVISITN ``=`` ``dplyr``::`[`case_when`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)`(`\
`    ``AVISIT`` ``==`` ``"SCREENING"`` ``~`` ``-``1``,`\
`    ``AVISIT`` ``==`` ``"BASELINE"`` ``~`` ``0``,`\
`    ``(`[`grepl`](https://rdrr.io/r/base/grep.html)`(``"^WEEK"``, ``AVISIT``)`` ``|`` `[`grepl`](https://rdrr.io/r/base/grep.html)`(``"^CYCLE"``, ``AVISIT``)``)`` ``~`` `[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(``AVISIT``)`` ``-`` ``2``,`\
`    ``TRUE`` ``~`` ``999000`\
`  ``)``)`\
\
`  ``adex2`` ``<-`` `[`split`](https://rdrr.io/r/base/split.html)`(``adex``, ``adex``$``USUBJID``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``function``(``pinfo``)`` ``{`\
`      ``pinfo`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`        ``dplyr``::`[`filter`](https://dplyr.tidyverse.org/reference/filter.html)`(``.data``$``PARAMCD`` ``==`` ``"DOSE"``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`        ``dplyr``::`[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(``.data``$``USUBJID``, ``.data``$``PARCAT2``, ``.data``$``AVISIT``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`        ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``changeind ``=`` ``dplyr``::`[`case_when`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)`(`\
`          ``.data``$``AVISIT`` ``==`` ``"SCREENING"`` ``~`` ``0``,`\
`          ``.data``$``AVISIT`` ``!=`` ``"SCREENING"`` ``~`` `[`sample`](https://rdrr.io/r/base/sample.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``-``1``, ``0``, ``1``)``,`\
`            size ``=`` ``1``,`\
`            prob ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0.25``, ``0.5``, ``0.25``)``,`\
`            replace ``=`` ``TRUE`\
`          ``)`\
`        ``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`        ``dplyr``::`[`ungroup`](https://dplyr.tidyverse.org/reference/group_by.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`        ``dplyr``::`[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(``.data``$``USUBJID``, ``.data``$``PARCAT2``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`        ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(`\
`          csum ``=`` `[`cumsum`](https://rdrr.io/r/base/cumsum.html)`(``.data``$``changeind``)``,`\
`          changeind ``=`` ``dplyr``::`[`case_when`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)`(`\
`            ``.data``$``csum`` ``<=`` ``-``3`` ``~`` `[`sample`](https://rdrr.io/r/base/sample.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``0``, ``1``)``, size ``=`` ``1``, prob ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0.5``, ``0.5``)``)``,`\
`            ``.data``$``csum`` ``>=`` ``3`` ``~`` `[`sample`](https://rdrr.io/r/base/sample.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``0``, ``-``1``)``, size ``=`` ``1``, prob ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0.5``, ``0.5``)``)``,`\
`            ``TRUE`` ``~`` ``.data``$``changeind`\
`          ``)`\
`        ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`        ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``csum ``=`` `[`cumsum`](https://rdrr.io/r/base/cumsum.html)`(``.data``$``changeind``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`        ``dplyr``::`[`ungroup`](https://dplyr.tidyverse.org/reference/group_by.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`        ``dplyr``::`[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(``.data``$``USUBJID``, ``.data``$``PARCAT2``, ``.data``$``AVISIT``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`        ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``AVAL ``=`` ``dplyr``::`[`case_when`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)`(`\
`          ``.data``$``csum`` ``==`` ``-``2`` ``~`` ``480``,`\
`          ``.data``$``csum`` ``==`` ``-``1`` ``~`` ``720``,`\
`          ``.data``$``csum`` ``==`` ``0`` ``~`` ``960``,`\
`          ``.data``$``csum`` ``==`` ``1`` ``~`` ``1200``,`\
`          ``.data``$``csum`` ``==`` ``2`` ``~`` ``1440`\
`        ``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`        ``dplyr``::`[`select`](https://dplyr.tidyverse.org/reference/select.html)`(``-`[`c`](https://rdrr.io/r/base/c.html)`(``"csum"``, ``"changeind"``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`        ``dplyr``::`[`ungroup`](https://dplyr.tidyverse.org/reference/group_by.html)`(``)`\
`    ``}``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    `[`Reduce`](https://rdrr.io/r/base/funprog.html)`(``rbind``, ``.``)`\
\
`  ``adextmp`` ``<-`` ``dplyr``::`[`full_join`](https://dplyr.tidyverse.org/reference/mutate-joins.html)`(``adex2``, ``adex``, by ``=`` `[`names`](https://rdrr.io/r/base/names.html)`(``adex``)``)`\
`  ``adex`` ``<-`` ``adextmp`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(``.data``$``USUBJID``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``AVAL ``=`` `[`ifelse`](https://rdrr.io/r/base/ifelse.html)`(``.data``$``PARAMCD`` ``==`` ``"NDOSE"``, ``1``, ``.data``$``AVAL``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``AVAL ``=`` `[`ifelse`](https://rdrr.io/r/base/ifelse.html)`(`\
`      ``.data``$``PARAMCD`` ``==`` ``"TNDOSE"``,`\
`      `[`sum`](https://rdrr.io/r/base/sum.html)`(``.data``$``AVAL``[``.data``$``PARAMCD`` ``==`` ``"NDOSE"``]``)``,`\
`      ``.data``$``AVAL`\
`    ``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`ungroup`](https://dplyr.tidyverse.org/reference/group_by.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(``.data``$``USUBJID``, ``.data``$``STUDYID``, ``.data``$``PARCAT2``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``AVAL ``=`` `[`ifelse`](https://rdrr.io/r/base/ifelse.html)`(`\
`      ``.data``$``PARAMCD`` ``==`` ``"TDOSE"``,`\
`      `[`sum`](https://rdrr.io/r/base/sum.html)`(``.data``$``AVAL``[``.data``$``PARAMCD`` ``==`` ``"DOSE"``]``)``,`\
`      ``.data``$``AVAL`\
`    ``)``)`\
\
`  ``adex`` ``<-`` ``dplyr``::`[`inner_join`](https://dplyr.tidyverse.org/reference/mutate-joins.html)`(``adsl``, ``adex``, by ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"STUDYID"``, ``"USUBJID"``)``, multiple ``=`` ``"all"``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`rowwise`](https://dplyr.tidyverse.org/reference/rowwise.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``TRTENDT ``=`` ``lubridate``::`[`date`](https://lubridate.tidyverse.org/reference/date.html)`(``dplyr``::`[`case_when`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)`(`\
`      `[`is.na`](https://rdrr.io/r/base/NA.html)`(``TRTEDTM``)`` ``~`` ``lubridate``::`[`floor_date`](https://lubridate.tidyverse.org/reference/round_date.html)`(``lubridate``::`[`date`](https://lubridate.tidyverse.org/reference/date.html)`(``TRTSDTM``)`` ``+`` ``study_duration_secs``, unit ``=`` ``"day"``)``,`\
`      ``TRUE`` ``~`` ``TRTEDTM`\
`    ``)``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``ASTDTM ``=`` `[`sample`](https://rdrr.io/r/base/sample.html)`(`\
`      `[`seq`](https://rdrr.io/r/base/seq.html)`(``lubridate``::`[`as_datetime`](https://lubridate.tidyverse.org/reference/as_date.html)`(``TRTSDTM``)``, ``lubridate``::`[`as_datetime`](https://lubridate.tidyverse.org/reference/as_date.html)`(``TRTENDT``)``, by ``=`` ``"day"``)``,`\
`      size ``=`` ``1`\
`    ``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`select`](https://dplyr.tidyverse.org/reference/select.html)`(``-``TRTENDT``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`ungroup`](https://dplyr.tidyverse.org/reference/group_by.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`arrange`](https://dplyr.tidyverse.org/reference/arrange.html)`(``.data``$``STUDYID``, ``.data``$``USUBJID``, ``.data``$``ASTDTM``)`\
\
`  ``adex`` ``<-`` ``adex`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(``.data``$``USUBJID``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``EXSEQ ``=`` `[`seq_len`](https://rdrr.io/r/base/seq.html)`(``dplyr``::`[`n`](https://dplyr.tidyverse.org/reference/context.html)`(``)``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`ungroup`](https://dplyr.tidyverse.org/reference/group_by.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`arrange`](https://dplyr.tidyverse.org/reference/arrange.html)`(`\
`      ``.data``$``STUDYID``,`\
`      ``.data``$``USUBJID``,`\
`      ``.data``$``PARAMCD``,`\
`      ``.data``$``ASTDTM``,`\
`      ``.data``$``AVISITN`\
`    ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    `[`col_relabel`](https://insightsengineering.github.io/teal.data/latest-tag/reference/col_labels.html)`(`\
`      PARCAT1 ``=`` ``"Parameter Category (Individual/Overall)"``,`\
`      PARCAT2 ``=`` ``"Parameter Category (Drug A/Drug B)"``,`\
`      EXSEQ ``=`` ``"Analysis Sequence Number"`\
`    ``)`\
\
`  ``visit_levels`` ``<-`` ``str_extract``(`[`levels`](https://rdrr.io/r/base/levels.html)`(``adex``$``AVISIT``)``, pattern ``=`` ``"[0-9]+"``)`\
`  ``vl_extracted`` ``<-`` `[`vapply`](https://rdrr.io/r/base/lapply.html)`(``visit_levels``, ``function``(``x``)`` `[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(``x``[``2``]``)``, `[`numeric`](https://rdrr.io/r/base/numeric.html)`(``1``)``)`\
`  ``vl_extracted`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``-``1``, ``1``, ``vl_extracted``[``!`[`is.na`](https://rdrr.io/r/base/NA.html)`(``vl_extracted``)``]``)`\
\
`  ``tmc_ex_adex`` ``<-`` ``adex`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``VISITDY ``=`` `[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(`[`as.character`](https://rdrr.io/r/base/character.html)`(`[`factor`](https://rdrr.io/r/base/factor.html)`(``AVISIT``, labels ``=`` ``vl_extracted``)``)``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``ASTDTM ``=`` ``lubridate``::`[`as_datetime`](https://lubridate.tidyverse.org/reference/as_date.html)`(``TRTSDTM``)`` ``+`` ``lubridate``::`[`days`](https://lubridate.tidyverse.org/reference/period.html)`(``VISITDY``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`distinct`](https://dplyr.tidyverse.org/reference/distinct.html)`(``USUBJID``, .keep_all ``=`` ``TRUE``)`\
\
`  ``i_lbls`` ``<-`` `[`sapply`](https://rdrr.io/r/base/lapply.html)`(`\
`    `[`names`](https://rdrr.io/r/base/names.html)`(`[`col_labels`](https://insightsengineering.github.io/teal.data/latest-tag/reference/col_labels.html)`(``tmc_ex_adex``)``[`[`is.na`](https://rdrr.io/r/base/NA.html)`(`[`col_labels`](https://insightsengineering.github.io/teal.data/latest-tag/reference/col_labels.html)`(``tmc_ex_adex``)``)``]``)``, ``function``(``x``)`` `[`which`](https://rdrr.io/r/base/which.html)`(`[`names`](https://rdrr.io/r/base/names.html)`(``common_var_labels``)`` ``==`` ``x``)`\
`  ``)`\
`  `[`col_labels`](https://insightsengineering.github.io/teal.data/latest-tag/reference/col_labels.html)`(``tmc_ex_adex``[`[`names`](https://rdrr.io/r/base/names.html)`(``i_lbls``)``]``)`` ``<-`` ``common_var_labels``[``i_lbls``]`\
\
`  `[`save`](https://rdrr.io/r/base/save.html)`(``tmc_ex_adex``, file ``=`` ``"data/tmc_ex_adex.rda"``, compress ``=`` ``"xz"``)`\
`}`

## `ADLB`

\
`generate_adlb`` ``<-`` ``function``(``adsl`` ``=`` ``tmc_ex_adsl``,`\
`                          ``n_assessments`` ``=`` ``3L``,`\
`                          ``n_days`` ``=`` ``3L``,`\
`                          ``max_n_lbs`` ``=`` ``3L``)`` ``{`\
`  `[`set.seed`](https://rdrr.io/r/base/Random.html)`(``1``)`\
`  ``lbcat`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``"CHEMISTRY"``, ``"CHEMISTRY"``, ``"IMMUNOLOGY"``)`\
`  ``param`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(`\
`    ``"Alanine Aminotransferase Measurement"``,`\
`    ``"C-Reactive Protein Measurement"``,`\
`    ``"Immunoglobulin A Measurement"`\
`  ``)`\
`  ``paramcd`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``"ALT"``, ``"CRP"``, ``"IGA"``)`\
`  ``paramu`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``"U/L"``, ``"mg/L"``, ``"g/L"``)`\
`  ``aval_mean`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``20``, ``1``, ``2``)`\
`  ``visit_format`` ``<-`` ``"WEEK"`\
\
`  ``# validate and initialize related variables`\
`  ``lbcat_init_list`` ``<-`` ``relvar_init``(``param``, ``lbcat``)`\
`  ``param_init_list`` ``<-`` ``relvar_init``(``param``, ``paramcd``)`\
`  ``unit_init_list`` ``<-`` ``relvar_init``(``param``, ``paramu``)`\
\
`  ``adlb`` ``<-`` `[`expand.grid`](https://rdrr.io/r/base/expand.grid.html)`(`\
`    STUDYID ``=`` `[`unique`](https://rdrr.io/r/base/unique.html)`(``adsl``$``STUDYID``)``,`\
`    USUBJID ``=`` ``adsl``$``USUBJID``,`\
`    PARAM ``=`` `[`as.factor`](https://rdrr.io/r/base/factor.html)`(``param_init_list``$``relvar1``)``,`\
`    AVISIT ``=`` ``visit_schedule``(``visit_format ``=`` ``visit_format``, n_assessments ``=`` ``n_assessments``, n_days ``=`` ``n_days``)``,`\
`    stringsAsFactors ``=`` ``FALSE`\
`  ``)`\
\
`  ``# assign AVAL based on different test`\
`  ``adlb`` ``<-`` ``adlb`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``AVAL ``=`` ``stats``::`[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(`[`nrow`](https://rdrr.io/r/base/nrow.html)`(``adlb``)``, mean ``=`` ``1``, sd ``=`` ``0.2``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`left_join`](https://dplyr.tidyverse.org/reference/mutate-joins.html)`(`[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(``PARAM ``=`` ``param``, ADJUST ``=`` ``aval_mean``)``, by ``=`` ``"PARAM"``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``AVAL ``=`` ``.data``$``AVAL`` ``*`` ``.data``$``ADJUST``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`select`](https://dplyr.tidyverse.org/reference/select.html)`(``-``"ADJUST"``)`\
\
`  ``# assign related variable values: PARAMxLBCAT are related`\
`  ``adlb``$``LBCAT`` ``<-`` `[`as.factor`](https://rdrr.io/r/base/factor.html)`(``rel_var``(`\
`    df ``=`` ``adlb``,`\
`    var_name ``=`` ``"LBCAT"``,`\
`    var_values ``=`` ``lbcat_init_list``$``relvar2``,`\
`    related_var ``=`` ``"PARAM"`\
`  ``)``)`\
\
`  ``# assign related variable values: PARAMxPARAMCD are related`\
`  ``adlb``$``PARAMCD`` ``<-`` `[`as.factor`](https://rdrr.io/r/base/factor.html)`(``rel_var``(`\
`    df ``=`` ``adlb``,`\
`    var_name ``=`` ``"PARAMCD"``,`\
`    var_values ``=`` ``param_init_list``$``relvar2``,`\
`    related_var ``=`` ``"PARAM"`\
`  ``)``)`\
\
`  ``adlb``$``AVALU`` ``<-`` `[`as.factor`](https://rdrr.io/r/base/factor.html)`(``rel_var``(`\
`    df ``=`` ``adlb``,`\
`    var_name ``=`` ``"AVALU"``,`\
`    var_values ``=`` ``unit_init_list``$``relvar2``,`\
`    related_var ``=`` ``"PARAM"`\
`  ``)``)`\
\
`  ``adlb`` ``<-`` ``adlb`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``AVISITN ``=`` ``dplyr``::`[`case_when`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)`(`\
`    ``AVISIT`` ``==`` ``"SCREENING"`` ``~`` ``-``1``,`\
`    ``AVISIT`` ``==`` ``"BASELINE"`` ``~`` ``0``,`\
`    ``(`[`grepl`](https://rdrr.io/r/base/grep.html)`(``"^WEEK"``, ``AVISIT``)`` ``|`` `[`grepl`](https://rdrr.io/r/base/grep.html)`(``"^CYCLE"``, ``AVISIT``)``)`` ``~`` `[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(``AVISIT``)`` ``-`` ``2``,`\
`    ``TRUE`` ``~`` ``NA_real_`\
`  ``)``)`\
\
`  ``adlb`` ``<-`` ``adlb`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``AVISITN ``=`` ``dplyr``::`[`case_when`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)`(`\
`      ``AVISIT`` ``==`` ``"SCREENING"`` ``~`` ``-``1``,`\
`      ``AVISIT`` ``==`` ``"BASELINE"`` ``~`` ``0``,`\
`      ``(`[`grepl`](https://rdrr.io/r/base/grep.html)`(``"^WEEK"``, ``AVISIT``)`` ``|`` `[`grepl`](https://rdrr.io/r/base/grep.html)`(``"^CYCLE"``, ``AVISIT``)``)`` ``~`` `[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(``AVISIT``)`` ``-`` ``2``,`\
`      ``TRUE`` ``~`` ``NA_real_`\
`    ``)``)`\
\
`  ``# order to prepare for change from screening and baseline values`\
`  ``adlb`` ``<-`` ``adlb``[`[`order`](https://rdrr.io/r/base/order.html)`(``adlb``$``STUDYID``, ``adlb``$``USUBJID``, ``adlb``$``PARAMCD``, ``adlb``$``AVISITN``)``, ``]`\
\
`  ``adlb`` ``<-`` `[`Reduce`](https://rdrr.io/r/base/funprog.html)`(``rbind``, `[`lapply`](https://rdrr.io/r/base/lapply.html)`(`[`split`](https://rdrr.io/r/base/split.html)`(``adlb``, ``adlb``$``USUBJID``)``, ``function``(``x``)`` ``{`\
`    ``x``$``STUDYID`` ``<-`` ``adsl``$``STUDYID``[`[`which`](https://rdrr.io/r/base/which.html)`(``adsl``$``USUBJID`` ``==`` ``x``$``USUBJID``[``1``]``)``]`\
`    ``x``$``ABLFL2`` ``<-`` `[`ifelse`](https://rdrr.io/r/base/ifelse.html)`(``x``$``AVISIT`` ``==`` ``"SCREENING"``, ``"Y"``, ``""``)`\
`    ``x``$``ABLFL`` ``<-`` `[`ifelse`](https://rdrr.io/r/base/ifelse.html)`(`[`toupper`](https://rdrr.io/r/base/chartr.html)`(``visit_format``)`` ``==`` ``"WEEK"`` ``&`` ``x``$``AVISIT`` ``==`` ``"BASELINE"``,`\
`      ``"Y"``,`\
`      `[`ifelse`](https://rdrr.io/r/base/ifelse.html)`(`[`toupper`](https://rdrr.io/r/base/chartr.html)`(``visit_format``)`` ``==`` ``"CYCLE"`` ``&`` ``x``$``AVISIT`` ``==`` ``"CYCLE 1 DAY 1"``, ``"Y"``, ``""``)`\
`    ``)`\
`    ``x`\
`  ``}``)``)`\
\
`  ``adlb``$``BASE`` ``<-`` `[`ifelse`](https://rdrr.io/r/base/ifelse.html)`(``adlb``$``ABLFL2`` ``!=`` ``"Y"``, ``retain``(``adlb``, ``adlb``$``AVAL``, ``adlb``$``ABLFL`` ``==`` ``"Y"``)``, ``NA``)`\
`  ``anrind_choices`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``"HIGH"``, ``"LOW"``, ``"NORMAL"``)`\
`  ``adlb`` ``<-`` ``adlb`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``BASETYPE ``=`` ``"LAST"``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``ANRIND ``=`` ``sample_fct``(``anrind_choices``, `[`nrow`](https://rdrr.io/r/base/nrow.html)`(``adlb``)``, prob ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0.1``, ``0.1``, ``0.8``)``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``ANRLO ``=`` ``dplyr``::`[`case_when`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)`(`\
`      ``.data``$``PARAMCD`` ``==`` ``"ALT"`` ``~`` ``7``,`\
`      ``.data``$``PARAMCD`` ``==`` ``"CRP"`` ``~`` ``8``,`\
`      ``.data``$``PARAMCD`` ``==`` ``"IGA"`` ``~`` ``0.8`\
`    ``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``ANRHI ``=`` ``dplyr``::`[`case_when`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)`(`\
`      ``.data``$``PARAMCD`` ``==`` ``"ALT"`` ``~`` ``55``,`\
`      ``.data``$``PARAMCD`` ``==`` ``"CRP"`` ``~`` ``10``,`\
`      ``.data``$``PARAMCD`` ``==`` ``"IGA"`` ``~`` ``3`\
`    ``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``DTYPE ``=`` ``NA``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(`\
`      ATOXGR ``=`` `[`factor`](https://rdrr.io/r/base/factor.html)`(``dplyr``::`[`case_when`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)`(`\
`        ``.data``$``ANRIND`` ``==`` ``"LOW"`` ``~`` `[`sample`](https://rdrr.io/r/base/sample.html)`(`\
`          `[`c`](https://rdrr.io/r/base/c.html)`(``"-1"``, ``"-2"``, ``"-3"``, ``"-4"``, ``"-5"``)``,`\
`          `[`nrow`](https://rdrr.io/r/base/nrow.html)`(``adlb``)``,`\
`          replace ``=`` ``TRUE``,`\
`          prob ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0.30``, ``0.25``, ``0.20``, ``0.15``, ``0``)`\
`        ``)``,`\
`        ``.data``$``ANRIND`` ``==`` ``"HIGH"`` ``~`` `[`sample`](https://rdrr.io/r/base/sample.html)`(`\
`          `[`c`](https://rdrr.io/r/base/c.html)`(``"1"``, ``"2"``, ``"3"``, ``"4"``, ``"5"``)``,`\
`          `[`nrow`](https://rdrr.io/r/base/nrow.html)`(``adlb``)``,`\
`          replace ``=`` ``TRUE``,`\
`          prob ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0.30``, ``0.25``, ``0.20``, ``0.15``, ``0``)`\
`        ``)``,`\
`        ``.data``$``ANRIND`` ``==`` ``"NORMAL"`` ``~`` ``"0"`\
`      ``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `[`with_label`](https://pharmaverse.github.io/formatters/latest-tag/reference/with_label.html)`(``"Analysis Toxicity Grade"``)`\
`    ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(``.data``$``USUBJID``, ``.data``$``PARAMCD``, ``.data``$``BASETYPE``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``BTOXGR ``=`` ``.data``$``ATOXGR``[``.data``$``ABLFL`` ``==`` ``"Y"``]``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`ungroup`](https://dplyr.tidyverse.org/reference/group_by.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    `[`col_relabel`](https://insightsengineering.github.io/teal.data/latest-tag/reference/col_labels.html)`(``BTOXGR ``=`` ``"Baseline Toxicity Grade"``)`\
\
`  ``# High and low descriptions of the different PARAMCD values`\
`  ``# This is currently hard coded as the GDSR does not have these descriptions yet`\
`  ``grade_lookup`` ``<-`` ``tibble``::`[`tribble`](https://tibble.tidyverse.org/reference/tribble.html)`(`\
`    ``~``PARAMCD``, ``~``ATOXDSCL``, ``~``ATOXDSCH``,`\
`    ``"ALB"``, ``"Hypoalbuminemia"``, ``NA_character_``,`\
`    ``"ALKPH"``, ``NA_character_``, ``"Alkaline phosphatase increased"``,`\
`    ``"ALT"``, ``NA_character_``, ``"Alanine aminotransferase increased"``,`\
`    ``"AST"``, ``NA_character_``, ``"Aspartate aminotransferase increased"``,`\
`    ``"BILI"``, ``NA_character_``, ``"Blood bilirubin increased"``,`\
`    ``"CA"``, ``"Hypocalcemia"``, ``"Hypercalcemia"``,`\
`    ``"CHOLES"``, ``NA_character_``, ``"Cholesterol high"``,`\
`    ``"CK"``, ``NA_character_``, ``"CPK increased"``,`\
`    ``"CREAT"``, ``NA_character_``, ``"Creatinine increased"``,`\
`    ``"CRP"``, ``NA_character_``, ``"C reactive protein increased"``,`\
`    ``"GGT"``, ``NA_character_``, ``"GGT increased"``,`\
`    ``"GLUC"``, ``"Hypoglycemia"``, ``"Hyperglycemia"``,`\
`    ``"HGB"``, ``"Anemia"``, ``"Hemoglobin increased"``,`\
`    ``"IGA"``, ``NA_character_``, ``"Immunoglobulin A increased"``,`\
`    ``"POTAS"``, ``"Hypokalemia"``, ``"Hyperkalemia"``,`\
`    ``"LYMPH"``, ``"CD4 lymphocytes decreased"``, ``NA_character_``,`\
`    ``"PHOS"``, ``"Hypophosphatemia"``, ``NA_character_``,`\
`    ``"PLAT"``, ``"Platelet count decreased"``, ``NA_character_``,`\
`    ``"SODIUM"``, ``"Hyponatremia"``, ``"Hypernatremia"``,`\
`    ``"WBC"``, ``"White blood cell decreased"``, ``"Leukocytosis"``,`\
`  ``)`\
\
`  ``# merge grade_lookup onto adlb`\
`  ``adlb`` ``<-`` ``dplyr``::`[`left_join`](https://dplyr.tidyverse.org/reference/mutate-joins.html)`(``adlb``, ``grade_lookup``, by ``=`` ``"PARAMCD"``)`\
\
`  ``# merge adsl to be able to add LB date and study day variables`\
`  ``adlb`` ``<-`` ``dplyr``::`[`inner_join`](https://dplyr.tidyverse.org/reference/mutate-joins.html)`(`\
`    ``adsl``,`\
`    ``adlb``,`\
`    by ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"STUDYID"``, ``"USUBJID"``)``,`\
`    multiple ``=`` ``"all"`\
`  ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`rowwise`](https://dplyr.tidyverse.org/reference/rowwise.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``TRTENDT ``=`` ``lubridate``::`[`date`](https://lubridate.tidyverse.org/reference/date.html)`(``dplyr``::`[`case_when`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)`(`\
`      `[`is.na`](https://rdrr.io/r/base/NA.html)`(``TRTEDTM``)`` ``~`` ``lubridate``::`[`floor_date`](https://lubridate.tidyverse.org/reference/round_date.html)`(``lubridate``::`[`date`](https://lubridate.tidyverse.org/reference/date.html)`(``TRTSDTM``)`` ``+`` ``study_duration_secs``, unit ``=`` ``"day"``)``,`\
`      ``TRUE`` ``~`` ``TRTEDTM`\
`    ``)``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`ungroup`](https://dplyr.tidyverse.org/reference/group_by.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(``USUBJID``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`arrange`](https://dplyr.tidyverse.org/reference/arrange.html)`(``USUBJID``, ``AVISITN``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``ADTM ``=`` `[`rep`](https://rdrr.io/r/base/rep.html)`(`\
`      `[`sort`](https://rdrr.io/r/base/sort.html)`(`[`sample`](https://rdrr.io/r/base/sample.html)`(`\
`        `[`seq`](https://rdrr.io/r/base/seq.html)`(``lubridate``::`[`as_datetime`](https://lubridate.tidyverse.org/reference/as_date.html)`(``TRTSDTM``[``1``]``)``, ``lubridate``::`[`as_datetime`](https://lubridate.tidyverse.org/reference/as_date.html)`(``TRTENDT``[``1``]``)``, by ``=`` ``"day"``)``,`\
`        size ``=`` `[`nlevels`](https://rdrr.io/r/base/nlevels.html)`(``AVISIT``)`\
`      ``)``)``,`\
`      each ``=`` `[`n`](https://dplyr.tidyverse.org/reference/context.html)`(``)`` ``/`` `[`nlevels`](https://rdrr.io/r/base/nlevels.html)`(``AVISIT``)`\
`    ``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`ungroup`](https://dplyr.tidyverse.org/reference/group_by.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`select`](https://dplyr.tidyverse.org/reference/select.html)`(``-``TRTENDT``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`arrange`](https://dplyr.tidyverse.org/reference/arrange.html)`(``.data``$``STUDYID``, ``.data``$``USUBJID``, ``.data``$``ADTM``)`\
\
`  ``adlb`` ``<-`` ``adlb`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(``.data``$``USUBJID``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``LBSEQ ``=`` `[`seq_len`](https://rdrr.io/r/base/seq.html)`(``dplyr``::`[`n`](https://dplyr.tidyverse.org/reference/context.html)`(``)``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`ungroup`](https://dplyr.tidyverse.org/reference/group_by.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`arrange`](https://dplyr.tidyverse.org/reference/arrange.html)`(`\
`      ``.data``$``STUDYID``,`\
`      ``.data``$``USUBJID``,`\
`      ``.data``$``PARAMCD``,`\
`      ``.data``$``BASETYPE``,`\
`      ``.data``$``AVISITN``,`\
`      ``.data``$``DTYPE``,`\
`      ``.data``$``ADTM``,`\
`      ``.data``$``LBSEQ`\
`    ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    `[`col_relabel`](https://insightsengineering.github.io/teal.data/latest-tag/reference/col_labels.html)`(``LBSEQ ``=`` ``"Lab Test or Examination Sequence Number"``)`\
\
`  ``adlb`` ``<-`` ``adlb`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``ONTRTFL ``=`` `[`factor`](https://rdrr.io/r/base/factor.html)`(``dplyr``::`[`case_when`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)`(`\
`    `[`is.na`](https://rdrr.io/r/base/NA.html)`(``.data``$``TRTSDTM``)`` ``~`` ``""``,`\
`    `[`is.na`](https://rdrr.io/r/base/NA.html)`(``.data``$``ADTM``)`` ``~`` ``"Y"``,`\
`    ``(``.data``$``ADTM`` ``<`` ``.data``$``TRTSDTM``)`` ``~`` ``""``,`\
`    ``(``.data``$``ADTM`` ``>`` ``.data``$``TRTEDTM``)`` ``~`` ``""``,`\
`    ``TRUE`` ``~`` ``"Y"`\
`  ``)``)``)`\
\
`  ``flag_variables`` ``<-`` ``function``(``data``,`\
`                             ``apply_grouping``,`\
`                             ``apply_filter``,`\
`                             ``apply_mutate``)`` ``{`\
`    ``data_compare`` ``<-`` ``data`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``row_check ``=`` `[`seq_len`](https://rdrr.io/r/base/seq.html)`(`[`nrow`](https://rdrr.io/r/base/nrow.html)`(``data``)``)``)`\
`    ``data`` ``<-`` ``data_compare`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`      ``{`\
`        ``if`` ``(``apply_grouping`` ``==`` ``TRUE``)`` ``{`\
`          ``dplyr``::`[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(``.``, ``.data``$``USUBJID``, ``.data``$``PARAMCD``, ``.data``$``BASETYPE``, ``.data``$``AVISIT``)`\
`        ``}`` ``else`` ``{`\
`          ``dplyr``::`[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(``.``, ``.data``$``USUBJID``, ``.data``$``PARAMCD``, ``.data``$``BASETYPE``)`\
`        ``}`\
`      ``}`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`      ``dplyr``::`[`arrange`](https://dplyr.tidyverse.org/reference/arrange.html)`(``.data``$``ADTM``, ``.data``$``LBSEQ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`      ``{`\
`        ``if`` ``(``apply_filter`` ``==`` ``TRUE``)`` ``{`\
`          ``dplyr``::`[`filter`](https://dplyr.tidyverse.org/reference/filter.html)`(`\
`            ``.``,`\
`            ``(``.data``$``AVISIT`` ``!=`` ``"BASELINE"`` ``&`` ``.data``$``AVISIT`` ``!=`` ``"SCREENING"``)`` ``&`\
`              ``(``.data``$``ONTRTFL`` ``==`` ``"Y"`` ``|`` ``.data``$``ADTM`` ``<=`` ``.data``$``TRTSDTM``)`\
`          ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`            ``dplyr``::`[`filter`](https://dplyr.tidyverse.org/reference/filter.html)`(``.data``$``ATOXGR`` ``==`` `[`max`](https://rdrr.io/r/base/Extremes.html)`(`[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(`[`as.character`](https://rdrr.io/r/base/character.html)`(``.data``$``ATOXGR``)``)``)``)`\
`        ``}`` ``else`` ``if`` ``(``apply_filter`` ``==`` ``FALSE``)`` ``{`\
`          ``dplyr``::`[`filter`](https://dplyr.tidyverse.org/reference/filter.html)`(`\
`            ``.``,`\
`            ``(``.data``$``AVISIT`` ``!=`` ``"BASELINE"`` ``&`` ``.data``$``AVISIT`` ``!=`` ``"SCREENING"``)`` ``&`\
`              ``(``.data``$``ONTRTFL`` ``==`` ``"Y"`` ``|`` ``.data``$``ADTM`` ``<=`` ``.data``$``TRTSDTM``)`\
`          ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`            ``dplyr``::`[`filter`](https://dplyr.tidyverse.org/reference/filter.html)`(``.data``$``ATOXGR`` ``==`` `[`min`](https://rdrr.io/r/base/Extremes.html)`(`[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(`[`as.character`](https://rdrr.io/r/base/character.html)`(``.data``$``ATOXGR``)``)``)``)`\
`        ``}`` ``else`` ``{`\
`          ``dplyr``::`[`filter`](https://dplyr.tidyverse.org/reference/filter.html)`(`\
`            ``.``,`\
`            ``.data``$``AVAL`` ``==`` `[`min`](https://rdrr.io/r/base/Extremes.html)`(``.data``$``AVAL``)`` ``&`\
`              ``(``.data``$``AVISIT`` ``!=`` ``"BASELINE"`` ``&`` ``.data``$``AVISIT`` ``!=`` ``"SCREENING"``)`` ``&`\
`              ``(``.data``$``ONTRTFL`` ``==`` ``"Y"`` ``|`` ``.data``$``ADTM`` ``<=`` ``.data``$``TRTSDTM``)`\
`          ``)`\
`        ``}`\
`      ``}`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`      ``dplyr``::`[`slice`](https://dplyr.tidyverse.org/reference/slice.html)`(``1``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`      ``{`\
`        ``if`` ``(``apply_mutate`` ``==`` ``TRUE``)`` ``{`\
`          ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``.``, new_var ``=`` `[`ifelse`](https://rdrr.io/r/base/ifelse.html)`(`[`is.na`](https://rdrr.io/r/base/NA.html)`(``.data``$``DTYPE``)``, ``"Y"``, ``""``)``)`\
`        ``}`` ``else`` ``{`\
`          ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``.``, new_var ``=`` `[`ifelse`](https://rdrr.io/r/base/ifelse.html)`(`[`is.na`](https://rdrr.io/r/base/NA.html)`(``.data``$``AVAL``)`` ``==`` ``FALSE`` ``&`` `[`is.na`](https://rdrr.io/r/base/NA.html)`(``.data``$``DTYPE``)``, ``"Y"``, ``""``)``)`\
`        ``}`\
`      ``}`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`      ``dplyr``::`[`ungroup`](https://dplyr.tidyverse.org/reference/group_by.html)`(``)`\
\
`    ``data_compare``$``new_var`` ``<-`` `[`ifelse`](https://rdrr.io/r/base/ifelse.html)`(``data_compare``$``row_check`` `[`%in%`](https://rdrr.io/r/base/match.html)` ``data``$``row_check``, ``"Y"``, ``""``)`\
`    ``data_compare`` ``<-`` ``data_compare``[``, ``-`[`which`](https://rdrr.io/r/base/which.html)`(`[`names`](https://rdrr.io/r/base/names.html)`(``data_compare``)`` `[`%in%`](https://rdrr.io/r/base/match.html)` `[`c`](https://rdrr.io/r/base/c.html)`(``"row_check"``)``)``]`\
`    ``data_compare`\
`  ``}`\
`  ``adlb`` ``<-`` ``flag_variables``(``adlb``, ``TRUE``, ``"ELSE"``, ``FALSE``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` ``dplyr``::`[`rename`](https://dplyr.tidyverse.org/reference/rename.html)`(``WORS01FL ``=`` ``"new_var"``)`\
`  ``adlb`` ``<-`` ``flag_variables``(``adlb``, ``FALSE``, ``TRUE``, ``TRUE``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` ``dplyr``::`[`rename`](https://dplyr.tidyverse.org/reference/rename.html)`(``WGRHIFL ``=`` ``"new_var"``)`\
`  ``adlb`` ``<-`` ``flag_variables``(``adlb``, ``FALSE``, ``FALSE``, ``TRUE``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` ``dplyr``::`[`rename`](https://dplyr.tidyverse.org/reference/rename.html)`(``WGRLOFL ``=`` ``"new_var"``)`\
`  ``adlb`` ``<-`` ``flag_variables``(``adlb``, ``TRUE``, ``TRUE``, ``TRUE``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` ``dplyr``::`[`rename`](https://dplyr.tidyverse.org/reference/rename.html)`(``WGRHIVFL ``=`` ``"new_var"``)`\
`  ``adlb`` ``<-`` ``flag_variables``(``adlb``, ``TRUE``, ``FALSE``, ``TRUE``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` ``dplyr``::`[`rename`](https://dplyr.tidyverse.org/reference/rename.html)`(``WGRLOVFL ``=`` ``"new_var"``)`\
\
`  ``tmc_ex_adlb`` ``<-`` ``adlb`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(`\
`    ANL01FL ``=`` `[`ifelse`](https://rdrr.io/r/base/ifelse.html)`(`\
`      ``(``.data``$``ABLFL`` ``==`` ``"Y"`` ``|`` ``(``.data``$``WORS01FL`` ``==`` ``"Y"`` ``&`` `[`is.na`](https://rdrr.io/r/base/NA.html)`(``.data``$``DTYPE``)``)``)`` ``&`\
`        ``(``.data``$``AVISIT`` ``!=`` ``"SCREENING"``)``,`\
`      ``"Y"``,`\
`      ``""`\
`    ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `[`with_label`](https://pharmaverse.github.io/formatters/latest-tag/reference/with_label.html)`(``"Analysis Flag 01 Baseline Post-Baseline"``)``,`\
`    PARAM ``=`` `[`as.factor`](https://rdrr.io/r/base/factor.html)`(``.data``$``PARAM``)`\
`  ``)`\
\
`  ``tmc_ex_adlb`` ``<-`` ``tmc_ex_adlb`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    `[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(``.data``$``USUBJID``, ``.data``$``PARAMCD``, ``.data``$``BASETYPE``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``BNRIND ``=`` ``.data``$``ANRIND``[``.data``$``ABLFL`` ``==`` ``"Y"``]``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    `[`ungroup`](https://dplyr.tidyverse.org/reference/group_by.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``ADY ``=`` `[`ceiling`](https://rdrr.io/r/base/Round.html)`(`[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(`[`difftime`](https://rdrr.io/r/base/difftime.html)`(``.data``$``ADTM``, ``.data``$``TRTSDTM``, units ``=`` ``"days"``)``)``)``)`\
\
`  ``tmc_ex_adlb``$``PARAMCD`` ``<-`` `[`as.factor`](https://rdrr.io/r/base/factor.html)`(``tmc_ex_adlb``$``PARAMCD``)`\
`  ``tmc_ex_adlb`` ``<-`` ``tmc_ex_adlb`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``CHG ``=`` ``.data``$``AVAL`` ``-`` ``.data``$``BASE``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``PCHG ``=`` ``100`` ``*`` ``(``.data``$``CHG`` ``/`` ``.data``$``BASE``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    `[`col_relabel`](https://insightsengineering.github.io/teal.data/latest-tag/reference/col_labels.html)`(`\
`      LBCAT ``=`` ``"Category for Lab Test"``,`\
`      ATOXDSCL ``=`` ``"Analysis Toxicity Description Low"``,`\
`      ATOXDSCH ``=`` ``"Analysis Toxicity Description High"``,`\
`      WGRHIFL ``=`` ``"Worst High Grade per Patient"``,`\
`      WGRLOFL ``=`` ``"Worst Low Grade per Patient"``,`\
`      WGRHIVFL ``=`` ``"Worst High Grade per Patient per Visit"``,`\
`      WGRLOVFL ``=`` ``"Worst Low Grade per Patient per Visit"`\
`    ``)`\
\
`  ``i_lbls`` ``<-`` `[`sapply`](https://rdrr.io/r/base/lapply.html)`(`\
`    `[`names`](https://rdrr.io/r/base/names.html)`(`[`col_labels`](https://insightsengineering.github.io/teal.data/latest-tag/reference/col_labels.html)`(``tmc_ex_adlb``)``[`[`is.na`](https://rdrr.io/r/base/NA.html)`(`[`col_labels`](https://insightsengineering.github.io/teal.data/latest-tag/reference/col_labels.html)`(``tmc_ex_adlb``)``)``]``)``, ``function``(``x``)`` `[`which`](https://rdrr.io/r/base/which.html)`(`[`names`](https://rdrr.io/r/base/names.html)`(``common_var_labels``)`` ``==`` ``x``)`\
`  ``)`\
`  `[`col_labels`](https://insightsengineering.github.io/teal.data/latest-tag/reference/col_labels.html)`(``tmc_ex_adlb``[`[`names`](https://rdrr.io/r/base/names.html)`(``i_lbls``)``]``)`` ``<-`` ``common_var_labels``[``i_lbls``]`\
\
`  `[`save`](https://rdrr.io/r/base/save.html)`(``tmc_ex_adlb``, file ``=`` ``"data/tmc_ex_adlb.rda"``, compress ``=`` ``"xz"``)`\
`}`

## `ADMH`

\
`generate_admh`` ``<-`` ``function``(``adsl`` ``=`` ``tmc_ex_adsl``,`\
`                          ``max_n_mhs`` ``=`` ``10L``)`` ``{`\
`  `[`set.seed`](https://rdrr.io/r/base/Random.html)`(``1``)`\
`  ``lookup_mh`` ``<-`` ``tibble``::`[`tribble`](https://tibble.tidyverse.org/reference/tribble.html)`(`\
`    ``~``MHBODSYS``, ``~``MHDECOD``, ``~``MHSOC``,`\
`    ``"cl A"``, ``"trm A_1/2"``, ``"cl A"``,`\
`    ``"cl A"``, ``"trm A_2/2"``, ``"cl A"``,`\
`    ``"cl B"``, ``"trm B_1/3"``, ``"cl B"``,`\
`    ``"cl B"``, ``"trm B_2/3"``, ``"cl B"``,`\
`    ``"cl B"``, ``"trm B_3/3"``, ``"cl B"``,`\
`    ``"cl C"``, ``"trm C_1/2"``, ``"cl C"``,`\
`    ``"cl C"``, ``"trm C_2/2"``, ``"cl C"``,`\
`    ``"cl D"``, ``"trm D_1/3"``, ``"cl D"``,`\
`    ``"cl D"``, ``"trm D_2/3"``, ``"cl D"``,`\
`    ``"cl D"``, ``"trm D_3/3"``, ``"cl D"`\
`  ``)`\
\
`  ``admh`` ``<-`` `[`Map`](https://rdrr.io/r/base/funprog.html)`(`\
`    ``function``(``id``, ``sid``)`` ``{`\
`      ``n_mhs`` ``<-`` `[`sample`](https://rdrr.io/r/base/sample.html)`(``0``:``max_n_mhs``, ``1``)`\
`      ``i`` ``<-`` `[`sample`](https://rdrr.io/r/base/sample.html)`(`[`seq_len`](https://rdrr.io/r/base/seq.html)`(`[`nrow`](https://rdrr.io/r/base/nrow.html)`(``lookup_mh``)``)``, ``n_mhs``, ``TRUE``)`\
`      ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(`\
`        ``lookup_mh``[``i``, ``]``,`\
`        USUBJID ``=`` ``id``,`\
`        STUDYID ``=`` ``sid`\
`      ``)`\
`    ``}``,`\
`    ``adsl``$``USUBJID``,`\
`    ``adsl``$``STUDYID`\
`  ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    `[`Reduce`](https://rdrr.io/r/base/funprog.html)`(``rbind``, ``.``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``` `[` ```(`[`c`](https://rdrr.io/r/base/c.html)`(``4``, ``5``, ``1``, ``2``, ``3``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``MHTERM ``=`` ``.data``$``MHDECOD`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `[`with_label`](https://pharmaverse.github.io/formatters/latest-tag/reference/with_label.html)`(``"Reported Term for the Medical History"``)``)`\
\
`  ``admh`` ``<-`` ``dplyr``::`[`inner_join`](https://dplyr.tidyverse.org/reference/mutate-joins.html)`(`\
`    ``adsl``,`\
`    ``admh``,`\
`    by ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"STUDYID"``, ``"USUBJID"``)``,`\
`    multiple ``=`` ``"all"`\
`  ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`rowwise`](https://dplyr.tidyverse.org/reference/rowwise.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``TRTENDT ``=`` ``lubridate``::`[`date`](https://lubridate.tidyverse.org/reference/date.html)`(``dplyr``::`[`case_when`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)`(`\
`      `[`is.na`](https://rdrr.io/r/base/NA.html)`(``TRTEDTM``)`` ``~`` ``lubridate``::`[`floor_date`](https://lubridate.tidyverse.org/reference/round_date.html)`(``lubridate``::`[`date`](https://lubridate.tidyverse.org/reference/date.html)`(``TRTSDTM``)`` ``+`` ``study_duration_secs``, unit ``=`` ``"day"``)``,`\
`      ``TRUE`` ``~`` ``TRTEDTM`\
`    ``)``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``ASTDTM ``=`` `[`sample`](https://rdrr.io/r/base/sample.html)`(`\
`      `[`seq`](https://rdrr.io/r/base/seq.html)`(``lubridate``::`[`as_datetime`](https://lubridate.tidyverse.org/reference/as_date.html)`(``TRTSDTM``)``, ``lubridate``::`[`as_datetime`](https://lubridate.tidyverse.org/reference/as_date.html)`(``TRTENDT``)``, by ``=`` ``"day"``)``,`\
`      size ``=`` ``1`\
`    ``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    `[`select`](https://dplyr.tidyverse.org/reference/select.html)`(``-``TRTENDT``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`ungroup`](https://dplyr.tidyverse.org/reference/group_by.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`arrange`](https://dplyr.tidyverse.org/reference/arrange.html)`(``.data``$``STUDYID``, ``.data``$``USUBJID``, ``.data``$``ASTDTM``, ``.data``$``MHTERM``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``MHDISTAT ``=`` `[`sample`](https://rdrr.io/r/base/sample.html)`(`\
`      x ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"Resolved"``, ``"Ongoing with treatment"``, ``"Ongoing without treatment"``)``,`\
`      prob ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0.6``, ``0.2``, ``0.2``)``,`\
`      size ``=`` ``dplyr``::`[`n`](https://dplyr.tidyverse.org/reference/context.html)`(``)``,`\
`      replace ``=`` ``TRUE`\
`    ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `[`with_label`](https://pharmaverse.github.io/formatters/latest-tag/reference/with_label.html)`(``"Status of Disease"``)``)`\
\
`  ``tmc_ex_admh`` ``<-`` ``admh`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(``.data``$``USUBJID``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``MHSEQ ``=`` `[`seq_len`](https://rdrr.io/r/base/seq.html)`(``dplyr``::`[`n`](https://dplyr.tidyverse.org/reference/context.html)`(``)``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`ungroup`](https://dplyr.tidyverse.org/reference/group_by.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`arrange`](https://dplyr.tidyverse.org/reference/arrange.html)`(``.data``$``STUDYID``, ``.data``$``USUBJID``, ``.data``$``ASTDTM``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    `[`col_relabel`](https://insightsengineering.github.io/teal.data/latest-tag/reference/col_labels.html)`(`\
`      MHBODSYS ``=`` ``"Body System or Organ Class"``,`\
`      MHDECOD ``=`` ``"Dictionary-Derived Term"``,`\
`      MHSOC ``=`` ``"Primary System Organ Class"``,`\
`      MHSEQ ``=`` ``"Sponsor-Defined Identifier"`\
`    ``)`\
\
`  ``i_lbls`` ``<-`` `[`sapply`](https://rdrr.io/r/base/lapply.html)`(`\
`    `[`names`](https://rdrr.io/r/base/names.html)`(`[`col_labels`](https://insightsengineering.github.io/teal.data/latest-tag/reference/col_labels.html)`(``tmc_ex_admh``)``[`[`is.na`](https://rdrr.io/r/base/NA.html)`(`[`col_labels`](https://insightsengineering.github.io/teal.data/latest-tag/reference/col_labels.html)`(``tmc_ex_admh``)``)``]``)``, ``function``(``x``)`` `[`which`](https://rdrr.io/r/base/which.html)`(`[`names`](https://rdrr.io/r/base/names.html)`(``common_var_labels``)`` ``==`` ``x``)`\
`  ``)`\
`  `[`col_labels`](https://insightsengineering.github.io/teal.data/latest-tag/reference/col_labels.html)`(``tmc_ex_admh``[`[`names`](https://rdrr.io/r/base/names.html)`(``i_lbls``)``]``)`` ``<-`` ``common_var_labels``[``i_lbls``]`\
\
`  `[`save`](https://rdrr.io/r/base/save.html)`(``tmc_ex_admh``, file ``=`` ``"data/tmc_ex_admh.rda"``, compress ``=`` ``"xz"``)`\
`}`

## `ADQS`

\
`generate_adqs`` ``<-`` ``function``(``adsl`` ``=`` ``tmc_ex_adsl``,`\
`                          ``n_assessments`` ``=`` ``5L``,`\
`                          ``n_days`` ``=`` ``5L``)`` ``{`\
`  `[`set.seed`](https://rdrr.io/r/base/Random.html)`(``1``)`\
`  ``param`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(`\
`    ``"BFI All Questions"``,`\
`    ``"Fatigue Interference"``,`\
`    ``"Function/Well-Being (GF1,GF3,GF7)"``,`\
`    ``"Treatment Side Effects (GP2,C5,GP5)"``,`\
`    ``"FKSI-19 All Questions"`\
`  ``)`\
`  ``paramcd`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``"BFIALL"``, ``"FATIGI"``, ``"FKSI-FWB"``, ``"FKSI-TSE"``, ``"FKSIALL"``)`\
`  ``visit_format`` ``<-`` ``"WEEK"`\
\
`  ``param_init_list`` ``<-`` ``relvar_init``(``param``, ``paramcd``)`\
\
`  ``adqs`` ``<-`` `[`expand.grid`](https://rdrr.io/r/base/expand.grid.html)`(`\
`    STUDYID ``=`` `[`unique`](https://rdrr.io/r/base/unique.html)`(``adsl``$``STUDYID``)``,`\
`    USUBJID ``=`` ``adsl``$``USUBJID``,`\
`    PARAM ``=`` ``param_init_list``$``relvar1``,`\
`    AVISIT ``=`` ``visit_schedule``(``visit_format ``=`` ``visit_format``, n_assessments ``=`` ``n_assessments``, n_days ``=`` ``n_days``)``,`\
`    stringsAsFactors ``=`` ``FALSE`\
`  ``)`\
\
`  ``adqs`` ``<-`` ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(`\
`    ``adqs``,`\
`    AVISITN ``=`` ``dplyr``::`[`case_when`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)`(`\
`      ``AVISIT`` ``==`` ``"SCREENING"`` ``~`` ``-``1``,`\
`      ``AVISIT`` ``==`` ``"BASELINE"`` ``~`` ``0``,`\
`      ``(`[`grepl`](https://rdrr.io/r/base/grep.html)`(``"^WEEK"``, ``AVISIT``)`` ``|`` `[`grepl`](https://rdrr.io/r/base/grep.html)`(``"^CYCLE"``, ``AVISIT``)``)`` ``~`` `[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(``AVISIT``)`` ``-`` ``2``,`\
`      ``TRUE`` ``~`` ``NA_real_`\
`    ``)`\
`  ``)`\
\
`  ``adqs``$``PARAMCD`` ``<-`` ``rel_var``(``df ``=`` ``adqs``, var_name ``=`` ``"PARAMCD"``, var_values ``=`` ``param_init_list``$``relvar2``, related_var ``=`` ``"PARAM"``)`\
`  ``adqs``$``AVAL`` ``<-`` ``stats``::`[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(`[`nrow`](https://rdrr.io/r/base/nrow.html)`(``adqs``)``, mean ``=`` ``50``, sd ``=`` ``8``)`` ``+`` ``adqs``$``AVISITN`` ``*`` ``stats``::`[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(`[`nrow`](https://rdrr.io/r/base/nrow.html)`(``adqs``)``, mean ``=`` ``5``, sd ``=`` ``2``)`\
`  ``adqs`` ``<-`` ``adqs``[`[`order`](https://rdrr.io/r/base/order.html)`(``adqs``$``STUDYID``, ``adqs``$``USUBJID``, ``adqs``$``PARAMCD``, ``adqs``$``AVISITN``)``, ``]`\
\
`  ``adqs`` ``<-`` `[`Reduce`](https://rdrr.io/r/base/funprog.html)`(`\
`    ``rbind``,`\
`    `[`lapply`](https://rdrr.io/r/base/lapply.html)`(`\
`      `[`split`](https://rdrr.io/r/base/split.html)`(``adqs``, ``adqs``$``USUBJID``)``,`\
`      ``function``(``x``)`` ``{`\
`        ``x``$``STUDYID`` ``<-`` ``adsl``$``STUDYID``[`[`which`](https://rdrr.io/r/base/which.html)`(``adsl``$``USUBJID`` ``==`` ``x``$``USUBJID``[``1``]``)``]`\
`        ``x``$``ABLFL2`` ``<-`` `[`ifelse`](https://rdrr.io/r/base/ifelse.html)`(``x``$``AVISIT`` ``==`` ``"SCREENING"``, ``"Y"``, ``""``)`\
`        ``x``$``ABLFL`` ``<-`` `[`ifelse`](https://rdrr.io/r/base/ifelse.html)`(`\
`          `[`toupper`](https://rdrr.io/r/base/chartr.html)`(``visit_format``)`` ``==`` ``"WEEK"`` ``&`` ``x``$``AVISIT`` ``==`` ``"BASELINE"``,`\
`          ``"Y"``,`\
`          `[`ifelse`](https://rdrr.io/r/base/ifelse.html)`(`\
`            `[`toupper`](https://rdrr.io/r/base/chartr.html)`(``visit_format``)`` ``==`` ``"CYCLE"`` ``&`` ``x``$``AVISIT`` ``==`` ``"CYCLE 1 DAY 1"``,`\
`            ``"Y"``,`\
`            ``""`\
`          ``)`\
`        ``)`\
`        ``x`\
`      ``}`\
`    ``)`\
`  ``)`\
\
`  ``adqs``$``BASE`` ``<-`` `[`ifelse`](https://rdrr.io/r/base/ifelse.html)`(``adqs``$``ABLFL2`` ``!=`` ``"Y"``, ``retain``(``adqs``, ``adqs``$``AVAL``, ``adqs``$``ABLFL`` ``==`` ``"Y"``)``, ``NA``)`\
`  ``adqs`` ``<-`` ``adqs`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``CHG ``=`` ``.data``$``AVAL`` ``-`` ``.data``$``BASE``)`\
\
`  ``adqs`` ``<-`` ``dplyr``::`[`inner_join`](https://dplyr.tidyverse.org/reference/mutate-joins.html)`(`\
`    ``adsl``,`\
`    ``adqs``,`\
`    by ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"STUDYID"``, ``"USUBJID"``)``,`\
`    multiple ``=`` ``"all"`\
`  ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`rowwise`](https://dplyr.tidyverse.org/reference/rowwise.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``TRTENDT ``=`` ``lubridate``::`[`date`](https://lubridate.tidyverse.org/reference/date.html)`(``dplyr``::`[`case_when`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)`(`\
`      `[`is.na`](https://rdrr.io/r/base/NA.html)`(``TRTEDTM``)`` ``~`` ``lubridate``::`[`floor_date`](https://lubridate.tidyverse.org/reference/round_date.html)`(``lubridate``::`[`date`](https://lubridate.tidyverse.org/reference/date.html)`(``TRTSDTM``)`` ``+`` ``study_duration_secs``, unit ``=`` ``"day"``)``,`\
`      ``TRUE`` ``~`` ``TRTEDTM`\
`    ``)``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    `[`ungroup`](https://dplyr.tidyverse.org/reference/group_by.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    `[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(``USUBJID``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    `[`arrange`](https://dplyr.tidyverse.org/reference/arrange.html)`(``USUBJID``, ``AVISITN``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``ADTM ``=`` `[`rep`](https://rdrr.io/r/base/rep.html)`(`\
`      `[`sort`](https://rdrr.io/r/base/sort.html)`(`[`sample`](https://rdrr.io/r/base/sample.html)`(`\
`        `[`seq`](https://rdrr.io/r/base/seq.html)`(``lubridate``::`[`as_datetime`](https://lubridate.tidyverse.org/reference/as_date.html)`(``TRTSDTM``[``1``]``)``, ``lubridate``::`[`as_datetime`](https://lubridate.tidyverse.org/reference/as_date.html)`(``TRTENDT``[``1``]``)``, by ``=`` ``"day"``)``,`\
`        size ``=`` `[`nlevels`](https://rdrr.io/r/base/nlevels.html)`(``AVISIT``)`\
`      ``)``)``,`\
`      each ``=`` `[`n`](https://dplyr.tidyverse.org/reference/context.html)`(``)`` ``/`` `[`nlevels`](https://rdrr.io/r/base/nlevels.html)`(``AVISIT``)`\
`    ``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`ungroup`](https://dplyr.tidyverse.org/reference/group_by.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`select`](https://dplyr.tidyverse.org/reference/select.html)`(``-``TRTENDT``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`arrange`](https://dplyr.tidyverse.org/reference/arrange.html)`(``.data``$``STUDYID``, ``.data``$``USUBJID``, ``.data``$``ADTM``)`\
\
`  ``tmc_ex_adqs`` ``<-`` ``adqs`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(``.data``$``USUBJID``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`ungroup`](https://dplyr.tidyverse.org/reference/group_by.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`arrange`](https://dplyr.tidyverse.org/reference/arrange.html)`(`\
`      ``.data``$``STUDYID``,`\
`      ``.data``$``USUBJID``,`\
`      ``.data``$``PARAMCD``,`\
`      ``.data``$``AVISITN``,`\
`      ``.data``$``ADTM`\
`    ``)`\
\
`  ``i_lbls`` ``<-`` `[`sapply`](https://rdrr.io/r/base/lapply.html)`(`\
`    `[`names`](https://rdrr.io/r/base/names.html)`(`[`col_labels`](https://insightsengineering.github.io/teal.data/latest-tag/reference/col_labels.html)`(``tmc_ex_adqs``)``[`[`is.na`](https://rdrr.io/r/base/NA.html)`(`[`col_labels`](https://insightsengineering.github.io/teal.data/latest-tag/reference/col_labels.html)`(``tmc_ex_adqs``)``)``]``)``, ``function``(``x``)`` `[`which`](https://rdrr.io/r/base/which.html)`(`[`names`](https://rdrr.io/r/base/names.html)`(``common_var_labels``)`` ``==`` ``x``)`\
`  ``)`\
`  `[`col_labels`](https://insightsengineering.github.io/teal.data/latest-tag/reference/col_labels.html)`(``tmc_ex_adqs``[`[`names`](https://rdrr.io/r/base/names.html)`(``i_lbls``)``]``)`` ``<-`` ``common_var_labels``[``i_lbls``]`\
\
`  `[`save`](https://rdrr.io/r/base/save.html)`(``tmc_ex_adqs``, file ``=`` ``"data/tmc_ex_adqs.rda"``, compress ``=`` ``"xz"``)`\
`}`

## `ADRS`

\
`generate_adrs`` ``<-`` ``function``(``adsl`` ``=`` ``tmc_ex_adsl``)`` ``{`\
`  `[`set.seed`](https://rdrr.io/r/base/Random.html)`(``1``)`\
`  ``param_codes`` ``<-`` ``stats``::`[`setNames`](https://rdrr.io/r/stats/setNames.html)`(``1``:``5``, `[`c`](https://rdrr.io/r/base/c.html)`(``"CR"``, ``"PR"``, ``"SD"``, ``"PD"``, ``"NE"``)``)`\
\
`  ``lookup_ars`` ``<-`` `[`expand.grid`](https://rdrr.io/r/base/expand.grid.html)`(`\
`    ARM ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"A: Drug X"``, ``"B: Placebo"``, ``"C: Combination"``)``,`\
`    AVALC ``=`` `[`names`](https://rdrr.io/r/base/names.html)`(``param_codes``)`\
`  ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(`\
`    AVAL ``=`` ``param_codes``[``.data``$``AVALC``]``,`\
`    p_scr ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(`[`rep`](https://rdrr.io/r/base/rep.html)`(``0``, ``3``)``, `[`rep`](https://rdrr.io/r/base/rep.html)`(``0``, ``3``)``, `[`c`](https://rdrr.io/r/base/c.html)`(``1``, ``1``, ``1``)``, `[`c`](https://rdrr.io/r/base/c.html)`(``0``, ``0``, ``0``)``, `[`c`](https://rdrr.io/r/base/c.html)`(``0``, ``0``, ``0``)``)``,`\
`    p_bsl ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(`[`rep`](https://rdrr.io/r/base/rep.html)`(``0``, ``3``)``, `[`rep`](https://rdrr.io/r/base/rep.html)`(``0``, ``3``)``, `[`c`](https://rdrr.io/r/base/c.html)`(``1``, ``1``, ``1``)``, `[`c`](https://rdrr.io/r/base/c.html)`(``0``, ``0``, ``0``)``, `[`c`](https://rdrr.io/r/base/c.html)`(``0``, ``0``, ``0``)``)``,`\
`    p_cycle ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``.35``, ``.25``, ``.4``)``, `[`c`](https://rdrr.io/r/base/c.html)`(``.30``, ``.20``, ``.20``)``, `[`c`](https://rdrr.io/r/base/c.html)`(``.2``, ``.25``, ``.3``)``, `[`c`](https://rdrr.io/r/base/c.html)`(``.14``, ``0.20``, ``0.18``)``, `[`c`](https://rdrr.io/r/base/c.html)`(``.01``, ``0.1``, ``0.02``)``)``,`\
`    p_eoi ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``.35``, ``.25``, ``.4``)``, `[`c`](https://rdrr.io/r/base/c.html)`(``.30``, ``.20``, ``.20``)``, `[`c`](https://rdrr.io/r/base/c.html)`(``.2``, ``.25``, ``.3``)``, `[`c`](https://rdrr.io/r/base/c.html)`(``.14``, ``0.20``, ``0.18``)``, `[`c`](https://rdrr.io/r/base/c.html)`(``.01``, ``0.1``, ``0.02``)``)``,`\
`    p_fu ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``.25``, ``.15``, ``.3``)``, `[`c`](https://rdrr.io/r/base/c.html)`(``.15``, ``.05``, ``.25``)``, `[`c`](https://rdrr.io/r/base/c.html)`(``.3``, ``.25``, ``.3``)``, `[`c`](https://rdrr.io/r/base/c.html)`(``.3``, ``.55``, ``.25``)``, `[`rep`](https://rdrr.io/r/base/rep.html)`(``0``, ``3``)``)`\
`  ``)`\
\
`  ``adrs`` ``<-`` `[`split`](https://rdrr.io/r/base/split.html)`(``adsl``, ``adsl``$``USUBJID``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``function``(``pinfo``)`` ``{`\
`      ``probs`` ``<-`` ``dplyr``::`[`filter`](https://dplyr.tidyverse.org/reference/filter.html)`(``lookup_ars``, ``.data``$``ARM`` ``==`` `[`as.character`](https://rdrr.io/r/base/character.html)`(``pinfo``$``ACTARM``)``)`\
`      ``# screening`\
`      ``rsp_screen`` ``<-`` `[`sample`](https://rdrr.io/r/base/sample.html)`(``probs``$``AVALC``, ``1``, prob ``=`` ``probs``$``p_scr``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `[`as.character`](https://rdrr.io/r/base/character.html)`(``)`\
`      ``# baseline`\
`      ``rsp_bsl`` ``<-`` `[`sample`](https://rdrr.io/r/base/sample.html)`(``probs``$``AVALC``, ``1``, prob ``=`` ``probs``$``p_bsl``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `[`as.character`](https://rdrr.io/r/base/character.html)`(``)`\
`      ``# cycle`\
`      ``rsp_c2d1`` ``<-`` `[`sample`](https://rdrr.io/r/base/sample.html)`(``probs``$``AVALC``, ``1``, prob ``=`` ``probs``$``p_cycle``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `[`as.character`](https://rdrr.io/r/base/character.html)`(``)`\
`      ``rsp_c4d1`` ``<-`` `[`sample`](https://rdrr.io/r/base/sample.html)`(``probs``$``AVALC``, ``1``, prob ``=`` ``probs``$``p_cycle``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `[`as.character`](https://rdrr.io/r/base/character.html)`(``)`\
`      ``# end of induction`\
`      ``rsp_eoi`` ``<-`` `[`sample`](https://rdrr.io/r/base/sample.html)`(``probs``$``AVALC``, ``1``, prob ``=`` ``probs``$``p_eoi``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `[`as.character`](https://rdrr.io/r/base/character.html)`(``)`\
`      ``# follow up`\
`      ``rsp_fu`` ``<-`` `[`sample`](https://rdrr.io/r/base/sample.html)`(``probs``$``AVALC``, ``1``, prob ``=`` ``probs``$``p_fu``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `[`as.character`](https://rdrr.io/r/base/character.html)`(``)`\
\
`      ``best_rsp`` ``<-`` `[`min`](https://rdrr.io/r/base/Extremes.html)`(``param_codes``[`[`c`](https://rdrr.io/r/base/c.html)`(``rsp_screen``, ``rsp_bsl``, ``rsp_eoi``, ``rsp_fu``, ``rsp_c2d1``, ``rsp_c4d1``)``]``)`\
`      ``best_rsp_i`` ``<-`` `[`which.min`](https://rdrr.io/r/base/which.min.html)`(``param_codes``[`[`c`](https://rdrr.io/r/base/c.html)`(``rsp_screen``, ``rsp_bsl``, ``rsp_eoi``, ``rsp_fu``, ``rsp_c2d1``, ``rsp_c4d1``)``]``)`\
\
`      ``avisit`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``"SCREENING"``, ``"BASELINE"``, ``"CYCLE 2 DAY 1"``, ``"CYCLE 4 DAY 1"``, ``"END OF INDUCTION"``, ``"FOLLOW UP"``)`\
\
`      ``# meaningful date information`\
`      ``TRTSTDT`` ``<-`` ``lubridate``::`[`date`](https://lubridate.tidyverse.org/reference/date.html)`(``pinfo``$``TRTSDTM``)`` `\
`      ``TRTENDT`` ``<-`` ``lubridate``::`[`date`](https://lubridate.tidyverse.org/reference/date.html)`(``dplyr``::`[`if_else`](https://dplyr.tidyverse.org/reference/if_else.html)`(`` `\
`        ``!`[`is.na`](https://rdrr.io/r/base/NA.html)`(``pinfo``$``TRTEDTM``)``, ``pinfo``$``TRTEDTM``,`\
`        ``lubridate``::`[`floor_date`](https://lubridate.tidyverse.org/reference/round_date.html)`(``TRTSTDT`` ``+`` ``study_duration_secs``, unit ``=`` ``"day"``)`\
`      ``)``)`\
`      ``scr_date`` ``<-`` ``TRTSTDT`` ``-`` ``lubridate``::`[`days`](https://lubridate.tidyverse.org/reference/period.html)`(``100``)`\
`      ``bs_date`` ``<-`` ``TRTSTDT`\
`      ``flu_date`` ``<-`` `[`sample`](https://rdrr.io/r/base/sample.html)`(`[`seq`](https://rdrr.io/r/base/seq.html)`(``lubridate``::`[`as_datetime`](https://lubridate.tidyverse.org/reference/as_date.html)`(``TRTSTDT``)``, ``lubridate``::`[`as_datetime`](https://lubridate.tidyverse.org/reference/as_date.html)`(``TRTENDT``)``, by ``=`` ``"day"``)``, size ``=`` ``1``)`\
`      ``eoi_date`` ``<-`` `[`sample`](https://rdrr.io/r/base/sample.html)`(`[`seq`](https://rdrr.io/r/base/seq.html)`(``lubridate``::`[`as_datetime`](https://lubridate.tidyverse.org/reference/as_date.html)`(``TRTSTDT``)``, ``lubridate``::`[`as_datetime`](https://lubridate.tidyverse.org/reference/as_date.html)`(``TRTENDT``)``, by ``=`` ``"day"``)``, size ``=`` ``1``)`\
`      ``c2d1_date`` ``<-`` `[`sample`](https://rdrr.io/r/base/sample.html)`(`[`seq`](https://rdrr.io/r/base/seq.html)`(``lubridate``::`[`as_datetime`](https://lubridate.tidyverse.org/reference/as_date.html)`(``TRTSTDT``)``, ``lubridate``::`[`as_datetime`](https://lubridate.tidyverse.org/reference/as_date.html)`(``TRTENDT``)``, by ``=`` ``"day"``)``, size ``=`` ``1``)`\
`      ``c4d1_date`` ``<-`` `[`min`](https://rdrr.io/r/base/Extremes.html)`(``lubridate``::`[`date`](https://lubridate.tidyverse.org/reference/date.html)`(``c2d1_date`` ``+`` ``lubridate``::`[`days`](https://lubridate.tidyverse.org/reference/period.html)`(``60``)``)``, ``TRTENDT``)`\
\
`      ``tibble``::`[`tibble`](https://tibble.tidyverse.org/reference/tibble.html)`(`\
`        STUDYID ``=`` ``pinfo``$``STUDYID``,`\
`        USUBJID ``=`` ``pinfo``$``USUBJID``,`\
`        PARAMCD ``=`` `[`as.factor`](https://rdrr.io/r/base/factor.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(`[`rep`](https://rdrr.io/r/base/rep.html)`(``"OVRINV"``, ``6``)``, ``"BESRSPI"``, ``"INVET"``)``)``,`\
`        PARAM ``=`` `[`as.factor`](https://rdrr.io/r/base/factor.html)`(``dplyr``::`[`recode`](https://dplyr.tidyverse.org/reference/recode.html)`(`\
`          ``.data``$``PARAMCD``,`\
`          OVRINV ``=`` ``"Overall Response by Investigator - by visit"``,`\
`          OVRSPI ``=`` ``"Best Overall Response by Investigator (no confirmation required)"``,`\
`          BESRSPI ``=`` ``"Best Confirmed Overall Response by Investigator"``,`\
`          INVET ``=`` ``"Investigator End Of Induction Response"`\
`        ``)``)``,`\
`        AVALC ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(`\
`          ``rsp_screen``, ``rsp_bsl``, ``rsp_c2d1``, ``rsp_c4d1``, ``rsp_eoi``, ``rsp_fu``,`\
`          `[`names`](https://rdrr.io/r/base/names.html)`(``param_codes``)``[``best_rsp``]``,`\
`          ``rsp_eoi`\
`        ``)``,`\
`        AVAL ``=`` ``param_codes``[``.data``$``AVALC``]``,`\
`        AVISIT ``=`` `[`factor`](https://rdrr.io/r/base/factor.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``avisit``, ``avisit``[``best_rsp_i``]``, ``avisit``[``5``]``)``, levels ``=`` ``avisit``)`\
`      ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`        `[`merge`](https://rdrr.io/r/base/merge.html)`(`\
`          ``tibble``::`[`tibble`](https://tibble.tidyverse.org/reference/tibble.html)`(`\
`            AVISIT ``=`` ``avisit``,`\
`            ADTM ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``scr_date``, ``bs_date``, ``c2d1_date``, ``c4d1_date``, ``eoi_date``, ``flu_date``)``,`\
`            AVISITN ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``-``1``, ``0``, ``2``, ``4``, ``999``, ``999``)``,`\
`            TRTSDTM ``=`` ``pinfo``$``TRTSDTM`\
`          ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`            ``dplyr``::`[`select`](https://dplyr.tidyverse.org/reference/select.html)`(``-``"TRTSDTM"``)``,`\
`          by ``=`` ``"AVISIT"`\
`        ``)`\
`    ``}``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    `[`Reduce`](https://rdrr.io/r/base/funprog.html)`(``rbind``, ``.``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(`\
`      AVALC ``=`` `[`factor`](https://rdrr.io/r/base/factor.html)`(``.data``$``AVALC``, levels ``=`` `[`names`](https://rdrr.io/r/base/names.html)`(``param_codes``)``)``,`\
`      DTHFL ``=`` `[`factor`](https://rdrr.io/r/base/factor.html)`(`[`sample`](https://rdrr.io/r/base/sample.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``"Y"``, ``"N"``)``, `[`nrow`](https://rdrr.io/r/base/nrow.html)`(``.``)``, replace ``=`` ``TRUE``, prob ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``1``, ``0.8``)``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`        `[`with_label`](https://pharmaverse.github.io/formatters/latest-tag/reference/with_label.html)`(``"Death Flag"``)`\
`    ``)`\
\
`  ``# merge ADSL to be able to add RS date and study day variables`\
`  ``adrs`` ``<-`` ``dplyr``::`[`inner_join`](https://dplyr.tidyverse.org/reference/mutate-joins.html)`(`\
`    ``adsl``,`\
`    ``adrs``,`\
`    by ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"STUDYID"``, ``"USUBJID"``)``,`\
`    multiple ``=`` ``"all"`\
`  ``)`\
\
`  ``tmc_ex_adrs`` ``<-`` ``adrs`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`arrange`](https://dplyr.tidyverse.org/reference/arrange.html)`(`\
`      ``.data``$``STUDYID``,`\
`      ``.data``$``USUBJID``,`\
`      ``.data``$``PARAMCD``,`\
`      ``.data``$``AVISITN``,`\
`      ``.data``$``ADTM`\
`    ``)`\
\
`  ``i_lbls`` ``<-`` `[`sapply`](https://rdrr.io/r/base/lapply.html)`(`\
`    `[`names`](https://rdrr.io/r/base/names.html)`(`[`col_labels`](https://insightsengineering.github.io/teal.data/latest-tag/reference/col_labels.html)`(``tmc_ex_adrs``)``[`[`is.na`](https://rdrr.io/r/base/NA.html)`(`[`col_labels`](https://insightsengineering.github.io/teal.data/latest-tag/reference/col_labels.html)`(``tmc_ex_adrs``)``)``]``)``, ``function``(``x``)`` `[`which`](https://rdrr.io/r/base/which.html)`(`[`names`](https://rdrr.io/r/base/names.html)`(``common_var_labels``)`` ``==`` ``x``)`\
`  ``)`\
`  `[`col_labels`](https://insightsengineering.github.io/teal.data/latest-tag/reference/col_labels.html)`(``tmc_ex_adrs``[`[`names`](https://rdrr.io/r/base/names.html)`(``i_lbls``)``]``)`` ``<-`` ``common_var_labels``[``i_lbls``]`\
\
`  `[`save`](https://rdrr.io/r/base/save.html)`(``tmc_ex_adrs``, file ``=`` ``"data/tmc_ex_adrs.rda"``, compress ``=`` ``"xz"``)`\
`}`

## `ADTTE`

\
`generate_adtte`` ``<-`` ``function``(``adsl`` ``=`` ``tmc_ex_adsl``)`` ``{`\
`  `[`set.seed`](https://rdrr.io/r/base/Random.html)`(``1``)`\
`  ``lookup_tte`` ``<-`` ``tibble``::`[`tribble`](https://tibble.tidyverse.org/reference/tribble.html)`(`\
`    ``~``ARM``, ``~``PARAMCD``, ``~``PARAM``, ``~``LAMBDA``, ``~``CNSR_P``,`\
`    ``"ARM A"``, ``"OS"``, ``"Overall Survival"``, `[`log`](https://rdrr.io/r/base/Log.html)`(``2``)`` ``/`` ``610``, ``0.4``,`\
`    ``"ARM B"``, ``"OS"``, ``"Overall Survival"``, `[`log`](https://rdrr.io/r/base/Log.html)`(``2``)`` ``/`` ``490``, ``0.3``,`\
`    ``"ARM C"``, ``"OS"``, ``"Overall Survival"``, `[`log`](https://rdrr.io/r/base/Log.html)`(``2``)`` ``/`` ``365``, ``0.2``,`\
`    ``"ARM A"``, ``"PFS"``, ``"Progression Free Survival"``, `[`log`](https://rdrr.io/r/base/Log.html)`(``2``)`` ``/`` ``365``, ``0.4``,`\
`    ``"ARM B"``, ``"PFS"``, ``"Progression Free Survival"``, `[`log`](https://rdrr.io/r/base/Log.html)`(``2``)`` ``/`` ``305``, ``0.3``,`\
`    ``"ARM C"``, ``"PFS"``, ``"Progression Free Survival"``, `[`log`](https://rdrr.io/r/base/Log.html)`(``2``)`` ``/`` ``243``, ``0.2``,`\
`    ``"ARM A"``, ``"EFS"``, ``"Event Free Survival"``, `[`log`](https://rdrr.io/r/base/Log.html)`(``2``)`` ``/`` ``365``, ``0.4``,`\
`    ``"ARM B"``, ``"EFS"``, ``"Event Free Survival"``, `[`log`](https://rdrr.io/r/base/Log.html)`(``2``)`` ``/`` ``305``, ``0.3``,`\
`    ``"ARM C"``, ``"EFS"``, ``"Event Free Survival"``, `[`log`](https://rdrr.io/r/base/Log.html)`(``2``)`` ``/`` ``243``, ``0.2``,`\
`    ``"ARM A"``, ``"CRSD"``, ``"Duration of Confirmed Response"``, `[`log`](https://rdrr.io/r/base/Log.html)`(``2``)`` ``/`` ``305``, ``0.4``,`\
`    ``"ARM B"``, ``"CRSD"``, ``"Duration of Confirmed Response"``, `[`log`](https://rdrr.io/r/base/Log.html)`(``2``)`` ``/`` ``243``, ``0.3``,`\
`    ``"ARM C"``, ``"CRSD"``, ``"Duration of Confirmed Response"``, `[`log`](https://rdrr.io/r/base/Log.html)`(``2``)`` ``/`` ``182``, ``0.2`\
`  ``)`\
\
`  ``evntdescr_sel`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(`\
`    ``"Death"``,`\
`    ``"Disease Progression"``,`\
`    ``"Last Tumor Assessment"``,`\
`    ``"Adverse Event"``,`\
`    ``"Last Date Known To Be Alive"`\
`  ``)`\
\
`  ``cnsdtdscr_sel`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(`\
`    ``"Preferred Term"``,`\
`    ``"Clinical Cut Off"``,`\
`    ``"Completion or Discontinuation"``,`\
`    ``"End of AE Reporting Period"`\
`  ``)`\
\
`  ``adtte`` ``<-`` `[`split`](https://rdrr.io/r/base/split.html)`(``adsl``, ``adsl``$``USUBJID``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``FUN ``=`` ``function``(``pinfo``)`` ``{`\
`      ``lookup_tte`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`        ``dplyr``::`[`filter`](https://dplyr.tidyverse.org/reference/filter.html)`(``.data``$``ARM`` ``==`` `[`as.character`](https://rdrr.io/r/base/character.html)`(``pinfo``$``ACTARMCD``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`        ``dplyr``::`[`rowwise`](https://dplyr.tidyverse.org/reference/rowwise.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`        ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(`\
`          STUDYID ``=`` ``pinfo``$``STUDYID``,`\
`          USUBJID ``=`` ``pinfo``$``USUBJID``,`\
`          CNSR ``=`` `[`sample`](https://rdrr.io/r/base/sample.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``0``, ``1``)``, ``1``, prob ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``1`` ``-`` ``.data``$``CNSR_P``, ``.data``$``CNSR_P``)``)``,`\
`          AVAL ``=`` ``stats``::`[`rexp`](https://rdrr.io/r/stats/Exponential.html)`(``1``, ``.data``$``LAMBDA``)``,`\
`          AVALU ``=`` ``"DAYS"``,`\
`          EVNTDESC ``=`` ``if`` ``(``.data``$``CNSR`` ``==`` ``1``)`` ``{`\
`            `[`sample`](https://rdrr.io/r/base/sample.html)`(``evntdescr_sel``[``-`[`c`](https://rdrr.io/r/base/c.html)`(``1``:``2``)``]``, ``1``)`\
`          ``}`` ``else`` ``{`\
`            `[`ifelse`](https://rdrr.io/r/base/ifelse.html)`(``.data``$``PARAMCD`` ``==`` ``"OS"``,`\
`              `[`sample`](https://rdrr.io/r/base/sample.html)`(``evntdescr_sel``[``1``]``, ``1``)``,`\
`              `[`sample`](https://rdrr.io/r/base/sample.html)`(``evntdescr_sel``[`[`c`](https://rdrr.io/r/base/c.html)`(``1``:``2``)``]``, ``1``)`\
`            ``)`\
`          ``}`\
`        ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`        ``dplyr``::`[`select`](https://dplyr.tidyverse.org/reference/select.html)`(``-``"LAMBDA"``, ``-``"CNSR_P"``)`\
`    ``}``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    `[`Reduce`](https://rdrr.io/r/base/funprog.html)`(``rbind``, ``.``)`\
\
`  ``# merge ADSL to be able to add TTE date and study day variables`\
`  ``adtte`` ``<-`` ``dplyr``::`[`inner_join`](https://dplyr.tidyverse.org/reference/mutate-joins.html)`(`\
`    ``adsl``,`\
`    ``dplyr``::`[`select`](https://dplyr.tidyverse.org/reference/select.html)`(``adtte``, ``-``"ARM"``)``,`\
`    by ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"STUDYID"``, ``"USUBJID"``)``,`\
`    multiple ``=`` ``"all"`\
`  ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`rowwise`](https://dplyr.tidyverse.org/reference/rowwise.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``TRTENDT ``=`` ``lubridate``::`[`date`](https://lubridate.tidyverse.org/reference/date.html)`(``dplyr``::`[`case_when`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)`(`\
`      `[`is.na`](https://rdrr.io/r/base/NA.html)`(``TRTEDTM``)`` ``~`` ``lubridate``::`[`floor_date`](https://lubridate.tidyverse.org/reference/round_date.html)`(``lubridate``::`[`date`](https://lubridate.tidyverse.org/reference/date.html)`(``TRTSDTM``)`` ``+`` ``study_duration_secs``, unit ``=`` ``"day"``)``,`\
`      ``TRUE`` ``~`` ``TRTEDTM`\
`    ``)``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``ADTM ``=`` `[`sample`](https://rdrr.io/r/base/sample.html)`(`\
`      `[`seq`](https://rdrr.io/r/base/seq.html)`(``lubridate``::`[`as_datetime`](https://lubridate.tidyverse.org/reference/as_date.html)`(``TRTSDTM``)``, ``lubridate``::`[`as_datetime`](https://lubridate.tidyverse.org/reference/as_date.html)`(``TRTENDT``)``, by ``=`` ``"day"``)``,`\
`      size ``=`` ``1`\
`    ``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`select`](https://dplyr.tidyverse.org/reference/select.html)`(``-``TRTENDT``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`ungroup`](https://dplyr.tidyverse.org/reference/group_by.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`arrange`](https://dplyr.tidyverse.org/reference/arrange.html)`(``.data``$``STUDYID``, ``.data``$``USUBJID``, ``.data``$``ADTM``)`\
\
`  ``adtte`` ``<-`` ``adtte`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(``.data``$``USUBJID``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``PARAM ``=`` `[`as.factor`](https://rdrr.io/r/base/factor.html)`(``.data``$``PARAM``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``PARAMCD ``=`` `[`as.factor`](https://rdrr.io/r/base/factor.html)`(``.data``$``PARAMCD``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`ungroup`](https://dplyr.tidyverse.org/reference/group_by.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`arrange`](https://dplyr.tidyverse.org/reference/arrange.html)`(`\
`      ``.data``$``STUDYID``,`\
`      ``.data``$``USUBJID``,`\
`      ``.data``$``PARAMCD``,`\
`      ``.data``$``ADTM`\
`    ``)`\
`  ``lbls`` ``<-`` `[`col_labels`](https://insightsengineering.github.io/teal.data/latest-tag/reference/col_labels.html)`(``adtte``)`\
\
`  ``# adding adverse event counts and log follow-up time`\
`  ``tmc_ex_adtte`` ``<-`` ``dplyr``::`[`bind_rows`](https://dplyr.tidyverse.org/reference/bind_rows.html)`(`\
`    ``adtte``,`\
`    `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(``adtte`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`      ``dplyr``::`[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(``.data``$``USUBJID``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`      ``dplyr``::`[`slice_head`](https://dplyr.tidyverse.org/reference/slice.html)`(``n ``=`` ``1``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`      ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(`\
`        PARAMCD ``=`` ``"TNE"``,`\
`        PARAM ``=`` ``"Total Number of Exacerbations"``,`\
`        AVAL ``=`` ``stats``::`[`rpois`](https://rdrr.io/r/stats/Poisson.html)`(``1``, ``3``)``,`\
`        AVALU ``=`` ``"COUNT"``,`\
`        lgTMATRSK ``=`` `[`log`](https://rdrr.io/r/base/Log.html)`(``stats``::`[`rexp`](https://rdrr.io/r/stats/Exponential.html)`(``1``, rate ``=`` ``3``)``)``,`\
`        ``dplyr``::`[`across`](https://dplyr.tidyverse.org/reference/across.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``"ADTM"``, ``"EVNTDESC"``)``, ``~``NA``)`\
`      ``)``)`\
`  ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`arrange`](https://dplyr.tidyverse.org/reference/arrange.html)`(`\
`      ``.data``$``STUDYID``,`\
`      ``.data``$``USUBJID``,`\
`      ``.data``$``PARAMCD``,`\
`      ``.data``$``ADTM`\
`    ``)`\
`  `[`col_labels`](https://insightsengineering.github.io/teal.data/latest-tag/reference/col_labels.html)`(``tmc_ex_adtte``)`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``lbls``, lgTMATRSK ``=`` ``"Log Time At Risk"``)`\
\
`  ``i_lbls`` ``<-`` `[`sapply`](https://rdrr.io/r/base/lapply.html)`(`\
`    `[`names`](https://rdrr.io/r/base/names.html)`(`[`col_labels`](https://insightsengineering.github.io/teal.data/latest-tag/reference/col_labels.html)`(``tmc_ex_adtte``)``[`[`is.na`](https://rdrr.io/r/base/NA.html)`(`[`col_labels`](https://insightsengineering.github.io/teal.data/latest-tag/reference/col_labels.html)`(``tmc_ex_adtte``)``)``]``)``, ``function``(``x``)`` `[`which`](https://rdrr.io/r/base/which.html)`(`[`names`](https://rdrr.io/r/base/names.html)`(``common_var_labels``)`` ``==`` ``x``)`\
`  ``)`\
`  `[`col_labels`](https://insightsengineering.github.io/teal.data/latest-tag/reference/col_labels.html)`(``tmc_ex_adtte``[`[`names`](https://rdrr.io/r/base/names.html)`(``i_lbls``)``]``)`` ``<-`` ``common_var_labels``[``i_lbls``]`\
\
`  `[`save`](https://rdrr.io/r/base/save.html)`(``tmc_ex_adtte``, file ``=`` ``"data/tmc_ex_adtte.rda"``, compress ``=`` ``"xz"``)`\
`}`

## `ADVS`

\
`generate_advs`` ``<-`` ``function``(``adsl`` ``=`` ``tmc_ex_adsl``,`\
`                          ``n_assessments`` ``=`` ``5L``,`\
`                          ``n_days`` ``=`` ``5L``)`` ``{`\
`  `[`set.seed`](https://rdrr.io/r/base/Random.html)`(``1``)`\
`  ``param`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(`\
`    ``"Diastolic Blood Pressure"``,`\
`    ``"Pulse Rate"``,`\
`    ``"Respiratory Rate"``,`\
`    ``"Systolic Blood Pressure"``,`\
`    ``"Temperature"``, ``"Weight"`\
`  ``)`\
`  ``paramcd`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``"DIABP"``, ``"PULSE"``, ``"RESP"``, ``"SYSBP"``, ``"TEMP"``, ``"WEIGHT"``)`\
`  ``paramu`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``"Pa"``, ``"beats/min"``, ``"breaths/min"``, ``"Pa"``, ``"C"``, ``"Kg"``)`\
`  ``visit_format`` ``<-`` ``"WEEK"`\
\
`  ``param_init_list`` ``<-`` ``relvar_init``(``param``, ``paramcd``)`\
`  ``unit_init_list`` ``<-`` ``relvar_init``(``param``, ``paramu``)`\
\
`  ``advs`` ``<-`` `[`expand.grid`](https://rdrr.io/r/base/expand.grid.html)`(`\
`    STUDYID ``=`` `[`unique`](https://rdrr.io/r/base/unique.html)`(``adsl``$``STUDYID``)``,`\
`    USUBJID ``=`` ``adsl``$``USUBJID``,`\
`    PARAM ``=`` `[`as.factor`](https://rdrr.io/r/base/factor.html)`(``param_init_list``$``relvar1``)``,`\
`    AVISIT ``=`` ``visit_schedule``(``visit_format ``=`` ``visit_format``, n_assessments ``=`` ``n_assessments``)``,`\
`    stringsAsFactors ``=`` ``FALSE`\
`  ``)`\
\
`  ``advs`` ``<-`` ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(`\
`    ``advs``,`\
`    AVISITN ``=`` ``dplyr``::`[`case_when`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)`(`\
`      ``AVISIT`` ``==`` ``"SCREENING"`` ``~`` ``-``1``,`\
`      ``AVISIT`` ``==`` ``"BASELINE"`` ``~`` ``0``,`\
`      ``(`[`grepl`](https://rdrr.io/r/base/grep.html)`(``"^WEEK"``, ``AVISIT``)`` ``|`` `[`grepl`](https://rdrr.io/r/base/grep.html)`(``"^CYCLE"``, ``AVISIT``)``)`` ``~`` `[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(``AVISIT``)`` ``-`` ``2``,`\
`      ``TRUE`` ``~`` ``NA_real_`\
`    ``)`\
`  ``)`\
\
`  ``advs``$``PARAMCD`` ``<-`` `[`as.factor`](https://rdrr.io/r/base/factor.html)`(``rel_var``(`\
`    df ``=`` ``advs``,`\
`    var_name ``=`` ``"PARAMCD"``,`\
`    var_values ``=`` ``param_init_list``$``relvar2``,`\
`    related_var ``=`` ``"PARAM"`\
`  ``)``)`\
`  ``advs``$``AVALU`` ``<-`` `[`as.factor`](https://rdrr.io/r/base/factor.html)`(``rel_var``(`\
`    df ``=`` ``advs``,`\
`    var_name ``=`` ``"AVALU"``,`\
`    var_values ``=`` ``unit_init_list``$``relvar2``,`\
`    related_var ``=`` ``"PARAM"`\
`  ``)``)`\
\
`  ``advs``$``AVAL`` ``<-`` ``stats``::`[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(`[`nrow`](https://rdrr.io/r/base/nrow.html)`(``advs``)``, mean ``=`` ``50``, sd ``=`` ``8``)`\
`  ``advs`` ``<-`` ``advs``[`[`order`](https://rdrr.io/r/base/order.html)`(``advs``$``STUDYID``, ``advs``$``USUBJID``, ``advs``$``PARAMCD``, ``advs``$``AVISITN``)``, ``]`\
\
`  ``advs`` ``<-`` ``dplyr``::`[`inner_join`](https://dplyr.tidyverse.org/reference/mutate-joins.html)`(`\
`    ``adsl``,`\
`    ``advs``,`\
`    by ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"STUDYID"``, ``"USUBJID"``)``,`\
`    multiple ``=`` ``"all"`\
`  ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`rowwise`](https://dplyr.tidyverse.org/reference/rowwise.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``TRTENDT ``=`` ``lubridate``::`[`date`](https://lubridate.tidyverse.org/reference/date.html)`(``dplyr``::`[`case_when`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)`(`\
`      `[`is.na`](https://rdrr.io/r/base/NA.html)`(``TRTEDTM``)`` ``~`` ``lubridate``::`[`floor_date`](https://lubridate.tidyverse.org/reference/round_date.html)`(``lubridate``::`[`date`](https://lubridate.tidyverse.org/reference/date.html)`(``TRTSDTM``)`` ``+`` ``study_duration_secs``, unit ``=`` ``"day"``)``,`\
`      ``TRUE`` ``~`` ``TRTEDTM`\
`    ``)``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``ADTM ``=`` `[`sample`](https://rdrr.io/r/base/sample.html)`(`\
`      `[`seq`](https://rdrr.io/r/base/seq.html)`(``lubridate``::`[`as_datetime`](https://lubridate.tidyverse.org/reference/as_date.html)`(``TRTSDTM``)``, ``lubridate``::`[`as_datetime`](https://lubridate.tidyverse.org/reference/as_date.html)`(``TRTENDT``)``, by ``=`` ``"day"``)``,`\
`      size ``=`` ``1`\
`    ``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``ADY ``=`` `[`ceiling`](https://rdrr.io/r/base/Round.html)`(`[`difftime`](https://rdrr.io/r/base/difftime.html)`(``ADTM``, ``TRTSDTM``, units ``=`` ``"days"``)``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`select`](https://dplyr.tidyverse.org/reference/select.html)`(``-``TRTENDT``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`ungroup`](https://dplyr.tidyverse.org/reference/group_by.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`arrange`](https://dplyr.tidyverse.org/reference/arrange.html)`(``.data``$``STUDYID``, ``.data``$``USUBJID``, ``.data``$``ADTM``)`\
\
`  ``tmc_ex_advs`` ``<-`` ``advs`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(``.data``$``USUBJID``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`ungroup`](https://dplyr.tidyverse.org/reference/group_by.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    ``dplyr``::`[`arrange`](https://dplyr.tidyverse.org/reference/arrange.html)`(`\
`      ``.data``$``STUDYID``,`\
`      ``.data``$``USUBJID``,`\
`      ``.data``$``PARAMCD``,`\
`      ``.data``$``AVISITN``,`\
`      ``.data``$``ADTM`\
`    ``)`\
\
`  ``i_lbls`` ``<-`` `[`sapply`](https://rdrr.io/r/base/lapply.html)`(`\
`    `[`names`](https://rdrr.io/r/base/names.html)`(`[`col_labels`](https://insightsengineering.github.io/teal.data/latest-tag/reference/col_labels.html)`(``tmc_ex_advs``)``[`[`is.na`](https://rdrr.io/r/base/NA.html)`(`[`col_labels`](https://insightsengineering.github.io/teal.data/latest-tag/reference/col_labels.html)`(``tmc_ex_advs``)``)``]``)``, ``function``(``x``)`` `[`which`](https://rdrr.io/r/base/which.html)`(`[`names`](https://rdrr.io/r/base/names.html)`(``common_var_labels``)`` ``==`` ``x``)`\
`  ``)`\
`  `[`col_labels`](https://insightsengineering.github.io/teal.data/latest-tag/reference/col_labels.html)`(``tmc_ex_advs``[`[`names`](https://rdrr.io/r/base/names.html)`(``i_lbls``)``]``)`` ``<-`` ``common_var_labels``[``i_lbls``]`\
\
`  `[`save`](https://rdrr.io/r/base/save.html)`(``tmc_ex_advs``, file ``=`` ``"data/tmc_ex_advs.rda"``, compress ``=`` ``"xz"``)`\
`}`

## Generate Data

\
`# Generate & load adsl`\
`tmp_fol`` ``<-`` `[`getwd`](https://rdrr.io/r/base/getwd.html)`(``)`\
[`setwd`](https://rdrr.io/r/base/getwd.html)`(`[`dirname`](https://rdrr.io/r/base/basename.html)`(``tmp_fol``)``)`\
`generate_adsl``(``)`\
[`load`](https://rdrr.io/r/base/load.html)`(``"data/tmc_ex_adsl.rda"``)`\
\
`# Generate other datasets`\
`generate_adae``(``)`\
`generate_adaette``(``)`\
`generate_adcm``(``)`\
`generate_adeg``(``)`\
`generate_adex``(``)`\
`generate_adlb``(``)`\
`generate_admh``(``)`\
`generate_adqs``(``)`\
`generate_adrs``(``)`\
`generate_adtte``(``)`\
`generate_advs``(``)`\
\
[`setwd`](https://rdrr.io/r/base/getwd.html)`(``tmp_fol``)`
