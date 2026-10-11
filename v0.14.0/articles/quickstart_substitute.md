# Quick start: \`substitute\` for NSE

## Introduction

### 

Considering an expression, R usually evaluates it and returns its value.
Instead of focusing on the value, it is also possible to work with the
**code** which generated the **value**. This is where non standard
evaluation, or NSE, starts. The function `substitute` is an important
element of non-standard evaluation. For instance, if we consider `a`
defined as `a <- 5`, then the expression `a` returns 5, and the
`substitute(a)` returns the code to obtain the value: `a`.

This is the principle `teal` relies on to:

1.  generate expressions.
2.  return the result of the expression in the result panel of the app.
3.  return the corresponding code (or expression) with `Show R Code`.

The expression returning the displayed value must be reactive. The
information in the encoding on one hand, and the filtering panel on the
other hand modify the expression and the displayed value. As such,
`teal` needs to work both on expressions and values and relies heavily
on NSE.

The NSE is an advanced notion and mixing it with Shiny app development
is a source of difficulties such as:

- hindered coding efficiency as the Shiny app must be run in order to
  check the correct execution of the code.
- limited possibilities for testing.

As an alternative, it is possible to focus first on the NSE aspects in
plain R, and only once ready, integrate it in the Shiny App. The
following are a few practical examples demonstrating how NSE works. The
choice was made to focus on `substitute`.

## The Basics

### NSE Principle

\
`non_evaluated_expression`` ``<-`` `[`substitute`](https://rdrr.io/r/base/substitute.html)`(``expr ``=`` ``a`` ``+`` ``b``)`\
`non_evaluated_expression`\
`## a + b`\
[`eval`](https://rdrr.io/r/base/eval.html)`(``non_evaluated_expression``)`\
`## ``Error``:`\
`## ``!`` object 'b' not found`

What happened?

- `substitute` returns the code and not the value,
- it does not attempt to run the code, therefore it is possible to
  return an expression which does not make sense (yet), for instance
  involving two non defined objects.
- If the values of `a` and `b` exist, the expression can run without
  error:

\
`non_evaluated_expression`` ``<-`` `[`substitute`](https://rdrr.io/r/base/substitute.html)`(``expr ``=`` ``a`` ``+`` ``b``)`\
`a`` ``<-`` ``1`\
`b`` ``<-`` ``5`\
[`eval`](https://rdrr.io/r/base/eval.html)`(``non_evaluated_expression``)`\
`## [1] 6`

Now, the function name `substitute` is for a reason. Not only returning
the expression, it also **operates substitutions** of some terms within
a given expression.

\
`fun`` ``<-`` ``function``(``a``, ``b``)`` ``{`\
`  `[`substitute`](https://rdrr.io/r/base/substitute.html)`(``expr ``=`` ``a`` ``+`` ``b``)`\
`}`\
`non_evaluated_expression`` ``<-`` ``fun``(``5``, ``-``2``)`\
`non_evaluated_expression`\
`## 5 + -2`\
[`eval`](https://rdrr.io/r/base/eval.html)`(``non_evaluated_expression``)`\
`## [1] 3`

What happened?

- the objects `a` and `b` exist in the function environment where
  `substitute` is called.
- the terms of the expression within `substitute` were replaced by the
  values of `a` and `b`.

Indeed, before returning the expression, `substitute` verifies if `a`
and `b` don’t have any value existing in the evaluation environment. If
so, values of `a` and `b` are used in the expression.

It is also possible to use the second argument of `substitute`, `env`,
an environment (or a list) containing objects. If the expression
submitted in `substitute` has corresponding objects in `env`, the terms
within the expression will be substituted with provided values:

\
`non_evaluated_expression`` ``<-`` `[`substitute`](https://rdrr.io/r/base/substitute.html)`(`\
`  expr ``=`` ``a`` ``+`` ``b``,`\
`  env ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``a ``=`` ``5``, b ``=`` ``5``)`\
`)`\
`non_evaluated_expression`\
`## 5 + 5`\
[`eval`](https://rdrr.io/r/base/eval.html)`(``non_evaluated_expression``)`\
`## [1] 10`

What happened?

- The environment in which the values of `a` and `b` were taken from was
  directly declared within the `substitute` expression (argument `expr`)
  and the values were substituted (argument `env`).
- `substitute` returned a non-evaluated expression, use
  [`eval()`](https://rdrr.io/r/base/eval.html) to evaluate it.

With a slightly more elaborate expression:

\
`non_evaluated_expression`` ``<-`` `[`substitute`](https://rdrr.io/r/base/substitute.html)`(`\
`  expr ``=`` `[`plot`](https://rdrr.io/r/graphics/plot.default.html)`(``x ``=`` ``x``, y ``=`` `[`exp`](https://rdrr.io/r/base/Log.html)`(``x``)``, main ``=`` ``text``)``,`\
`  env ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``x ``=`` ``0``:``10``, text ``=`` ``"A graph"``)`\
`)`\
`non_evaluated_expression`\
`## plot(x = 0:10, y = exp(0:10), main = "A graph")`\
[`eval`](https://rdrr.io/r/base/eval.html)`(``non_evaluated_expression``)`

![](quickstart_substitute_files/figure-html/unnamed-chunk-6-1.svg)

Note that:

- `x` as an argument name in plot has been preserved, while `x` as an
  object has been replaced.

### Replace an object name

In formulas, character strings are not accepted, how do we execute the
substitution?

\
`# Error expected:`\
`plot_expr`` ``<-`` `[`substitute`](https://rdrr.io/r/base/substitute.html)`(`\
`  expr ``=`` `[`plot`](https://rdrr.io/r/graphics/plot.default.html)`(``y`` ``~`` ``x``, data ``=`` ``iris``, main ``=`` ``text``)``,`\
`  env ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(`\
`    x ``=`` ``Sepal.Length``,`\
`    y ``=`` ``Sepal.Width``,`\
`    text ``=`` ``"Iris, again ..."`\
`  ``)`\
`)`\
`## ``Error``:`\
`## ``!`` object 'Sepal.Length' not found`

\
`# Error expected:`\
`plot_expr`` ``<-`` `[`substitute`](https://rdrr.io/r/base/substitute.html)`(`\
`  expr ``=`` `[`plot`](https://rdrr.io/r/graphics/plot.default.html)`(``y`` ``~`` ``x``, data ``=`` ``iris``, main ``=`` ``text``)``,`\
`  env ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(`\
`    x ``=`` ``"Sepal.Length"``,`\
`    y ``=`` ``"Sepal.Width"``,`\
`    text ``=`` ``"Iris, again ..."`\
`  ``)`\
`)`\
`plot_expr`\
`## plot("Sepal.Width" ~ "Sepal.Length", data = iris, main = "Iris, again ...")`\
[`eval`](https://rdrr.io/r/base/eval.html)`(``plot_expr``)`\
`## ``Error```  in `terms.formula()`: ``\
`## ``!`` invalid term in model formula`

The object names have a specific *class* (`name`); `as.names` coerces a
character string to an object name (alternatively, `as.symbol` provides
an identical result):

\
`plot_expr`` ``<-`` `[`substitute`](https://rdrr.io/r/base/substitute.html)`(`\
`  expr ``=`` `[`plot`](https://rdrr.io/r/graphics/plot.default.html)`(``y`` ``~`` ``x``, data ``=`` ``iris``, main ``=`` ``text``)``,`\
`  env ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(`\
`    x ``=`` `[`as.name`](https://rdrr.io/r/base/name.html)`(``"Sepal.Length"``)``,`\
`    y ``=`` `[`as.symbol`](https://rdrr.io/r/base/name.html)`(``"Sepal.Width"``)``,`\
`    text ``=`` ``"Iris, again ..."`\
`  ``)`\
`)`\
`plot_expr`\
`## plot(Sepal.Width ~ Sepal.Length, data = iris, main = "Iris, again ...")`\
[`eval`](https://rdrr.io/r/base/eval.html)`(``plot_expr``)`

![](quickstart_substitute_files/figure-html/unnamed-chunk-9-1.svg)

### What about dataframe names?

Lets imagine a pipe-flavored expression, with `df` being the term
corresponding to the dataframe which should be substituted:
`df %>% plot(y ~ x, data = ., main = text)`.

The principle exposed above can work directly without addition. However,
`df` in the expression is then replaced directly by the value of the
object provided and not the expression generating the dataframe: the
pipeline is working but not humanly readable.

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`dplyr`](https://dplyr.tidyverse.org)`)`\
`## `\
`## Attaching package: 'dplyr'`\
`## The following objects are masked from 'package:stats':`\
`## `\
`##     filter, lag`\
`## The following objects are masked from 'package:base':`\
`## `\
`##     intersect, setdiff, setequal, union`\
\
`short_iris`` ``<-`` `[`head`](https://rdrr.io/r/utils/head.html)`(``iris``)`\
`plot_expr`` ``<-`` `[`substitute`](https://rdrr.io/r/base/substitute.html)`(`\
`  expr ``=`` ``df`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `[`plot`](https://rdrr.io/r/graphics/plot.default.html)`(``y`` ``~`` ``x``, data ``=`` ``.``, main ``=`` ``text``)``,`\
`  env ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(`\
`    df ``=`` ``short_iris``,`\
`    x ``=`` `[`as.name`](https://rdrr.io/r/base/name.html)`(``"Sepal.Length"``)``,`\
`    y ``=`` `[`as.symbol`](https://rdrr.io/r/base/name.html)`(``"Sepal.Width"``)``,`\
`    text ``=`` ``"Iris, again ..."`\
`  ``)`\
`)`\
[`eval`](https://rdrr.io/r/base/eval.html)`(``plot_expr``)`

![](quickstart_substitute_files/figure-html/unnamed-chunk-10-1.svg)

\
`plot_expr`\
`## list(Sepal.Length = c(5.1, 4.9, 4.7, 4.6, 5, 5.4), Sepal.Width = c(3.5, `\
`## 3, 3.2, 3.1, 3.6, 3.9), Petal.Length = c(1.4, 1.4, 1.3, 1.5, `\
`## 1.4, 1.7), Petal.Width = c(0.2, 0.2, 0.2, 0.2, 0.2, 0.4), Species = c(1L, `\
`## 1L, 1L, 1L, 1L, 1L)) %>% plot(Sepal.Width ~ Sepal.Length, data = ., `\
`##     main = "Iris, again ...")`

How can we replace the value by the expression generating this value?

That is pretty much the topic of the vignette: `substitute`.

\
`plot_expr`` ``<-`` `[`substitute`](https://rdrr.io/r/base/substitute.html)`(`\
`  expr ``=`` ``df`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `[`plot`](https://rdrr.io/r/graphics/plot.default.html)`(``y`` ``~`` ``x``, data ``=`` ``.``, main ``=`` ``text``)``,`\
`  env ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(`\
`    df ``=`` `[`substitute`](https://rdrr.io/r/base/substitute.html)`(``iris``)``,`\
`    x ``=`` `[`as.name`](https://rdrr.io/r/base/name.html)`(``"Sepal.Length"``)``,`\
`    y ``=`` `[`as.symbol`](https://rdrr.io/r/base/name.html)`(``"Sepal.Width"``)``,`\
`    text ``=`` ``"Iris, again ..."`\
`  ``)`\
`)`\
`plot_expr`\
`## iris %>% plot(Sepal.Width ~ Sepal.Length, data = ., main = "Iris, again ...")`\
[`eval`](https://rdrr.io/r/base/eval.html)`(``plot_expr``)`

![](quickstart_substitute_files/figure-html/unnamed-chunk-11-1.svg)

### In a nutshell

- `substitute` is relevant when the expression needs to be modified. It
  takes 2 arguments:
  - `expr` the expression to be (eventually) substituted.
  - `env` the environment in which potential replacement value might be
    needed.
- If the replacement value should be slightly more special like:
  - an **object name** (like in formulas e.g. `y ~ x`) then, use
    `as.name` or `as.symbol`.
  - a **data frame** name (like `iris`) then, use `substitute`.

## `rtables`

### Direct use of `substitute`

The `substitute` approach can be used with the `rtables` pipelines.

Lets prepare an example for reporting data from the LB domain. The
example is based on the template `LBT01`; the target is to report in
columns the lab test result per study arm, as values (`AVAL`) and
changes from baseline (`CHG`), per analysis visit in rows.

The data can be prepared as follows:

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`teal.modules.clinical`](https://insightsengineering.github.io/teal.modules.clinical/)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`rtables`](https://github.com/pharmaverse/rtables)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`tern`](https://pharmaverse.github.io/tern/)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`dplyr`](https://dplyr.tidyverse.org)`)`\
\
`adlb`` ``<-`` ``tmc_ex_adlb`\
`adlb_f`` ``<-`` ``adlb`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`filter`](https://dplyr.tidyverse.org/reference/filter.html)`(`\
`    ``PARAM`` ``==`` ``"Alanine Aminotransferase Measurement"`` ``&`\
`      ``ARMCD`` `[`%in%`](https://rdrr.io/r/base/match.html)` `[`c`](https://rdrr.io/r/base/c.html)`(``"ARM A"``, ``"ARM B"``)`` ``&`` ``AVISIT`` ``==`` ``"WEEK 1 DAY 8"`\
`  ``)`

And the `rtables` expression is obtained as:

\
`rtables_expr`` ``<-`` `[`substitute`](https://rdrr.io/r/base/substitute.html)`(`\
`  expr ``=`` `[`basic_table`](https://insightsengineering.github.io/rtables/latest-tag/reference/basic_table.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    `[`split_cols_by`](https://insightsengineering.github.io/rtables/latest-tag/reference/split_cols_by.html)`(``arm``, split_fun ``=`` ``drop_split_levels``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    `[`split_rows_by`](https://insightsengineering.github.io/rtables/latest-tag/reference/split_rows_by.html)`(``visit``, split_fun ``=`` ``drop_split_levels``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    `[`split_cols_by_multivar`](https://insightsengineering.github.io/rtables/latest-tag/reference/split_cols_by_multivar.html)`(`\
`      vars ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"AVAL"``, ``"CHG"``)``,`\
`      varlabels ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"Value"``, ``"Change"``)`\
`    ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    `[`summarize_colvars`](https://pharmaverse.github.io/tern/latest-tag/reference/summarize_colvars.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    `[`build_table`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html)`(``df ``=`` ``df``)``,`\
`  env ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(`\
`    df ``=`` `[`substitute`](https://rdrr.io/r/base/substitute.html)`(``adlb_f``)``,`\
`    arm ``=`` ``"ARM"``,`\
`    visit ``=`` ``"AVISIT"`\
`  ``)`\
`)`

The expression is valid … :

\
[`eval`](https://rdrr.io/r/base/eval.html)`(``rtables_expr``)`\
`##                        A: Drug X                    B: Placebo        `\
`##                   Value         Change         Value         Change   `\
`## ——————————————————————————————————————————————————————————————————————`\
`## WEEK 1 DAY 8                                                          `\
`##   n                69             69            73             73     `\
`##   Mean (SD)    20.8 (4.1)     1.6 (6.1)     20.2 (4.1)     -0.2 (5.6) `\
`##   Median          20.4           2.4           20.0           -0.2    `\
`##   Min - Max    12.8 - 34.6   -11.3 - 14.2   12.6 - 29.0   -12.8 - 10.8`

… but not easily readable …:

\
`rtables_expr`\
`## basic_table() %>% split_cols_by("ARM", split_fun = drop_split_levels) %>% `\
`##     split_rows_by("AVISIT", split_fun = drop_split_levels) %>% `\
`##     split_cols_by_multivar(vars = c("AVAL", "CHG"), varlabels = c("Value", `\
`##         "Change")) %>% summarize_colvars() %>% build_table(df = adlb_f)`

… but that can be arranged:

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`teal`](https://insightsengineering.github.io/teal/)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`styler`](https://github.com/r-lib/styler)`)`\
\
`#' Stylish code`\
`#'`\
`#' Deparse an expression and display the code following NEST conventions.`\
`#'`\
`` #' @param expr (`call`)\cr or possibly understood as so. ``\
`#'`\
`styled_expr`` ``<-`` ``function``(``expr``)`` ``{`\
`  `[`print`](https://rdrr.io/r/base/print.html)`(`\
`    ``styler``::`[`style_text`](https://styler.r-lib.org/reference/style_text.html)`(``text ``=`` `[`deparse`](https://rdrr.io/r/base/deparse.html)`(``expr``)``)``,`\
`    colored ``=`` ``FALSE`\
`  ``)`\
`}`\
`#'`\
`#' @examples`\
`styled_expr``(``rtables_expr``)`\
`## basic_table() %>%`\
`##   split_cols_by("ARM", split_fun = drop_split_levels) %>%`\
`##   split_rows_by("AVISIT", split_fun = drop_split_levels) %>%`\
`##   split_cols_by_multivar(vars = c("AVAL", "CHG"), varlabels = c(`\
`##     "Value",`\
`##     "Change"`\
`##   )) %>%`\
`##   summarize_colvars() %>%`\
`##   build_table(df = adlb_f)`

### `substitute` in a function

Moving further, `substitute` can actually be wrapped in a function, this
way the `rtables` pipelines are *programmatically* obtained:

\
`rtables_expr`` ``<-`` ``function``(``df``,`\
`                         ``arm``,`\
`                         ``visit``)`` ``{`\
`  `[`substitute`](https://rdrr.io/r/base/substitute.html)`(`\
`    expr ``=`` `[`basic_table`](https://insightsengineering.github.io/rtables/latest-tag/reference/basic_table.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`      `[`split_cols_by`](https://insightsengineering.github.io/rtables/latest-tag/reference/split_cols_by.html)`(``arm``, split_fun ``=`` ``drop_split_levels``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`      `[`split_rows_by`](https://insightsengineering.github.io/rtables/latest-tag/reference/split_rows_by.html)`(``visit``, split_fun ``=`` ``drop_split_levels``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`      `[`split_cols_by_multivar`](https://insightsengineering.github.io/rtables/latest-tag/reference/split_cols_by_multivar.html)`(`\
`        vars ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"AVAL"``, ``"CHG"``)``,`\
`        varlabels ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"Value"``, ``"Change"``)`\
`      ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`      `[`summarize_colvars`](https://pharmaverse.github.io/tern/latest-tag/reference/summarize_colvars.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`      `[`build_table`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html)`(``df ``=`` ``df``)``,`\
`    env ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(`\
`      df ``=`` `[`substitute`](https://rdrr.io/r/base/substitute.html)`(``df``)``,`\
`      arm ``=`` ``arm``,`\
`      visit ``=`` ``visit`\
`    ``)`\
`  ``)`\
`}`\
`result`` ``<-`` ``rtables_expr``(``df ``=`` ``adlb_f``, arm ``=`` ``"ARM"``, visit ``=`` ``"AVISIT"``)`\
`styled_expr``(``result``)`\
`## basic_table() %>%`\
`##   split_cols_by("ARM", split_fun = drop_split_levels) %>%`\
`##   split_rows_by("AVISIT", split_fun = drop_split_levels) %>%`\
`##   split_cols_by_multivar(vars = c("AVAL", "CHG"), varlabels = c(`\
`##     "Value",`\
`##     "Change"`\
`##   )) %>%`\
`##   summarize_colvars() %>%`\
`##   build_table(df = adlb_f)`\
[`eval`](https://rdrr.io/r/base/eval.html)`(``result``)`\
`##                        A: Drug X                    B: Placebo        `\
`##                   Value         Change         Value         Change   `\
`## ——————————————————————————————————————————————————————————————————————`\
`## WEEK 1 DAY 8                                                          `\
`##   n                69             69            73             73     `\
`##   Mean (SD)    20.8 (4.1)     1.6 (6.1)     20.2 (4.1)     -0.2 (5.6) `\
`##   Median          20.4           2.4           20.0           -0.2    `\
`##   Min - Max    12.8 - 34.6   -11.3 - 14.2   12.6 - 29.0   -12.8 - 10.8`

- The same results as before are obtained …
- while, fine tuning is easier.
- For instance, the variable designating the study arm and the visit can
  be changed, which is an expected feature in `teal` module encoding
  panel.

\
`result`` ``<-`` ``rtables_expr``(``df ``=`` ``adlb_f``, arm ``=`` ``"ARMCD"``, visit ``=`` ``"AVISITN"``)`\
[`eval`](https://rdrr.io/r/base/eval.html)`(``result``)`\
`## Split var [AVISITN] was not character or factor. Converting to factor`\
`##                         ARM A                        ARM B           `\
`##                  Value         Change         Value         Change   `\
`## —————————————————————————————————————————————————————————————————————`\
`## 1                                                                    `\
`##   n               69             69            73             73     `\
`##   Mean (SD)   20.8 (4.1)     1.6 (6.1)     20.2 (4.1)     -0.2 (5.6) `\
`##   Median         20.4           2.4           20.0           -0.2    `\
`##   Min - Max   12.8 - 34.6   -11.3 - 14.2   12.6 - 29.0   -12.8 - 10.8`\
`styled_expr``(``result``)`\
`## basic_table() %>%`\
`##   split_cols_by("ARMCD", split_fun = drop_split_levels) %>%`\
`##   split_rows_by("AVISITN", split_fun = drop_split_levels) %>%`\
`##   split_cols_by_multivar(vars = c("AVAL", "CHG"), varlabels = c(`\
`##     "Value",`\
`##     "Change"`\
`##   )) %>%`\
`##   summarize_colvars() %>%`\
`##   build_table(df = adlb_f)`

### Chain expressions in a pipeline

It is also possible to manipulate expressions, for instance, expressions
might be chained in a pipeline.

\
`#' Expressions as a pipeline`\
`#'`\
`` #' Accepts expressions to be chained using the `magrittr` pipeline-flavor. ``\
`` #' @param ... (`call`)\cr or object which can be interpreted as so. ``\
`` #'    (e.g. `name`) ``\
`#'`\
`pipe_expr`` ``<-`` ``function``(``...``)`` ``{`\
`  ``exprs`` ``<-`` `[`unlist`](https://rdrr.io/r/base/unlist.html)`(`[`list`](https://rdrr.io/r/base/list.html)`(``...``)``)`\
`  ``exprs`` ``<-`` `[`lapply`](https://rdrr.io/r/base/lapply.html)`(`\
`    ``exprs``,`\
`    ``function``(``x``)`` ``{`\
`      ``x`` ``<-`` `[`deparse`](https://rdrr.io/r/base/deparse.html)`(``x``)`\
`      `[`paste`](https://rdrr.io/r/base/paste.html)`(``x``, collapse ``=`` ``" "``)`\
`    ``}`\
`  ``)`\
`  ``exprs`` ``<-`` `[`unlist`](https://rdrr.io/r/base/unlist.html)`(``exprs``)`\
`  ``exprs`` ``<-`` `[`paste`](https://rdrr.io/r/base/paste.html)`(``exprs``, collapse ``=`` ``" %>% "``)`\
`  `[`str2lang`](https://rdrr.io/r/base/parse.html)`(``exprs``)`\
`}`\
\
`#' @examples`\
`result`` ``<-`` `[`pipe_expr`](https://insightsengineering.github.io/teal.modules.clinical/reference/pipe_expr.md)`(`\
`  expr1 ``=`` `[`substitute`](https://rdrr.io/r/base/substitute.html)`(``df``)``,`\
`  expr2 ``=`` `[`substitute`](https://rdrr.io/r/base/substitute.html)`(``head``)`\
`)`\
`result`\
`## df %>% head`

- Expressions can be arranged in a list, this way, it is possible to
  have **conditional** editing of expressions.
- In the context of `rtables`, layers enclosing `analyze` call handle
  `.stats` option. The lean expression should include the `.stats`
  option, **only when the default value is changed**.
- This is again an expected feature in `teal` module when rendering the
  code with `Show R Code`:

\
`rtables_expr`` ``<-`` ``function``(``df``,`\
`                         ``arm``,`\
`                         ``visit``,`\
`                         ``.stats`` ``=`` ``NULL``)`` ``{`\
`  ``# The rtables layout is decomposed into a list of expressions.`\
`  ``lyt`` ``<-`` `[`list`](https://rdrr.io/r/base/list.html)`(``)`\
`  ``# 1. First the columns and rows:`\
`  ``lyt``$``structure`` ``<-`` `[`substitute`](https://rdrr.io/r/base/substitute.html)`(`\
`    expr ``=`` `[`basic_table`](https://insightsengineering.github.io/rtables/latest-tag/reference/basic_table.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`      `[`split_cols_by`](https://insightsengineering.github.io/rtables/latest-tag/reference/split_cols_by.html)`(``arm``, split_fun ``=`` ``drop_split_levels``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`      `[`split_rows_by`](https://insightsengineering.github.io/rtables/latest-tag/reference/split_rows_by.html)`(``visit``, split_fun ``=`` ``drop_split_levels``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`      `[`split_cols_by_multivar`](https://insightsengineering.github.io/rtables/latest-tag/reference/split_cols_by_multivar.html)`(`\
`        vars ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"AVAL"``, ``"CHG"``)``,`\
`        varlabels ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"Value"``, ``"Change"``)`\
`      ``)``,`\
`    env ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(`\
`      arm ``=`` ``arm``,`\
`      visit ``=`` ``visit`\
`    ``)`\
`  ``)`\
`  ``# 2. The analyze layer which depends on the use of .stats.`\
`  ``lyt``$``analyze`` ``<-`` ``if`` ``(`[`is.null`](https://rdrr.io/r/base/NULL.html)`(``.stats``)``)`` ``{`\
`    `[`substitute`](https://rdrr.io/r/base/substitute.html)`(`\
`      `[`summarize_colvars`](https://pharmaverse.github.io/tern/latest-tag/reference/summarize_colvars.html)`(``)`\
`    ``)`\
`  ``}`` ``else`` ``{`\
`    `[`substitute`](https://rdrr.io/r/base/substitute.html)`(`\
`      `[`summarize_colvars`](https://pharmaverse.github.io/tern/latest-tag/reference/summarize_colvars.html)`(``.stats ``=`` ``.stats``)``,`\
`      `[`list`](https://rdrr.io/r/base/list.html)`(``.stats ``=`` ``.stats``)`\
`    ``)`\
`  ``}`\
`  ``# 3. And finishing with rtables::build_table.`\
`  ``lyt``$``build`` ``<-`` `[`substitute`](https://rdrr.io/r/base/substitute.html)`(`\
`    `[`build_table`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html)`(``df ``=`` ``df``)``,`\
`    `[`list`](https://rdrr.io/r/base/list.html)`(``df ``=`` `[`substitute`](https://rdrr.io/r/base/substitute.html)`(``df``)``)`\
`  ``)`\
`  ``# As previously demonstrated, expressions can be manipulated and`\
`  ``# chained in a pipeline.`\
`  `[`pipe_expr`](https://insightsengineering.github.io/teal.modules.clinical/reference/pipe_expr.md)`(``lyt``)`\
`}`

- First application with standard statistics:

\
`result`` ``<-`` ``rtables_expr``(``df ``=`` ``adlb_f``, arm ``=`` ``"ARM"``, visit ``=`` ``"AVISIT"``)`\
`styled_expr``(``result``)`\
`## basic_table() %>%`\
`##   split_cols_by("ARM", split_fun = drop_split_levels) %>%`\
`##   split_rows_by("AVISIT", split_fun = drop_split_levels) %>%`\
`##   split_cols_by_multivar(vars = c("AVAL", "CHG"), varlabels = c(`\
`##     "Value",`\
`##     "Change"`\
`##   )) %>%`\
`##   summarize_colvars() %>%`\
`##   build_table(df = adlb_f)`\
[`eval`](https://rdrr.io/r/base/eval.html)`(``result``)`\
`##                        A: Drug X                    B: Placebo        `\
`##                   Value         Change         Value         Change   `\
`## ——————————————————————————————————————————————————————————————————————`\
`## WEEK 1 DAY 8                                                          `\
`##   n                69             69            73             73     `\
`##   Mean (SD)    20.8 (4.1)     1.6 (6.1)     20.2 (4.1)     -0.2 (5.6) `\
`##   Median          20.4           2.4           20.0           -0.2    `\
`##   Min - Max    12.8 - 34.6   -11.3 - 14.2   12.6 - 29.0   -12.8 - 10.8`

- Then with statistics specifications:

\
`result`` ``<-`` ``rtables_expr``(`\
`  df ``=`` ``adlb_f``, arm ``=`` ``"ARM"``, visit ``=`` ``"AVISIT"``,`\
`  .stats ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"n"``, ``"mean_sd"``)`\
`)`\
`styled_expr``(``result``)`\
`## basic_table() %>%`\
`##   split_cols_by("ARM", split_fun = drop_split_levels) %>%`\
`##   split_rows_by("AVISIT", split_fun = drop_split_levels) %>%`\
`##   split_cols_by_multivar(vars = c("AVAL", "CHG"), varlabels = c(`\
`##     "Value",`\
`##     "Change"`\
`##   )) %>%`\
`##   summarize_colvars(.stats = c("n", "mean_sd")) %>%`\
`##   build_table(df = adlb_f)`\
[`eval`](https://rdrr.io/r/base/eval.html)`(``result``)`\
`##                      A: Drug X                B: Placebo       `\
`##                  Value       Change       Value        Change  `\
`## ———————————————————————————————————————————————————————————————`\
`## WEEK 1 DAY 8                                                   `\
`##   n                69          69           73           73    `\
`##   Mean (SD)    20.8 (4.1)   1.6 (6.1)   20.2 (4.1)   -0.2 (5.6)`

### Including pre-processing

Finally, it would also be possible to wrap several expressions into a
single function.

- For instance, the teal module generally includes a pre-processing
  section:

\
`rtables_expr`` ``<-`` ``function``(``df``,`\
`                         ``paramcd``,`\
`                         ``arm``,`\
`                         ``visit``,`\
`                         ``.stats`` ``=`` ``NULL``)`` ``{`\
`  ``# y is a list which will collect two expressions:`\
`  ``# 1. y$data with the preprocessing steps.`\
`  ``# 2. y$rtables the table layout and build.`\
`  ``y`` ``<-`` `[`list`](https://rdrr.io/r/base/list.html)`(``)`\
`  ``# 1. Preprocessing ---`\
`  ``y``$``data`` ``<-`` `[`substitute`](https://rdrr.io/r/base/substitute.html)`(`\
`    ``df`` ``<-`` ``df`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`      `[`filter`](https://dplyr.tidyverse.org/reference/filter.html)`(`\
`        ``PARAMCD`` ``==`` ``paramcd`` ``&`\
`          ``ARMCD`` `[`%in%`](https://rdrr.io/r/base/match.html)` `[`c`](https://rdrr.io/r/base/c.html)`(``"ARM A"``, ``"ARM B"``)`` ``&`` ``AVISIT`` ``==`` ``"WEEK 1 DAY 8"`\
`      ``)``,`\
`    `[`list`](https://rdrr.io/r/base/list.html)`(`\
`      df ``=`` `[`substitute`](https://rdrr.io/r/base/substitute.html)`(``df``)``,`\
`      paramcd ``=`` ``paramcd`\
`    ``)`\
`  ``)`\
`  ``# 2. rtables layout ---`\
`  ``lyt`` ``<-`` `[`list`](https://rdrr.io/r/base/list.html)`(``)`\
`  ``lyt``$``structure`` ``<-`` `[`substitute`](https://rdrr.io/r/base/substitute.html)`(`\
`    expr ``=`` `[`basic_table`](https://insightsengineering.github.io/rtables/latest-tag/reference/basic_table.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`      `[`split_cols_by`](https://insightsengineering.github.io/rtables/latest-tag/reference/split_cols_by.html)`(``arm``, split_fun ``=`` ``drop_split_levels``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`      `[`split_rows_by`](https://insightsengineering.github.io/rtables/latest-tag/reference/split_rows_by.html)`(``visit``, split_fun ``=`` ``drop_split_levels``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`      `[`split_cols_by_multivar`](https://insightsengineering.github.io/rtables/latest-tag/reference/split_cols_by_multivar.html)`(`\
`        vars ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"AVAL"``, ``"CHG"``)``,`\
`        varlabels ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"Value"``, ``"Change"``)`\
`      ``)``,`\
`    env ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(`\
`      arm ``=`` ``arm``,`\
`      visit ``=`` ``visit`\
`    ``)`\
`  ``)`\
`  ``lyt``$``analyze`` ``<-`` ``if`` ``(`[`is.null`](https://rdrr.io/r/base/NULL.html)`(``.stats``)``)`` ``{`\
`    `[`substitute`](https://rdrr.io/r/base/substitute.html)`(`\
`      `[`summarize_colvars`](https://pharmaverse.github.io/tern/latest-tag/reference/summarize_colvars.html)`(``)`\
`    ``)`\
`  ``}`` ``else`` ``{`\
`    `[`substitute`](https://rdrr.io/r/base/substitute.html)`(`\
`      `[`summarize_colvars`](https://pharmaverse.github.io/tern/latest-tag/reference/summarize_colvars.html)`(``.stats ``=`` ``.stats``)``,`\
`      `[`list`](https://rdrr.io/r/base/list.html)`(``.stats ``=`` ``.stats``)`\
`    ``)`\
`  ``}`\
`  ``lyt``$``build`` ``<-`` `[`substitute`](https://rdrr.io/r/base/substitute.html)`(`\
`    `[`build_table`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html)`(``df ``=`` ``df``)``,`\
`    `[`list`](https://rdrr.io/r/base/list.html)`(``df ``=`` `[`substitute`](https://rdrr.io/r/base/substitute.html)`(``df``)``)`\
`  ``)`\
`  ``y``$``rtables`` ``<-`` `[`pipe_expr`](https://insightsengineering.github.io/teal.modules.clinical/reference/pipe_expr.md)`(``lyt``)`\
`  ``# Finally returns y as a list with two expressions.`\
`  ``y`\
`}`

It is now possible to modify the studied parameter (`PARAMCD`) in
addition to the study arm and visit variables names.

\
`adlb`` ``<-`` ``tmc_ex_adlb`\
`result`` ``<-`` ``rtables_expr``(`\
`  df ``=`` ``adlb``, paramcd ``=`` ``"CRP"``, arm ``=`` ``"ARM"``, visit ``=`` ``"AVISIT"``,`\
`  .stats ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"n"``, ``"mean_sd"``)`\
`)`

The two expressions are consistent:

\
`styled_expr``(``result``$``data``)`\
`## adlb <- adlb %>% filter(PARAMCD == "CRP" & ARMCD %in% c(`\
`##   "ARM A",`\
`##   "ARM B"`\
`## ) & AVISIT == "WEEK 1 DAY 8")`\
`styled_expr``(``result``$``rtables``)`\
`## basic_table() %>%`\
`##   split_cols_by("ARM", split_fun = drop_split_levels) %>%`\
`##   split_rows_by("AVISIT", split_fun = drop_split_levels) %>%`\
`##   split_cols_by_multivar(vars = c("AVAL", "CHG"), varlabels = c(`\
`##     "Value",`\
`##     "Change"`\
`##   )) %>%`\
`##   summarize_colvars(.stats = c("n", "mean_sd")) %>%`\
`##   build_table(df = adlb)`

The two expressions can be executed and return the `rtables`:

\
`result_exec`` ``<-`` `[`mapply`](https://rdrr.io/r/base/mapply.html)`(``eval``, ``result``)`\
`result_exec``$``rtables`\
`##                      A: Drug X              B: Placebo      `\
`##                  Value      Change       Value      Change  `\
`## ————————————————————————————————————————————————————————————`\
`## WEEK 1 DAY 8                                                `\
`##   n               69          69          73          73    `\
`##   Mean (SD)    1.0 (0.2)   0.0 (0.3)   1.0 (0.2)   0.0 (0.3)`

### In a nutshell

At this point, it is then possible to:

- generate `rtables` pipelines.
- chain expressions in a pipeline (e.g. `pipe_expr`)
- decompose a `rtables` pipeline to add **conditional** layers
  (e.g. `.stats`).
- group expressions into a single list and control both
  **pre-processing** and **`rtables` pipeline**.
