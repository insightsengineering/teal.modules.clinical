
# teal.modules.tern

The `teal.modules.tern` R package contains interactive `teal` modules for the outputs
(TLGs) in [`tern`](https://github.roche.com/Rpackages/tern).

See a live demo [on the Roche Shiny Server](http://shiny.roche.com/users/waddella/teal_tern/).

To build your own app, see [Getting Started](#getting-started) below.

## Training

We are currently working on trainings. Here is a list of available content:

 * [go.gene.com/go-teal: a gentle introduction and getting started webpage](http://go.gene.com/go-teal)
 * [Agile R Leader Perspectives March 2018](https://streamingmedia.roche.com/media/AgileR+Leader+Perspectives+March+2018/1_ccr8716n)
 * [General Teal and Agile R Framework](http://pdwebdev01.gene.com/groups/devo/multimedia/Gen_Teal/story.html)

Videos explaining the indiviual teal modules can be found under [Articles](https://pages.github.roche.com/Rpackages/teal.tern/articles/) on the web documentation.

# Installation

## Stable Release

Please install the package dependencies as follows:

``` r
devtools::install_github(
  repo = "Rpackages/random.cdisc.data",
  ref = "v0.1.0", 
  host = "https://github.roche.com/api/v3",
  upgrade_dependencies = FALSE, build_vignettes = FALSE
)

devtools::install_github("Roche/rtables", ref = "v0.1.0",
  upgrade_dependencies = FALSE, build_vignettes = FALSE)

devtools::install_github(
  repo = "Rpackages/tern",
  ref = "v0.5.0", 
  host = "https://github.roche.com/api/v3",
  upgrade_dependencies = FALSE, build_vignettes = FALSE
)

devtools::install_github(
  repo = "Rpackages/teal",
  ref = "v0.0.4", 
  host = "https://github.roche.com/api/v3",
  upgrade_dependencies = FALSE, build_vignettes = FALSE
)

devtools::install_github(
  repo = "Rpackages/teal.tern",
  ref = "v0.5.0", 
  host = "https://github.roche.com/api/v3",
  upgrade_dependencies = FALSE, build_vignettes = FALSE
)
```

## Development Version

``` r
devtools::install_github(
  repo = "Rpackages/random.cdisc.data",
  ref = "master", 
  host = "https://github.roche.com/api/v3",
  upgrade_dependencies = FALSE, build_vignettes = FALSE
)

devtools::install_github("Roche/rtables", ref = "master",
  upgrade_dependencies = FALSE, build_vignettes = FALSE)

devtools::install_github(
  repo = "Rpackages/tern",
  ref = "master", 
  host = "https://github.roche.com/api/v3",
  upgrade_dependencies = FALSE, build_vignettes = FALSE
)

devtools::install_github(
  repo = "Rpackages/teal",
  ref = "master", 
  host = "https://github.roche.com/api/v3",
  upgrade_dependencies = FALSE, build_vignettes = FALSE
)

devtools::install_github(
  repo = "Rpackages/teal.tern",
  ref = "master", 
  host = "https://github.roche.com/api/v3",
  upgrade_dependencies = FALSE, build_vignettes = FALSE
)
```

# Getting Started

Here is an example app that shows all modules using random data. If you save
this code into a file named `app.R` then it is a valid [single-file shiny
application](https://shiny.rstudio.com/articles/app-formats.html).

## Simple app

Copy the following code into a new R file and execute it line by line:

```r
library(teal.modules.tern) # library(teal.tern) if you are using older version
library(random.cdisc.data)

ASL <- radsl(N = 600)

x <- teal::init(
  data = list(ASL = ASL),
  modules = root_modules(
    tm_data_table("Data Table"),
    tm_variable_browser("Variable Browser"),
    tm_t_summary(
      label = "Demographic Table",
      dataname = "ASL",
      arm_var = choices_selected(c("ACTARMCD", "ARMCD"), "ACTARMCD"),
      summarize_vars = choices_selected(c("BMRKR2", "SEX", "RACE"), c("BMRKR2", "SEX"))
    )
  )
)

shinyApp(x$ui, x$server)
```

This should start the teal web app. Now save the above code into an R file named
`app.R` and replace `radam` with `read_bce` and select a dataset from a study of
your choice. Start the app again and configure the arguments of
`tm_t_summary` according to the information you would like to
summarize in your `ASL` dataset.


## App setup with all available modules

```r
library(teal.modules.tern) # library(teal.tern) if you are using older version
library(random.cdisc.data)
library(dplyr)

## Generate Data
ASL <- radsl(seed = 1) %>% 
  mutate(., 
         RACE = droplevels(RACE),
         ARM1 = sample(c("DUMMY A", "DUMMY B"), n(), TRUE),
         STRATM1 = sample(paste("STRATM1", 1:3), n(), TRUE),
         STRATM2 = sample(paste("STRATM2", 1:4), n(), TRUE),
         TCICLVL2 = sample(paste("STRATM2", letters[1:3]), n(), TRUE),
         T1I1FL = sample(c(T,F), n(), TRUE),
         T0I0FL = sample(c(T,F), n(), TRUE),
         ITTGEFL = sample(c(T,F), n(), TRUE),
         ITTWTFL = sample(c(T,F), n(), TRUE),
         ITTGE2FL = sample(c(T,F), n(), TRUE),
         ITTGE3FL = sample(c(T,F), n(), TRUE))

ATE <- radtte(ADSL = ASL, seed = 1)
ARS <- radrs(ADSL = ASL, seed = 1)

attr(ASL, "source") <- "random.cdisc.data::radsl(seed = 1) %>% mutate(...)"
attr(ATE, "source") <- "random.cdisc.data::radtte(ASL, seed = 1)"
attr(ARS, "source") <- "random.cdisc.data::radrs(ASL, seed = 1)"

## Reusable Configuration For Modules
arm_var_selected <- "ARM"
arm_var_choices <- c("ARM", "ARMCD", "ARM1", "STRATM1", "STRATM2")
arm_var <- choices_selected(arm_var_choices, arm_var_selected)

strata_var_selected <- c("STRATM1", "STRATM2", "TCICLVL2") %>%
  intersect(names(ASL))
strata_var_choices <-  names(ASL)[(sapply(ASL, is.character))] %>%
  intersect(names(ASL))
strata_var <- choices_selected(strata_var_choices, strata_var_selected)

dm_summarize_vars_selected <- c("SEX", "BAGE", "RACE") %>% 
  intersect(names(ASL))
dm_summarize_vars_choices <- names(ASL)
dm_summarize_vars <- choices_selected(dm_summarize_vars_choices, dm_summarize_vars_selected)

facet_var_selected <- "TCICLVL2"
facet_var_choices <- names(ASL)[(sapply(ASL, is.character))] %>%
  intersect(names(ASL))
facet_var <- choices_selected(facet_var_choices, facet_var_selected)

paramcd_tte_selected <- "OS"
paramcd_tte_choices <- unique(ATE$PARAMCD)
paramcd_tte <- choices_selected(paramcd_tte_choices, paramcd_tte_selected)

paramcd_rsp_selected <- "INVET"
paramcd_rsp_choices <- c("BESRSPI", "INVET", "OVRINV") %>% 
  intersect(ARS$PARAMCD)
paramcd_rsp <- choices_selected(paramcd_rsp_choices, paramcd_rsp_selected)

x_var_ct_selected <- "T1I1FL"
y_var_ct_selected <- "T0I0FL"
ct_choices <- c("T1I1FL", "T0I0FL", "ITTGEFL", "ITTWTFL", "ITTGE2FL", "ITTGE3FL") %>%
  intersect(names(ASL))
x_var_ct <- choices_selected(ct_choices, x_var_ct_selected)
y_var_ct <- choices_selected(ct_choices, y_var_ct_selected)

# reference & comparison arm selection when switching the arm variable
arm_ref_comp <- list(
  ACTARMCD = list(ref = "ARM A", comp = c("ARM B", "ARM C")),
  ARM1 = list(ref = "DUMMY B", comp = "DUMMY A")
)

## Setup App
x <- teal::init(
  data = list(ASL = ASL, ARS = ARS, ATE = ATE),
  modules = root_modules(
    module(
      label = "Study Information",
      server = function(input, output, session, datasets) {},
      ui = function(id) {
        tagList(
          tags$p("Info about data source:"),
          tags$p("Radom data is used that has been created with the ",
                 tags$code("random.cdisc.data"), "R package.")
        )
      },
      filters = NULL
    ),
    tm_data_table("Data Table"),
    tm_variable_browser("Variable Browser"),
    tm_t_summary(
      label = "Demographic Table",
      dataname = "ASL",
      arm_var = arm_var,
      summarize_vars = dm_summarize_vars
    ),
    modules(
      "Forest Plots",
      tm_g_forest_tte(
        label = "Survival Forest Plot",
        dataname = "ATE",
        arm_var = arm_var,
        subgroup_var = strata_var,
        paramcd = paramcd_tte,
        plot_height = c(800, 200, 4000)
      ),
      tm_g_forest_rsp(
        label = "Response Forest Plot",
        dataname = "ARS",
        arm_var = arm_var,
        subgroup_var = strata_var,
        paramcd = paramcd_rsp,
        plot_height = c(800, 200, 4000)
      )
    ),
    tm_g_km(
      label = "Kaplan Meier Plot",
      dataname = "ATE",
      arm_var = arm_var,
      arm_ref_comp = arm_ref_comp,
      paramcd = paramcd_tte,
      facet_var = facet_var,
      strata_var = strata_var,
      plot_height = c(1800, 200, 4000)
    ),
    tm_t_rsp(
      label = "Response Table",
      dataname = "ARS",
      arm_var = arm_var,
      arm_ref_comp = arm_ref_comp,
      paramcd = paramcd_rsp,
      strata_var = strata_var
    ),
    # @TODO
    tm_t_tte(
      label = "Time To Event Table",
      dataname = "ATE",
      arm_var = arm_var,
      arm_ref_comp = arm_ref_comp,
      paramcd = paramcd_tte,
      strata_var = strata_var,
      time_points = choices_selected(c(6, 12, 18, 24, 30, 36, 42), c(6, 12, 18)),
      time_unit = "month",
      event_desrc_var = "EVNTDESC"
    ),
    tm_t_percentage_cross_table(
      "Cross Table",
      dataname = "ASL",
      x_var = x_var_ct,
      y_var = y_var_ct
    )
  ),
  header = div(
    class = "",
    style = "margin-bottom: 2px;",
    tags$h1("Example App With teal.modules.tern module", tags$span("SPA", class="pull-right"))
  ),
  footer = tags$p(class="text-muted", "Info About Authors")
)  

shinyApp(x$ui, x$server)
```


Each teal module in `teal` will be explained in a separate vignette and
is accessile via the articles tab on the [project site][ghs].


## Deployment

### Introduction

We currently recommend to deploy your teal apps to the shiny server at
[shiny.roche.com](http://shiny.roche.com). We will look into the deployment
mechanism with [rstudio connect](https://rsconnect.roche.com/) soon.

Please also have a look at the [following
video](https://streamingmedia.roche.com/media/Deploy+a+Teal+App+to+the+Shiny+server/1_t59abpdw)
that shows the deployment step by step.

### Deployment Setup

Teal apps should be setup such that the R packages `teal`, `tea.tern`, `tern`,
and `rtables` are installed locally for every app. This way you can use
different versions of those libraries for your different projects.

1. Create a [GitHub](http://github.roche.com) repository with your `app.R` file.
Add `libs/*` to the `.gitignore` file.

2. Save the following code in a file `install.R`

```r
project.path <- getwd() #"/srv/shiny-server/users/..."

path.teal.libs <-  file.path(project.path, "libs")

if (!dir.exists(path.teal.libs)) dir.create(path.teal.libs)

# delete all current versions of teal and teal.oncology in path.teal.libs
lapply(list.dirs(path.teal.libs, recursive = FALSE), unlink, recursive = TRUE, force = TRUE)

orig_libpaths <- .libPaths()

.libPaths(c(
    path.teal.libs,
    as.vector(Filter(function(x) !grepl("^/opt/bee/home", x), orig_libpaths))
))

devtools::install_github(
  repo = "Rpackages/random.cdisc.data",
  ref = "v0.1.2.1", 
  host = "https://github.roche.com/api/v3",
  upgrade_dependencies = FALSE, build_vignettes = FALSE
)

devtools::install_github("Roche/rtables", ref = "v0.1.0",
  upgrade_dependencies = FALSE, build_vignettes = FALSE)

devtools::install_github(
  repo = "Rpackages/tern",
  ref = "v0.5.1", 
  host = "https://github.roche.com/api/v3",
  upgrade_dependencies = FALSE, build_vignettes = FALSE
)

devtools::install_github(
  repo = "Rpackages/teal",
  ref = "v0.0.5.9000", 
  host = "https://github.roche.com/api/v3",
  upgrade_dependencies = FALSE, build_vignettes = FALSE
)

devtools::install_github(
  repo = "Rpackages/teal.tern",
  ref = "v0.5.1", 
  host = "https://github.roche.com/api/v3",
  upgrade_dependencies = FALSE, build_vignettes = FALSE
)

.libPaths(orig_libpaths)
```

3. Add the following line as the first in `app.R` line 

```r
.libPaths(c(normalizePath("./libs"), .libPaths()))
```

4. Commit and push you changes to the GitHub repository.


### Run Locally (Laptop and r.roche.com)

1. Install the libraries specified in `install.R` in your Terminal (shell)
```bash
Rscript install.R
```

2. Restart R in RStudio with the menu item `Session` > `Restart R`.

3. Execute the code in `app.R` either line by line or all at once.


### Run on the Shiny server

1. Choose a location on the shiny server. If you want your app to be accessible
under `http://shiny.roche.com/user/waddella/my-app/` then you need to put the
`app.R` file to  `/srv/shiny-server/user/waddella/my-app/`.

2. Clone your repository to the chosen location in your Terminal:
```bash
cd /srv/shiny-server/user/waddella/
git clone <repository-url> my-app
```

3. Install the dependencies defined in `install.R`. If your shiny app is
installed under `/srv/shiny-server/3.*` you know the R version that shiny uses
from the path. For all other paths shiny uses R version `3.3.1`. Use the correct
R version to  install the packages, e.g. to install the packages with R version
`3.3.1` run in your terminal:
```bash
cd my-app
Rscript-3.3.1 install.R
```
This will install the dependent R packages under `./libs`

4. Run your shiny app in your web browser

5. If you run into errors inspect the logs under: `/var/log/shiny-server`

6. Modify your `app.R` push the changes to github and pull the in your deployed
shiny app with `git pull`. If you do not see the changes in your app then the
shiny server has likely cashed your current session in which case the app is not
restarted. The easiest way to deal with that is to rename the folder with your
app (e.g. `mv my-app my-app-2`) and open the app the new web address.

[ghs]: http://pages.github.roche.com/Rpackages/teal.tern
