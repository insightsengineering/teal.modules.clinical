
# teal.tern

The `teal.tern` R package contains interactive `teal` modules for the outputs
(TLGs) in [`tern`](https://github.roche.com/Rpackages/tern).

A test application with random data can be found [here](http://shiny.roche.com/users/waddella/teal_tern/).

## Training

We are currently working on trainings. Here is a list of available content:

 * [Agile R Leader Perspectives March 2018](https://streamingmedia.roche.com/media/AgileR+Leader+Perspectives+March+2018/1_ccr8716n)
 * [General Teal and Agile R Framework](http://pdwebdev01.gene.com/groups/devo/multimedia/Gen_Teal/story.html)

Videos explaining the indiviual teal modules in `teal.tern` can be found under [Articles](https://pages.github.roche.com/Rpackages/teal.tern/articles/) on the web documentation.

# Installation

Please install the package dependencies as follows:

``` r
devtools::install_github(
  repo = "Rpackages/random.cdisc.data",
  ref = "v0.1.0", 
  host = "https://github.roche.com/api/v3"
)

devtools::install_github("Roche/rtables", ref = "v0.1.0")

devtools::install_github(
  repo = "Rpackages/tern",
  ref = "v0.5.0", 
  host = "https://github.roche.com/api/v3"
)

devtools::install_github(
  repo = "Rpackages/teal",
  ref = "v0.0.3", 
  host = "https://github.roche.com/api/v3"
)

devtools::install_github(
  repo = "Rpackages/teal.tern",
  ref = "v0.5.0", 
  host = "https://github.roche.com/api/v3"
)
```


# Getting Started

Here is an example app that shows all modules using random data. If you save
this code into a file named `app.R` then it is a valid [single-file shiny
application](https://shiny.rstudio.com/articles/app-formats.html).

## Simple app

Copy the following code into a new R file and execute it line by line:

```r
library(teal.tern)
library(random.cdisc.data)

ASL <- radam("ASL", N = 600)

x <- teal::init(
  data = list(ASL = ASL),
  modules = root_modules(
    tm_data_table("Data Table"),
    tm_variable_browser("Variable Browser"),
    tm_t_summarize_variables(
      label = "Demographic Table",
      dataname = "ASL",
      arm_var = "ARM",
      arm_var_choices = c("ARM", "ARMCD"),
      summarize_vars =  c("BAGE", "SEX"),
      summarize_vars_choices =  c("BAGE", "SEX", "RACE")
    )
  )
)

shinyApp(x$ui, x$server)
```

This should start the teal web app. Now save the above code into an R file named
`app.R` and replace `radam` with `read_bce` and select a dataset from a study of
your choice. Start the app again and configure the arguments of
`tm_t_summarize_variables` according to the information you would like to
summarize in your `ASL` dataset.


## App setup with all available modules

```r
library(teal.tern)
library(random.cdisc.data)
library(htmltools) # for study source

## Generate Data
ASL <- radam("ASL", N = 600,
             arm_choices = c("Arm A", "Arm B", "Arm C"),
             start_with = list(
               ARM1 = c("DUMMY A", "DUMMY B"),
               STRATM1 = paste("STRATM1", 1:3),
               STRATM2 = paste("STRATM2", 1:4),
               TCICLVL2 = paste("STRATM2", letters[1:3]),
               T1I1FL = c(T,F),
               T0I0FL = c(T,F),
               ITTGEFL = c(T,F),
               ITTWTFL = c(T,F),
               ITTGE2FL = c(T,F),
               ITTGE3FL = c(T,F)
             )) 
ATE <- radam("ATE", ADSL = ASL)
ARS <- radam("ARS", ADSL = ASL)

## Reusable Configuration For Modules
arm_var <- "ARM"
arm_var_choices <- c("ARM", "ARMCD", "ARM1", "STRATM1", "STRATM2")

strata_var <- c("STRATM1", "STRATM2", "TCICLVL2") %>%
  intersect(names(ASL))
strata_var_choices <-  names(ASL)[(sapply(ASL, is.character))] %>%
  intersect(names(ASL))

dm_summarize_vars <- c("SEX", "BAGE", "RACE") %>% 
  intersect(names(ASL))
dm_summarize_vars_choices <- names(ASL)

facet_var <-  "TCICLVL2"
facet_var_choices <- names(ASL)[(sapply(ASL, is.character))] %>%
  intersect(names(ASL))

paramcd_tte <- "OS"
paramcd_choices_tte <- unique(ATE$PARAMCD)

paramcd_rsp <- "OVRSPI"
paramcd_choices_rsp <- c("BESRSPI", "OVRINV", "OVRSPI") %>% 
  intersect(ARS$PARAMCD)

x_var_ct <- "T1I1FL"
y_var_ct <- "T0I0FL"
ct_var_choices <- c("T1I1FL", "T0I0FL", "ITTGEFL", "ITTWTFL", "ITTGE2FL", "ITTGE3FL") %>%
  intersect(names(ASL))

# reference & comparison arm selection when switching the arm variable
arm_ref_comp <- list(
  ARM = list(ref="Arm A", comp=c("Arm B", "Arm C")),
  ARM1 = list(ref="DUMMY B", comp="DUMMY A")
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
    tm_t_summarize_variables(
      label = "Demographic Table",
      dataname = "ASL",
      arm_var = arm_var,
      arm_var_choices = arm_var_choices,
      summarize_vars =  dm_summarize_vars,
      summarize_vars_choices = dm_summarize_vars_choices
    ),
    modules(
      "Forest Plots",
      tm_g_forest_tte(
        label = "Survival Forest Plot",
        dataname = "ATE",
        arm_var = arm_var,
        arm_var_choices = arm_var_choices,
        subgroup_var = strata_var,
        subgroup_var_choices = strata_var_choices,        
        paramcd = paramcd_tte,
        paramcd_choices = paramcd_choices_tte,
        plot_height = c(800, 200, 4000)
      ),
      tm_g_forest_rsp(
        label = "Response Forest Plot",
        dataname = "ARS",
        arm_var = arm_var,
        arm_var_choices = arm_var_choices,
        subgroup_var = strata_var,
        subgroup_var_choices = strata_var_choices,
        paramcd = paramcd_rsp,
        paramcd_choices = paramcd_choices_rsp,
        plot_height = c(800, 200, 4000)
      )
    ),
    tm_g_km(
      label = "Kaplan Meier Plot",
      dataname = "ATE",
      arm_var = arm_var,
      arm_var_choices = arm_var_choices,
      arm_ref_comp = arm_ref_comp,
      paramcd = paramcd_tte,
      paramcd_choices = paramcd_choices_tte,
      facet_var = facet_var,
      facet_var_choices = facet_var_choices,
      strata_var = strata_var,
      strata_var_choices = strata_var_choices,
      plot_height = c(1800, 200, 4000)
    ), 
    tm_t_rsp(
      label = "Response Table",
      dataname = "ARS",
      arm_var = arm_var,
      arm_var_choices = arm_var_choices,
      arm_ref_comp = arm_ref_comp,
      paramcd = paramcd_rsp,
      paramcd_choices = paramcd_choices_rsp,
      strata_var = strata_var,
      strata_var_choices = strata_var_choices
    ),
    tm_t_tte(
      label = "Time To Event Table",
      dataname = "ATE",
      arm_var = arm_var,
      arm_var_choices = arm_var_choices,
      arm_ref_comp = arm_ref_comp,
      paramcd = paramcd_tte,
      paramcd_choices = paramcd_choices_tte,
      strata_var = strata_var,
      strata_var_choices = strata_var_choices,
      time_points = c(6, 12, 18),
      time_points_choices = c(6, 12, 18, 24, 30, 36, 42),
      time_unit = "month"
    ),
    tm_t_percentage_cross_table(
      "Cross Table",
      dataname = "ASL",
      x_var = x_var_ct,
      x_var_choices = ct_var_choices,
      y_var = y_var_ct,
      y_var_choices = ct_var_choices
    )
  ),
  header = div(
    class="",
    style="margin-bottom: 2px;",
    tags$h1("Example App With teal.tern Teal Modules", tags$span("SPA", class="pull-right"))
  ),
  footer = tags$p(class="text-muted", "Info About Authors")
)  

shinyApp(x$ui, x$server)
```


Each teal module in `teal.tern` will be explained in a separate vignette and
is accessile via the articles tab on the [project site][ghs].


## Deployment

Save the following code in a file `install.R` and run this to reinstall all the
dependencies local relative to the working directory (also on the shiny server).
Then execute the script with `Rscript install.R` (note if you `ssh` into
`r.roche.com` then you need to specify the correct R version, e.g. with
`Rscript-3.3.1`). Then add `.libPaths(c(normalizePath("./libs"), .libPaths()))`
as the first line in `app.R`.

```r
## clone this project here:
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
  ref = "v0.1.0", 
  host = "https://github.roche.com/api/v3"
)

devtools::install_github("Roche/rtables", ref = "v0.1.0")

devtools::install_github(
  repo = "Rpackages/tern",
  ref = "v0.5.0", 
  host = "https://github.roche.com/api/v3",
  upgrade_dependencies = FALSE, build_vignettes = FALSE
)

devtools::install_github(
  repo = "Rpackages/teal",
  ref = "v0.0.3", 
  host = "https://github.roche.com/api/v3",
  upgrade_dependencies = FALSE, build_vignettes = FALSE
)

devtools::install_github(
  repo = "Rpackages/teal.tern",
  ref = "v0.5.0", 
  host = "https://github.roche.com/api/v3",
  upgrade_dependencies = FALSE, build_vignettes = FALSE
)

.libPaths(orig_libpaths)
```


[ghs]: http://pages.github.roche.com/Rpackages/teal.tern