# We follow the instructions on https://pages.github.roche.com/NEST/docs/hugo/NEST/agile-R/devel/quick_start/installation/

# install.packages(c( "colorspace", "ggplot2",  "scales",  "gridExtra",  "tibble",  "dplyr", "testthat",  "knitr",  "rmarkdown",  "forcats",  "lattice"), repos = "http://cran.rstudio.com")

install_nest <- function(pkgnames, ref = "master") {
  lapply(pkgnames, function(x) {
    devtools::install_github(
      repo = paste0("NEST/", x),
      ref = ref,
      host = "https://github.roche.com/api/v3",
      upgrade_dependencies = FALSE, build_vignettes = FALSE
    )
  })
}


devtools::install_github(repo = "Roche/rtables", ref = "v0.1.1")

install_nest("random.cdisc.data", ref = "v0.2.0")

install_nest("tern", ref = "v0.6.0")

install_nest("teal", ref = "v0.6.0")

install_nest("teal.modules.clinical", ref = "v0.6.0")

# devtools::install_github(repo = "Roche/rtables", ref = "devel")
# 
# install_nest("random.cdisc.data", ref = "devel")
# 
# install_nest("tern", ref = "devel")
# 
# install_nest("teal", ref = "devel")
# 
# install_nest("teal.modules.clinical", ref = "devel")
