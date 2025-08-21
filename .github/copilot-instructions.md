# teal.modules.clinical

Always reference these instructions first and fallback to search or bash commands only when you encounter unexpected information that does not match the info here.

`teal.modules.clinical` is an R package providing clinical trial analysis modules for the `teal` framework. The package contains 45+ modules for tables, graphs, and statistical models commonly used in clinical trials, including MMRM, Cox regression, Kaplan-Meier plots, forest plots, and patient profile modules.

## Working Effectively

### Essential Setup and Build Process

**CRITICAL: All build and test commands can take 30-60+ minutes. NEVER CANCEL long-running operations.**

Bootstrap and install dependencies:
- Install R (≥ 4.0): Use system package manager or download from https://cran.r-project.org
- Install system dependencies: `sudo apt-get install -y build-essential libcurl4-gnutls-dev libxml2-dev libssl-dev libfontconfig1-dev libharfbuzz-dev libfribidi-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev`
- Set up R library path: `mkdir -p ~/R/library && echo 'R_LIBS_USER="~/R/library"' >> ~/.Renviron`

Install package dependencies:
- `R -e "install.packages(c('devtools', 'pak'), repos='https://cran.r-project.org')"`
- `R -e "pak::pak(c('insightsengineering/teal', 'insightsengineering/tern', 'insightsengineering/teal.transform', 'insightsengineering/teal.data'))"`
- `R -e "devtools::install_deps('.')"`

**WARNING: Dependency installation takes 45-60 minutes. NEVER CANCEL. Set timeout to 75+ minutes.**

### Building the Package

Build package (basic, without vignettes):
- `R CMD build . --no-build-vignettes` -- takes less than 1 minute
- Creates: `teal.modules.clinical_<version>.tar.gz` (~860KB)

Build package (full with vignettes):
- `R CMD build .` -- takes 15-20 minutes. NEVER CANCEL. Set timeout to 30+ minutes.

### Testing and Validation

Run unit tests:
- `R -e "devtools::test()"` -- takes 15-25 minutes. NEVER CANCEL. Set timeout to 35+ minutes.
- Alternative: `R CMD check <package>.tar.gz` -- takes 30-45 minutes. NEVER CANCEL. Set timeout to 60+ minutes.

Run specific test suites:
- Unit tests only: `R -e "testthat::test_dir('tests/testthat', filter = '^test-(?!shinytest)', perl = TRUE)"`
- Shinytest2 tests: `R -e "testthat::test_dir('tests/testthat', filter = 'shinytest2')"`

**WARNING: Shinytest2 tests require display/browser environment and take 20-30 minutes. Set timeout to 45+ minutes.**

### Linting and Style Checks

Run linting:
- `R -e "lintr::lint_package()"` -- takes 2-5 minutes
- Configuration in `.lintr` file (snake_case naming, 120 char line length)

Run style checks:
- `R -e "styler::style_pkg()"` -- takes 3-5 minutes

**ALWAYS run these before committing or CI will fail.**

### Running Applications

Test basic functionality:
```r
library(teal.modules.clinical)
data(tmc_ex_adsl, tmc_ex_adae)

app <- teal::init(
  data = teal.data::cdisc_data(
    ADSL = tmc_ex_adsl,
    ADAE = tmc_ex_adae,
    code = "ADSL <- tmc_ex_adsl; ADAE <- tmc_ex_adae"
  ),
  modules = list(
    tm_g_barchart_simple(
      label = "ADAE Bar Chart",
      x = teal.transform::data_extract_spec(
        dataname = "ADAE",
        select = teal.transform::select_spec(
          choices = teal.transform::variable_choices(tmc_ex_adae, c("ARM", "SEX", "RACE")),
          selected = "ARM"
        )
      )
    )
  )
)

# Launch app - opens in browser
shiny::shinyApp(app$ui, app$server)
```

### Documentation Generation

Build documentation:
- `R -e "devtools::document()"` -- takes 2-3 minutes
- `R -e "pkgdown::build_site()"` -- takes 10-15 minutes. NEVER CANCEL. Set timeout to 25+ minutes.

## Validation

**CRITICAL: Always validate changes through complete scenarios, not just builds.**

Essential validation workflow after making changes:
1. **Build validation**: `R CMD build . --no-build-vignettes`
2. **Dependency check**: Verify all imports are available
3. **Unit test validation**: Run relevant test files for your changes
4. **Module functionality test**: Create and run a simple teal app using affected modules
5. **Example validation**: Test at least one example from modified module documentation
6. **Style validation**: `R -e "lintr::lint_package(); styler::style_pkg()"`

Manual testing scenarios:
- Always test module initialization without errors
- Test basic module functionality with example data
- Verify output generation (tables/plots) works correctly
- Test parameter changes update outputs appropriately

## Common Tasks

### Package Structure Reference
```
Repository root structure:
DESCRIPTION              # Package metadata and dependencies
NAMESPACE               # Package exports/imports
R/                      # Package source code (45+ modules)
├── tm_g_*.R           # Graph modules (15+ modules)
├── tm_t_*.R           # Table modules (20+ modules)  
├── tm_a_*.R           # Analysis modules (MMRM, GEE)
├── utils.R            # Utility functions
└── zzz.R              # Package startup
data/                   # Example datasets (12 .rda files)
tests/testthat/         # Test suite (76 test files)
├── test-*.R           # Unit tests (40 files)
└── test-shinytest2-*.R # UI/integration tests (36 files)
vignettes/              # Documentation and examples
man/                    # Generated help files
.github/workflows/      # CI/CD pipelines
├── check.yaml         # Main check workflow
├── docs.yaml          # Documentation build
└── scheduled.yaml     # Maintenance tasks
```

### Key Dependencies (automatically managed by staged_dependencies.yaml)
```
Core teal ecosystem:
- teal (>= 0.16.0.9002)
- teal.transform (>= 0.6.0) 
- teal.data (>= 0.7.0)
- tern (>= 0.9.7.9016)

Analysis packages:
- tern.mmrm (>= 0.3.1)
- tern.gee (>= 0.1.5)

UI/plotting:
- shiny (>= 1.8.1)
- ggplot2 (>= 3.4.0)
- DT (>= 0.13)
```

### Module Categories
- **Graph modules**: `tm_g_*` - Interactive plots (forest plots, KM curves, line plots, etc.)
- **Table modules**: `tm_t_*` - Statistical tables (summary tables, safety tables, etc.)
- **Analysis modules**: `tm_a_*` - Statistical models (MMRM, GEE)
- **Patient profile modules**: `tm_*_pp_*` - Patient-level views

### Testing Depth Control
- Environment variable `TESTING_DEPTH` controls test scope (default: 3, CI uses: 5)
- Higher values = more comprehensive but slower tests
- Set `TESTING_DEPTH=1` for faster development testing

### Staged Dependencies
- Package uses `staged_dependencies.yaml` for dependency management
- Branch names must match across related repositories for development
- Use format: `<issue_id>_<description>` for single-repo changes
- Use format: `<issue_id>_<issue_repo>_<description>` for multi-repo changes

### CI/CD Integration  
- GitHub Actions run on every PR/push to main
- Full check takes 45-60 minutes including all test suites
- Coverage reports generated automatically
- Documentation deployed to GitHub Pages

## Known Issues and Limitations

- **Build time**: Full builds with all tests can exceed 60 minutes
- **Memory usage**: Large dependency tree requires significant RAM during installation
- **Browser dependencies**: Shinytest2 tests require Chrome/Chromium for UI testing
- **Network dependency**: Installation requires stable internet for GitHub packages
- **Platform-specific**: Some tests may behave differently on different operating systems

## Troubleshooting

**Build fails with missing packages**: Install dependencies first with `pak::pak('.')` or `devtools::install_deps('.')`

**Tests timeout**: Increase timeout values, especially for shinytest2 (45+ minutes recommended)

**Shinytest2 fails**: Ensure Chrome/Chromium is installed and `TESTING_DEPTH` is appropriate

**Linting errors**: Check `.lintr` configuration and run `styler::style_pkg()` before committing

**Memory issues during testing**: Run tests in smaller batches or reduce `TESTING_DEPTH`

Always build and exercise your changes before committing. The comprehensive test suite in CI will catch issues, but local validation prevents wasted CI cycles.