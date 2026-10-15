# csemapp

Point-and-click **PLS-SEM** on top of [cSEM](https://cran.r-project.org/package=cSEM).

- drag-and-drop model builder (constructs, paths, moderators, quadratic effects)
- PLS and consistent PLS (PLSc)
- bootstrapping with parallel workers
- importance-performance map analysis (IPMA)
- confirmatory tetrad analysis (CTA)
- PLSpredict and CVPAT (k-fold cross-validated prediction)
- results diagram and cSEM-style report tables with Excel export

## Installation

```r
# install.packages("remotes")
remotes::install_github("YOUR-USERNAME/csemapp")
```

From a downloaded source archive:

```r
install.packages("path/to/csemapp_0.3.0.tar.gz", repos = NULL, type = "source")
```

Install in a fresh R session (restart R first if an older `csemapp` is loaded).
The package depends on `shiny`, `cSEM`, `writexl` and `jsonlite`; they are
installed automatically with `remotes::install_github()`.

## Usage

```r
library(csemapp)
run_csem_gui()      # opens the app in your browser
csem_gui_guide()    # prints the step-by-step user guide
?run_csem_gui       # help page
```

To deploy on a server (shiny-server, shinyapps.io, Posit Connect), use an `app.R`
containing `csemapp::csem_gui_app()`.

The application script is shipped unchanged in `inst/app/app.R`, so it behaves
exactly like running `shiny::runApp("app_csem.R")`.

## Documentation

| Where | What |
| --- | --- |
| `?run_csem_gui` | launching the app, deployment |
| `csem_gui_guide()` | complete user guide (also in `inst/guide/user-guide.md`) |
| `vignette("csemapp-guide")` | short introduction (when vignettes are installed) |
| `NEWS.md` | changes between versions |

## License

MIT. See `LICENSE.md`.
