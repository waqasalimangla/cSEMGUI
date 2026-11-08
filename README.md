# csemapp

**A point-and-click interface for PLS-SEM, built on the [cSEM](https://CRAN.R-project.org/package=cSEM) R package.**

`csemapp` ships a Shiny application ("cSEM GUI") that lets you draw a structural equation model, estimate it, and read or export the results, without writing R code once the app is running. All estimation is performed by **cSEM**; the GUI adds the interface, the bootstrapping / CTA / IPMA workflows and the report tables.

![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)
![R >= 4.1](https://img.shields.io/badge/R-%E2%89%A5%204.1-276DC3.svg)

## Features

- **Drag-and-drop model builder**: reflective and composite (Mode A / Mode B) constructs, moderators (interaction terms) and quadratic effects
- **Estimation**: PLS-SEM and Consistent PLS (PLSc), with the main cSEM algorithm options exposed in dialogs
- **Bootstrapping**: parallel and seed-reproducible, for PLS and PLSc; percentile or bias-corrected confidence intervals, one- or two-tailed tests
- **Importance-performance map analysis (IPMA)** at construct and indicator level
- **Confirmatory tetrad analysis (CTA-PLS)** with p-value adjustment
- **PLSpredict and CVPAT**: k-fold cross-validated out-of-sample prediction (Q² predict, RMSE, MAE against linear-model and indicator-average benchmarks) and the cross-validated predictive ability test
- **Results diagram** showing loadings/weights, path coefficients and R²; export as SVG
- **cSEM-style reports**: reliability and validity, HTMT / HTMT2, Fornell-Larcker, VIF, f², model fit and more. Copy any table to Word/Excel or download Excel workbooks

## Installation

```r
# 1. Dependencies (once)
install.packages(c("shiny", "cSEM", "writexl", "jsonlite"))

# 2. The package, straight from GitHub (recommended)
install.packages("remotes")
remotes::install_github("waqasalimangla/cSEMGUI")

# ... or a fixed, citable release (version 0.3.0)
remotes::install_github("waqasalimangla/cSEMGUI@v0.3.0")
```

**Alternative: install from a local copy** (a downloaded source folder or `.tar.gz`):

```r
install.packages("path/to/csemapp", repos = NULL, type = "source")
# or: remotes::install_local("path/to/csemapp")
```

Then start the app:

```r
library(csemapp)
run_csem_gui()
```

Requires R (>= 4.1.0). Released versions are listed under [Releases](https://github.com/waqasalimangla/cSEMGUI/releases)
and in [NEWS.md](NEWS.md); each release is tagged `vX.Y.Z`.

## Usage

```r
library(csemapp)
run_csem_gui()      # opens the app in your browser
csem_gui_guide()    # prints the user guide
```

Useful arguments: `run_csem_gui(launch.browser = FALSE, port = 4321)`.

To deploy on a server (shiny-server, shinyapps.io, Posit Connect), put one line in `app.R`:

```r
csemapp::csem_gui_app()
```

## Quick start

1. **Load data**: in the *Model builder* tab, upload a CSV file or tick *Moderated Mediation Model* to use the bundled data set. Its default model (SAT -> USAGE -> CON, SAT -> CON, NHI moderating SAT -> USAGE) is drawn automatically and can be edited afterwards.
2. **Draw the model**: drag variables onto the canvas and connect constructs with arrows.
3. **Choose the measurement mode** of each construct (reflective or composite).
4. **Calculate**: open the *Calculate* menu (top right) and pick an analysis.
5. **Read the results** in the tabs that appear; copy tables to Word/Excel or download Excel files.

<p align="center">
  <img width="708" height="398" alt="Model builder" src="https://github.com/user-attachments/assets/e0c43911-9f68-4fcf-bffb-2a5874a27ba3" />
  <br>
  <em>Example: a moderated mediation PLS-SEM model with four constructs (SAT, USAGE, CON and NHI) built in the csemapp Model builder (data: <code>ModeratedMediation.csv</code>, bundled with the package). NHI moderates the path SAT &rarr; USAGE.</em>
</p>

<p align="center">
  <img width="684" height="418" alt="Results diagram" src="https://github.com/user-attachments/assets/22f7cbd7-8185-4f19-87c3-049619705efe" />
  <br>
  <em>Example: the results diagram of the same four-construct model (data: <code>ModeratedMediation.csv</code>) after estimation, showing loadings, path coefficients, the NHI &times; SAT interaction effect and R&sup2;.</em>
</p>

## Data requirements

- CSV file with a header row, one column per indicator, numeric indicator columns
- Column names are made syntactically valid (`make.names`): spaces become dots, duplicates get a suffix
- Files up to 100 MB are accepted
- The left panel lists all variables (MET = numeric, CAT = text); use *Filter variables...* to search. Variables already used in the model are marked.

<img width="898" height="544" alt="Model builder" src="https://github.com/user-attachments/assets/5167269f-ac4b-4d9a-9fda-16caf39080a9" />

## Model builder

**Quadratic effect** (button at the top right) switches the canvas to quadratic mode. Every path gets a *Click* label; click it to add the quadratic (curvilinear) effect of the path's source construct, or *Remove* to delete it. Press the button again (or Esc) to finish.

**Adding indicators**: drag a variable from the left list onto a construct, or drop it on an empty spot to create a new construct. Shift/Ctrl+click selects several variables to drag at once. Every construct needs at least one indicator before you can calculate.

**Right-hand panel**

| Section | Function |
|---|---|
| Indicator position | Place indicators left / right / above / below; *Rotate* turns them clockwise |
| Selected construct | Name and measurement mode: Reflective (common factor), Composite Mode A, Composite Mode B (formative) |
| Move selected | Nudge buttons with a step size in pixels |
| Align constructs | Align vertically / horizontally / left / right / top / bottom; equal spacing for three or more constructs |
| View | Auto-arrange, Fit, zoom, Save SVG, Indicators follow construct, Snap to grid |

The arrangement made here is re-used in the results diagram.

<img width="893" height="642" alt="Calculate menu" src="https://github.com/user-attachments/assets/c6a0035f-7fec-4895-9b9d-ec0bd55c41e5" />

## Analyses

### PLS-SEM and Consistent PLS (PLSc)

Plain PLS treats common factors like composites (loadings slightly too high, paths slightly too low). PLSc corrects the correlations of the *reflective* constructs for measurement error; composite constructs (Mode A / B) are not corrected.

<img width="893" height="644" alt="Algorithm settings" src="https://github.com/user-attachments/assets/aa2ed508-ae6e-4e0f-b696-d80506dc26c7" />

### Importance-performance map analysis (IPMA)

Choose the target construct (an endogenous construct). *Importance* is the unstandardized total effect on the target; *performance* is the mean of the 0-100 rescaled construct (or indicator) scores. In the IPMA tab you can switch the target and the map level (constructs / indicators), view the map and tables, and download them. Warnings are shown for negative outer weights and for constructs that cannot be included (e.g. interaction terms).

### PLSpredict and CVPAT

*Calculate → PLSpredict & CVPAT* runs one k-fold cross-validation (default 10 folds × 10 repetitions, seed 123) of the current PLS model and fills two tabs. **PLSpredict** reports Q² predict, RMSE and MAE for every indicator of the endogenous constructs against a linear-model (LM) and an indicator-average (IA) benchmark, plus a predictive-power assessment per construct. **CVPAT** tests whether the PLS model has a significantly lower prediction loss than these benchmarks. Both also work for models with moderators and quadratic effects (cSEM::predict() does not support nonlinear models, so the app then performs the structural prediction itself); for linear models the PLSpredict tab additionally shows the output of cSEM::predict() against a benchmark of your choice.

### Confirmatory tetrad analysis (CTA)

Tests whether the indicators of each reflective construct with at least four indicators are consistent with a common-factor model (vanishing tetrads). If at least one non-redundant tetrad is significantly different from zero, a reflective specification is rejected and a composite / formative one should be considered.

<img width="899" height="257" alt="CTA settings" src="https://github.com/user-attachments/assets/7279e0c4-2ba1-435b-bcd9-acb78bafe831" />

## Results

- **Results diagram**: the model with loadings (reflective) or weights (composite) on the indicator arrows, path coefficients on the paths (interaction and quadratic terms included) and R² under the endogenous constructs. *Save SVG* downloads it.
- **cSEM report**: path coefficients, total / indirect / specific indirect effects, outer loadings and weights, latent variable scores and descriptives, residuals, R² (and adjusted), f², construct reliability and validity (Cronbach's alpha, rho_a, rho_c, AVE), discriminant validity (Fornell-Larcker, HTMT, HTMT2), collinearity (VIF) and model fit (SRMR, d_ULS, d_G, chi-square, NFI). *Download Excel report* saves every section as a worksheet.
- **Bootstrapping report**: one table per section with original sample, sample mean, standard deviation, T statistic, P value, confidence-interval limits and a Significant Yes/No column. *Download Excel (bootstrapping)* saves all sections.
- **Confirmatory tetrad analysis**: a summary per construct plus one table of tetrads per construct.
- **PLSpredict / CVPAT**: prediction metrics, PLS-minus-benchmark differences, predictive-power assessment and the CVPAT tests, each with an Excel download.
- **IPMA**: map and tables, as described above.

Every table has a **Copy to Excel/Word** button; formatting is preserved when pasting.

<img width="896" height="592" alt="Results" src="https://github.com/user-attachments/assets/d33351ab-64ee-4e0c-8e02-2de53d04fe40" />

## R functions

| Function | Purpose |
|---|---|
| `run_csem_gui()` | Start the app |
| `csem_gui_app()` | Return the Shiny app object (for server deployment) |
| `csem_gui_guide()` | Print the user guide |

## Citation

All estimation is done by the **cSEM** package. Please cite cSEM and this package in publications; `citation("csemapp")` prints the reference for the installed version.

## License

MIT, see [LICENSE.md](LICENSE.md). Copyright (c) 2026 Waqas Ali.

## Author and support

Waqas Ali, Department of Business Administration, Mirpur University of Science and Technology (MUST), Pakistan. Questions: Waqas.mbs@must.edu.pk. Bugs and feature requests: <https://github.com/waqasalimangla/cSEMGUI/issues>.
