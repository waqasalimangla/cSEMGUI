# csemapp

**A point-and-click interface for PLS-SEM, built on the [cSEM](https://CRAN.R-project.org/package=cSEM) R package.**

`csemapp` ships a Shiny application ("cSEM GUI") that lets you draw a structural equation model, estimate it, and read or export the results, without writing R code once the app is running. All estimation is performed by **cSEM**; the GUI adds the interface, the bootstrapping / CTA / IPMA workflows and the report tables.

![License: GPL-3](https://img.shields.io/badge/license-GPL--3-blue.svg)
![R >= 4.0](https://img.shields.io/badge/R-%E2%89%A5%204.0-276DC3.svg)

## Features

- **Drag-and-drop model builder**: reflective and composite (Mode A / Mode B) constructs, moderators (interaction terms) and quadratic effects
- **Estimation**: PLS-SEM and Consistent PLS (PLSc), with the main cSEM algorithm options exposed in dialogs
- **Bootstrapping**: parallel and seed-reproducible, for PLS and PLSc; percentile or bias-corrected confidence intervals, one- or two-tailed tests
- **Importance-performance map analysis (IPMA)** at construct and indicator level
- **Confirmatory tetrad analysis (CTA-PLS)** with p-value adjustment
- **Results diagram** showing loadings/weights, path coefficients and R²; export as SVG
- **cSEM-style reports**: reliability and validity, HTMT / HTMT2, Fornell-Larcker, VIF, f², model fit and more. Copy any table to Word/Excel or download Excel workbooks

## Installation

```r
# 1. Dependencies (once)
install.packages(c("shiny", "cSEM", "writexl", "jsonlite"))

# 2. The package (from a source folder or tar.gz)
install.packages("path/to/csemapp", repos = NULL, type = "source")
# or: remotes::install_local("path/to/csemapp")
```

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

1. **Load data**: in the *Model builder* tab, upload a CSV file or tick *Use cSEM demo data (satisfaction)*. With the demo data, *Load example model* builds the classic IMAG-EXPE-QUAL-VAL-SAT-LOY model.
2. **Draw the model**: drag variables onto the canvas and connect constructs with arrows.
3. **Choose the measurement mode** of each construct (reflective or composite).
4. **Calculate**: open the *Calculate* menu (top right) and pick an analysis.
5. **Read the results** in the tabs that appear; copy tables to Word/Excel or download Excel files.

<p align="center">
  <img src="docs/images/model-builder.png" width="900" alt="A PLS-SEM model built in the csemapp Model builder using the file mobi250.csv">
  <br>
  <em>Example: a PLS-SEM model with seven constructs built entirely in the csemapp Model builder (data: <code>mobi250.csv</code>).</em>
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

### Confirmatory tetrad analysis (CTA)

Tests whether the indicators of each reflective construct with at least four indicators are consistent with a common-factor model (vanishing tetrads). If at least one non-redundant tetrad is significantly different from zero, a reflective specification is rejected and a composite / formative one should be considered.

<img width="899" height="257" alt="CTA settings" src="https://github.com/user-attachments/assets/7279e0c4-2ba1-435b-bcd9-acb78bafe831" />

## Results

- **Results diagram**: the model with loadings (reflective) or weights (composite) on the indicator arrows, path coefficients on the paths (interaction and quadratic terms included) and R² under the endogenous constructs. *Save SVG* downloads it.
- **cSEM report**: path coefficients, total / indirect / specific indirect effects, outer loadings and weights, latent variable scores and descriptives, residuals, R² (and adjusted), f², construct reliability and validity (Cronbach's alpha, rho_a, rho_c, AVE), discriminant validity (Fornell-Larcker, HTMT, HTMT2), collinearity (VIF) and model fit (SRMR, d_ULS, d_G, chi-square, NFI). *Download Excel report* saves every section as a worksheet.
- **Bootstrapping report**: one table per section with original sample, sample mean, standard deviation, T statistic, P value, confidence-interval limits and a Significant Yes/No column. *Download Excel (bootstrapping)* saves all sections.
- **Confirmatory tetrad analysis**: a summary per construct plus one table of tetrads per construct.
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

All estimation is done by the **cSEM** package. Please cite cSEM and this package in publications.

## License

GPL (>= 3)
