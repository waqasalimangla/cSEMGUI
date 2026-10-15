# cSEM GUI - user guide

The cSEM GUI is a point-and-click interface for partial least squares structural
equation modelling (PLS-SEM) built on the cSEM package. You draw the model on a
canvas, choose an analysis from the Calculate menu and read the results in
report tables that can be copied to Word / Excel or downloaded as Excel files.

Start the app with:

    library(csemapp)
    run_csem_gui()

## 1. Quick start (5 minutes)

1. Start the app (`run_csem_gui()`). It opens in your web browser.
2. In the left panel tick **Use cSEM demo data (satisfaction)**. The variables
   of the data set appear in the list.
3. Click **Load example model**. A model with six constructs (IMAG, EXPE, QUAL,
   VAL, SAT, LOY) is drawn on the canvas.
4. Click **Calculate** (top right) and choose **PLS-SEM algorithm**, then
   **Start calculation**.
5. The **Results diagram** tab shows loadings, path coefficients and R-square.
   The **cSEM report** tab has all tables.

## 2. Loading data

- **Upload CSV**: a comma-separated file with a header row. Uploads up to
  100 MB are accepted. Column names are made syntactically valid
  (for example spaces become dots) and made unique.
- **Use cSEM demo data**: the `satisfaction` data set of the cSEM package.
- Numeric variables are labelled `MET`, all others `CAT`. Only numeric
  variables can be used as indicators.
- Missing values: clean or impute the data before uploading. The app does not
  offer its own missing-value handling, and rows with missing indicator values
  can lead to errors or to fewer observations being used.

## 3. Building the model

The canvas is shared by the **Model builder** tab and the **Results diagram**
tab, so the arrangement you make while building is kept in the results.

### 3.1 Constructs and indicators

- Drag one or several variables from the list onto an empty part of the canvas
  to create a construct (the name is derived from the indicator names).
- Drag variables onto an existing construct to add them as its indicators.
  Use Shift+click, Ctrl+click or Shift/Ctrl + arrow keys in the list to select
  several variables. Variables already used are greyed out.
- Alternatively choose the **Latent variable** tool and click on the canvas.
- Select a construct to edit its **name** and **measurement mode** in the right
  panel (or double-click it to rename):
  - *Reflective (common factor)* - indicators are caused by the construct.
  - *Composite - Mode A* - correlation weights (default composite).
  - *Composite - Mode B (formative)* - regression weights.

### 3.2 Paths

- Choose the **Connect** tool. Drag from the source construct to the target, or
  click the source and then the target. A path points from predictor to
  outcome.
- Every construct that receives an arrow becomes endogenous.

### 3.3 Moderators and quadratic effects

- **Moderator** tool: drag from the moderating construct onto a path (the
  arrow), or click the path and then the moderator. The app adds the
  interaction term and the main effect of the moderator automatically.
- **Quadratic effect** (button in the header): every direct path shows a
  *Click* label. Click it to add the quadratic effect of the source construct;
  *Remove* deletes it again. Press Esc or the button to finish.
- Models with moderators or quadratic effects cannot be used with 2SLS and are
  not supported by PLSpredict / CVPAT.

### 3.4 Editing and layout

| Action | How |
| --- | --- |
| Move | drag; arrow keys nudge (Shift = 5 times as far); the step size is in the right panel |
| Select several | Shift+click |
| Delete | Del key or the Delete button |
| Undo / redo | Ctrl+Z / Ctrl+Y or the buttons |
| Clear everything | Clear button (asks for confirmation) |
| Zoom / pan | mouse wheel / drag the background |
| Indicator side | the four arrow buttons (left, up, right, down) for the selected construct(s) |
| Rotate indicators | Rotate button (all constructs if nothing is selected) |
| Align / spread | select two or more constructs, then use the Align buttons |
| Hide / show indicators | Alt+X / Alt+C for the selected constructs |
| Reset layout | Auto-arrange |
| Export picture | Save SVG |

Options: *Indicators follow construct*, *Snap to grid*, and *Alignment guides
while dragging*.

## 4. The Calculate menu

Click **Calculate** and choose an analysis. A dialog shows the settings; press
the blue button to start.

### 4.1 PLS-SEM algorithm

Estimates the model with the PLS algorithm.

- *Weighting approach*: PLS-PM.
- *Inner weighting scheme*: path (default), centroid or factorial.
- *Path estimator*: OLS (default) or 2SLS.
- *Stop criterion* and *maximum iterations* of the algorithm.

Common-factor constructs are treated like composites, so loadings are slightly
overestimated and path coefficients slightly underestimated. Use the consistent
algorithm to correct this.

### 4.2 Consistent PLS-SEM algorithm (PLSc)

Runs PLS and then corrects the correlations of the common-factor constructs for
measurement error. Choose the *correction factor approach*. Composite
constructs are not corrected.

### 4.3 Bootstrapping and consistent PLS bootstrapping

Re-estimates the model on random resamples of the data to obtain standard
errors, t values, p values and confidence intervals.

- *Number of resamples*: 500 for a quick look, 5000 for final results.
- *Seed*: makes the result reproducible.
- *Test type*: two-tailed or one-tailed. *Significance level*: 0.10 to 0.001.
- *Confidence interval method*: percentile or bias-corrected.
- *Include model fit* (SRMR, d_ULS, d_G): slower; switch it off for a much
  faster run.
- *Parallel workers*: number of CPU cores. The default leaves one core free.
  Results are identical for any number of workers.

Results appear in the **Bootstrapping report** tab. Test type, significance
level and interval method can be changed afterwards without re-running.

### 4.4 IPMA (importance-performance map analysis)

Choose the target construct. Importance is the unstandardised total effect on
the target; performance is the mean of the 0-100 rescaled scores. The
**IPMA** tab shows the map and the tables, at construct or indicator level.

### 4.5 Confirmatory tetrad analysis (CTA)

Tests whether the indicators of a construct can be modelled as reflective
(common factor). Needs reflective constructs with at least four indicators.
Settings: bootstrap resamples (default 499), seed, significance level and
p-value adjustment (none, Bonferroni, Holm, Hochberg, Hommel, BH, BY). If at
least one tetrad differs significantly from zero, a reflective specification is
rejected and a composite / formative specification should be considered.

### 4.6 PLSpredict and CVPAT

Runs one k-fold cross-validation and fills two tabs.

- *Number of folds* (default 10), *repetitions* (default 10), *seed*.
- *Prediction approach*: earliest antecedent (cSEM default) or direct antecedent.
- *Inadmissible results in a fold*: stop, ignore, or set predictions to NA.
- *CVPAT test type and significance level* (can be changed afterwards).

The **PLSpredict** tab reports Q2 predict, RMSE and MAE of the PLS model for
every indicator of the endogenous constructs, compared with a linear model (LM)
and an indicator-average (IA) benchmark, plus a predictive power assessment per
construct. The **CVPAT** tab tests whether PLS predicts significantly better
than the benchmarks. These analyses work only for linear models (no
moderators or quadratic effects).

## 5. Reading the report tables

Tables use colours to flag the usual cut-offs (green = met, red = not met):

| Table | Rule |
| --- | --- |
| Outer loadings | at least 0.70 |
| Cronbach's alpha, rho_a, rho_c | at least 0.70 |
| AVE | at least 0.50 |
| HTMT | below 0.90 |
| Fornell-Larcker | diagonal (square root of AVE) larger than the correlations |
| VIF | at most 5 |
| SRMR | below 0.08 |
| NFI | at least 0.90 |
| PLSpredict | Q2 predict above 0; PLS minus benchmark below 0 |

The **cSEM report** tab has a selector with these sections: Path coefficients,
Total indirect effects, Specific indirect effects, Total effects, Outer
loadings, Outer weights, Latent variables, Residuals, R-square, f-square,
Construct reliability and validity, Discriminant validity, Collinearity
statistics (VIF) and Model fit.

Each table has a **Copy** button (paste into Word or Excel with formatting) and
every tab has a **Download Excel** button.

## 6. Troubleshooting

- *"Drag indicators onto these constructs first"*: a construct has no
  indicators. Drag variables onto it or delete it.
- *"Define constructs and paths first"*: the model needs at least one
  construct and one path.
- *PLSpredict / CVPAT refuse to run*: the model contains a moderator or a
  quadratic effect.
- *CTA refuses to run*: no reflective construct has four or more indicators.
- *Bootstrapping is slow*: reduce the number of resamples, switch off
  *Include model fit*, or increase the number of parallel workers.
- *Missing package*: the app lists what to install, for example
  `install.packages("writexl")`.
- *Parallel workers cannot start*: the app falls back to a single core.

## 7. Citing

Please cite the cSEM package and the original methodological papers for the
analyses you report (PLS-SEM, PLSc, HTMT, CTA-PLS, IPMA, PLSpredict, CVPAT).
Use `citation("cSEM")` in R.
