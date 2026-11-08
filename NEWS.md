# csemapp 0.3.0

* New app version: **PLSpredict** and **CVPAT** (k-fold cross-validated prediction and
  predictive ability test) added to the Calculate menu, with their own result tabs and
  Excel downloads. They also run for models with moderators and quadratic effects (the app
  performs the structural prediction itself); for linear models the PLSpredict tab also
  shows the output of `cSEM::predict()` against a selectable benchmark.
* Moderators: the main effect M -> Y is now added whenever it is missing, also when M already predicts, or is predicted by, other constructs (previously it was only added if M appeared nowhere else in the model). It is skipped only if the reverse path Y -> M exists.
* The built-in demo data option is now **Moderated Mediation Model** (bundled in `inst/extdata/`); ticking it draws the default model (SAT -> USAGE -> CON, SAT -> CON, NHI moderating SAT -> USAGE), which can be edited afterwards. The cSEM `satisfaction` demo data and its example-model button were removed.
* Documentation: help pages for every exported function, a user guide
  (`csem_gui_guide()`), a vignette and tests.
* The app script is shipped unchanged in `inst/app/app.R` and launched by `run_csem_gui()`.
* New `csem_gui_app()` returns the app as a Shiny app object for server deployment.
* Package metadata completed: author, copyright holder and repository URLs (MIT licence); `remotes::install_github()` installation route and `inst/CITATION` added.

# csemapp 0.2.0

* The cSEM GUI script is shipped unchanged in `inst/app/app.R` and launched by `run_csem_gui()`.
