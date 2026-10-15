\name{run_csem_gui}
\alias{run_csem_gui}
\alias{csem_gui_app}
\title{Launch the cSEM GUI}
\usage{
run_csem_gui(
  launch.browser = TRUE,
  port = getOption("shiny.port"),
  host = getOption("shiny.host", "127.0.0.1"),
  ...
)

csem_gui_app()
}
\arguments{
\item{launch.browser}{Open the app in the default web browser? Default \code{TRUE}.}

\item{port}{Port to listen on. \code{NULL} (default) lets Shiny pick a free port.}

\item{host}{Host/IP address. Default \code{"127.0.0.1"} (this computer only).}

\item{...}{Further arguments passed to \code{\link[shiny:runApp]{shiny::runApp()}}.}
}
\value{
\code{run_csem_gui()} runs until the app is stopped and returns \code{NULL}
invisibly. \code{csem_gui_app()} returns a \code{\link[shiny:shinyApp]{shiny::shinyApp()}} object.
}
\description{
Starts the interactive Shiny application: a point-and-click interface for
PLS-SEM with the \pkg{cSEM} package (model builder, PLS / consistent PLS,
parallel bootstrapping, IPMA, confirmatory tetrad analysis, PLSpredict and
CVPAT, reports with Excel export). The application script is shipped
unchanged in the package (\code{inst/app/app.R}).
}
\details{
\code{run_csem_gui()} starts the app on this computer and blocks the R console
until the app is stopped (press Esc / the stop button). \code{csem_gui_app()}
returns the app as a \code{\link[shiny:shinyApp]{shiny::shinyApp()}} object instead, which is what a
server needs: put \code{csemapp::csem_gui_app()} into an \code{app.R} file for
shiny-server, shinyapps.io or Posit Connect.

Call \code{\link[=csem_gui_guide]{csem_gui_guide()}} for a step-by-step user guide.
}
\examples{
if (interactive()) run_csem_gui()
}
\seealso{
\code{\link[=csem_gui_guide]{csem_gui_guide()}}
}
