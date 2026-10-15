\name{csem_gui_guide}
\alias{csem_gui_guide}
\title{Print the cSEM GUI user guide}
\usage{
csem_gui_guide(vignette = FALSE)
}
\arguments{
\item{vignette}{If \code{TRUE}, open the HTML vignette instead (needs the
vignettes to be installed). If the vignette is not available, the guide is
printed to the console.}
}
\value{
Invisibly, the guide as a character vector (one element per line).
}
\description{
Prints the step-by-step user guide to the R console, or opens it as a
vignette. The guide explains the model builder, every analysis in the
Calculate menu, how to read the report tables and how to export results.
}
\examples{
csem_gui_guide()
}
\seealso{
\code{\link[=run_csem_gui]{run_csem_gui()}}
}
