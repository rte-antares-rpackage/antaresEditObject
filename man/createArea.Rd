% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createArea.R
\name{createArea}
\alias{createArea}
\title{Create an area in an Antares study}
\usage{
createArea(
  name,
  color = grDevices::rgb(230, 108, 44, max = 255),
  localization = c(0, 0),
  nodalOptimization = nodalOptimizationOptions(),
  filtering = filteringOptions(),
  adequacy = adequacyOptions(),
  overwrite = FALSE,
  opts = antaresRead::simOptions()
)
}
\arguments{
\item{name}{Name of the area as a character, without punctuation except - and _.}

\item{color}{Color of the node}

\item{localization}{Localization on the map}

\item{nodalOptimization}{Nodal optimization parameters, see \code{\link[=nodalOptimizationOptions]{nodalOptimizationOptions()}}.}

\item{filtering}{Filtering parameters, see \code{\link[=filteringOptions]{filteringOptions()}}.}

\item{adequacy}{Adequacy parameters, see \code{\link[=adequacyOptions]{adequacyOptions()}}.}

\item{overwrite}{Overwrite the area if already exist.}

\item{opts}{List of simulation parameters returned by the function
\code{\link[antaresRead:setSimulationPath]{antaresRead::setSimulationPath()}}}
}
\value{
An updated list containing various information about the simulation.
}
\description{
\ifelse{html}{\figure{badge_api_ok.svg}{options: alt='Antares API OK'}}{Antares API: \strong{OK}}

Create a new area in an Antares study.
}
\examples{
\dontrun{

library(antaresRead)

# Set simulation path
setSimulationPath(path = "PATH/TO/SIMULATION", simulation = "input")

# Create a new area
createArea("fictive_area")

}
}
\seealso{
\code{\link[=editArea]{editArea()}}, \code{\link[=removeArea]{removeArea()}}
}
