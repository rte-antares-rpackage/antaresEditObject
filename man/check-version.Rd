% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checkVersion.R
\name{check-version}
\alias{check-version}
\alias{is_antares_v7}
\alias{is_antares_v820}
\title{Is study an Antares v7 study ?}
\usage{
is_antares_v7(opts = antaresRead::simOptions())

is_antares_v820(opts = antaresRead::simOptions())
}
\arguments{
\item{opts}{List of simulation parameters returned by the function
\code{\link[antaresRead:setSimulationPath]{antaresRead::setSimulationPath()}}}
}
\value{
a logical, \code{TRUE} if study is v7 or above, \code{FALSE} otherwise.
}
\description{
Is study an Antares v7 study ?
}
\examples{

\dontrun{
# setSimulationPath

is_antares_v7()

}

}
