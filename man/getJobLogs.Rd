% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/API.R
\name{getJobLogs}
\alias{getJobLogs}
\title{Retrieve job log from API}
\usage{
getJobLogs(job_id, opts = antaresRead::simOptions())
}
\arguments{
\item{job_id}{The job identifier.}

\item{opts}{List of simulation parameters returned by the function
\code{\link[antaresRead:setSimulationPath]{antaresRead::setSimulationPath()}}}
}
\value{
Logs as character string.
}
\description{
Retrieve job log from API
}
\examples{
\dontrun{

antaresRead::setSimulationPathAPI(
  host = "http://localhost:8080",
  study_id = "39c604fc-687f-46c4-9fa6-59b57ff9c8d1",
  token = NULL,
  simulation = "input"
)
job <- runSimulation()
getJobLogs(job)

}
}
