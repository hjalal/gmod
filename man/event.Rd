% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_gmod_build.R
\name{event}
\alias{event}
\title{Add event mapping}
\usage{
event(name, scenarios, probs, outcomes)
}
\arguments{
\item{name}{}

\item{scenarios}{}

\item{probs}{}

\item{outcomes}{}
}
\value{
gmod layer
}
\description{
Add event mapping
}
\examples{
event_mapping(name = "event_progress", 
scenarios = c(TRUE,FALSE), 
probs = c(p_progress_function(state), Inf), 
outcomes = c("Severe","curr_state")

}
