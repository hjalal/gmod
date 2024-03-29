% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_gmod_build.R
\name{states}
\alias{states}
\title{Add Markov states to a gmod}
\usage{
states(names, init_probs, max_cycle_in_states = NULL)
}
\arguments{
\item{...}{Markov state names}
}
\value{
a gmod layer with Markov state names
}
\description{
Add Markov states to a gmod
}
\examples{
states("Healthy", "Sick", "Dead")
}
