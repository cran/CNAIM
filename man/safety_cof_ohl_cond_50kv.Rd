% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cof_ohl_cond_50kv.R
\name{safety_cof_ohl_cond_50kv}
\alias{safety_cof_ohl_cond_50kv}
\title{Safety cost of Failure for 50kV Overhead Line Conductors}
\usage{
safety_cof_ohl_cond_50kv(location_risk, type_risk)
}
\arguments{
\item{location_risk}{String Type Financial factor criteria for Overhead Line Conductors
Options: \code{location_risk = c("Low", "Medium", "High")}.
The default setting is
\code{location_risk = "Medium"}.}

\item{type_risk}{String. Asses Financial factor criteria for Overhead Line Conductors
Options: \code{type_risk = c("Low", "Medium", "High")}.
The default setting is
\code{type_risk = "Medium"}.}
}
\value{
Numeric. Safety consequences of failure for Overhead Line Conductors
}
\description{
This function calculates safety consequences of failure
Outputted in DKK
}
\examples{
safety_cof_ohl_cond_50kv(
location_risk = "Default",
type_risk = "Default")
}
