% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cof_switchgear_secondary_10kv.R
\name{safety_cof_switchgear_secondary_10kv}
\alias{safety_cof_switchgear_secondary_10kv}
\title{Safety cost of Failure for 10 kV Switchgear Secondary}
\usage{
safety_cof_switchgear_secondary_10kv(location_risk, type_risk)
}
\arguments{
\item{location_risk}{String Type Financial factor criteria for 10kV switchgear secondary
(cf. section D1.2.1, page 178, CNAIM, 2021).
Options: \code{location_risk = c("Low", "Medium", "High")}.
The default setting is
\code{location_risk = "Medium"}.}

\item{type_risk}{String. Asses Financial factor criteria for 10kV switchgear secondary
setting.
Options: \code{type_risk = c("Low", "Medium", "High")}.
The default setting is
\code{type_risk = "Medium"}.}
}
\value{
Numeric. Financial consequences of failure for 10kV switchgear secondary
}
\description{
This function calculates safety consequences of failure.
Safety consequences of failure is used in
the derivation of consequences of failure see \code{\link{cof}}().
Outputted in DKK.
}
\examples{
safety_cof_switchgear_secondary_10kv(
location_risk = "Default",
type_risk = "Default")
}
