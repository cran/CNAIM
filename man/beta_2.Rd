% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/beta_2.R
\name{beta_2}
\alias{beta_2}
\title{Forecast Ageing Rate}
\source{
DNO Common Network Asset Indices Methodology (CNAIM),
Health & Criticality - Version 2.1, 2021:
\url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
}
\usage{
beta_2(current_health_score, age)
}
\arguments{
\item{current_health_score}{Numeric. The output returned by the
function \code{\link{current_health}}().}

\item{age}{Numeric. Age of the asset.}
}
\value{
Numeric. Forecast ageing rate for an electric network asset.
}
\description{
This function calculates the forecast Ageing Rate for an
electric network asset. See section 6.1.8 on page 38 in CNAIM (2021).
}
\examples{
beta_2(current_health_score = 1, age = 25)
}
