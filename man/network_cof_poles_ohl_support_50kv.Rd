% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cof_poles_ohl_support_50kv.R
\name{network_cof_poles_ohl_support_50kv}
\alias{network_cof_poles_ohl_support_50kv}
\title{Network cost of Failure for Poles OHL Support 50kV}
\usage{
network_cof_poles_ohl_support_50kv(
  pole_asset_category,
  actual_load_mva,
  secure = T
)
}
\arguments{
\item{pole_asset_category}{String The type of Pole asset category}

\item{actual_load_mva}{Numeric. The actual load on the asset}

\item{secure}{Boolean If the asset is in a secure network or not}
}
\value{
Numeric. Network cost of failure.
}
\description{
This function calculates network cost of failure for
all asset categories exclusive the assets EHV and 132kV transformers.
Network cost of failure is used in the derivation
 of consequences of failure see \code{\link{cof}}().
 Outputted in DKK.
}
\examples{
network_cof_poles_ohl_support_50kv(
actual_load_mva = 15)
}
