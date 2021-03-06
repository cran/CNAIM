% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cof_ehv_fittings.R
\name{environmental_cof_ehv_fittings}
\alias{environmental_cof_ehv_fittings}
\title{Environmental cost of Failure for EHV/132kV fittings}
\source{
DNO Common Network Asset Indices Methodology (CNAIM),
Health & Criticality - Version 1.1, 2017:
\url{https://www.ofgem.gov.uk/system/files/docs/2017/05/dno_common_network_asset_indices_methodology_v1.1.pdf}
}
\usage{
environmental_cof_ehv_fittings(ehv_asset_category)
}
\arguments{
\item{ehv_asset_category}{String The type of EHV asset category}
}
\description{
This function calculates environmental consequences of failure
(cf. section 7.3, page 75, CNAIM, 2017). Environmental consequences
of failure is used in
the derivation of consequences of failure see \code{\link{cof}}().#' @return Numeric. Financial consequences of failure for LV switchgear
}
\examples{
environmental_cof_ehv_fittings(ehv_asset_category = "33kV Fittings")
}
