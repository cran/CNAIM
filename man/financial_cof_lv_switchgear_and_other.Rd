% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cof_lv_switchgear_and_other.R
\name{financial_cof_lv_switchgear_and_other}
\alias{financial_cof_lv_switchgear_and_other}
\title{Financial cost of Failure for LV swicthgear and others}
\source{
DNO Common Network Asset Indices Methodology (CNAIM),
Health & Criticality - Version 1.1, 2017:
\url{https://www.ofgem.gov.uk/system/files/docs/2017/05/dno_common_network_asset_indices_methodology_v1.1.pdf}
}
\usage{
financial_cof_lv_switchgear_and_other(
  lv_asset_category,
  type_financial_factor_criteria,
  access_factor_criteria
)
}
\arguments{
\item{lv_asset_category}{String The type of LV asset category}

\item{type_financial_factor_criteria}{String Type Financial factor criteria for LV switchgear
(cf. section D1.2.1, page 162, CNAIM, 2017).}

\item{access_factor_criteria}{String. Asses Financial factor criteria for LV switchgear
setting (cf. table 214, page 164, CNAIM, 2017).}
}
\value{
Numeric. Financial consequences of failure for LV switchgear
}
\description{
This function calculates financial consequences of failure
(cf. section 7.3, page 75, CNAIM, 2017). Financial consequences
of failure is used in
the derivation of consequences of failure see \code{\link{cof}}().
}
\examples{
financial_cof_lv_switchgear_and_other(lv_asset_category = "LV Board (WM)",
type_financial_factor_criteria = "Asbestos clad",
access_factor_criteria = "Type A")
}
