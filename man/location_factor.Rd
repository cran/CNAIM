% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/location_factor.R
\name{location_factor}
\alias{location_factor}
\title{Location Factor (Excl.Submarine Cables)}
\source{
DNO Common Network Asset Indices Methodology (CNAIM),
Health & Criticality - Version 2.1, 2021:
\url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
}
\usage{
location_factor(
  placement = "Default",
  altitude_m = "Default",
  distance_from_coast_km = "Default",
  corrosion_category_index = "Default",
  asset_type = "6.6/11kV Transformer (GM)",
  sub_division = NULL
)
}
\arguments{
\item{placement}{String. Specify if the asset is located outdoor or indoor.
A setting of \code{"Outdoor"} means the asset is
located in an outside environment,
and a setting of \code{"Indoor"} means the asset is located in an
 indoor environment. A setting of \code{"Default"} will result
  in either an indoor or an outdoor environment setting that depends
  on the specification of \code{asset_type}. See page 110-113,
  table 26 in CNAIM (2021) for default environments.}

\item{altitude_m}{Numeric. Specify the altitude location for
the asset measured in meters from sea level.\code{altitude_m}
is used to derive the altitude factor. See page 111,
table 23 in CNAIM (2021). A setting of \code{"Default"}
will set the altitude factor to 1 independent of \code{asset_type}.}

\item{distance_from_coast_km}{Numeric. Specify the distance from the
coast measured in kilometers. \code{distance_from_coast_km} is used
to derive the distance from coast factor See page 110,
table 22 in CNAIM (2021). A setting of \code{"Default"} will set the
 distance from coast factor to 1 independent of \code{asset_type}.}

\item{corrosion_category_index}{Integer.
Specify the corrosion index category, 1-5.
\code{corrosion_category_index} is used to derive the corrosion
category factor. See page 111, table 24 in CNAIM (2021).
A setting of \code{"Default"} will set the corrosion category factor
to 1 independent of \code{asset_type}.}

\item{asset_type}{String.
A sting that refers to the specific asset category.
For LV UGB and non-submarine cables a location factor of 1 is assigned.
See See page 17, table 1 in CNAIM (2021).
Options:
\code{asset_type = c("LV Poles", "LV Circuit Breaker",
"LV Pillar (ID)", "LV Pillar (OD at Substation)",
"LV Pillar (OD not at a Substation)", "LV Board (WM)",
"LV UGB", "LV Board (X-type Network) (WM)", "6.6/11kV Poles",
 "20kV Poles", "6.6/11kV CB (GM) Primary",
"6.6/11kV CB (GM) Secondary", "6.6/11kV Switch (GM)", "6.6/11kV RMU",
"6.6/11kV X-type RMU", "20kV CB (GM) Primary", "20kV CB (GM) Secondary",
 "20kV Switch (GM)", "20kV RMU", "6.6/11kV Transformer (GM)",
 "20kV Transformer (GM)", "33kV Pole", "66kV Pole",
 "33kV OHL (Tower Line) Conductor", "33kV Tower", "33kV Fittings",
"66kV OHL (Tower Line) Conductor", "66kV Tower", "66kV Fittings",
"33kV UG Cable (Non Pressurised)", "33kV UG Cable (Oil)",
"33kV UG Cable (Gas)", "66kV UG Cable (Non Pressurised)",
"66kV UG Cable (Oil)", "66kV UG Cable (Gas)",
"33kV CB (Air Insulated Busbars)(ID) (GM)",
"33kV CB (Air Insulated Busbars)(OD) (GM)",
"33kV CB (Gas Insulated Busbars)(ID) (GM)",
"33kV CB (Gas Insulated Busbars)(OD) (GM)", "33kV Switch (GM)",
"33kV RMU", "66kV CB (Air Insulated Busbars)(ID) (GM)",
"66kV CB (Air Insulated Busbars)(OD) (GM)",
"66kV CB (Gas Insulated Busbars)(ID) (GM)",
"66kV CB (Gas Insulated Busbars)(OD) (GM)", "33kV Transformer (GM)",
"66kV Transformer (GM)", "132kV OHL (Tower Line) Conductor",
"132kV Tower", "132kV Fittings", "132kV UG Cable (Non Pressurised)",
"132kV UG Cable (Oil)", "132kV UG Cable (Gas)",
"132kV CB (Air Insulated Busbars)(ID) (GM)",
"132kV CB (Air Insulated Busbars)(OD) (GM)",
"132kV CB (Gas Insulated Busbars)(ID) (GM)",
"132kV CB (Gas Insulated Busbars)(OD) (GM)", "132kV Transformer (GM)")
}}

\item{sub_division}{String. Refers to material the sub division in the asset category}
}
\value{
Numeric. Location factor
}
\description{
This function calculates the location factor for
an electric network asset based in the specific location of the asset.
See section 6.4 on page 46 in CNAIM (2021). For calculating the location
factor for submarine cables please see the function
\code{\link{location_factor_sub}()}. Note the location factor for all other
cables are always equal to 1 hence the function will return a location
factor of 1 for other cables than submarine cables.
}
\examples{
 # Location factor for a 6.6/11 kV Transformer with default values
location_factor(placement = "Default", altitude_m = "Default",
distance_from_coast_km = "Default",
corrosion_category_index = "Default",
asset_type = "6.6/11kV Transformer (GM)")
}
