% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pof_future_building.R
\name{pof_future_building}
\alias{pof_future_building}
\title{Future Probability of Failure for Primary Substation Building
and Secondary Substation Building.}
\usage{
pof_future_building(
  substation_type = "Secondary",
  material_type = "Wood",
  placement = "Outdoor",
  altitude_m = "Default",
  distance_from_coast_km = "Default",
  corrosion_category_index = "Default",
  age,
  temperature_reading = "Default",
  coolers_radiator = "Default",
  kiosk = "Default",
  cable_boxes = "Default",
  reliability_factor = "Default",
  k_value = "Default",
  c_value = 1.087,
  normal_expected_life_building = "Default",
  simulation_end_year = 100
)
}
\arguments{
\item{substation_type}{String. A sting that refers to the specific
substation type.
Options:
\code{substation_type = c("Primary", "Secondary")}.
The default setting is
\code{substation_type = "Secondary"}}

\item{material_type}{String. A sting that refers to the specific
material_type.
Options:
\code{material_type = c("Brick", "Steel", "Wood")}.
The default setting is
\code{substation_type = "Wood"}}

\item{placement}{String. Specify if the asset is located outdoor or indoor.}

\item{altitude_m}{Numeric. Specify the altitude location for
the asset measured in meters from sea level.\code{altitude_m}
is used to derive the altitude factor. A setting of \code{"Default"}
will set the altitude factor to 1 independent of \code{asset_type}.}

\item{distance_from_coast_km}{Numeric. Specify the distance from the
coast measured in kilometers. \code{distance_from_coast_km} is used
to derive the distance from coast factor. A setting of \code{"Default"} will set the
 distance from coast factor to 1 independent of \code{asset_type}.}

\item{corrosion_category_index}{Integer.
Specify the corrosion index category, 1-5.}

\item{age}{Numeric. The current age in years
of the building.}

\item{temperature_reading}{String. Indicating the criticality.
Options:
\code{temperature_reading = c("Normal", "Moderately High",
"Very High", "Default")}.}

\item{coolers_radiator}{String. Indicating the observed condition of the
coolers/radiators. Options:
\code{coolers_radiator = c("Superficial/minor deterioration", "Some Deterioration",
"Substantial Deterioration", "Default")}.
in CNAIM (2021).}

\item{kiosk}{String. Indicating the observed condition of the
kiosk. Options:
\code{kiosk = c("Superficial/minor deterioration", "Some Deterioration",
"Substantial Deterioration", "Default")}.}

\item{cable_boxes}{String. Indicating the observed condition of the
cable boxes. Options:
\code{cable_boxes = c("No Deterioration","Superficial/minor deterioration", "Some Deterioration",
"Substantial Deterioration", "Default")}..}

\item{reliability_factor}{Numeric. \code{reliability_factor}
shall have a value between 0.6 and 1.5. A setting of \code{"Default"}
 sets the \code{reliability_factor} to 1.
See section 6.14 on page 73 in CNAIM (2021).}

\item{k_value}{Numeric. \code{k_value = "Default"} by default. This number is
given in a percentage.}

\item{c_value}{Numeric. \code{c_value = 1.087} by default.
The default value is accordingly to the CNAIM standard see page 110}

\item{normal_expected_life_building}{Numeric.
\code{normal_expected_life_building = "Default"} by default.}

\item{simulation_end_year}{Numeric. The last year of simulating probability
of failure. Default is 100.}
}
\value{
DataFrame. Future probability of failure
along with future health score
}
\description{
This function calculates the future
annual probability of failure for primary substation building
and secondary substation building.
The function is a cubic curve that is based on
the first three terms of the Taylor series for an
exponential function.
}
\examples{
# Future probability of failure for a Secondary substation Building
pof_future_building(substation_type = "Secondary",
material_type = "Wood",
placement = "Outdoor",
altitude_m = "Default",
distance_from_coast_km = "Default",
corrosion_category_index = "Default",
age = 1,
temperature_reading = "Default",
coolers_radiator = "Default",
kiosk = "Default",
cable_boxes = "Default",
reliability_factor = "Default",
k_value = "Default",
c_value = 1.087,
normal_expected_life_building = "Default",
simulation_end_year = 100)
}
