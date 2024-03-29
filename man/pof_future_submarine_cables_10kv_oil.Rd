% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pof_future_submarine_cables_10kv_oil.R
\name{pof_future_submarine_cables_10kv_oil}
\alias{pof_future_submarine_cables_10kv_oil}
\title{Future Probability of Failure for 10kV Oil Submarine Cables}
\usage{
pof_future_submarine_cables_10kv_oil(
  utilisation_pct = "Default",
  operating_voltage_pct = "Default",
  topography = "Default",
  sitution = "Default",
  wind_wave = "Default",
  intensity = "Default",
  landlocked = "no",
  sheath_test = "Default",
  partial_discharge = "Default",
  fault_hist = "Default",
  condition_armour = "Default",
  age,
  reliability_factor = "Default",
  k_value = 2.0944,
  c_value = 1.087,
  normal_expected_life = 60,
  simulation_end_year = 100
)
}
\arguments{
\item{utilisation_pct}{Numeric. The max percentage of utilisation
under normal operating conditions.}

\item{operating_voltage_pct}{Numeric. The ratio in percent of
operating/design voltage.}

\item{topography}{String Topography}

\item{sitution}{String Situation}

\item{wind_wave}{String Wind Wave}

\item{intensity}{String Intensity}

\item{landlocked}{String Land Locked}

\item{sheath_test}{String. Only applied for non pressurised cables.
Indicating the state of the sheath. Options:
\code{sheath_test = c("Pass", "Failed Minor", "Failed Major",
"Default")}.}

\item{partial_discharge}{String. Only applied for non pressurised cables.
Indicating the level of partial discharge. Options:
\code{partial_discharge = c("Low", "Medium", "High",
 "Default")}.}

\item{fault_hist}{Numeric. Only applied for non pressurised cables.
The calculated fault rate for the cable in the period per kilometer.
A setting of \code{"No historic faults recorded"}
indicates no fault.}

\item{condition_armour}{String Condition Armour}

\item{age}{Numeric. The current age in years of the cable.}

\item{reliability_factor}{Numeric. \code{reliability_factor}
shall have a value between 0.6 and 1.5. A setting of \code{"Default"}
 sets the \code{reliability_factor} to 1.
See section 6.14 on page 73 in CNAIM (2021).}

\item{k_value}{Numeric. \code{k_value = 0.0658} by default.}

\item{c_value}{Numeric. \code{c_value = 1.087} by default.
The default value is accordingly to the CNAIM standard see page 110}

\item{normal_expected_life}{Numeric. \code{normal_expected_life = 80} by default.}

\item{simulation_end_year}{Numeric. The last year of simulating probability
of failure. Default is 100.}
}
\value{
DataFrame. Future probability of failure
along with future health score
}
\description{
This function calculates the future
annual probability of failure per kilometer for a 10kV Oil submarine cables
The function is a cubic curve that is based on
the first three terms of the Taylor series for an
exponential function.
}
\examples{
# Future annual probability of failure for 1 km 10kV Oil Sub Cable
pof_future_submarine_cables_10kv_oil(
utilisation_pct = "Default",
operating_voltage_pct = "Default",
topography = "Default",
sitution = "Default",
wind_wave = "Default",
intensity = "Default",
landlocked = "no",
sheath_test = "Default",
partial_discharge = "Default",
fault_hist = "Default",
condition_armour = "Default",
age = 10,
reliability_factor = "Default",
k_value = 0.0202,
c_value = 1.087,
normal_expected_life = 60,
simulation_end_year = 100)
}
