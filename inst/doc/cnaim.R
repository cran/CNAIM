## ----echo=F, message=F, class.source='highlight',comment=""-------------------
library(CNAIM)

## ----echo=TRUE, message=FALSE, warning=FALSE, class.source='highlight',comment=""----
pof <- pof_transformer_11_20kv(
  hv_transformer_type = "6.6/11kV Transformer (GM)",
  utilisation_pct = 55,
  placement = "Indoor",
  altitude_m = 75,
  distance_from_coast_km = 20,
  corrosion_category_index = 2,
  age = 25,
  partial_discharge = "Low",
  oil_acidity = 0.1,
  temperature_reading = "Normal",
  observed_condition = "Default",
  reliability_factor = "Default")
sprintf("The probability of failure is %.2f%% per year", 100*pof) 

## ----echo=TRUE, message=FALSE, warning=FALSE, class.source='highlight',comment=""----
pof <- pof_transformer_11_20kv(age = 55)
sprintf("The probability of failure is %.2f%% per year", 100*pof) 

## ----echo=FALSE, message=F----------------------------------------------------
library(CNAIM)
library(r2d3)
library(jsonlite)
library(dplyr)
library(widgetframe)
library(r2d3)

## ----echo=TRUE, message=FALSE, warning=FALSE, class.source='highlight',comment=""----
financial_cof <- f_cof_transformer_11kv(kva = 750, type = "Type B")
sprintf("The financial consequences of failure is GBP %.f", round(financial_cof))

## ----echo=TRUE, message=FALSE, warning=FALSE, class.source='highlight',comment=""----
safety_cof <- s_cof_swg_tf_ohl(type_risk = "Low", 
                               location_risk = "Medium", 
                               asset_type_scf = "6.6/11kV Transformer (GM)")
sprintf("The safety consequences of failure is GBP %.f", round(safety_cof))

## ----echo=TRUE, message=FALSE, warning=FALSE, class.source='highlight',comment=""----
environmental_cof <- e_cof_tf(asset_type_tf = "6.6/11kV Transformer (GM)",
                              rated_capacity = 750,
                              prox_water = 95,
                              bunded = "Yes")
sprintf("The environmental consequences of failure is GBP %.f", round(environmental_cof)) 

## ----echo=TRUE, message=FALSE, warning=FALSE, class.source='highlight',comment=""----
network_cof <- n_cof_excl_ehv_132kv_tf(asset_type_ncf = "6.6/11kV Transformer (GM)",
                                       no_customers = 750,
                                       kva_per_customer = 1)
sprintf("The network performance consequences of failure is GBP %.f", round(network_cof)) 

## ----echo=TRUE, message=FALSE, warning=FALSE, class.source='highlight',comment=""----
cof_transformer <- cof(financial_cof, safety_cof, environmental_cof, network_cof)
sprintf("The consequences of failure is GBP %.f", cof_transformer) 

## ----echo=TRUE, message=FALSE, warning=FALSE, class.source='highlight',comment=""----
cof_short_cut <- cof_transformer_11kv(kva = 750, type = "Type B",
                                      type_risk = "Low", location_risk = "Medium",
                                      prox_water =  95, bunded = "Yes",
                                      no_customers =  750, kva_per_customer = 1)
all.equal(cof_transformer, cof_short_cut)

## -----------------------------------------------------------------------------
source('fns_monetary_risk.R')

## ----fake-calculation, echo=TRUE, message=FALSE, warning=FALSE, eval=F, class.source='highlight',comment=""----
#  # Generate an empty 5x4 matrix
#  matrix_structure <- risk_matrix_structure(5,4,NA)
#  # Monetary risk for one asset
#  risk_coordinates <- risk_calculation(matrix_dimensions = matrix_structure,
#                                       id = "Transformer1",
#                                       pof = 0.08,
#                                       cof = 18232,
#                                       asset_type = "6.6/11kV Transformer (GM)")
#  risk_matrix_points_plot(matrix_structure,
#                          dots_vector = risk_coordinates,
#                          dot_radius = 4)

## ----fake-calculation-2, echo=TRUE, message=FALSE, warning=FALSE, eval=F, class.source='highlight',comment=""----
#  # Generate an empty 5x4 matrix
#  risk_data_matrix <- risk_matrix_structure(5,4,NA)
#  risk_data_matrix$value <- sample(1:30,size=nrow(matrix_structure),replace = T)
#  risk_matrix_summary_plot(risk_data_matrix)

## ----fake-calculation-3, echo=TRUE, message=FALSE, warning=FALSE, eval=F, class.source='highlight',comment=""----
#  # Generate an empty 5x4 matrix
#  risk_data_matrix <- risk_matrix_structure(5,4,NA)
#  risk_data_matrix$value <- sample(1:30,size=nrow(matrix_structure),replace = T)
#  risk_matrix_summary_plot(risk_data_matrix,
#                           x_intervals = c(0.1,0.1,0.1,0.2,0.3),
#                           y_intervals = c(0.75,0.75,1,1.5))

## ----fake-calculation-4, echo=TRUE, message=FALSE, warning=FALSE, eval=F, class.source='highlight',comment=""----
#  # Generate an empty 4x4 matrix
#  risk_data_matrix <- risk_matrix_structure(5,4,NA)
#  risk_data_matrix$value <- sample(1:30,size=nrow(matrix_structure),replace = T)
#  risk_matrix_summary_plot(risk_data_matrix)

