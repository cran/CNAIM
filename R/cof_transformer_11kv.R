#' @title Consequences of Failure for a 6.6/11 kV transformer
#' @description This function calculates consequences of failure
#' for a 6.6/11 kV transformer (cf.section 7, page 75, CNAIM, 2021).
#' @inheritParams f_cof_transformer_11kv
#' @inheritParams s_cof_swg_tf_ohl
#' @inheritParams e_cof_tf
#' @inheritParams n_cof_excl_ehv_132kv_tf
#' @return Numeric. Consequences of failure for a 6.6/11 kV transformer.
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 2.1, 2021:
#' \url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' @export
#' @examples
#' # Consequences of failure for a 6.6/11 kV transformer
#' cof_transformer_11kv(kva = 500, type = "Type C",
#'                      type_risk = "High", location_risk = "High",
#'                      prox_water = 50, bunded = "No",
#'                      no_customers = 500, kva_per_customer = 1)


cof_transformer_11kv <- function(kva, type,
                                 type_risk, location_risk,
                                 prox_water, bunded,
                                 no_customers, kva_per_customer) {

  finance <- f_cof_transformer_11kv(kva, type)

  safety <- s_cof_swg_tf_ohl(type_risk, location_risk,
                             asset_type_scf = "6.6/11kV Transformer (GM)")

  environmental <-  e_cof_tf(asset_type_tf = "6.6/11kV Transformer (GM)",
                             rated_capacity = kva,
                             prox_water, bunded)

  network <-
    n_cof_excl_ehv_132kv_tf(asset_type_ncf = "6.6/11kV Transformer (GM)",
                            no_customers, kva_per_customer)

  return(finance + safety + environmental + network)
}
