#' @title Site Utilization Criteria
#' @description Targets related to the utilization _criteria_ of a site.
#' @return All utilization criteria commands return a named list.

#' @rdname utilization_criteria
#' @export
make_utilization_criteria <- function(utilization_criteria_lot_size,
                            utilization_criteria_ratio,
                            utilization_criteria_ratio_bins){

  utilization_criteria <- c(utilization_criteria_lot_size,
                            utilization_criteria_ratio,
                            utilization_criteria_ratio_bins)

  return(utilization_criteria)
}


#' @rdname utilization_criteria
#' @export
make_utilization_criteria_lot_size <- function(city_block_sqft, lot_types){

  eight_block <- city_block_sqft/8

  quarter_block <- city_block_sqft/4

  crit_lot_size <- list("lot_size_breaks" = c(-Inf,eight_block,quarter_block, Inf),
                        "lot_size_labels" =  lot_types$LOT_SIZE_DESC)

  utilization_criteria_lot_size <- crit_lot_size

  return(utilization_criteria_lot_size)
}

#' @rdname utilization_criteria
#' @export
make_utilization_criteria_ratio <- function(){

  utilization_criteria_ratio <- list("ratio_gentle" = 1e-2,
                          "ratio_moderate" = 2/3,
                          "ratio_aggressive" = as.double(1)
  )

  return(utilization_criteria_ratio)
}

#' @rdname utilization_criteria
#' @export
make_utilization_criteria_ratio_bins <- function(){

  utilization_criteria_ratio_bins <- list("util_ratio_breaks" = c(-Inf,.25,.5,.75,1,Inf),
                                "util_ratio_labels" =  c("Less than 1/4","1/4 to 1/2", "1/2 to 3/4", "3/4 to 1", "Greater than 1")
  )

  return(utilization_criteria_ratio_bins)
}
