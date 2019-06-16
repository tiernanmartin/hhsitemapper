#' @title Site Suiteability Criteria
#' @description Targets related to the suitability _criteria_ of a site for affordable
#'   housing and/or early learning facility development.
#' @return All suitability criteria commands return a named list.

#' @rdname suitability_criteria
#' @export
make_suitability_criteria <- function(criteria_tax_exempt,
                                      criteria_max_water_overlap_pct,
                                      criteria_within_uga,
                                      criteria_developable_zoning,
                                      criteria_undevelopable_presentuse,
                                      criteria_lot_size,
                                      criteria_area_ratio,
                                      criteria_steep_vacant,
                                      criteria_unbuildable,
                                      criteria_other){
  suitability_criteria_list <- list(
    criteria_tax_exempt,
    criteria_max_water_overlap_pct,
    criteria_within_uga,
    criteria_developable_zoning,
    criteria_undevelopable_presentuse,
    criteria_lot_size,
    criteria_area_ratio,
    criteria_steep_vacant,
    criteria_unbuildable,
    criteria_other
  )

  suitability_criteria <- purrr::reduce(suitability_criteria_list,c)

  return(suitability_criteria)

}


#' @rdname suitability_criteria
#' @export
make_criteria_tax_exempt <- function(){

  crit_tax_e <- list("tax_exempt" = TRUE)

  criteria_tax_exempt <- crit_tax_e

  return(criteria_tax_exempt)

}

#' @rdname suitability_criteria
#' @export
make_criteria_max_water_overlap_pct <- function(){

  crit_wtr_overlap <- list("water_overlap" = 0.5)

  criteria_max_water_overlap_pct <- crit_wtr_overlap

  return(criteria_max_water_overlap_pct)
}

#' @rdname suitability_criteria
#' @export
make_criteria_within_uga <- function(){

  crit_within_uga <- list("within_uga" = TRUE)

  criteria_within_uga <- crit_within_uga

  return(criteria_within_uga)
}

#' @rdname suitability_criteria
#' @export
make_criteria_developable_zoning <- function(development_assumptions_zoning){

  dz <- development_assumptions_zoning %>%
    dplyr::filter(DEVELOPABLE_LGL) %>%
    dplyr::pull(CONSOL_20)

  crit_dz <- list("developable_zoning" = dz)

  criteria_developable_zoning <-  crit_dz

  return(criteria_developable_zoning)

}

#' @rdname suitability_criteria
#' @export
make_criteria_undevelopable_present_use <- function(){

  list_uses <- function(){

    loadd(parcel_ready)

    parcel_ready %>%
      sf::st_drop_geometry() %>%
      dplyr::count(PRESENT_USE, sort = TRUE) %>%
      print(n = Inf)
  }

  undev_presentuse <- c(
    "Park Public Zoo Arbor",
    "Mortuary Cemetery Crematory",
    "Open Space Timber Land Greenbelt",
    "Open Space Current Use RCW 84.34",
    "Mining Quarry Ore Processing",
    "Farm",
    "Reserve Wilderness Area",
    "Open Space Agriculture RCW 84.34",
    "Forest Land Designated RCW 84.33",
    "Forest Land Class RCW 84.33",
    "Tideland 1st Class",
    "Tideland 2nd Class",
    "Air Terminal and Hangers",
    "Terminal Marine Commercial Fish",
    "River Creek Stream",
    "Art Gallery Museum Social Service",
    "Right of Way Utility Road",
    "Easement"
  )

  crit_undev_presentuse <- list( "undevelopable_presentuse" = undev_presentuse)

  criteria_undevelopable_presentuse <-  crit_undev_presentuse

  return(criteria_undevelopable_presentuse)

}

#' @rdname suitability_criteria
#' @export
make_criteria_lot_size <- function(lot_size_breaks){

  # criteria_area <- list("area_max" = set_units(as.integer(40), acre),
  #                       "area_min" = set_units(as.double(1/8), acre))  # This is an educated-guess placeholder and may need to be adjusted

  lot_sizes_discard <- c("under-sized", "over-sized (undevelopable)")

  criteria_lot_size <- lot_size_breaks %>%
    purrr::pluck("names") %>%
    purrr::discard(~ .x %in% lot_sizes_discard ) %>%
    list() %>%
    purrr::set_names("lot_size")

  return(criteria_lot_size)

}


#' @rdname suitability_criteria
#' @export
make_criteria_area_ratio <- function(){

  crit_ar <- list("area_ratio" = as.double(.2))  # This is an educated-guess placeholder and may need to be adjusted

  criteria_area_ratio <-  crit_ar

  return(criteria_area_ratio)

}

#' @rdname suitability_criteria
#' @export
make_criteria_steep_vacant <- function(){

  criteria_steep_vacant <- list("steep_vacant" = FALSE)

  return(criteria_steep_vacant)
}

#' @rdname suitability_criteria
#' @export
make_criteria_unbuildable <- function(){

  criteria_unbuildable <- list("unbuildable" = FALSE)

  return(criteria_unbuildable)
}

#' @rdname suitability_criteria
#' @export
make_criteria_other <- function(){

  criteria_other <- list("other" = TRUE)

  return(criteria_other)
}
