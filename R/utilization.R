#' @title Site Utilization
#' @description Targets related to the current utilization of a site
#'   (e.g., what exists on the site, how much of the site is developed, how many residential units are there, etc.)
#' @return All utilization-related commands return a \code{\link[tibble:tbl_df]{tbl_df}} object.

#' @rdname utilization
#' @export
make_utilization <- function(suitability, utilization_criteria, utilization_list = list()){

  suit_pin_only <- suitability %>%
    sf::st_drop_geometry() %>%
    dplyr::select(PIN)

  util_join <-  list(suit_pin_only) %>%
    append(utilization_list) %>%
    purrr::reduce(dplyr::left_join, by = "PIN")

  util_ready <-  util_join %>%
    dplyr::mutate(SEATTLE_UTIL_RATIO_DBL_TRIM = dplyr::if_else(SEATTLE_UTIL_RATIO_DBL > 5, 5, SEATTLE_UTIL_RATIO_DBL),
                  SEATTLE_UTIL_RATIO_CAT = cut(SEATTLE_UTIL_RATIO_DBL, breaks = utilization_criteria[["util_ratio_breaks"]], labels = utilization_criteria[["util_ratio_labels"]]) %>% as.character(),
                  UTILIZATION_RATIO = round(safe_divide(UTIL_PRESENT, UTIL_POTENTIAL_UTILIZATION_SQFT),2),
                  UTILIZATION_RATIO_CAT = cut(UTILIZATION_RATIO, right = FALSE, breaks = utilization_criteria[["util_ratio_breaks"]], labels = utilization_criteria[["util_ratio_labels"]]) %>% as.character(),
                  UTIL_UNDER_UTILIZED_GENTLE_LGL = dplyr::if_else(UTILIZATION_RATIO < utilization_criteria["ratio_gentle"],TRUE,FALSE,NA),
                  UTIL_UNDER_UTILIZED_MODERATE_LGL = dplyr::if_else(UTILIZATION_RATIO < utilization_criteria["ratio_moderate"],TRUE,FALSE,NA),
                  UTIL_UNDER_UTILIZED_AGGR_LGL = dplyr::if_else(UTILIZATION_RATIO < utilization_criteria["ratio_aggressive"],TRUE,FALSE,NA),
                  UTILIZATION_GENTLE = dplyr::if_else(!UTIL_UNDER_UTILIZED_GENTLE_LGL, "fully-utilized", "under-utilized", LOT_SIZE_TYPE),
                  UTILIZATION_MODERATE = dplyr::if_else(!UTIL_UNDER_UTILIZED_MODERATE_LGL, "fully-utilized", "under-utilized", LOT_SIZE_TYPE),
                  UTILIZATION_AGGR = dplyr::if_else(!UTIL_UNDER_UTILIZED_AGGR_LGL, "fully-utilized", "under-utilized", LOT_SIZE_TYPE))

  utilization <- util_ready

  return(utilization)
}

#' @rdname utilization
#' @export
make_seattle_utilization_ratio <- function(parcel_sf_ready, seattle_dev_cap){

  p <- parcel_sf_ready %>%
    dplyr::select(PIN) %>%
    sf::st_drop_geometry()

  seattle_utilization_ratio <- p %>%
    dplyr::inner_join(seattle_dev_cap, by = "PIN") %>%
    dplyr::transmute(PIN,
                     SEATTLE_UTIL_RATIO_DBL = dplyr::case_when(
                       is.na(DR) ~ 0,
                       DR < 0 ~ 0,
                       TRUE ~ DR
                     )) %>%
    dplyr::right_join(p, by = "PIN")


  return(seattle_utilization_ratio)
}

#' @rdname utilization
#' @export
make_utilization_present <- function(parcel_ready, building){

  utilization_present <- parcel_ready %>%
    sf::st_drop_geometry() %>%
    dplyr::select(PIN) %>%
    dplyr::left_join(building, by = "PIN") %>%
    dplyr::mutate(BLDG_NBR = dplyr::if_else(is.na(BLDG_NBR), as.integer(0), as.integer(BLDG_NBR)),
                  UTIL_PRESENT = dplyr::if_else(is.na(BLDG_NET_SQ_FT),0,as.double(BLDG_NET_SQ_FT),missing = 0),
                  UTIL_BUILDING = dplyr::if_else(BLDG_NBR > 0, TRUE, FALSE, missing = TRUE)
    )

  return(utilization_present)
}

#' @rdname utilization
#' @export
make_utilization_lot_size <- function(parcel_ready, utilization_criteria){

  utilization_lot_size <- parcel_ready %>%
    sf::st_drop_geometry() %>%
    dplyr::transmute(PIN,
                     LOT_SIZE_DESC = as.character(cut(SQ_FT_LOT,
                                                      breaks = utilization_criteria[["lot_size_breaks"]],
                                                      labels = utilization_criteria[["lot_size_labels"]])))

  return(utilization_lot_size)
}

#' @rdname utilization
#' @export
make_utilization_potential <- function(suitability, development_assumptions_lot, utilization_lot_size){

  suit_trim <- suitability %>%
    sf::st_drop_geometry() %>%
    dplyr::transmute(PIN,
                     LOT_SQ_FT = SQ_FT_LOT,
                     UTIL_LOT_DEVELOPABLE_PCT = round(1-SUIT_WATER_OVERLAP_PCT,2),
                     CONSOL_20 = SUIT_ZONING_CONSOL_20
    )

  utilization_potential <- suit_trim %>%
    dplyr::left_join(utilization_lot_size, by = "PIN") %>%
    dplyr::left_join(development_assumptions_lot, by = c("CONSOL_20", "LOT_SIZE_DESC")) %>%
    dplyr::rename(UTIL_DEVELOPABLE_LGL = DEVELOPABLE_LGL,
                  UTIL_DEVELOPMENT_ASSUMPTION = DEVELOPMENT_ASSUMPTION) %>%
    dplyr::mutate(UTIL_DEVELOPABLE_ESTIMATE_LGL = dplyr::if_else(LOT_SIZE_TYPE %in% c("single family","small","medium","large"),
                                                                 TRUE,
                                                                 FALSE,
                                                                 missing = FALSE)) %>%
    dplyr::mutate(UTIL_DEVELOPABLE_LOT_COVERAGE_PCT = lesser_of(LOT_COVERAGE_PCT, UTIL_LOT_DEVELOPABLE_PCT) ) %>% # take the lesser of lot coverage estimate and water overlap
    dplyr::mutate(UTIL_POTENTIAL_UTILIZATION_SQFT = round(LOT_SQ_FT * UTIL_DEVELOPABLE_LOT_COVERAGE_PCT * LOT_STORIES_NBR)) %>%  # this is where the math happens!
    dplyr::select(PIN,
                  tidyselect::starts_with("LOT"),
                  tidyselect::starts_with("UTIL"))

  return(utilization_potential)
}
