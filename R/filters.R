#' @title Filters
#' @description Characteristics of parcels that may be useful for users who want
#'   to filter the dataset (e.g., parcels within 1/4 mile of a light rail station).
#' @return All filter commands return a \code{\link[tibble:tbl_df]{tbl_df}} object.

#' @rdname filters
#' @export
make_filters <- function(parcel_ready, filter_list = list()){

  filter_df <- filter_list %>%
    purrr::reduce(dplyr::left_join, by = "PIN")

  filters <- parcel_ready %>%
    sf::st_drop_geometry() %>%
    dplyr::select(PIN) %>%
    dplyr::left_join(filter_df, by = "PIN")

  return(filters)


}


#' @rdname filters
#' @export
make_filters_census_tract <- function(parcel_sf_ready, census_tracts){

  p_ready_pt <- parcel_sf_ready %>%
    sf::st_set_geometry("geom_pt") %>%
    sf::st_transform(2926) %>%
    dplyr::transmute(PIN)

  tr_subdivide <- lwgeom::st_subdivide(census_tracts, 100) %>%
    sf::st_collection_extract()

  p_ready_pt$FILTER_CENSUS_TRACT <- st_over(p_ready_pt,tr_subdivide, "GEOID")

  p_ready_tr <- p_ready_pt %>%
    sf::st_drop_geometry() %>%
    dplyr::select(PIN, FILTER_CENSUS_TRACT)

  filters_census_tract <- p_ready_tr

  return(filters_census_tract)

}

#' @rdname filters
#' @export
make_filters_zcta <- function(parcel_sf_ready, zcta){

  p <- parcel_sf_ready %>%
    sf::st_set_geometry("geom_pt") %>%
    sf::st_transform(2926) %>%
    dplyr::transmute(PIN)

  zcta_subdivide <- lwgeom::st_subdivide(zcta, 100) %>%
    sf::st_collection_extract() %>%
    dplyr::transmute(FILTER_ZCTA = ZCTA_5_CE_10)

  p$FILTER_ZCTA <- st_over(p$geom_pt,zcta_subdivide, "FILTER_ZCTA")

  p_ready <- sf::st_drop_geometry(p)

  filters_zcta <- p_ready

  return(filters_zcta)

}

#' @rdname filters
#' @export
make_filters_place <- function(parcel_sf_ready, census_place){

  p <- parcel_sf_ready %>%
    sf::st_set_geometry("geom_pt") %>%
    sf::st_transform(2926) %>%
    dplyr::transmute(PIN)

  census_place_subdivide <- census_place %>%
    dplyr::transmute(PLACE = stringr::str_squish(NAME)) %>%
    lwgeom::st_subdivide(100) %>%
    sf::st_transform(2926) %>%
    sf::st_collection_extract()

  p$PLACE <- st_over(p$geom_pt,census_place_subdivide, "PLACE")


  p_ready <- p %>%
    dplyr::transmute(PIN,
              FILTER_PLACE = dplyr::if_else(is.na(PLACE),"Unnamed, Unincorporated King County",PLACE) %>% toupper()) %>%
    sf::st_drop_geometry()

  filters_place <- p_ready

  return(filters_place)

}

#' @rdname filters
#' @export
make_filters_place_name <- function(parcel_df_ready, filters_place){

  districts <- parcel_df_ready %>%
    purrr::pluck("DISTRICT_NAME") %>%
    purrr::discard(is.na) %>%
    unique()

  filters_place_name <- parcel_df_ready %>%
    dplyr::select(PIN, DISTRICT_NAME) %>%
    dplyr::left_join(filters_place, by = "PIN") %>%
    dplyr::transmute(PIN,
              FILTER_PLACE_NAME = dplyr::case_when(
                DISTRICT_NAME %in% "KING COUNTY" & FILTER_PLACE %in% districts ~ "UNINCORPORATED KC",
                DISTRICT_NAME %in% "KING COUNTY" & is.na(FILTER_PLACE) ~ "UNINCORPORATED KC",
                DISTRICT_NAME %in% "KING COUNTY" & stringr::str_detect(FILTER_PLACE, "UNNAMED") ~ "UNINCORPORATED KC",
                DISTRICT_NAME %in% "KING COUNTY" ~ stringr::str_c("UNINCORPORATED KC (",FILTER_PLACE,")"),
                DISTRICT_NAME %in% districts ~ DISTRICT_NAME,
                is.na(DISTRICT_NAME) & !is.na(FILTER_PLACE) ~ FILTER_PLACE,
                TRUE ~ "OTHER"
              ))


  return(filters_place_name)

}

#' @rdname filters
#' @export
make_filters_owner_category <- function(owner_category){

  filters_owner_category <- owner_category %>%
    dplyr::transmute(PIN,
              FILTER_OWNER_CATEGORY = OWNER_CATEGORY)

  return(filters_owner_category)

}

#' @rdname filters
#' @export
make_filters_public_owner <- function(owner_category){

  filters_public_owner <- owner_category %>%
    dplyr::transmute(PIN,
              FILTER_PUBLIC_OWNER = dplyr::case_when(
                OWNER_PUBLIC_LGL ~ OWNER_NAME_ORG,
                TRUE ~ NA_character_
              ))

  return(filters_public_owner)

}

#' @rdname filters
#' @export
make_filters_zoning_category <- function(suitability_developable_zoning){

  filters_public_owner <- suitability_developable_zoning %>%
    dplyr::rename(Z = SUIT_ZONING_CONSOL_20) %>%
    dplyr::transmute(PIN,
              FILTER_ZONING_CATEGORY = dplyr::case_when(
                is.na(Z) | Z %in% "Undesignated" ~ "Other",
                Z %in% c("General Mixed Use","Mixed Use Commercial/Residential") ~ "Mixed Use",
                Z %in% "General Commercial" ~ "Commercial",
                TRUE ~ Z
              )
    )

  return(filters_public_owner)

}

#' @rdname filters
#' @export
make_filters_proximity_transit <- function(parcel_sf_ready, transit_stops_osm){

  p_pt <- parcel_sf_ready %>%
    sf::st_set_geometry("geom_pt") %>%
    sf::st_transform(2926) %>%
    dplyr::transmute(PIN)

  buffer_dist_qtr <- units::set_units(1/4, "mile")
  buffer_dist_half <- units::set_units(1/2, "mile")

  ts_buff <- transit_stops_osm

  ts_buff$geom_qtr_mi_buff <- sf::st_buffer(sf::st_geometry(ts_buff), buffer_dist_qtr)

  ts_buff$geom_half_mi_buff <- sf::st_buffer(sf::st_geometry(ts_buff), buffer_dist_half)

  append_qtr <- function(x) stringr::str_c(x,"QTR",  sep = "_")

  append_half <- function(x) stringr::str_c(x, "HALF",  sep = "_")

  ts_buff_qtr <- ts_buff %>%
    sf::st_set_geometry("geom_qtr_mi_buff") %>%
    dplyr::select_if(not_sfc) %>%
    dplyr::rename_all(append_qtr)

  ts_buff_half <- ts_buff %>%
    sf::st_set_geometry("geom_half_mi_buff") %>%
    dplyr::select_if(not_sfc) %>%
    dplyr::rename_all(append_half)


  # ~ 1 min. operation
  p_ts_qtr <- sf::st_join(p_pt, ts_buff_qtr)

  # ~ 8 min. operation
  p_ts_half <- sf::st_join(p_pt, ts_buff_half)

  # ~ 8 min. operation
  p_prox_trans_qtr <- p_ts_qtr %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(PIN) %>%
    tidyr::nest() %>%
    dplyr::mutate(FILTER_PROXIMITY_TRANSIT_QTR = purrr::map_lgl(data, ~ !all(sapply(X = .x$TRANSIT_STOP_OSM_ID_QTR, FUN = is.na))),
                                    TRANSIT_STOP_TYPES_QTR = purrr::map_chr(data, ~ str_count_factor(.x$TRANSIT_STOP_TYPE_QTR))) %>%
    dplyr::transmute(PIN,
                     FILTER_PROXIMITY_TRANSIT_QTR,
                     TRANSIT_STOP_TYPES_QTR = dplyr::if_else(FILTER_PROXIMITY_TRANSIT_QTR,TRANSIT_STOP_TYPES_QTR, NA_character_))

  # ~ 13 min. operation
  p_prox_trans_half <- p_ts_half %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(PIN) %>%
    tidyr::nest() %>%
    dplyr::mutate(FILTER_PROXIMITY_TRANSIT_HALF = purrr::map_lgl(data, ~ !all(sapply(X = .x$TRANSIT_STOP_OSM_ID_HALF, FUN = is.na))),
           TRANSIT_STOP_TYPES_HALF = purrr::map_chr(data, ~ str_count_factor(.x$TRANSIT_STOP_TYPE_HALF))) %>%
    dplyr::transmute(PIN,
              FILTER_PROXIMITY_TRANSIT_HALF,
              TRANSIT_STOP_TYPES_HALF = dplyr::if_else(FILTER_PROXIMITY_TRANSIT_HALF,TRANSIT_STOP_TYPES_HALF, NA_character_))

  p_prox_trans <- dplyr::full_join(p_prox_trans_qtr,p_prox_trans_half, by = "PIN") %>%
    dplyr::mutate(FILTER_PROXIMITY_TRANSIT = dplyr::case_when(
      FILTER_PROXIMITY_TRANSIT_QTR ~ "1/4 mile",
      FILTER_PROXIMITY_TRANSIT_HALF ~ "1/2 mile",
      TRUE ~ "Greater than 1/2 mile"
    ))


  filters_proximity_transit <- p_prox_trans

  return(filters_proximity_transit)

}

#' @rdname filters
#' @export
make_filters_proximity_play_space <- function(...){

  p_pt <- parcel_sf_ready %>%
    sf::st_set_geometry("geom_pt") %>%
    sf::st_transform(2926) %>%
    dplyr::transmute(PIN)

  buffer_dist_eighth <- set_units(1/8, "mile")
  buffer_dist_qtr <- set_units(1/4, "mile")


  ps_buff <- play_spaces_osm

  ps_buff$geom_eighth_mi_buff <- sf::st_buffer(sf::st_geometry(ps_buff), buffer_dist_eighth)

  ps_buff$geom_qtr_mi_buff <- sf::st_buffer(sf::st_geometry(ps_buff), buffer_dist_qtr)



  append_eighth <- function(x) stringr::str_c(x, "EIGHTH",  sep = "_")

  append_qtr <- function(x) stringr::str_c(x,"QTR",  sep = "_")


  ps_buff_eighth <- ps_buff %>%
    sf::st_set_geometry("geom_eighth_mi_buff") %>%
    dplyr::select_if(not_sfc) %>%
    dplyr::rename_all(append_eighth)

  ps_buff_qtr <- ps_buff %>%
    sf::st_set_geometry("geom_qtr_mi_buff") %>%
    dplyr::select_if(not_sfc) %>%
    dplyr::rename_all(append_qtr)


  # ~ 1 min. operation
  p_ps_eighth <- sf::st_join(p_pt, ps_buff_eighth)


  # ~ 1 min. operation
  p_ps_qtr <- sf::st_join(p_pt, ps_buff_qtr)


  # ~ 12 min. operation

  p_prox_play_eighth <- p_ps_eighth %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(PIN) %>%
    tidyr::nest %>%
    dplyr::mutate(FILTER_PROXIMITY_PLAY_SPACE_EIGHTH = purrr::map_lgl(data, ~ !all(purrr::map_lgl(.x$PLAY_SPACE_OSM_ID_EIGHTH,is.na))),
           PLAY_SPACE_TYPE_EIGHTH = purrr::map_chr(data, ~ str_count_factor(.x$PLAY_SPACE_TYPE_EIGHTH))) %>%
    dplyr::transmute(PIN,
              FILTER_PROXIMITY_PLAY_SPACE_EIGHTH,
              PLAY_SPACE_TYPE_EIGHTH = dplyr::if_else(FILTER_PROXIMITY_PLAY_SPACE_EIGHTH,PLAY_SPACE_TYPE_EIGHTH, NA_character_))

  # ~ 8 min. operation
  p_prox_play_qtr <- p_ps_qtr %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(PIN) %>%
    tidyr::nest %>%
    dplyr::mutate(FILTER_PROXIMITY_PLAY_SPACE_QTR = purrr::map_lgl(data, ~ !all(purrr::map_lgl(.x$PLAY_SPACE_OSM_ID_QTR,is.na))),
           PLAY_SPACE_TYPES_QTR = purrr::map_chr(data, ~ str_count_factor(.x$PLAY_SPACE_TYPE_QTR))) %>%
    dplyr::transmute(PIN,
              FILTER_PROXIMITY_PLAY_SPACE_QTR,
              PLAY_SPACE_TYPE_QTR = dplyr::if_else(FILTER_PROXIMITY_PLAY_SPACE_QTR,PLAY_SPACE_TYPES_QTR, NA_character_))

  p_prox_play <- full_join(p_prox_play_eighth, p_prox_play_qtr, by = "PIN") %>%
    dplyr::mutate(FILTER_PROXIMITY_PLAY_SPACE = dplyr::case_when(
      FILTER_PROXIMITY_PLAY_SPACE_EIGHTH ~ "1/8 mile",
      FILTER_PROXIMITY_PLAY_SPACE_QTR ~ "1/4 mile",
      TRUE ~ "Greater than 1/4 mile"
    ))


  filters_proximity_play_space <- p_prox_play

  return(filters_proximity_play_space)

}

#' @rdname filters
#' @export
make_filters_proximity_marijuana <- function(...){
  p_prox_mj <-  parcel_sf_ready %>%
    sf::st_buffer(dist = set_units(1000, "ft")) %>%
    dplyr::transmute(PIN,
              FILTER_PROX_MJ_1000FT = st_intersects_any(.,mj_businesses),
              FILTER_PROX_MJ = dplyr::if_else(FILTER_PROX_MJ_1000FT, "Less than 1000ft", "Greater than 1000ft")) %>%
    sf::st_drop_geometry()

  filters_proximity_marijuana <- p_prox_mj

  return(filters_proximity_marijuana)

}

#' @rdname filters
#' @export
make_filters_proximity_el_facilities <- function(...){

  p_prox_el <-  parcel_sf_ready %>%
    sf::st_buffer(dist = set_units(500, "ft")) %>%
    dplyr::transmute(PIN,
              FILTER_PROX_EL_FACILITIES_500FT = st_intersects_any(., el_facilities),
              FILTER_PROX_EL_FACILITIES = dplyr::if_else(FILTER_PROX_EL_FACILITIES_500FT, "Less than 500ft", "Greater than 500ft")) %>%
    sf::st_drop_geometry()

  filters_proximity_el_facilities <- p_prox_el

  return(filters_proximity_el_facilities)

}

#' @rdname filters
#' @export
make_filters_proximity_affordable_housing <- function(parcel_sf_ready, affordable_housing_properties){

  loadd(parcel_sf_ready, affordable_housing_properties)

  p_pt <- parcel_sf_ready %>%
    sf::st_set_geometry("geom_pt") %>%
    sf::st_transform(2926) %>%
    dplyr::transmute(PIN)

  buffer_dist_qtr <- set_units(1/4, "mile")
  buffer_dist_half <- set_units(1/2, "mile")

  ah_buff <- affordable_housing_properties %>%
    sf::st_transform(2926)

  ah_buff$geom_qtr_mi_buff <- sf::st_buffer(sf::st_geometry(ah_buff), buffer_dist_qtr)

  ah_buff$geom_half_mi_buff <- sf::st_buffer(sf::st_geometry(ah_buff), buffer_dist_half)


  append_qtr <- function(x) stringr::str_c(x,"QTR",  sep = "_")

  append_half <- function(x) stringr::str_c(x, "HALF",  sep = "_")


  ah_buff_qtr <- ah_buff %>%
    sf::st_set_geometry("geom_qtr_mi_buff") %>%
    dplyr::select_if(not_sfc) %>%
    dplyr::rename_all(append_qtr)

  ah_buff_half <- ah_buff %>%
    sf::st_set_geometry("geom_half_mi_buff") %>%
    dplyr::select_if(not_sfc) %>%
    dplyr::rename_all(append_half)

  # ~ 1 min. operation

  p_ah_qtr <- sf::st_join(p_pt, ah_buff_qtr)

  # ~ 1 min. operation

  p_ah_half <- sf::st_join(p_pt, ah_buff_half)



  # ~ 11 min. operation


  p_prox_afford_qtr <- p_ah_qtr %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(PIN) %>%
    tidyr::nest %>%
    dplyr::mutate(FILTER_PROXIMITY_AFFORDABLE_HOUSING_QTR = purrr::map_lgl(data, ~ !all(purrr::map_lgl(.x$PROPERTY_NAME_QTR,is.na))),
           AFFORDABLE_HOUSING_TYPES_QTR = purrr::map_chr(data, ~ str_count_factor(.x$TARGET_TENANT_TYPE_QTR))) %>%
    dplyr::transmute(PIN,
              FILTER_PROXIMITY_AFFORDABLE_HOUSING_QTR,
              AFFORDABLE_HOUSING_TYPE_QTR = dplyr::if_else(FILTER_PROXIMITY_AFFORDABLE_HOUSING_QTR,AFFORDABLE_HOUSING_TYPES_QTR, NA_character_))


  # ~ 10 min. operation

  p_prox_afford_half <- p_ah_half %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(PIN) %>%
    tidyr::nest %>%
    dplyr::mutate(FILTER_PROXIMITY_AFFORDABLE_HOUSING_HALF = purrr::map_lgl(data, ~ !all(purrr::map_lgl(.x$PROPERTY_NAME_HALF,is.na))),
           AFFORDABLE_HOUSING_TYPE_HALF = purrr::map_chr(data, ~ str_count_factor(.x$TARGET_TENANT_TYPE_HALF))) %>%
    dplyr::transmute(PIN,
              FILTER_PROXIMITY_AFFORDABLE_HOUSING_HALF,
              AFFORDABLE_HOUSING_TYPE_HALF = dplyr::if_else(FILTER_PROXIMITY_AFFORDABLE_HOUSING_HALF,AFFORDABLE_HOUSING_TYPE_HALF, NA_character_))


  p_prox_afford <- full_join(p_prox_afford_qtr, p_prox_afford_half, by = "PIN") %>%
    dplyr::mutate(FILTER_PROXIMITY_AFFORDABLE_HOUSING = dplyr::case_when(
      FILTER_PROXIMITY_AFFORDABLE_HOUSING_QTR ~ "1/4 mile",
      FILTER_PROXIMITY_AFFORDABLE_HOUSING_HALF ~ "1/2 mile",
      TRUE ~ "Greater than 1/2 mile"
    ))


  filters_proximity_affordable_housing <- p_prox_afford

  return(filters_proximity_affordable_housing)

}

#' @rdname filters
#' @export
make_filters_potential_units <- function(parcel_ready){

  # THIS IS DUMMY DATA + SHOULD BE REPLACED

  p_ready_pu<- parcel_ready %>%
    sf::st_drop_geometry() %>%
    dplyr::transmute(PIN,
              FILTER_POTENTIAL_UNITS = integer_dummy(n(),10,5)
    )

  filters_potential_units <- p_ready_pu

  return(filters_potential_units)

}

#' @rdname filters
#' @export
make_filters_leg_district <- function(...){

  p_pt <- parcel_sf_ready %>%
    sf::st_set_geometry("geom_pt") %>%
    sf::st_transform(2926) %>%
    dplyr::select(PIN)

  leg <- leg_districts %>%
    dplyr::transmute(LEGISLATIVE_DISTRICT = stringr::str_c("District ",LEGDST)) %>%
    sf::st_transform(2926)

  leg_subd <- leg %>%
    lwgeom::st_subdivide(max_vertices = 100) %>%
    sf::st_collection_extract()

  p_leg <- p_pt

  p_leg$FILTER_LEGISLATIVE_DISTRICT <- st_over(p_leg, leg_subd, "LEGISLATIVE_DISTRICT")

  # Deal with outliers

  outside_pins <- p_leg %>%
    dplyr::filter(is.na(FILTER_LEGISLATIVE_DISTRICT)) %>%
    pluck("PIN")

  p_outside <- dplyr::filter(p_pt, PIN %in% outside_pins)

  leg_buff_2000 <- sf::st_buffer(leg, dist = 2000)

  p_outside$FILTER_LEGISLATIVE_DISTRICT_OUTSIDE <- st_over(p_outside, leg_buff_2000,"LEGISLATIVE_DISTRICT") %>%
    sf::st_drop_geometry()

  # Merge together

  p_leg_ready <- sf::st_drop_geometry(p_leg) %>%
    dplyr::left_join(sf::st_drop_geometry(p_outside), by = "PIN") %>%
    arrange(FILTER_LEGISLATIVE_DISTRICT_OUTSIDE) %>%
    dplyr::transmute(PIN,
              FILTER_LEGISLATIVE_DISTRICT = dplyr::case_when(
                !is.na(FILTER_LEGISLATIVE_DISTRICT_OUTSIDE) ~ FILTER_LEGISLATIVE_DISTRICT_OUTSIDE,
                !is.na(FILTER_LEGISLATIVE_DISTRICT) ~ FILTER_LEGISLATIVE_DISTRICT,
                TRUE ~ "Outside King County"
              ))

  leg_district <- p_leg_ready

  return(leg_district)

}

#' @rdname filters
#' @export
make_filters_kc_council_district <- function(...){

  p_pt <- parcel_sf_ready %>%
    sf::st_set_geometry("geom_pt") %>%
    sf::st_transform(2926) %>%
    dplyr::select(PIN)

  kcc <- kc_council_districts %>%
    dplyr::transmute(KC_COUNCIL_DISTRICT = stringr::str_c("District ",KCCDST),
              KC_COUNCIL_MEMBER = COUNCILMEM) %>%
    sf::st_transform(2926)

  kcc_members <- kcc %>%
    sf::st_drop_geometry %>%
    dplyr::select(KC_COUNCIL_DISTRICT,KC_COUNCIL_MEMBER) %>%
    distinct()

  kcc_subd <- kcc %>%
    lwgeom::st_subdivide(max_vertices = 100) %>%
    sf::st_collection_extract()

  p_kcc <- p_pt

  p_kcc$FILTER_KC_COUNCIL_DISTRICT <- st_over(p_kcc, kcc_subd, "KC_COUNCIL_DISTRICT")

  # Deal with outliers

  outside_pins <- p_kcc %>%
    dplyr::filter(is.na(FILTER_KC_COUNCIL_DISTRICT)) %>%
    pluck("PIN")

  p_outside <- dplyr::filter(p_pt, PIN %in% outside_pins)

  kcc_buff_2000 <- sf::st_buffer(kcc, dist = 2000)

  p_outside$FILTER_KC_COUNCIL_DISTRICT_OUTSIDE <- st_over(p_outside, kcc_buff_2000,"KC_COUNCIL_DISTRICT") %>%
    sf::st_drop_geometry()

  # Merge together

  p_kcc_ready <- sf::st_drop_geometry(p_kcc) %>%
    dplyr::left_join(sf::st_drop_geometry(p_outside), by = "PIN") %>%
    arrange(FILTER_KC_COUNCIL_DISTRICT_OUTSIDE) %>%
    dplyr::transmute(PIN,
              FILTER_KC_COUNCIL_DISTRICT = dplyr::case_when(
                !is.na(FILTER_KC_COUNCIL_DISTRICT_OUTSIDE) ~ FILTER_KC_COUNCIL_DISTRICT_OUTSIDE,
                !is.na(FILTER_KC_COUNCIL_DISTRICT) ~ FILTER_KC_COUNCIL_DISTRICT,
                TRUE ~ "Outside King County"
              )) %>%
    dplyr::left_join(kcc_members, by = c(FILTER_KC_COUNCIL_DISTRICT = "KC_COUNCIL_DISTRICT"))

  kc_council_district <- p_kcc_ready

  return(kc_council_district)

}

#' @rdname filters
#' @export
make_filters_seattle_council_district <- function(...){

  p_pt <- parcel_sf_ready %>%
    sf::st_set_geometry("geom_pt") %>%
    sf::st_transform(2926) %>%
    dplyr::select(PIN)

  seacc <- seattle_council_districts %>%
    sf::st_transform(2926)

  seacc_subd <- seacc %>%
    lwgeom::st_subdivide(max_vertices = 100) %>%
    sf::st_collection_extract()

  p_seacc <- p_pt

  p_seacc$FILTER_SEATTLE_COUNCIL_DISTRICT <- st_over(p_seacc, seacc_subd, "SEATTLE_COUNCIL_DISTRICT")

  filter_seattle_council_district <- p_seacc %>%
    sf::st_drop_geometry()

  return(filter_seattle_council_district)

}

#' @rdname filters
#' @export
make_filters_school_district <- function(...){

  p_pt <- parcel_sf_ready %>%
    sf::st_set_geometry("geom_pt") %>%
    sf::st_transform(2926) %>%
    dplyr::select(PIN)

  schl <- school_districts %>%
    dplyr::transmute(SCHOOL_DISTRICT = NAME) %>%
    sf::st_transform(2926) %>%
    lwgeom::st_subdivide(max_vertices = 100) %>%
    sf::st_collection_extract()

  p_schl <- p_pt

  p_schl$FILTER_SCHOOL_DISTRICT <- st_over(p_schl, schl, "SCHOOL_DISTRICT")

  p_schl_ready <- sf::st_drop_geometry(p_schl)

  school_district <- p_schl_ready

  return(school_district)

}

#' @rdname filters
#' @export
make_filters_historic <- function(parcel_ready){

  p_ready_hist<- parcel_ready %>%
    sf::st_drop_geometry() %>%
    dplyr::transmute(PIN,
              FILTER_HISTORIC_LGL = dplyr::if_else(HISTORIC_SITE %in% "DESIGNATED",TRUE,FALSE, missing = FALSE)
    )

  filters_historic <- p_ready_hist

  return(filters_historic)

}

#' @rdname filters
#' @export
make_filters_afford_expir_date <- function(parcel_sf_ready, affordable_housing_subsidies){

  p_ready_afford_expir_date <- parcel_sf_ready %>%
    sf::st_join(affordable_housing_subsidies) %>%
    sf::st_drop_geometry() %>%
    dplyr::transmute(PIN,
              FILTER_AFFORD_TYPE = dplyr::case_when(
                is.na(SUBSIDY_NAME) ~ "none",
                SUBSIDY_NAME %in% c("Section 202", "RHS 515") ~ "Other",
                TRUE ~ SUBSIDY_NAME
              ),
              FILTER_AFFORD_UNITS = ASSISTED_UNITS,
              FILTER_AFFORD_EXPIR_DATE = as.Date(END_DATE)
    ) %>%
    dplyr::group_by(PIN) %>%
    arrange(FILTER_AFFORD_EXPIR_DATE) %>%
    slice(1) %>%
    ungroup

  filters_afford_expir_date <- p_ready_afford_expir_date

  return(filters_afford_expir_date)

}

#' @rdname filters
#' @export
make_filters_eligibility_nmtc <- function(filters_census_tract){

  elig_nmtc_fp <- here("1-data/2-external/NMTC-2011-2015-LIC-Nov2-2017-4pm.xlsx")

  elig_nmtc_dr_id <- as_id("1gM7oRPZTEDM4N7Xhcmih8R4WFTxLj8rK")

  elig_nmtc_load <-
    make_or_read2(fp = elig_nmtc_fp,
                  dr_id = elig_nmtc_dr_id,
                  skip_get_expr = FALSE,
                  get_expr = function(fp){
                    # SOURCE: https://www.cdfifund.gov/Documents/NMTC%202011-2015%20LIC%20Nov2-2017-4pm.xlsx
                  },
                  make_expr = function(fp, dr_id){
                    drive_read(dr_id = dr_id,  path = fp, read_fun = read_xlsx,.tempfile = FALSE)
                  },
                  read_expr = function(fp){read_xlsx(fp)})

  elig_nmtc <- elig_nmtc_load %>%
    clean_names() %>%
    dplyr::rename_all(to_screaming_snake_case) %>%
    dplyr::transmute(FILTER_CENSUS_TRACT = X_2010_CENSUS_TRACT_NUMBER_FIPS_CODE_GEOID,
              NMTC = DOES_CENSUS_TRACT_QUALIFY_FOR_NMTC_LOW_INCOME_COMMUNITY_LIC_ON_POVERTY_OR_INCOME_CRITERIA,
              FILTER_ELIGIBILITY_NMTC = dplyr::if_else(NMTC %in% "Yes",TRUE,FALSE, missing = FALSE)
    )


  p_ready_eligibility_nmtc <- filters_census_tract %>%
    sf::st_drop_geometry() %>%
    dplyr::select(PIN, FILTER_CENSUS_TRACT) %>%
    dplyr::left_join(elig_nmtc, by = "FILTER_CENSUS_TRACT") %>%
    dplyr::transmute(PIN,
              FILTER_ELIGIBILITY_NMTC = dplyr::if_else(FILTER_ELIGIBILITY_NMTC, TRUE, FALSE, missing = FALSE))

  filters_eligibility_nmtc <- p_ready_eligibility_nmtc

  return(filters_eligibility_nmtc)

}

#' @rdname filters
#' @export
make_filters_eligibility_dda <- function(filters_zcta){

  elig_dda_fp <- here("1-data/2-external/DDA2018M.PDF")

  elig_dda_dr_id <- as_id("1KbD_gAHy_0DTTxHZgTbL96VOviTwdNRZ")

  make_or_read2(fp = elig_dda_fp,
                dr_id = elig_dda_dr_id,
                skip_get_expr = TRUE,
                get_expr = function(fp){
                  # SOURCE:  https://www.huduser.gov/portal/Datasets/qct/DDA2018M.PDF
                },
                make_expr = function(fp, dr_id){

                  drive_download(dr_id, path = fp)

                },
                read_expr = function(fp){
                  message(glue("* Note: This file is not actually read but it does exists here: '{fp}'."))
                })

  list_pages <- extract_tables(elig_dda_fp, output = "data.frame")

  tbl_pages <- list_pages %>%
    map(~ .x %>% t %>% as_tibble) %>%
    reduce(bind_cols) %>%
    dplyr::mutate_all(funs(empty_as_na))

  zcta_dda <- tbl_pages %>%
    slice(3:14) %>%
    gather(OLD_COL,ZCTA) %>%
    drop_na() %>%
    dplyr::transmute(FILTER_ZCTA = str_replace(ZCTA,"\\*",""),
              FILTER_ELIGIBILITY_DDA = TRUE)

  elig_dda <- filters_zcta %>%
    dplyr::left_join(zcta_dda, by = "FILTER_ZCTA") %>%
    dplyr::transmute(PIN,
              FILTER_ELIGIBILITY_DDA = dplyr::if_else(FILTER_ELIGIBILITY_DDA,TRUE,FALSE, missing = FALSE))

  filters_eligibility_dda <- elig_dda

  return(filters_eligibility_dda)
}

#' @rdname filters
#' @export
make_filters_eligibility_qct <- function(filters_census_tract){

  elig_qtc_fp <- here("1-data/2-external/QCT2018.DBF")

  elig_qtc_dr_id <- as_id("1JU0MQKta1mQurT89DJtk8nFvYcgxP4qk")

  elig_qtc_load <-
    make_or_read2(fp = elig_qtc_fp,
                  dr_id = elig_qtc_dr_id,
                  skip_get_expr = FALSE,
                  get_expr = function(fp){
                    # SOURCE:  https://www.huduser.gov/portal/datasets/qct/QCT2018dbf.zip
                  },
                  make_expr = function(fp, dr_id){

                    target_name <- "QCT2018.DBF"

                    dir_path <- here("1-data/2-external/")

                    drive_read_zip(dr_id = dr_id,
                                   .tempdir = FALSE,
                                   dir_path = dir_path,
                                   read_fun = foreign::read.dbf,
                                   target_name = target_name)
                  },
                  read_expr = function(fp){foreign::read.dbf(fp)})

  elig_qtc <- elig_qtc_load %>%
    dplyr::transmute(FILTER_CENSUS_TRACT = as.character(FIPS),
              FILTER_ELIGIBILITY_QCT = TRUE)


  p_ready_eligibility_qct <- filters_census_tract %>%
    sf::st_drop_geometry() %>%
    dplyr::select(PIN, FILTER_CENSUS_TRACT) %>%
    dplyr::left_join(elig_qtc, by = "FILTER_CENSUS_TRACT") %>%
    dplyr::transmute(PIN,
              FILTER_ELIGIBILITY_QCT = dplyr::if_else(FILTER_ELIGIBILITY_QCT, TRUE, FALSE, missing = FALSE))

  filters_eligibility_qct <- p_ready_eligibility_qct

  return(filters_eligibility_qct)

}

#' @rdname filters
#' @export
make_filters_eligibility_oz <- function(filters_census_tract){

  elig_oz_fp <- here("1-data/3-interim/KC-Opportunity-Zones-2018.xlsx")

  elig_oz_dr_id <- as_id("1-xCiQoCCO1esgmUUdlVt6Wlm-HLN4ONw0lUwrBYoXZA")

  elig_oz_load <-
    make_or_read2(fp = elig_oz_fp,
                  dr_id = elig_oz_dr_id,
                  skip_get_expr = FALSE,
                  get_expr = function(fp){
                    # SOURCE:  http://www.commerce.wa.gov/growing-the-economy/opportunity-zones/
                  },
                  make_expr = function(fp, dr_id){
                    drive_read(dr_id = elig_oz_dr_id, .tempfile = FALSE, path = fp, read_fun = read_excel)
                  },
                  read_expr = function(fp){
                    read_excel(fp)
                  })

  elig_oz <- elig_oz_load %>%
    dplyr::transmute(FILTER_CENSUS_TRACT = as.character(GEOID),
              FILTER_ELIGIBILITY_OZ = TRUE)


  p_ready_eligibility_oz <- filters_census_tract %>%
    dplyr::left_join(elig_oz, by = "FILTER_CENSUS_TRACT") %>%
    dplyr::transmute(PIN,
              FILTER_ELIGIBILITY_OZ = dplyr::if_else(FILTER_ELIGIBILITY_OZ, TRUE, FALSE, missing = FALSE))

  filters_eligibility_oz <- p_ready_eligibility_oz

  return(filters_eligibility_oz)

}

#' @rdname filters
#' @export
make_filters_parking <- function(parcel_df_ready){

  # CHECK OUT HOW MANY PARCELS THIS WILL INCLUDE

  # parcel_df_ready %>%
  #   dplyr::filter(stringr::str_detect(toupper(PRESENT_USE),"PARKING" )) %>%
  #   count(PRESENT_USE, sort = TRUE)

  # Total: 1,836

  filters_parking <- parcel_df_ready %>%
    dplyr::transmute(PIN,
              FILTER_PARKING = PRESENT_USE %in% c("Parking Assoc","Parking Commercial Lot"))

  return(filters_parking)
}

#' @rdname filters
#' @export
make_filters_proximity_lightrail <- function(parcel_sf_ready, future_lightrail){

  p_pt <- parcel_sf_ready %>%
    sf::st_set_geometry("geom_pt") %>%
    sf::st_transform(2926) %>%
    dplyr::transmute(PIN)

  flr <- future_lightrail %>%
    dplyr::filter(!stringr::str_detect(LOCATION_CERTAINTY,"exists")) %>%
    dplyr::transmute(PROJECT,
              NAME,
              LOCATION_STATUS = dplyr::case_when(
                stringr::str_detect(LOCATION_CERTAINTY, "high") ~ "confirmed",
                stringr::str_detect(LOCATION_CERTAINTY, "low") ~ "unconfirmed",
                TRUE ~ "other"
              )) %>%
    sf::st_transform(2926)


  buffer_dist_half <- set_units(1/2, "mile")

  p_flr <- sf::st_join(p_pt, sf::st_buffer(flr, buffer_dist_half))

  # ~ 10 min. operation

  p_prox_flr <- p_flr %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(PIN) %>%
    tidyr::nest %>%
    dplyr::mutate(TYPES = map(data, "LOCATION_STATUS")) %>%
    dplyr::group_by(PIN) %>%
    dplyr::transmute( FILTER_PROXIMITY_FUTURE_LIGHTRAIL = dplyr::case_when(
      any(flatten_chr(TYPES) %in% "confirmed") ~ "1/2 mile (confirmed station)",
      any(flatten_chr(TYPES) %in% "unconfirmed") ~ "1/2 mile (unconfirmed station)",
      TRUE ~ "Greater than 1/2 mile"),
      FILTER_PROXIMITY_FUTURE_LIGHTRAIL_HALF = dplyr::case_when(
        FILTER_PROXIMITY_FUTURE_LIGHTRAIL %in% "Greater than 1/2 mile" ~ FALSE,
        TRUE ~ TRUE
      ),
      FUTURE_LIGHTRAIL_STATION_TYPES = purrr::map_chr(data, ~ str_count_factor(.x$LOCATION_STATUS)),
      FUTURE_LIGHTRAIL_STATION_NAMES = purrr::map_chr(data, ~ stringr::str_c(.x$NAME, collapse = ", "))
    ) %>%
    ungroup %>%
    dplyr::select(PIN,
           FILTER_PROXIMITY_FUTURE_LIGHTRAIL_HALF,
           FUTURE_LIGHTRAIL_STATION_TYPES,
           FUTURE_LIGHTRAIL_STATION_NAMES,
           FILTER_PROXIMITY_FUTURE_LIGHTRAIL)

  filters_proximity_transit <- p_prox_flr

  return(filters_proximity_transit)

}

#' @rdname filters
#' @export
make_filters_brownfield <- function(parcel_sf_ready, brownfield_sites){

  brownfield_ready <- brownfield_sites %>%
    dplyr::transmute(FILTER_BROWNFIELD = TRUE,
              FILTER_BROWNFIELD_NAME = str_trim(CLEANUP_SITE_NAME),
              FILTER_BROWNFIELD_STATUS = str_to_lower(ECOLOGY_STATUS),
              FILTER_BROWNFIELD_TYPE = str_to_lower(CONTAMINANT_TYPE)) %>%
    sf::st_transform(2926)

  # note: as of 2018-07-11 there are no brownfield sites in King County,
  # so this join doesn't actually result in any data from RHS being joined to LHS

  p_ready_brownfield <- parcel_sf_ready %>%
    sf::st_join(brownfield_ready) %>%
    sf::st_drop_geometry() %>%
    dplyr::select_if(not_sfc)


  filters_brownfield <- p_ready_brownfield

  return(filters_brownfield)

}

#' @rdname filters
#' @export
make_filters_contaminated <- function(parcel_sf_ready, contaminated_sites){

  contaminated_ready <- sf::st_transform(contaminated_sites, 2926) %>%
    sf::st_buffer(set_units(10, "feet"))

  p_ready_contaminated <- parcel_sf_ready %>%
    sf::st_join(contaminated_ready) %>%
    sf::st_drop_geometry() %>%
    dplyr::select_if(not_sfc) %>%
    dplyr::select(-CLEANUP_SITE_ID)

  filters_contaminated <- p_ready_contaminated

  return(filters_contaminated)

}
