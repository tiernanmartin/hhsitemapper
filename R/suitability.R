#' @title Site Suiteability
#' @description Targets related to the suitability of a site for affordable
#'   housing and/or early learning facility development.
#' @return All suitability-related commands return a \code{\link[tibble:tbl_df]{tbl_df}} object.

#' @rdname suitability
#' @export
make_suitability <- function(parcel_ready,
                             suitability_criteria,
                             suitability_tax_exempt,
                             suitability_water_overlap,
                             suitability_within_uga,
                             suitability_developable_zoning,
                             suitability_present_use,
                             suitability_lot_size,
                             suitability_parcel_area_ratio,
                             suitability_steep_vacant,
                             suitability_unbuildable,
                             suitability_other){

  suitability_fields_list <- list(suitability_tax_exempt,
                                  suitability_water_overlap,
                                  suitability_within_uga,
                                  suitability_developable_zoning,
                                  suitability_present_use,
                                  suitability_lot_size,
                                  suitability_parcel_area_ratio,
                                  suitability_steep_vacant,
                                  suitability_unbuildable,
                                  suitability_other)

  parcel_suit <- suitability_fields_list %>%
    purrr::reduce(dplyr::left_join, by = "PIN") %>%
    dplyr::right_join(parcel_ready, by = "PIN")

  parcel_suit_lgl <- parcel_suit %>%
    dplyr::mutate(
      SUITABLE_OWNER_LGL = dplyr::if_else(SUIT_OWNER_TAX_E == suitability_criteria[["tax_exempt"]],TRUE,FALSE,FALSE),
      SUITABLE_WATER_OVERLAP_LGL = dplyr::if_else(SUIT_WATER_OVERLAP_PCT <= suitability_criteria[["water_overlap"]],TRUE,FALSE,FALSE),
      SUITABLE_WITHIN_UGA_LGL = dplyr::if_else(SUIT_WITHIN_UGA == suitability_criteria[["within_uga"]],TRUE,FALSE,FALSE),
      SUITABLE_ZONING_CONSOL_20_LGL = dplyr::if_else(SUIT_ZONING_CONSOL_20 %in% suitability_criteria[["developable_zoning"]],TRUE,FALSE,FALSE) ,
      SUITABLE_PRESENT_USE_LGL = dplyr::if_else(! SUIT_PRESENT_USE %in% suitability_criteria[["undevelopable_presentuse"]],TRUE,FALSE,FALSE),
      SUITABLE_LOT_SIZE_LGL = dplyr::if_else(SUIT_LOT_SIZE %in% suitability_criteria[["lot_size"]],TRUE,FALSE,FALSE),
      SUITABLE_PARCEL_AREA_RATIO_LGL = dplyr::if_else(SUIT_PARCEL_AREA_RATIO >= suitability_criteria[["area_ratio"]],TRUE,FALSE,FALSE),
      SUITABLE_STEEP_VACANT_LGL = dplyr::if_else(SUIT_STEEP_VACANT == suitability_criteria[["steep_vacant"]], TRUE, FALSE, FALSE),
      SUITABLE_UNBUILDABLE_LGL = dplyr::if_else(SUIT_UNBUILDABLE == suitability_criteria[["unbuildable"]], TRUE, FALSE, FALSE),
      SUITABLE_OTHER_LGL = dplyr::case_when(is.na(SUIT_OTHER) ~ NA,
                                            SUIT_OTHER == suitability_criteria[["other"]] ~ TRUE,
                                            TRUE ~ FALSE),
      SUITABLE_LGL = dplyr::case_when(!is.na(SUITABLE_OTHER_LGL) ~ SUITABLE_OTHER_LGL,
                                      TRUE ~ (SUITABLE_OWNER_LGL & SUITABLE_WATER_OVERLAP_LGL & SUITABLE_WITHIN_UGA_LGL & SUITABLE_ZONING_CONSOL_20_LGL & SUITABLE_PRESENT_USE_LGL & SUITABLE_LOT_SIZE_LGL & SUITABLE_PARCEL_AREA_RATIO_LGL & SUITABLE_STEEP_VACANT_LGL & SUITABLE_UNBUILDABLE_LGL)
      )
    )

  parcel_suit_sf <- sf::st_sf(parcel_suit_lgl)

  suitability <- parcel_suit_sf

  return(suitability)
}


#' @rdname suitability
#' @export
make_suitability_tax_exempt <- function(parcel_ready){

  suitability_tax_exempt <- parcel_ready %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate(SUIT_OWNER_PUBLIC = dplyr::if_else(ASSESSOR_PUB_LIST_LGL,TRUE,FALSE,FALSE),
                  SUIT_OWNER_NONPROFIT = dplyr::if_else(TAX_REASON %in% "non profit exemption",TRUE,FALSE,FALSE),
                  SUIT_OWNER_OTHER_EXEMPT = dplyr::if_else(TAX_REASON %in% "exempt" & !SUIT_OWNER_PUBLIC,TRUE,FALSE,FALSE),
                  SUIT_OWNER_TAX_E = SUIT_OWNER_PUBLIC | SUIT_OWNER_NONPROFIT | SUIT_OWNER_OTHER_EXEMPT) %>%
    dplyr::select(PIN,
                  SUIT_OWNER_PUBLIC,
                  SUIT_OWNER_NONPROFIT,
                  SUIT_OWNER_OTHER_EXEMPT,
                  SUIT_OWNER_TAX_E)

  return(suitability_tax_exempt)
}

#' @rdname suitability
#' @export
make_suitability_water_overlap <- function(parcel_sf_ready, kc_waterbodies, king_county){

  # Convert to EPSG 2926
  p_ready_poly <- parcel_sf_ready %>%
    sf::st_set_geometry("geometry") %>%
    dplyr::select(PIN) %>%
    sf::st_transform(2926)

  # Filter waterbodies to include only those larger than 1/2 sq km

  min_sq_km <- 0.5

  tolerance <- 5

  wtr <- kc_waterbodies %>%
    sf::st_transform(2926) %>%
    dplyr::filter(AREA_SQ_KM > min_sq_km) %>%
    sf::st_simplify(preserveTopology = TRUE, dTolerance = tolerance)

  wtr_in_kc <- sf::st_intersection(wtr,sf::st_union(king_county))

  p_water <- p_ready_poly

  # ~ 3 min. operation

  p_water$SUIT_WATER_OVERLAP_LGL <- st_intersects_any(x = p_water,y = wtr_in_kc)

  intersect_idx <- which(p_water$SUIT_WATER_OVERLAP_LGL)

  p_water$SUIT_WATER_OVERLAP_PCT <- as.double(0)

  wtr_union <- sf::st_union(wtr)

  # ~ 2 min. operation

  p_water[intersect_idx,"SUIT_WATER_OVERLAP_PCT"] <- st_intersect_area(x = p_water[intersect_idx,],
                                                                       y = wtr_union)


  p_water_ready <- p_water %>%
    sf::st_drop_geometry() %>%
    dplyr::select(PIN,
                  SUIT_WATER_OVERLAP_LGL,
                  SUIT_WATER_OVERLAP_PCT)

  suitability_water_overlap <- p_water_ready

  return(suitability_water_overlap)
}

#' @rdname suitability
#' @export
make_suitability_within_uga <- function(parcel_sf_ready, uga){

  p_ready_pt <- parcel_sf_ready %>%
    sf::st_set_geometry("geom_pt") %>%
    sf::st_transform(2926)

  uga_2926 <- sf::st_transform(uga, 2926)

  uga_subdivide <- lwgeom::st_subdivide(uga_2926, 100) %>%
    sf::st_collection_extract()

  # ~ 2 min. operation
  p_ready_pt$SUIT_WITHIN_UGA <- st_intersects_any(p_ready_pt, uga_subdivide)

  p_ready_within_uga <- p_ready_pt %>%
    sf::st_drop_geometry() %>%
    dplyr::select(PIN, SUIT_WITHIN_UGA)

  suitability_within_uga <- p_ready_within_uga

  return(suitability_within_uga)
}

#' @rdname suitability
#' @export
make_suitability_developable_zoning <- function(parcel_sf_ready, zoning){

  p_pt <- parcel_sf_ready %>%
    sf::st_set_geometry("geom_pt") %>%
    sf::st_transform(2926) %>%
    dplyr::select(PIN)

  zng <- zoning %>%
    sf::st_transform(2926) %>%
    lwgeom::st_subdivide(max_vertices = 100) %>%
    sf::st_collection_extract()

  p_zng <- p_pt

  p_zng$SUIT_ZONING_CONSOL_20 <- st_over(p_zng, zng, "CONSOL_20")

  p_dz_ready <- sf::st_drop_geometry(p_zng)

  suitability_developable_zoning <- p_dz_ready


  return(suitability_developable_zoning)
}

#' @rdname suitability
#' @export
make_suitability_present_use <- function(parcel_ready){

  suitability_present_use <- parcel_ready %>%
    sf::st_drop_geometry() %>%
    dplyr::transmute(PIN,
                     SUIT_PRESENT_USE = PRESENT_USE)


  return(suitability_present_use)
}

#' @rdname suitability
#' @export
make_suitability_lot_size <- function(parcel_sf_ready, lot_size_breaks){

  # ~ 10 min. operation

  # Create SUIT_PARCEL_AREA
  p_area_poly_all <- parcel_sf_ready %>%
    sf::st_set_geometry("geometry") %>%
    dplyr::select_if(not_sfc) %>%
    sf::st_cast("POLYGON") %>%
    dplyr::mutate(SUIT_PARCEL_AREA = units::set_units(sf::st_area(.), acre),
                  SUIT_LOT_SIZE = cut(SUIT_PARCEL_AREA,
                                      breaks = lot_size_breaks$breaks,
                                      labels = lot_size_breaks$names)
    )

  # select the largest polygon within the multipolygons
  p_area_poly_largest_only <- p_area_poly_all%>%
    dplyr::group_by(PIN) %>%
    dplyr::arrange(dplyr::desc(SUIT_PARCEL_AREA)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    sf::st_drop_geometry() %>%
    dplyr::transmute(PIN,
                     SUIT_PARCEL_AREA,
                     SUIT_LOT_SIZE)

  suitability_lot_size <- p_area_poly_largest_only

  return(suitability_lot_size)
}

#' @rdname suitability
#' @export
make_suitability_parcel_area_ratio <- function(parcel_sf_ready){

  # select the largest polygon within the multipolygons
  p_largest_polygons <- parcel_sf_ready %>%
    sf::st_set_geometry("geometry") %>%
    dplyr::select_if(not_sfc) %>%
    sf::st_cast("POLYGON") %>%
    dplyr::mutate(AREA_POLY = sf::st_area(.)) %>%
    dplyr::group_by(PIN) %>%
    dplyr::arrange(desc(AREA_POLY)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()


  # ~ 34 min. operation

  suitability_parcel_area_ratio <- p_largest_polygons %>%
    dplyr::transmute(PIN,
                     SUIT_PARCEL_AREA_RATIO = st_area_ratio(.)) %>%
    sf::st_drop_geometry()


  return(suitability_parcel_area_ratio)
}


#' @rdname suitability
#' @export
make_suitability_steep_vacant <- function(parcel_ready){

  suitability_steep_vacant <- parcel_ready %>%
    sf::st_drop_geometry() %>%
    dplyr::transmute(PIN,
                     SUIT_STEEP_VACANT = dplyr::if_else(PRESENT_USE == "Vacant Single family" & TOPOGRAPHY, TRUE, FALSE, FALSE)
    )

  return(suitability_steep_vacant)
}

#' @rdname suitability
#' @export
make_suitability_unbuildable <- function(parcel_ready){

  suitability_unbuildable <- parcel_ready %>%
    sf::st_drop_geometry() %>%
    dplyr::transmute(PIN,
                     SUIT_UNBUILDABLE = UNBUILDABLE
    )

  return(suitability_unbuildable)
}

#' @rdname suitability
#' @export
make_suitability_other <- function(parcel_ready, other_suitability_characteristics){

  suitability_other <- parcel_ready %>%
    sf::st_drop_geometry() %>%
    dplyr::left_join(other_suitability_characteristics, by = "PIN") %>%
    dplyr::select(PIN,
                  SUIT_OTHER,
                  SUIT_OTHER_TYPE,
                  SUIT_OTHER_NOTE)


  return(suitability_other)
}
