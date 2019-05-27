#' @title General Parcel Information
#' @description General information for all parcels in King County.
#' @note Data source: King County Assessor's Office
#' @param path
#' @rdname parcel_df
#' @export
make_parcel_df <- function(path){

  parcel_df <- readr::read_csv(path) %>%
    janitor::clean_names("screaming_snake")

  return(parcel_df)
}

#' @title General Parcel Information (Ready)
#' @description Desc
#' @param parcel_lookup desc
#' @param prop_type desc
#' @param pub_parcel desc
#' @param parcel_df desc
#' @return a character string
#' @rdname parcel_df
#' @export
make_parcel_df_ready <- function(parcel_lookup, prop_type, pub_parcel, parcel_df){

  # MAKE P_READY

  recode_cols <- unique(parcel_lookup$FIELD_NAME) %>% purrr::keep(~.x %in% colnames(parcel_df))

  fine_cols <- colnames(parcel_df) %>% purrr::discard(~.x %in% recode_cols)


  parcel_recode_cols <- parcel_df %>%
    dplyr::select_at(dplyr::vars(recode_cols)) %>%
    tidyr::gather(FIELD_NAME, LU_ITEM) %>%
    dplyr::left_join(parcel_lookup, by = c("FIELD_NAME", "LU_ITEM")) %>%
    dplyr::select(FIELD_NAME, LU_DESCRIPTION) %>%
    dplyr::group_by(FIELD_NAME) %>%
    dplyr::mutate(ROW = 1:dplyr::n()) %>%
    tidyr::spread(FIELD_NAME,LU_DESCRIPTION) %>%
    dplyr::select(-ROW)


  parcel_df_recoded <- parcel_df %>%
    dplyr::select_at(dplyr::vars(fine_cols)) %>%
    dplyr::bind_cols(parcel_recode_cols) %>%
    dplyr::mutate_if(is_logical_yn, recode_logical_yn) %>%
    dplyr::mutate_if(is_logical_01, recode_logical_01) %>%
    dplyr::mutate_if(is_logical_yesno, recode_logical_yesno) %>%
    dplyr::mutate(PIN = make_pin(MAJOR, MINOR),
           PROPERTY_NAME = str_clean_upper(PROP_NAME))

  pub_parcel_ready <- pub_parcel %>%
    dplyr::transmute(PIN,
              ASSESSOR_PUB_LIST_LGL = TRUE)

  p_df_ready <- parcel_df_recoded %>%
     dplyr::left_join(prop_type, by = "PROP_TYPE") %>%
     dplyr::left_join(pub_parcel_ready, by = "PIN") %>%
     dplyr::mutate(ASSESSOR_PUB_LIST_LGL =  dplyr::if_else(is.na(ASSESSOR_PUB_LIST_LGL),FALSE,ASSESSOR_PUB_LIST_LGL)) %>%
     dplyr::transmute(PIN,
              PROPERTY_NAME,
              PROP_TYPE = PROP_TYPE_DESC,
              ASSESSOR_PUB_LIST_LGL,
              DISTRICT_NAME = str_clean_upper(DISTRICT_NAME),
              CURRENT_ZONING,
              PRESENT_USE,
              HBU_AS_IF_VACANT,
              SQ_FT_LOT,
              ACCESS,
              TOPOGRAPHY,
              RESTRICTIVE_SZ_SHAPE,
              PCNT_UNUSABLE,
              CONTAMINATION,
              HISTORIC_SITE,
              CURRENT_USE_DESIGNATION,
              NATIVE_GROWTH_PROT_ESMT,
              EASEMENTS,
              OTHER_DESIGNATION,
              DEED_RESTRICTIONS,
              DEVELOPMENT_RIGHTS_PURCH,
              COAL_MINE_HAZARD,
              CRITICAL_DRAINAGE,
              EROSION_HAZARD,
              LANDFILL_BUFFER,
              HUNDRED_YR_FLOOD_PLAIN,
              SEISMIC_HAZARD,
              LANDSLIDE_HAZARD,
              STEEP_SLOPE_HAZARD,
              STREAM,
              WETLAND,
              SPECIES_OF_CONCERN,
              SENSITIVE_AREA_TRACT,
              WATER_PROBLEMS,
              TRANSP_CONCURRENCY,
              OTHER_PROBLEMS
    )


  parcel_df_ready <- p_df_ready


  return(parcel_df_ready)
}
