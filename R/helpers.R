#' @title helpers
#' @description Fields that help connect a given parcel to other databases
#'   (e.g., urls for Opportunity 360 or the King County Parcel Viewer webtool).
#' @return All helper commands return a \code{\link[tibble:tbl_df]{tbl_df}} object.

#' @rdname helpers
#' @export
make_helpers <- function(parcel_ready, helper_list = list()){

  helper_df <- helper_list %>%
    purrr::reduce(dplyr::left_join, by = "PIN")

  helpers <- parcel_ready %>%
    sf::st_drop_geometry() %>%
    dplyr::select(PIN) %>%
    dplyr::left_join(helper_df, by = "PIN")

  return(helpers)


}

#' @rdname helpers
#' @export
make_helpers_url_parcel_viewer <- function(parcel_df_ready){

  url <- "https://blue.kingcounty.com/Assessor/eRealProperty/Detail.aspx?ParcelNbr="

  url_pv <- parcel_df_ready %>%
    dplyr::transmute(PIN,
              HELPERS_URL_PARCEL_VIEWER = stringr::str_c(url,PIN,sep = ""))

  helpers_url_parcel_viewer <- url_pv

  return(helpers_url_parcel_viewer)


}

#' @rdname helpers
#' @export
make_helpers_url_opp360 <- function(filters_census_tract, opp360_xwalk){

  url_opp360 <- filters_census_tract %>%
    dplyr::left_join(opp360_xwalk, by = c("FILTER_CENSUS_TRACT" = "FIPS_TEXT")) %>%
    dplyr::transmute(PIN,
              HELPERS_URL_OPP360 = URL,
              HELPERS_PID_OPP360 = as.character(PID))

  helpers_url_opp360 <- url_opp360

  return(helpers_url_opp360)


}

#' @rdname helpers
#' @export
make_helpers_url_contaminated <- function(filters_contaminated, contaminated_sites){

  url_base <- "https://apps.ecology.wa.gov/tcpwebreporting/reports/cleanup/sitedetails/"

  sites_url_ready <- contaminated_sites %>%
    sf::st_drop_geometry() %>%
    dplyr::transmute(SITE_NAME,
              HELPERS_URL_CONTAMINATED = stringr::str_c(url_base, CLEANUP_SITE_ID, sep = ""))


  url_contaminated <- filters_contaminated %>%
    dplyr::left_join(sites_url_ready, by = c(FILTER_CONTAMINATED_NAME = "SITE_NAME"))  %>%
    dplyr::transmute(PIN,
              HELPERS_URL_CONTAMINATED)

  helpers_url_contaminated <- url_contaminated

  return(helpers_url_contaminated)


}
