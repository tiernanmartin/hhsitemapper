#' @title Marijuanna Businesses (King County)
#' @description Marijuana businesses within King County, Washington.
#' @return Returns an \code{\link[sf:sf]{sf object}}.
#' @note \itemize{
#'   \item{Data source: Washington Liquor Control Board (\url{https://lcb.wa.gov/sites/default/files/publications/Public_Records/2019/MarijuanaApplicants.xls})}
#' }
#' @rdname mj_businesses
#' @export
prepare_mj_businesses <- function(path){

  # GET DATA ----------------------------------------------------------------

  url <- "https://lcb.wa.gov/sites/default/files/publications/Public_Records/2019/MarijuanaApplicants.xls"

  downloader::download(url, destfile = path, mode = "wb")

  # RETURN ------------------------------------------------------------------

  mj_businesses_prep_status <- get_modified_time(path)

  return(mj_businesses_prep_status)

}

#' @rdname mj_businesses
#' @export
make_mj_businesses <- function(path){

  # PREPARE DATA FOR GEOCODING ----------------------------------------------

  retailer_sheet <- readxl::excel_sheets(path) %>% purrr::keep(~ stringr::str_detect(.x, "Retailer"))

  mj_biz <- readxl::read_xls(path, sheet = retailer_sheet)

  mj_biz_clean <- mj_biz %>%
    janitor::clean_names(case = "screaming_snake") %>%
    dplyr::transmute(TRADENAME,
                     PRIV_DESC = PRIV_DESC,
                     PRIVILEGE_STATUS = PRIVILEGE_STATUS,
                     ZIP_CODE = stringr::str_extract(ZIP_CODE,"^.{0,5}"),
                     COUNTY = COUNTY,
                     ADDRESS_FULL = stringr::str_c( STREET_ADDRESS, CITY, STATE, ZIP_CODE, sep = ", "))



  # FILTER RECORD (ACTIVE | PENDING ONLY) -----------------------------------

  mj_biz_active <- mj_biz_clean %>%
    dplyr::filter(COUNTY %in% "KING") %>%
    dplyr::filter(stringr::str_detect(PRIVILEGE_STATUS,"ACTIVE|PENDING") ) %>%
    dplyr::arrange(dplyr::desc(as.integer(ZIP_CODE)))

  # GEOCODE -----------------------------------------------------------------

  library(ggmap) # this is annoying but required for the geocoding to work

  mj_biz_sf <- mj_biz_active %>%
    ggmap::mutate_geocode(ADDRESS_FULL, output = "latlon") %>%
    dplyr::rename_all(snakecase::to_screaming_snake_case) %>%
    dplyr::filter(!is.na(LAT)) %>%
    sf::st_as_sf(coords = c("LON", "LAT")) %>%
    sf::st_set_crs(4326) %>%
    sf::st_transform(2926)

  mj_businesses <- mj_biz_sf


  # RETURN ------------------------------------------------------------------

  return(mj_businesses)
}

#' @rdname mj_businesses
#'@keywords internal
view_mj_businesses <- function(){
  loadd(mj_businesses)
  mapview::mapview(mj_businesses, zcol = "PRIVILEGE_STATUS", legend = TRUE)
}
