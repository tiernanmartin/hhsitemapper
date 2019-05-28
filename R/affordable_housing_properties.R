#' @title Affordable Housing Properties (King County)
#' @description Affordable housing properties within King County, Washington.
#' @return Returns an \code{\link[sf:sf]{sf object}}.
#' @note \itemize{
#'   \item{Data source: National Housing Preservation Database
#'     (\url{http://nhpd.preservationdatabase.org/Data})}
#'   \item{User registration is required to access this data}
#'   \item{User-applied filters: State = "WA", County = "King"}
#' }
#' @rdname affordable_housing_properties
#' @export
make_affordable_housing_properties <- function(path){

  # PREPARE DATA FOR GEOCODING ----------------------------------------------


  # STEPS:
  # 1. remove properties with fewer than 10 units
  # 2. remove properties without property names
  # 3. recode the target tenant type classes (reduce the number of levels)

  aff_hous_prop <- readxl::read_xlsx(path) %>%
    janitor::clean_names(case = "screaming_snake")


  aff_hous_prop_clean <- aff_hous_prop %>%
    dplyr::filter(TOTAL_UNITS >= 10) %>%
    dplyr::filter(!is.na(PROPERTY_NAME)) %>%
    dplyr::transmute(PROPERTY_NAME,
              PROPERTY_ADDRESS,
              CITY = stringr::str_to_title(CITY),
              STATE,
              ZIP_CODE,
              ADDRESS_FULL = stringr::str_c(PROPERTY_ADDRESS, CITY, STATE, ZIP_CODE, sep = ", "),
              LATITUDE,
              LONGITUDE,
              TOTAL_UNITS,
              OWNER_NAME,
              OWNER_TYPE,
              FAIR_MARKET_RENT,
              TARGET_TENANT_TYPE = dplyr::case_when(
                is.na(TARGET_TENANT_TYPE) ~ "Unknown",
                stringr::str_detect(TARGET_TENANT_TYPE, "Elderly|Disabled") ~ "Elderly or disabled",
                TRUE ~ TARGET_TENANT_TYPE
              ))

  # GEOCODE -----------------------------------------------------------------

  library(ggmap) # this is annoying but required for the geocoding to work

  aff_hous_prop_sf <- aff_hous_prop_clean %>%
    ggmap::mutate_geocode(ADDRESS_FULL, output = "latlon") %>%
    dplyr::rename_all(snakecase::to_screaming_snake_case) %>%
    dplyr::filter(!is.na(LAT)) %>%
    sf::st_as_sf(coords = c("LON", "LAT")) %>%
    sf::st_set_crs(4326) %>%
    sf::st_transform(2926)

  affordable_housing_properties <- aff_hous_prop_sf


  # RETURN ------------------------------------------------------------------

  return(affordable_housing_properties)
}

#' @rdname affordable_housing_properties
#'@keywords internal
view_affordable_housing_properties <- function(){
  loadd(affordable_housing_properties)
  mapview::mapview(affordable_housing_properties, zcol = "FAIR_MARKET_RENT", legend = TRUE)
}
