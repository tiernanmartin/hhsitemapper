#' @title Affordable Housing Properties & Subsidies (King County)
#' @description Affordable housing properties and subsidies within King County, Washington.
#' @return Returns an \code{\link[sf:sf]{sf object}}.
#' @note \itemize{
#'   \item{Data source: National Housing Preservation Database
#'     (\url{http://nhpd.preservationdatabase.org/Data})}
#'   \item{User registration is required to access this data}
#'   \item{User-applied filters: State = "WA", County = "King"}
#' }

#' @rdname affordable_housing
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

#' @rdname affordable_housing
view_affordable_housing_properties <- function(){
  loadd(affordable_housing_properties)
  mapview::mapview(affordable_housing_properties, zcol = "FAIR_MARKET_RENT", legend = TRUE)
}

#' @rdname affordable_housing
#' @export
make_affordable_housing_subsidies <- function(path, zcta){

  # PREPARE DATA FOR GEOCODING ----------------------------------------------


  # STEPS:
  # 1. remove properties with fewer than 10 units
  # 2. remove properties without property names
  # 3. recode the target tenant type classes (reduce the number of levels)

col_types <-  c("text", "text", "text",
        "text", "text", "text", "text", "date",
        "date", "numeric", "text", "text",
        "text", "text", "text", "numeric",
        "numeric", "text", "numeric", "numeric",
        "numeric", "numeric", "numeric",
        "numeric", "numeric", "numeric",
        "numeric", "numeric", "numeric",
        "numeric", "text", "text", "numeric",
        "numeric", "numeric", "numeric")

  aff_hous_subsidies <- readxl::read_xlsx(path,
                                          col_types = col_types) %>%
    janitor::clean_names(case = "screaming_snake")

  # Note: the data doesn't come with a COUNTY field, so in order to reduce
  # the number of addresses that are passed to the geocoding API
  # I filter the data using zip codes from King County first

  zipcodes_within_kc <- zcta %>%
    dplyr::filter(ZCTA_WITHIN_KC) %>%
    dplyr::pull(ZCTA_5_CE_10)

  aff_hous_subsidies_clean <- aff_hous_subsidies %>%
    dplyr::filter(STATE %in% c("WA")) %>%
    dplyr::filter(ZIP_CODE %in% zipcodes_within_kc) %>%
    dplyr::filter(SUBSIDY_STATUS %in% c("Active","Inconclusive")) %>%
    dplyr::select(SUBSIDY_NAME:MANAGER_TYPE, -LATITUDE, -LONGITUDE) %>%
    dplyr::mutate(CITY = stringr::str_to_title(CITY),
              ADDRESS_FULL = stringr::str_c(STREET_ADDRESS, CITY, STATE, ZIP_CODE, sep = ", ")
              )

  # GEOCODE -----------------------------------------------------------------

  library(ggmap) # this is annoying but required for the geocoding to work

  aff_hous_subsidies_sf <- aff_hous_subsidies_clean %>%
    ggmap::mutate_geocode(ADDRESS_FULL, output = "latlon") %>%
    dplyr::rename_all(snakecase::to_screaming_snake_case) %>%
    dplyr::filter(!is.na(LAT)) %>%
    sf::st_as_sf(coords = c("LON", "LAT")) %>%
    sf::st_set_crs(4326) %>%
    sf::st_transform(2926)

  affordable_housing_subsidies <- aff_hous_subsidies_sf


  # RETURN ------------------------------------------------------------------

  return(affordable_housing_subsidies)
}
