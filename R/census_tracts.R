#' @title Census Tracts (King County)
#' @description Boundaries of census tracts within King County, Washington.
#' @return Returns an \code{\link[sf:sf]{sf object}}.
#' @note \itemize{
#'   \item{Data source: \code{\link[tigris:tracts]{tigris::tracts()}}}
#' }
#'
#'
#'

#' @rdname census_tracts
#' @export
prepare_census_tracts <- function(path){

  # GET DATA ----------------------------------------------------------------

  tr <- tigris::tracts(state = 53, county = "King") %>%
    dplyr::rename_if(not_sfc,snakecase::to_screaming_snake_case) %>%
    sf::st_as_sf() %>%
    sf::st_transform(2926)


  # WRITE -------------------------------------------------------------------

  sf::st_write(tr, path, driver = "GPKG")

  # RETURN ------------------------------------------------------------------

  census_tracts_prep_status <- get_modified_time(path)

  return(census_tracts_prep_status)

}

#' @rdname census_tracts
#' @export
make_census_tracts <- function(path){

  census_tracts_kc <- sf::read_sf(path) %>%
    sf::st_transform(2926)

  census_tracts <- census_tracts_kc

  return(census_tracts)
}
