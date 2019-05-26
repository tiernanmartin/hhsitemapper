#' @title Census Places
#' @description Boundaries of census places within Washington State.
#' @return Returns an \code{\link[sf:sf]{sf object}}.
#' @note \itemize{
#'   \item{Data source: \code{\link[tigris:places]{tigris::places()}}}
#' }
#'
#' @rdname census_place
#' @export
prepare_census_place <- function(path){

  # GET DATA ----------------------------------------------------------------

  census_place_wa <- tigris::places(state = 53, cb = FALSE) %>%
    dplyr::rename_if(not_sfc,snakecase::to_screaming_snake_case) %>%
    sf::st_as_sf() %>%
    sf::st_transform(2926)

  # WRITE -------------------------------------------------------------------

  sf::st_write(census_place_wa, path, driver = "GPKG")

  # RETURN ------------------------------------------------------------------

  census_place_prep_status <- get_modified_time(path)

  return(census_place_prep_status)

}

#' @rdname census_place
#' @export
make_census_place <- function(path){

  census_place_wa <- sf::st_read(path) %>%
    sf::st_transform(2926)

  census_place <- census_place_wa

  return(census_place)
}
