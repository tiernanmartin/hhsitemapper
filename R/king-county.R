#' @title King County Boundary
#' @description Desc
#' @return Returns an \code{\link[sf:sf]{sf object}}.

#' @rdname king_county
#' @export
prepare_king_county <- function(path){

  # GET DATA ----------------------------------------------------------------


  kc <- tigris::counties(state = 53) %>%
    dplyr::filter(NAME %in% "King") %>%
    sf::st_transform(2926)

  # WRITE -------------------------------------------------------------------

  sf::st_write(kc, path, driver = "GPKG")


  # RETURN ------------------------------------------------------------------

  king_county_prep_status <- get_modified_time(path)

  return(king_county_prep_status)

}

#' @rdname king_county
#' @export
make_king_county <- function(path){

  king_county <- sf::st_read(path) %>%
    dplyr::rename_if(not_sfc, snakecase::to_screaming_snake_case) %>%
    sf::st_as_sf() %>%
    sf::st_transform(2926)

  return(king_county)
}
