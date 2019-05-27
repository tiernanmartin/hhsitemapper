#' @title Light Rail Stations
#' @description Light rail stations (exising and planned)
#' @return Returns an \code{\link[sf:sf]{sf object}}.
#' @note \itemize{
#'   \item{Data source: Github (\url{"https://github.com/tiernanmartin/datasets/raw/master/sound-transit-lightrail/data/sound-transit-lightrail.gpkg"})}
#'   \item{Note: these data reflect Sound Transit's representative alignments and are subject to change}
#' }
#'
#'
#'

#' @rdname future_lightrail
#' @export
prepare_future_lightrail <- function(path){

  # GET DATA ----------------------------------------------------------------

  url <- "https://github.com/tiernanmartin/datasets/raw/master/sound-transit-lightrail/data/sound-transit-lightrail.gpkg"

  sf::st_read(url) %>%
    sf::st_write(path, driver = "GPKG")

  # RETURN ------------------------------------------------------------------

  future_lightrail_prep_status <- get_modified_time(path)

  return(future_lightrail_prep_status)

}

#' @rdname future_lightrail
#' @export
make_future_lightrail <- function(path){

  future_lightrail_kc <- sf::read_sf(path) %>%
    dplyr::rename_if(not_sfc,snakecase::to_screaming_snake_case) %>%
    sf::st_as_sf() %>%
    sf::st_transform(2926)

  future_lightrail <- future_lightrail_kc

  return(future_lightrail)
}
