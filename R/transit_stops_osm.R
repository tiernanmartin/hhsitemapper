#' @title Transit Stops (Open Street Map)
#' @description Transit stops within King County, Washington.
#' @return Returns an \code{\link[sf:sf]{sf object}}.
#' @note \itemize{
#'   \item{Data source: \code{\link[osmdata:opq]{osmdata::opq()}}}
#' }
#'
#'
#'

#' @rdname transit_stops_osm
#' @export
prepare_transit_stops_osm <- function(path){

  # GET DATA ----------------------------------------------------------------

  q <- osmdata::opq(bbox = "King County, Washington") %>%
    osmdata::add_osm_feature(key = "public_transport", value = "",value_exact = FALSE)

  transit_pts <- q %>%
    osmdata::osmdata_sf() %>%
    purrr::pluck("osm_points") %>%
    dplyr::rename_all(snakecase::to_screaming_snake_case)


  # WRITE -------------------------------------------------------------------

  sf::st_write(transit_pts, path, driver = "GPKG")

  # RETURN ------------------------------------------------------------------

  transit_stops_osm_prep_status <- get_modified_time(path)

  return(transit_stops_osm_prep_status)

}

#' @rdname transit_stops_osm
#' @export
make_transit_stops_osm <- function(path){

  ts_sf <- sf::read_sf(path) %>%
    sf::st_transform(2926)

  ts_pts <- ts_sf %>%
    dplyr::transmute(OSM_ID = as.character(OSM_ID))

  ts_ready <- ts_sf %>%
    sf::st_drop_geometry() %>%
    dplyr::select(OSM_ID ,NAME,BUS, TRAIN, STEETCAR = TRAM, FERRY, PUBLIC_TRANSPORT, SOURCE) %>%
    tidyr::gather(TRANSIT_TYPE, VALUE, -OSM_ID, -NAME,-PUBLIC_TRANSPORT, -SOURCE) %>%
    dplyr::filter(!is.na(VALUE)) %>%
    dplyr::transmute(TRANSIT_STOP_OSM_ID = OSM_ID,
                     TRANSIT_STOP_NAME = NAME,
                     TRANSIT_STOP_TYPE = TRANSIT_TYPE,
                     TRANSIT_STOP_SOURCE = SOURCE) %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    dplyr::left_join(ts_pts, by = c(TRANSIT_STOP_OSM_ID = "OSM_ID")) %>%
    sf::st_sf()

  transit_stops_osm <- ts_ready

  return(transit_stops_osm)
}

#' @rdname transit_stops_osm
#'@keywords internal
view_transit_stops_osm <- function(){
  loadd(transit_stops_osm)
  mapview::mapview(transit_stops_osm, zcol = "TRANSIT_STOP_TYPE", legend = TRUE)
}
