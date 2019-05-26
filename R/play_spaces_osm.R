#' @title Play Spaces (Open Street Map)
#' @description Play spaces within King County, Washington.
#' @return Returns an \code{\link[sf:sf]{sf object}}.
#' @note \itemize{
#'   \item{Data source: \code{\link[osmdata:opq]{osmdata::opq()}}}
#' }
#'
#'
#'

#' @rdname play_spaces_osm
#' @export
prepare_play_spaces_osm <- function(path){

  # GET DATA ----------------------------------------------------------------

  q <- osmdata::opq(bbox = "King County, Washington") %>%
    osmdata::add_osm_feature(key = "leisure", value = "park",value_exact = FALSE)

  osm_list <-  list("osm_polygons",
                    "osm_multipolygons") %>%
    purrr::map(~ purrr::pluck(osmdata::osmdata_sf(q),.x))

  common_cols <- osm_list %>%
    purrr::map(names) %>%
    purrr::reduce(dplyr::intersect)

  play_spaces <- osm_list %>%
    purrr::map(~ dplyr::select_at(.x, .vars = dplyr::vars(common_cols))) %>%
    purrr::reduce(rbind)%>%
    sf::st_cast("MULTIPOLYGON") %>%
    tibble::as_tibble() %>%
    sf::st_sf() %>%
    sf::st_transform(2926) %>%
    dplyr::rename_if(not_sfc, snakecase::to_screaming_snake_case) %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    dplyr::select(PLAY_SPACE_OSM_ID = OSM_ID,
           PLAY_SPACE_NAME = NAME,
           PLAY_SPACE_TYPE = LEISURE)


  # WRITE -------------------------------------------------------------------

  sf::st_write(play_spaces, path, driver = "GPKG")

  # RETURN ------------------------------------------------------------------

  play_spaces_osm_prep_status <- get_modified_time(path)

  return(play_spaces_osm_prep_status)

}

#' @rdname play_spaces_osm
#' @export
make_play_spaces_osm <- function(path){

  play_spaces_osm <- sf::read_sf(path) %>%
    dplyr::filter(! is.na(PLAY_SPACE_TYPE)) %>%
    dplyr::filter(PLAY_SPACE_TYPE %in% c("park",
                                         "playground",
                                         "spraypark")) %>%
    sf::st_transform(2926)


  return(play_spaces_osm)
}

#' @rdname play_spaces_osm
#'@keywords internal
view_play_spaces_osm <- function(){
  loadd(play_spaces_osm)
  mapview::mapview(play_spaces_osm, zcol = "PLAY_SPACE_TYPE", legend = TRUE)
}
