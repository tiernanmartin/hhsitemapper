#' @title Zipcode Tabulation Areas (ZCTA)
#' @description Boundaries of zip code tabulation areas within King County, Washington.
#' @return Returns an \code{\link[sf:sf]{sf object}}.
#' @note \itemize{
#'   \item{Data source: \code{\link[tigris:zctas]{tigris::zctas()}}}
#' }
#'
#' @rdname  zcta
#' @export
prepare_zcta <- function(path){

  # GET DATA ----------------------------------------------------------------

  ztca_wa <- tigris::zctas(state = 53) %>%
    dplyr::rename_if(not_sfc,snakecase::to_screaming_snake_case) %>%
    sf::st_as_sf() %>%
    sf::st_transform(2926)

  # WRITE -------------------------------------------------------------------

  sf::st_write(ztca_wa, path, driver = "GPKG")

  # RETURN ------------------------------------------------------------------

  zcta_prep_status <- get_modified_time(path)

  return(zcta_prep_status)

}

#' @rdname  zcta
#' @export
make_zcta <- function(path, king_county){

  ztca_wa <- sf::read_sf(path) %>%
    sf::st_transform(2926)

  buff_dist <- 5280*2 # 2 miles in ft
  max_vertices <- 256

  kc_buff <-  king_county %>%
    sf::st_buffer(buff_dist) %>%
    lwgeom::st_subdivide(max_vertices) %>%
    sf::st_collection_extract()

  zcta_2926 <- sf::st_transform(ztca_wa, 2926)

  zcta_2926$geom_pt <- sf::st_centroid(sf::st_geometry(zcta_2926))

  zcta_2926$ZCTA_WITHIN_KC <- st_intersects_any(zcta_2926$geom_pt,kc_buff)

  zcta <- dplyr::filter(zcta_2926, ZCTA_WITHIN_KC)

  return(zcta)
}
