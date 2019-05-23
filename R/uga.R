#' @title Urban Growth Area (King County)
#' @description Desc
#' @return Returns an \code{\link[sf:sf]{sf object}}.
#' @note Data source: Name of the source (\url{ftp://ftp.kingcounty.gov/gis-web/GISData/urban_growth_SHP.zip})

#' @rdname uga
#' @export
prepare_uga <- function(path){

  # GET DATA ----------------------------------------------------------------

  url <- "ftp://ftp.kingcounty.gov/gis-web/GISData/urban_growth_SHP.zip"

  downloader::download(url, destfile = path)

  # RETURN ------------------------------------------------------------------

  uga_prep_status <- get_modified_time(path)

  return(uga_prep_status)

}

#' @rdname uga
#' @export
make_uga <- function(path, king_county){

  unzip(zipfile = path, exdir = dirname(path))

  uga_wa <- sf::st_read("extdata/osf/urban_growth/urban_growth.shp") %>%
    dplyr::rename_if(not_sfc,snakecase::to_screaming_snake_case) %>%
    sf::st_as_sf() %>%
    sf::st_transform(2926)

  uga_kc <- uga_wa[unlist(sf::st_intersects(uga_wa, king_county, sparse = FALSE)),]

  uga <- uga_kc

  return(uga)
}
