#' @title City Boundaries (King County)
#' @description Boundaries of cities within King County, Washington.
#' @return Returns an \code{\link[sf:sf]{sf object}}.
#' @note \itemize{
#'   \item{Data source: King County GIS (\url{ftp://ftp.kingcounty.gov/gis-web/GISData/city_kc_SHP.zip})}
#' }
#'
#'
#'

#' @rdname kc_city
#' @export
prepare_kc_city <- function(path){

  # GET DATA ----------------------------------------------------------------

  url <- "ftp://ftp.kingcounty.gov/gis-web/GISData/city_kc_SHP.zip"

  downloader::download(url, destfile = path)

  # RETURN ------------------------------------------------------------------

  kc_city_prep_status <- get_modified_time(path)

  return(kc_city_prep_status)

}

#' @rdname kc_city
#' @export
make_kc_city <- function(path){

  unzip(zipfile = path, exdir = dirname(path))

  kc_city_kc <- sf::read_sf("extdata/osf/city_kc/city_kc.shp") %>%
    dplyr::rename_if(not_sfc,snakecase::to_screaming_snake_case) %>%
    sf::st_as_sf() %>%
    sf::st_transform(2926)

  kc_city <- kc_city_kc

  return(kc_city)
}
