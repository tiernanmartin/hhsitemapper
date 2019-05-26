#' @title King County Council Districts
#' @description Boundaries of King County Council districts.
#' @return Returns an \code{\link[sf:sf]{sf object}}.
#' @note \itemize{
#'   \item{Data source: King County GIS (\url{ftp://ftp.kingcounty.gov/gis-web/GISData/kccdst_SHP.zip})}
#' }
#'
#'
#'

#' @rdname kc_council_districts
#' @export
prepare_kc_council_districts <- function(path){

  # GET DATA ----------------------------------------------------------------

  url <- "ftp://ftp.kingcounty.gov/gis-web/GISData/kccdst_SHP.zip"

  downloader::download(url, destfile = path)

  # RETURN ------------------------------------------------------------------

  kc_council_districts_prep_status <- get_modified_time(path)

  return(kc_council_districts_prep_status)

}

#' @rdname kc_council_districts
#' @export
make_kc_council_districts <- function(path){

  unzip(zipfile = path, exdir = dirname(path))

  kc_council_districts <- sf::read_sf("extdata/osf/kccdst/kccdst.shp") %>%
    dplyr::rename_if(not_sfc,snakecase::to_screaming_snake_case) %>%
    sf::st_as_sf() %>%
    sf::st_transform(2926)

  return(kc_council_districts)
}
