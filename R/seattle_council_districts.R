#' @title Seattle City Council Districts
#' @description Boundaries of Seattle City Council districts.
#' @return Returns an \code{\link[sf:sf]{sf object}}.
#' @note \itemize{
#'   \item{Data source: King County GIS (\url{ftp://ftp.kingcounty.gov/gis-web/GISData/sccdst_SHP.zip})}
#' }
#'
#'
#'

#' @rdname seattle_council_districts
#' @export
prepare_seattle_council_districts <- function(path){

  # GET DATA ----------------------------------------------------------------

  url <- "ftp://ftp.kingcounty.gov/gis-web/GISData/sccdst_SHP.zip"

  downloader::download(url, destfile = path)

  # RETURN ------------------------------------------------------------------

  seattle_council_districts_prep_status <- get_modified_time(path)

  return(seattle_council_districts_prep_status)

}

#' @rdname seattle_council_districts
#' @export
make_seattle_council_districts <- function(path){

  unzip(zipfile = path, exdir = dirname(path))

  seattle_council_districts <- sf::read_sf("extdata/osf/sccdst/sccdst.shp") %>%
    dplyr::rename_if(not_sfc,snakecase::to_screaming_snake_case) %>%
    dplyr::transmute(SEATTLE_COUNCIL_DISTRICT = stringr::str_c("District", stringr::str_extract(SCCDST,"\\d"),sep = " ")) %>%
    sf::st_as_sf() %>%
    sf::st_transform(2926)

  return(seattle_council_districts)
}
