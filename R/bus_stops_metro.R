#' @title Seattle City Council Districts
#' @description Boundaries of Seattle City Council districts.
#' @return Returns an \code{\link[sf:sf]{sf object}}.
#' @note \itemize{
#'   \item{Data source: King County GIS (\url{ftp://ftp.kingcounty.gov/gis-web/GISData/transitstop_SHP.zip})}
#' }
#'
#'
#'

#' @rdname bus_stops_metro
#' @export
prepare_bus_stops_metro <- function(path){

  # GET DATA ----------------------------------------------------------------

  url <- "ftp://ftp.kingcounty.gov/gis-web/GISData/transitstop_SHP.zip"

  downloader::download(url, destfile = path)

  # RETURN ------------------------------------------------------------------

  bus_stops_metro_prep_status <- get_modified_time(path)

  return(bus_stops_metro_prep_status)

}

#' @rdname bus_stops_metro
#' @export
make_bus_stops_metro <- function(path){

  unzip(zipfile = path, exdir = dirname(path))

  stop_status <- tibble::tribble(
    ~ STOP_STATUS,    ~ STOP_STATUS_DESC,
    "ACT",            	"Active",
    "CLO",	"Permanently Closed",
    "INA",	"Temporary Inactive",
    "PLN",	"Plan"
  )

  bus_stops_metro <- sf::read_sf("extdata/osf/transitstop/transitstop.shp") %>%
    dplyr::rename_if(not_sfc,snakecase::to_screaming_snake_case) %>%
    dplyr::left_join(stop_status, by = c(STOP_STATU = "STOP_STATUS")) %>%
    dplyr::transmute(STOP_ID = as.character(STOP_ID),
                     TRANSIT_TYPE = "bus",
                     TRANSIT_PROVIDER_NAME = "KING COUNTY METRO",
                     STOP_STATUS = STOP_STATUS_DESC) %>%
    dplyr::filter(STOP_STATUS %in% c("Active", "Temporary Inactive", "Plan")) %>%
    sf::st_as_sf() %>%
    sf::st_transform(2926)

  return(bus_stops_metro)
}
