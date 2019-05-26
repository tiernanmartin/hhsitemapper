#' @title Legislative Districts (King County)
#' @description Boundaries of legislative districts within King County, Washington.
#' @return Returns an \code{\link[sf:sf]{sf object}}.
#' @note \itemize{
#'   \item{Data source: King County GIS (\url{ftp://ftp.kingcounty.gov/gis-web/GISData/legdist_SHP.zip})}
#' }
#'
#'
#'

#' @rdname leg_districts
#' @export
prepare_leg_districts <- function(path){

  # GET DATA ----------------------------------------------------------------

  url <- "ftp://ftp.kingcounty.gov/gis-web/GISData/legdst_SHP.zip"

  downloader::download(url, destfile = path)

  # RETURN ------------------------------------------------------------------

  leg_districts_prep_status <- get_modified_time(path)

  return(leg_districts_prep_status)

}

#' @rdname leg_districts
#' @export
make_leg_districts <- function(path){

  unzip(zipfile = path, exdir = dirname(path))

  leg_districts <- sf::st_read("extdata/osf/legdst/legdst.shp") %>%
    dplyr::rename_if(not_sfc,snakecase::to_screaming_snake_case) %>%
    sf::st_as_sf() %>%
    sf::st_transform(2926)

  return(leg_districts)
}
