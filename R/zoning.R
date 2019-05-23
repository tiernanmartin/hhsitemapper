#' @title Zoning (King County)
#' @description Consolidated zoning categories for King County, Washington.
#' @return Returns an \code{\link[sf:sf]{sf object}}.
#' @note \itemize{
#'   \item{Data source: King County GIS (\url{ftp://ftp.kingcounty.gov/gis-web/GISData/zoning_kc_consol_20_SHP.zip})}
#'   \item{Metadata: \url{http://www5.kingcounty.gov/sdc/Metadata.aspx?Layer=zoning_kc_consol_20}}
#' }
#'
#'
#'

#' @rdname zoning
#' @export
prepare_zoning <- function(path){

  # GET DATA ----------------------------------------------------------------

  url <- "ftp://ftp.kingcounty.gov/gis-web/GISData/zoning_kc_consol_20_SHP.zip"

  downloader::download(url, destfile = path)

  # RETURN ------------------------------------------------------------------

  zoning_prep_status <- get_modified_time(path)

  return(zoning_prep_status)

}

#' @rdname zoning
#' @export
make_zoning <- function(path){

  unzip(zipfile = path, exdir = dirname(path))

  zoning_kc <- sf::st_read("extdata/osf/zoning_kc_consol_20/zoning_kc_consol_20.shp") %>%
    dplyr::rename_if(not_sfc,snakecase::to_screaming_snake_case) %>%
    sf::st_as_sf() %>%
    sf::st_transform(2926)

  zoning <- zoning_kc

  return(zoning)
}
