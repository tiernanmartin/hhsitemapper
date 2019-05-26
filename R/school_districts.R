#' @title School Districts (King County)
#' @description Boundaries of school districts within King County, Washington.
#' @return Returns an \code{\link[sf:sf]{sf object}}.
#' @note \itemize{
#'   \item{Data source: King County GIS (\url{ftp://ftp.kingcounty.gov/gis-web/GISData/schdst_SHP.zip})}
#' }
#'
#'
#'

#' @rdname school_districts
#' @export
prepare_school_districts <- function(path){

  # GET DATA ----------------------------------------------------------------

  url <- "ftp://ftp.kingcounty.gov/gis-web/GISData/schdst_SHP.zip"

  downloader::download(url, destfile = path)

  # RETURN ------------------------------------------------------------------

  school_districts_prep_status <- get_modified_time(path)

  return(school_districts_prep_status)

}

#' @rdname school_districts
#' @export
make_school_districts <- function(path){

  unzip(zipfile = path, exdir = dirname(path))

  school_districts <- sf::st_read("extdata/osf/schdst/schdst.shp") %>%
    dplyr::rename_if(not_sfc,snakecase::to_screaming_snake_case) %>%
    sf::st_as_sf() %>%
    sf::st_transform(2926)

  return(school_districts)
}
