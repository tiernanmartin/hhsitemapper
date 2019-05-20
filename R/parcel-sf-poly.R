#' @title Parcel Boundary
#' @description Desc
#' @return a character string
#' @note Source: King County GIS (\url{ftp://ftp.kingcounty.gov/gis-web/GISData/parcel_SHP.zip})
#' @rdname parcel_sf_poly
#' @export
prepare_parcel_sf_poly <- function(path){

  # GET DATA ----------------------------------------------------------------

   url <- "ftp://ftp.kingcounty.gov/gis-web/GISData/parcel_SHP.zip"

   path_zip <- paste0(tools::file_path_sans_ext(path),".zip")

   downloader::download(url, destfile = path_zip)

   unzip(path_zip, exdir = dirname(path_zip))

   parcel_sf_poly_raw <- sf::st_read(dsn = "extdata/source/parcel/parcel.shp") %>%
     dplyr::rename_if(not_sfc, snakecase::to_screaming_snake_case)



  # WRITE -------------------------------------------------------------------

  sf::st_write(parcel_sf_poly_raw, path, driver = "GPKG")


  # RETURN ------------------------------------------------------------------

  parcel_sf_poly_prep_status <- get_modified_time(path)

  return(parcel_sf_poly_prep_status)

}

#' @rdname parcel_sf_poly
#' @export
make_parcel_sf_poly <- function(path){

  parcel_sf_poly <- sf::st_read(path)

  return(parcel_sf_poly)
}
