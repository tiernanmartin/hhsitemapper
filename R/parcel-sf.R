#' @title Parcel Boundary
#' @description Desc
#' @param path desc
#' @return a character string
#' @note Source: King County GIS (\url{ftp://ftp.kingcounty.gov/gis-web/GISData/parcel_SHP.zip})
#' @rdname parcel_sf
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

#' @rdname parcel_sf
#' @export
make_parcel_sf_poly <- function(path){

  parcel_sf_poly <- sf::st_read(path)

  return(parcel_sf_poly)
}

#' @title Parcel Centroid
#' @description Desc
#' @param parcel_sf_poly desc
#' @return a character string
#' @rdname parcel_sf
#' @export
make_parcel_sf <- function(parcel_sf_poly){

  p_sf_2926 <- sf::st_transform(parcel_sf_poly, 2926)

  p_sf_2926$geom_pt <- sf::st_centroid(sf::st_geometry(p_sf_2926))

  p_sf_ready <- p_sf_2926 %>%
    dplyr::transmute(PIN = make_pin(MAJOR, MINOR),
              geom_pt) %>%
    tidyr::drop_na() %>%
    dplyr::rename(geometry = geom) %>%
    sf::st_set_geometry("geometry") %>%
    sf::st_transform(2926) %>%
    sf::st_sf()

  parcel_sf <- p_sf_ready

  return(parcel_sf)
}

#' @title Parcel Centroid
#' @description Desc
#' @param parcel_sf desc
#' @return a character string
#' @rdname parcel_sf
#' @export
make_parcel_sf_ready <- function(parcel_sf){

  p_sf_ready <-  parcel_sf %>%
    miscgis::subset_duplicated("PIN") %>%
    dplyr::group_by(PIN) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    rbind(subset_duplicated(parcel_sf,"PIN",notin = TRUE)) %>%
    sf::st_set_geometry("geometry")

  parcel_sf_ready <- p_sf_ready

  return(parcel_sf_ready)

  return(parcel_sf_ready)
}
