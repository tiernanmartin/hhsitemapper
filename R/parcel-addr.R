#' @title Parcel Address
#' @description Desc
#' @return a character string
#' @note Source: King County GIS (\url{ftp://ftp.kingcounty.gov/gis-web/GISData/address_SHP.zip})
#' @rdname parcel_addr
#' @export
prepare_parcel_addr <- function(path){

  # GET DATA ----------------------------------------------------------------

   url <- "ftp://ftp.kingcounty.gov/gis-web/GISData/address_SHP.zip"

   path_zip <- paste0(tools::file_path_sans_ext(path),".zip")

   downloader::download(url, destfile = path_zip)

   unzip(path_zip, exdir = dirname(path_zip))

   parcel_addr_raw <- sf::st_read(dsn = "extdata/source/address/address.shp") %>%
     sf::st_drop_geometry() %>%
     tibble::as_tibble() %>%
     janitor::clean_names("screaming_snake")



  # WRITE -------------------------------------------------------------------

  readr::write_csv(parcel_addr_raw, path)


  # RETURN ------------------------------------------------------------------

  parcel_addr_prep_status <- get_modified_time(path)

  return(parcel_addr_prep_status)

}

#' @rdname parcel_addr
#' @export
make_parcel_addr_ready <- function(path){

  parcel_addr_raw <- readr::read_csv(path)

  parcel_addr_ready <- parcel_addr_raw %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    dplyr::transmute(PIN,
              ADDR_ADDRESS_STREET = ADDR_FULL,
              ADDR_CITY_NAME = CTYNAME,
              ADDR_CITY_NAME_POSTAL = POSTALCTYN,
              ADDR_ZIPCODE = factor(ZIP5),
              ADDR_PRIMARY_ADDR_LGL = as.logical(PRIM_ADDR)) %>%
    dplyr::mutate(ADDR_CITY_NAME = factor(dplyr::case_when(
      all(is.na(ADDR_CITY_NAME),is.na(ADDR_CITY_NAME_POSTAL)) ~ NA_character_,
      is.na(ADDR_CITY_NAME) ~ ADDR_CITY_NAME_POSTAL,
      TRUE ~ ADDR_CITY_NAME
    ))) %>%
    dplyr::transmute(PIN,
              ADDR_ADDRESS_STREET,
              ADDR_ADDRESS_FULL = stringr::str_c(ADDR_ADDRESS_STREET, ADDR_CITY_NAME,sep = ", "),
              ADDR_CITY_NAME,
              ADDR_CITY_NAME_POSTAL,
              ADDR_ZIPCODE,
              ADDR_PRIMARY_ADDR_LGL
    ) %>%
    dplyr::group_by(PIN) %>%
    dplyr::arrange(dplyr::desc(ADDR_PRIMARY_ADDR_LGL)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()

  return(parcel_addr_ready)
}
