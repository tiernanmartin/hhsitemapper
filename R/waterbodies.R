#' @title National Hydrography Dataset Major Waterbodies (Washington)
#' @description Desc
#' @return Returns an \code{\link[sf:sf]{sf object}}.
#' @note Data source: Washington Department of Ecology GIS Data (\url{https://fortress.wa.gov/ecy/gispublic/DataDownload/ECY_WAT_NHDWAMajor.zip})

#' @rdname waterbodies
#' @export
prepare_wa_major_waterbodies <- function(path){

  # GET DATA ----------------------------------------------------------------

  url <- "https://fortress.wa.gov/ecy/gispublic/DataDownload/ECY_WAT_NHDWAMajor.zip"

  downloader::download(url, destfile = path)


  # RETURN ------------------------------------------------------------------

  wa_major_waterbodies_prep_status <- get_modified_time(path)

  return(wa_major_waterbodies_prep_status)

}

#' @rdname waterbodies
#' @export
make_wa_major_waterbodies <- function(path){

  unzip(zipfile = path, exdir = dirname(path))

  wtr <- sf::st_read("extdata/osf/NHDMajor.gdb", layer = "NHD_MajorWaterbodies") %>%
    sf::st_transform(2926)

  wa_major_waterbodies <- wtr

  return(wa_major_waterbodies)
}

#' @rdname waterbodies
#' @export
make_kc_waterbodies <- function(wa_major_waterbodies, king_county){

  kc_waterbodies <- wa_major_waterbodies[unlist(sf::st_intersects(wa_major_waterbodies, king_county, sparse = FALSE)),]

  return(kc_waterbodies)
}
