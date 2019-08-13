#' @title Write Inventory
#' @description Functions for writing the dataset in a variety of
#'   data formats.
#' @return All functions return \code{NULL}.


#' @rdname write_inventory
#' @export
write_inventory_rda <- function(x, path){

  inventory_table <- x %>%
    sf::st_drop_geometry() %>%
    dplyr::select_if(not_sfc)

  save(inventory_table,file = path)

}

#' @rdname write_inventory
#' @export
write_inventory_csv <- function(x, path){

  inventory_table <- x %>%
    sf::st_drop_geometry() %>%
    dplyr::select_if(not_sfc)

  readr::write_csv(inventory_table, path)

}


#' @rdname write_inventory
#' @export
write_inventory_xlsx <- function(x, path){

  inventory_table <- x %>%
    sf::st_drop_geometry() %>%
    dplyr::select_if(not_sfc)

  writexl::write_xlsx(inventory_table, path)

}


#' @rdname write_inventory
#' @export
write_inventory_xml <- function(inventory_suitable, dd, path){

  table_fields <- dd %>%
    dplyr::filter(FIELD_TAG_TABLE) %>%
    purrr::pull(FIELD_NAME_DEV)

  inventory_table <- inventory_suitable %>%
    sf::st_drop_geometry() %>%
    dplyr::select_at(vars(table_fields))

  rio::export(inventory_table, file = path)

}


#' @rdname write_inventory
#' @export
write_inventory_geojson <- function(obj, dsn){

  obj_4326 <- sf::st_transform(obj, 4326)

  sf::st_write(obj_4326, dsn, driver = "GeoJSON",delete_dsn = TRUE)

}

#' @rdname write_inventory
#' @export
write_inventory_shp <- function(obj, dsn){

  obj_2926 <- sf::st_transform(obj, 2926)

  sf::st_write(obj_2926, dsn, driver = "ESRI Shapefile",delete_dsn = TRUE)

}
