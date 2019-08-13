#' @title Site Inventory
#' @description The complete dataset of the Home and Hope Site Finder Tool.
#' @return All helper commands return an \code{\link[sf:sf]{sf}} object.

#' @rdname inventory
#' @export
make_inventory <- function(field_list = list()){

  inventory <- field_list %>%
    purrr::reduce(dplyr::left_join, by = "PIN") %>%
    sf::st_sf()

  return(inventory)

}

#' @rdname inventory
#' @export
make_inventory_suitable <- function(inventory){

  inventory_suitable <- dplyr::filter(inventory, SUITABLE_LGL)

}


#' @rdname inventory
#' @export
make_inventory_suitable_poly <- function(inventory_suitable){

  inventory_suitable_poly <- inventory_suitable %>%
    sf::st_set_geometry("geometry") %>%
    dplyr::select(PIN)

  return(inventory_suitable_poly)
}

#' @rdname inventory
#' @export
make_inventory_suitable_point <- function(inventory_suitable){

  inventory_suitable_point <- inventory_suitable %>%
    sf::st_set_geometry("geom_pt") %>%
    dplyr::select(PIN, geom_pt)

  return(inventory_suitable_point)
}

