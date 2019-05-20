#' @title Publicly-Owned Parcels
#' @description Publicly-owned parcels in King County.
#' @note Data source: King County Assessor's Office (specially curated list)
#' @param path
#' @rdname pub_parcel
#' @export
make_pub_parcel <- function(path){

  pub_parcel <- readr::read_csv(path) %>%
    janitor::clean_names("screaming_snake")

  return(pub_parcel)
}
