#' @title Parcel Environmental Restrictions
#' @description Environmental restrictions of parcels in King County.
#' @note Data source: King County Assessor Data Portal
#' @param path
#' @rdname env_restrictions
#' @export
make_env_restrictions <- function(path){

  env_restrictions <- readr::read_csv(path) %>%
    janitor::clean_names("screaming_snake")

  return(env_restrictions)
}
