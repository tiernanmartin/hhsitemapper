#' @title General Parcel Information
#' @description General information for all parcels in King County.
#' @note Data source: King County Assessor's Office
#' @param path
#' @rdname parcel_df
#' @export
make_parcel_df <- function(path){

  parcel_df <- readr::read_csv(path) %>%
    janitor::clean_names("screaming_snake")

  return(parcel_df)
}
