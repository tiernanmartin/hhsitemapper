#' @title Make the KC Assessor Lookup Table
#' @description A lookup table for codes used in the parcel data
#' @note Data source: King County Assessor Data Portal (\url{http://aqua.kingcounty.gov/extranet/assessor/Lookup.zip})
#' @param path
#' @rdname lookup
#' @export
make_lookup <- function(path){

  lookup <- readr::read_csv(path, col_types = "ccc") %>%
    janitor::clean_names("screaming_snake")

  return(lookup)
}
