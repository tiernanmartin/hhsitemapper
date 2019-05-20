#' @title Real Property Account Extract
#' @description Account information for owners of parcels in King County.
#' @note Data source: King County Assessor's Office (restricted access)
#' @param path
#' @rdname acct
#' @export
make_acct <- function(path){

  acct <- readr::read_csv(path) %>%
    janitor::clean_names("screaming_snake")

  return(acct)
}
