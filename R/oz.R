#' @title Opportunity Zones (OZ)
#' @description Census tracts designated as Opportunity Zones (OZ) by the Treasury Department.
#' @return Returns an \code{\link[tibble:tbl_df]{tbl_df}} object.
#' @note \itemize{
#'   \item{Data source: Community Development Financial Institutions (CDFI) Fund - Opportunity Zones
#'     (\url{https://www.cdfifund.gov/Pages/Opportunity-Zones.aspx})}
#'   \item{Data download: List of designated Qualified Opportunity Zones (QOZs)
#'     (\url{https://www.cdfifund.gov/Documents/Designated\%20QOZs.12.14.18.xlsx})}
#' }

#' @rdname oz
#' @export
prepare_oz <- function(path){

  # GET DATA ----------------------------------------------------------------

  url <- "https://www.cdfifund.gov/Documents/Designated%20QOZs.12.14.18.xlsx"

  downloader::download(url, destfile = path, mode = "wb")

  # RETURN ------------------------------------------------------------------

  oz_prep_status <- get_modified_time(path)

  return(oz_prep_status)

}

#' @rdname oz
#' @export
make_oz <- function(path){

  oz_raw <- readxl::read_excel(path, skip = 4) %>%
    janitor::clean_names("screaming_snake")

  oz <- oz_raw %>%
    dplyr::rename(
      STATE = CLICK_ARROW_TO_FILTER_STATE_STATE,
      TRACT = CENSUS_TRACT_NUMBER)

  return(oz)
}
