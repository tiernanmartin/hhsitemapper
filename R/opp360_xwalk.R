#' @title Opportunity 360 Census Tract Crosswalk
#' @description A "crosswalk" document that allows a user to identify which
#'   page of the Opportunity 360 online document corresponds with each census tract.
#' @return Returns an \code{\link[tibble:tbl_df]{tbl_df}} object.
#' @note \itemize{
#'   \item{Data source: Enterprise Community Partners
#'     (via an email from Zach Patton on Feb. 13, 2018)}
#' }

#' @rdname opp360_xwalk
#' @export
make_opp360_xwalk <- function(path){

  opp360_xwalk <- readxl::read_excel(path) %>%
    janitor::clean_names("screaming_snake")

  return(opp360_xwalk)
}
