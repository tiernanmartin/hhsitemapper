#' @title New Market Tax Credits (NMTC)
#' @description Census tracts designated as New Market Tax Credit-elligible by the
#'   U.S. Department of Treasury.
#' @return Returns an \code{\link[tibble:tbl_df]{tbl_df}} object.
#' @note \itemize{
#'   \item{Data source: U.S. Department of Treasury Community Development Financial Institutions Fund (CDFI)
#'     (\url{https://www.cdfifund.gov/Pages/default.aspx})}
#'   \item{Data download: NMTC 2011-2015 LIC Nov2-2017-4pm
#'     (\url{https://www.cdfifund.gov/Documents/NMTC\%202011-2015\%20LIC\%20Nov2-2017-4pm.xlsx})}
#' }

#' @rdname nmtc
#' @export
prepare_nmtc <- function(path){

  # GET DATA ----------------------------------------------------------------

  url <- "https://www.cdfifund.gov/Documents/NMTC%202011-2015%20LIC%20Nov2-2017-4pm.xlsx"

  downloader::download(url, destfile = path, mode = "wb")

  # RETURN ------------------------------------------------------------------

  nmtc_prep_status <- get_modified_time(path)

  return(nmtc_prep_status)

}

#' @rdname nmtc
#' @export
make_nmtc <- function(path){


  col_types <-  c("text", "text", "text",
        "numeric", "text", "numeric", "text",
        "numeric", "text", "text", "text",
        "text", "numeric", "text", "numeric")

  nmtc <- readxl::read_excel(path,
                             na = "N/A",
                             col_types = col_types) %>%
    janitor::clean_names(case = "screaming_snake") %>%
    dplyr::mutate_if(is_logical_yesno, recode_logical_yesno)

  return(nmtc)
}
