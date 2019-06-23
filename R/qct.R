#' @title Qualified Census Tracts (QCT)
#' @description Census tracts designated as Qualified Census Tracts (QCT) by HUD.
#' @return Returns an \code{\link[tibble:tbl_df]{tbl_df}} object.
#' @note \itemize{
#'   \item{Data source: HUD's Office of Policy Development and Research (PD&R)
#'     (\url{https://www.huduser.gov/portal/home.html})}
#'   \item{Data download: 2019 Qualified Census Tracts - Geocoded dBase File
#'     (\url{https://www.huduser.gov/portal/datasets/qct/QCT2019dbf.zip})}
#' }

#' @rdname qct
#' @export
prepare_qct <- function(path){

  # GET DATA ----------------------------------------------------------------

  url <- "https://www.huduser.gov/portal/datasets/qct/QCT2019dbf.zip"

  downloader::download(url, destfile = path, mode = "wb")

  # RETURN ------------------------------------------------------------------

  qct_prep_status <- get_modified_time(path)

  return(qct_prep_status)

}

#' @rdname qct
#' @export
make_qct <- function(path){

  unzip(zipfile = path, exdir = dirname(path))

  qct <- foreign::read.dbf("extdata/osf/QCT2019.DBF") %>%
    tibble::as_tibble() %>%
    janitor::clean_names("screaming_snake")

  return(qct)
}
