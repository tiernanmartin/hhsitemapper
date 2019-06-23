#' @title Difficult Development Areas (DDA)
#' @description Census tracts designated as Difficult Development Areas (DDA) by HUD.
#' @return Returns an \code{\link[tibble:tbl_df]{tbl_df}} object.
#' @note \itemize{
#'   \item{Data source: HUD's Office of Policy Development and Research (PD&R)
#'     (\url{https://www.huduser.gov/portal/home.html})}
#'   \item{Data download: 2019 Metropolitan Difficult Development Areas
#'     (\url{https://www.huduser.gov/portal/Datasets/qct/DDA2019M.PDF})}
#' }

#' @rdname dda
#' @export
prepare_dda <- function(path){

  # GET DATA ----------------------------------------------------------------

  url <- "https://www.huduser.gov/portal/Datasets/qct/DDA2019M.PDF"

  downloader::download(url, destfile = path, mode = "wb")

  # RETURN ------------------------------------------------------------------

  dda_prep_status <- get_modified_time(path)

  return(dda_prep_status)

}

#' @rdname dda
#' @export
make_dda <- function(path){

  list_pages <- tabulizer::extract_tables(path, output = "data.frame")

  tbl_pages <- list_pages %>%
    purrr::map(~ .x %>% t() %>% tibble::as_tibble()) %>%
    purrr::reduce(dplyr::bind_cols) %>%
    dplyr::mutate_all(dplyr::funs(empty_as_na))

  zcta_dda <- tbl_pages %>%
    dplyr::slice(3:14) %>%
    tidyr::gather(OLD_COL,ZCTA) %>%
    tidyr::drop_na() %>%
    dplyr::transmute(FILTER_ZCTA = stringr::str_replace(ZCTA,"\\*",""),
                     FILTER_ELIGIBILITY_DDA = TRUE)

  dda <- zcta_dda

  return(dda)
}
