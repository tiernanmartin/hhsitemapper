#' @title Real Estate Development Capacity (Seattle)
#' @description Real estate development capacity model for Seattle, Washington.
#' @note
#'   \itemize{
#'     \item{Data source: Seattle Office of Planning and Community Development
#'       (\url{https://data.seattle.gov/resource/n2mk-9di2.csv})
#'       }
#'    }
#' @param path
#' @rdname seattle_dev_cap
#' @export
prepare_seattle_dev_cap <- function(path){

  # GET DATA ----------------------------------------------------------------

  url <- "https://data.seattle.gov/resource/n2mk-9di2.csv"

  downloader::download(url, destfile = path, mode = "wb")

  # RETURN ------------------------------------------------------------------

  seattle_dev_cap_prep_status <- get_modified_time(path)


  return(seattle_dev_cap_prep_status)
}

#' @rdname seattle_dev_cap
#' @export
make_seattle_dev_cap <- function(path){

  seattle_dev_cap <- readr::read_csv(path) %>%
    dplyr::rename_all(snakecase::to_screaming_snake_case)

  return(seattle_dev_cap)
}
