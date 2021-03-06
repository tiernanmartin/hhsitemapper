#' @title Brownfield Sites
#' @description Brownfield sites in King County.
#' @return Returns an \code{\link[sf:sf]{sf object}}.
#' @note
#'   \itemize{
#'     \item{Data source: Washington Department of Ecology - \href{https://fortress.wa.gov/ecy/tcpwebreporting/report.aspx}{Toxics Cleanup Program}
#'       }
#'     \item{Data download: \href{https://apps.ecology.wa.gov/tcpwebreporting/reports/cleanup/contaminated/export?format=csv}{.csv file}
#'       }
#'    }
#' @param path
#' @rdname contaminated_sites
#' @export
prepare_contaminated_sites <- function(path){

  # GET DATA ----------------------------------------------------------------

  url <- "https://apps.ecology.wa.gov/tcpwebreporting/reports/cleanup/contaminated/export?format=csv"

  downloader::download(url, destfile = path, mode = "wb")

  # RETURN ------------------------------------------------------------------

  contaminated_sites_prep_status <- get_modified_time(path)


  return(contaminated_sites_prep_status)
}

#' @rdname contaminated_sites
#' @export
make_contaminated_sites <- function(path){

  cleanup_website_url <- "https://fortress.wa.gov/ecy/gsp/Sitepage.aspx?csid="

  contaminated_sites_wide <- readr::read_csv(path) %>%
    janitor::clean_names(case = "screaming_snake") %>%
    dplyr::mutate(CLEANUP_WEBSITE = stringr::str_c(cleanup_website_url,CLEANUP_SITE_ID, sep = ""))

  contaminated_sites_kc <- contaminated_sites_wide %>%
    dplyr::filter(COUNTY %in% "King")

  contaminated_sites_contaminant <- contaminated_sites_kc %>%
    dplyr::select(CLEANUP_SITE_ID,
                  CONTAMINANT_NAME:BEDROCK) %>%
    tidyr::gather(MEDIA, CODE, GROUND_WATER:BEDROCK) %>%
    dplyr::group_by(CLEANUP_SITE_ID) %>%
    tidyr::nest()  %>%
    dplyr::mutate(CONTAMINANT_NAME = purrr::map_chr(data, ~ dplyr::pull(.x, CONTAMINANT_NAME) %>% str_unique_lower()),
                  CONTAMINANT_NAME = dplyr::case_when(
                    stringr::str_detect(CONTAMINANT_NAME, ",") ~ "multiple contaminants",
                    TRUE ~ CONTAMINANT_NAME
                  ),
                  CONTAMINATED_MEDIA = purrr::map_chr(data, ~ dplyr::filter(.x, !is.na(CODE)) %>% dplyr::pull(MEDIA) %>% str_unique_lower()))  %>%
    dplyr::select(-data)

  contaminated_sites_tidy <- contaminated_sites_wide %>%
    dplyr::select(-(CONTAMINANT_NAME:BEDROCK)) %>%
    dplyr::distinct() %>%
    dplyr::left_join(contaminated_sites_contaminant, by = "CLEANUP_SITE_ID")

  contaminated_sites_sf <- contaminated_sites_tidy %>%
    sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE")) %>%
    sf::st_set_crs(4326) %>%
    sf::st_transform(2926)

  contaminated_sites <- contaminated_sites_sf

  return(contaminated_sites)

}

#' @rdname contaminated_sites
#'@keywords internal
view_contaminated_sites <- function(){
  loadd(contaminated_sites)
  mapview::mapview(contaminated_sites, zcol = "CONTAMINANT_NAME", legend = TRUE)
}
