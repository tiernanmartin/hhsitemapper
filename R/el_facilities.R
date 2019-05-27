#' @title Early Learning Facilities (King County)
#' @description Early learning facilities within King County, Washington.
#' @return Returns an \code{\link[sf:sf]{sf object}}.
#' @note \itemize{
#'   \item{Data source: Washington Department of Children, Youth, and Families (provided by \href{https://team3si.com/}{Third Sector Intelligence})}
#' }
#' @rdname el_facilities
#' @export
make_el_facilities <- function(path){

  el_facilities <- readxl::read_excel(path, sheet = "Provider Data") %>%
    janitor::clean_names(case = "screaming_snake") %>%
    sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE")) %>%
    sf::st_set_crs(4326) %>%
    sf::st_transform(2926)


  # RETURN ------------------------------------------------------------------

  return(el_facilities)
}

#' @rdname el_facilities
#'@keywords internal
view_el_facilities <- function(){
  loadd(el_facilities)
  mapview::mapview(el_facilities, zcol = "FACILITY_TYPE", legend = TRUE)
}
