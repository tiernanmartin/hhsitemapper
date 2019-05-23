#' @title Parcel Environmental Restrictions
#' @description Environmental restrictions of parcels in King County.
#' @note Data source: King County Assessor Data Portal
#' @param path
#' @rdname env_restrictions
#' @export
make_env_restrictions <- function(path){

  env_restrictions <- readr::read_csv(path) %>%
    janitor::clean_names("screaming_snake")

  return(env_restrictions)
}

#' @title Parcel Environmental Restrictions
#' @description Environmental restrictions of parcels in King County.
#' @note Data source: King County Assessor Data Portal
#' @param env_restrictions desc
#' @rdname env_restrictions
#' @export
make_parcel_env_ready <- function(env_restrictions){

  env_frmt <- env_restrictions %>%
    dplyr::rename_all(snakecase::to_screaming_snake_case) %>%
    dplyr::filter(!is.na(TYPE)) %>%
    dplyr::transmute(PIN = make_pin(MAJOR, MINOR),
              TYPE = snakecase::to_parsed_case(TYPE) %>% toupper() %>% stringr::str_replace_all("_"," "),
              PCNT_AFFECTED,
              PCT = dplyr::if_else(PCNT_AFFECTED>0, stringr::str_c(PCNT_AFFECTED,"%"),""),
              RESTRICTION = stringr::str_c(TYPE, " (",PCT,")") %>% stringr::str_remove_all("\\s\\(\\)"))

  env_res <- env_frmt %>%
    dplyr::group_by(PIN) %>%
    tidyr::nest() %>%
    dplyr::transmute(PIN,
              ENV_RESTRICTIONS = purrr::map_chr(data, ~ purrr::pluck(.x,"RESTRICTION") %>% stringr::str_c(collapse = ", ")))

  parcel_env_ready <- env_res

  return(parcel_env_ready)

}
