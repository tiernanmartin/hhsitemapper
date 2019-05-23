#' @title Parcel Data (Ready)
#' @description Desc
#' @param parcel_addr_ready desc
#' @param parcel_env_ready desc
#' @param parcel_acct_ready desc
#' @param parcel_sf_ready desc
#' @param parcel_df_ready desc
#' @return a character string
#' @rdname parcel_df_ready
#' @export
make_parcel_ready <- function(parcel_addr_ready, parcel_env_ready, parcel_acct_ready, parcel_sf_ready, parcel_df_ready){

  # MAKE PARCEL_READY

  parcel_ready <- list(parcel_acct_ready, parcel_sf_ready, parcel_df_ready) %>%
    purrr::reduce(.f = dplyr::inner_join, by = "PIN") %>%
    dplyr::left_join(parcel_addr_ready, by = "PIN") %>%
    dplyr::left_join(parcel_env_ready, by = "PIN") %>%
    sf::st_as_sf()

  return(parcel_ready)
}
