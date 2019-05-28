#' @title Development Assumptions
#' @description Targets related to modeling decisions about real estate development.
#' @return
#'   \itemize{
#'     \item{\code{make_city_block_sqft()} returns an object with \code{class = "units"}.}
#'     \item{\code{make_city_block_acre()} returns an object with \code{class = "units"}.}
#'     \item{\code{make_lot_types()} returns a \code{\link[tibble:tbl_df]{tbl_df}} object.}
#'     \item{\code{make_lot_size_breaks()} returns a named list.}
#'   }


#' @rdname development_assumptions
#' @export
make_city_block_sqft <- function(){
  city_block_sqft <- units::set_units(66000,ft^2)

  return(city_block_sqft)
}

#' @rdname development_assumptions
#' @export
make_city_block_acre <- function(city_block_sqft){

  city_block_acre <- units::set_units(city_block_sqft, acre)

  return(city_block_acre)

}

#' @rdname development_assumptions
#' @export
make_lot_types <- function(){
  lot_types <- tibble::tribble(
    ~LOT_SIZE_DESC,
    "less than 1/8 block",
    "1/4 block",
    "greater than 1/4 block"
  )


  return(lot_types)
}

#' @rdname development_assumptions
#' @export
make_lot_size_breaks <- function(city_block_acre){

  lot_brks <- list(names = c("under-sized",
                             "quarter-block",
                             "half-block",
                             "whole-block",
                             "over-sized (developable)",
                             "over-sized (undevelopable)"),
                   breaks = c( units::set_units(-Inf, acre),
                               units::set_units(0.1, acre),
                               1/4 * city_block_acre,
                               1/2 * city_block_acre,
                               city_block_acre,
                               units::set_units(27.5, acre) ,
                               units::set_units(Inf,acre) ))

  lot_size_breaks <- lot_brks

  return(lot_size_breaks)

}


#' @rdname development_assumptions
#' @export
prepare_development_assumptions_zoning <- function(path){

  # GET DATA ----------------------------------------------------------------

  # Source: Google Drive

  dev_assm_zoning_gs <- googlesheets::gs_key("1UHRi5NDgSQ-ideq74xMqHxMPCCfuPjOoxsWUgjCVWI8")

  development_assumptions_zoning <- googlesheets::gs_read(dev_assm_zoning_gs, ws = "dev-assumptions-zoning-original.csv")


  # WRITE -------------------------------------------------------------------

  readr::write_csv(development_assumptions_zoning, path)


  # RETURN ------------------------------------------------------------------

  development_assumptions_prep_status <- get_modified_time(path)

  return(development_assumptions_prep_status)

}

#' @rdname development_assumptions
#' @export
make_development_assumptions_zoning <- function(path){

  development_assumptions_zoning <- readr::read_csv(path)

  return(development_assumptions_zoning)
}

#' @rdname development_assumptions
#' @export
make_lot_development_parameters <- function(){
  lot_development_parameters <- tibble::tribble(
    ~LOT_SIZE_TYPE, ~LOT_COVERAGE_PCT, ~ LOT_STORIES_NBR,
    NA,            NA,              NA,
    "undevelopable",            NA,              NA,
    "potentially developable; no assumption",            NA,              NA,
    "single family",          0.75,               2,
    "small",          0.75,               3,
    "medium",          0.75,               7,
    "large",           0.5,               7
  )

  return(lot_development_parameters)
}

#' @rdname development_assumptions
#' @export
make_development_assumptions_lot <- function(lot_types, lot_development_parameters, development_assumptions_zoning){

  lot_dev_assumptions <-
    tidyr::crossing(LOT_SIZE_DESC = lot_types$LOT_SIZE_DESC,
                    CONSOL_20 = unique(development_assumptions_zoning$CONSOL_20) ) %>%
    dplyr::left_join(development_assumptions_zoning, by = "CONSOL_20") %>%
    dplyr::mutate(LOT_SIZE_TYPE = dplyr::case_when(is.na(DEVELOPMENT_ASSUMPTION) ~ NA_character_,
                                                   !DEVELOPABLE_LGL ~ "undevelopable zoning",
                                                   stringr::str_detect(DEVELOPMENT_ASSUMPTION,"^no") ~ "potentially developable; no assumption",
                                                   stringr::str_detect(DEVELOPMENT_ASSUMPTION,"^one") ~ "single family",
                                                   stringr::str_detect(LOT_SIZE_DESC,"less") ~ "small",
                                                   stringr::str_detect(LOT_SIZE_DESC,"^1") ~ "medium",
                                                   TRUE ~ "large")) %>%
    dplyr::left_join(lot_development_parameters, by = "LOT_SIZE_TYPE") %>%
    dplyr::select(CONSOL_20,
                  DEVELOPABLE_LGL,
                  DEVELOPMENT_ASSUMPTION,
                  LOT_SIZE_DESC,
                  LOT_SIZE_TYPE,
                  LOT_COVERAGE_PCT,
                  LOT_STORIES_NBR)


  development_assumptions_lot <- lot_dev_assumptions

  return(development_assumptions_lot)
}
