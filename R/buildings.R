#' @title Building Data
#' @rdname buildings
#' @description Building information for structures in King County, Washington.
#' @return Returns a \code{\link[tibble:tbl_df]{tbl_df}} object.
#' @note The data for these targets come from the King County Assessor's Office Assessments Data Download
#'   \href{https://info.kingcounty.gov/assessor/DataDownload/default.aspx}{web portal}:
#'   \itemize{
#'     \item{Residential buildings: \href{https://info.kingcounty.gov/assessor/DataDownload/default.aspx}{Residential Building (.ZIP)}
#'    }
#'     \item{Apartment Complexes: \href{https://info.kingcounty.gov/assessor/DataDownload/default.aspx}{Apartment Complex (.ZIP)}
#'    }
#'     \item{Condominium Complexes: \href{https://info.kingcounty.gov/assessor/DataDownload/default.aspx}{Condo Complex and Units (.ZIP)}
#'    }
#'     \item{Commercial buildings: \href{https://info.kingcounty.gov/assessor/DataDownload/default.aspx}{Commercial Building (.ZIP)}
#'    }
#'   }

#' @rdname buildings
#' @export
make_building_template <- function(){

  building_template <- tibble::tibble(PIN = as.character(""),
                       BLDG_NBR = as.integer(""),
                       BLDG_NET_SQ_FT= as.integer(""),
                       NBR_LIVING_UNITS = as.integer(""),
                       NBR_BLDGS = as.integer(""),
                       BLDG_CAT = as.character("")) %>%
    dplyr::slice(0)


  return(building_template)
}


#' @rdname buildings
#' @export
make_building <- function(building_template, building_residential, building_apartment, building_condo, building_commercial){

comm_join <- building_commercial %>%
    dplyr::transmute(PIN,
              BLDG_NBR,
              BLDG_NET_SQ_FT,
              NBR_BLDGS,
              BLDG_CAT = "commercial")

  res_join <- building_residential %>%
    dplyr::transmute(PIN,
              BLDG_NBR,
              BLDG_NET_SQ_FT = SQ_FT_TOT_LIVING,
              NBR_LIVING_UNITS,
              BLDG_CAT = "residential")

  apt_join <- building_apartment %>%
    dplyr::transmute(PIN,
              NBR_BLDGS,
              NBR_LIVING_UNITS = NBR_UNITS,
              BLDG_CAT = "apartment")

  condo_join <- building_condo %>%
    dplyr::transmute(PIN = MAJOR,
              NBR_LIVING_UNITS = NBR_UNITS,
              BLDG_CAT = "condo")

  # Join all bulding objects to the empty tibble
  bldg_all <- list(building_template, comm_join, res_join, apt_join, condo_join) %>%
    purrr::reduce(dplyr::full_join)

  # ~ 2 min. operation

  bldg_all_sum <- bldg_all %>%
    dplyr::mutate(NBR_BLDGS = dplyr::if_else(BLDG_CAT %in% c("residential", "condo"),as.integer(1),as.integer(NBR_BLDGS))) %>%
    dplyr::mutate(CAT_LGL = TRUE,
           COL_NAME = stringr::str_c("TYPE",toupper(BLDG_CAT),"LGL", sep = "_")) %>%
    tidyr::spread(COL_NAME, CAT_LGL) %>%
    dplyr::mutate_at(dplyr::vars(TYPE_APARTMENT_LGL:TYPE_RESIDENTIAL_LGL), ~ dplyr::if_else(is.na(.),FALSE,.)) %>%
    dplyr::group_by(PIN) %>%
    dplyr::summarise(BLDG_NBR = max(dplyr::n(),na.rm = TRUE),
              BLDG_NET_SQ_FT = sum(BLDG_NET_SQ_FT, na.rm = TRUE),
              BLDG_LIVING_UNITS = sum(NBR_LIVING_UNITS, na.rm = TRUE),
              BLDG_TYPE_APARTMENT_LGL = any(TYPE_APARTMENT_LGL),
              BLDG_TYPE_COMMERCIAL_LGL = any(TYPE_COMMERCIAL_LGL),
              BLDG_TYPE_CONDO_LGL = any(TYPE_CONDO_LGL),
              BLDG_TYPE_RESIDENTIAL_LGL = any(TYPE_RESIDENTIAL_LGL))


  building <- bldg_all_sum

# RETURN ------------------------------------------------------------------

  return(building)
}

#' @rdname buildings
#' @export
make_building_residential <- function(path){

  building_residential <- readr::read_csv(path) %>%
    dplyr::rename_all(snakecase::to_screaming_snake_case) %>%
    dplyr::mutate(PIN = stringr::str_c(MAJOR,MINOR,sep = ""))

  return(building_residential)
}

#' @rdname buildings
#' @export
make_building_apartment <- function(path){

  building_apartment <- readr::read_csv(path) %>%
    dplyr::rename_all(snakecase::to_screaming_snake_case) %>%
    dplyr::mutate(PIN = stringr::str_c(MAJOR,MINOR,sep = ""))

  return(building_apartment)
}

#' @rdname buildings
#' @export
make_building_condo <- function(path){

  building_condo <- readr::read_csv(path) %>%
    dplyr::rename_all(snakecase::to_screaming_snake_case)

  return(building_condo)
}


#' @rdname buildings
#' @export
make_building_commercial <- function(path){

  building_commercial <- readr::read_csv(path) %>%
    dplyr::rename_all(snakecase::to_screaming_snake_case) %>%
    dplyr::mutate(PIN = stringr::str_c(MAJOR,MINOR,sep = ""))

  return(building_commercial)
}
