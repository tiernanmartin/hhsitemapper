#' @title Official Names
#' @description The official names of the public offices, departments, and taxing authorities of various levels
#'   of govenment found in Washington State.
#' @return Returns a \code{\link[tibble:tbl_df]{tbl_df}} object.
#' @note The following datasets are stored in the a Github repository which is maintained by the package's author:
#'   \itemize{
#'     \item{Seattle data download: \href{https://raw.githubusercontent.com/tiernanmartin/datasets/master/seattle-bureaucracy/data/seattle-bureaucracy.csv}{seattle-bureaucracy.csv}
#'       }
#'     \item{King County data download: \href{https://raw.githubusercontent.com/tiernanmartin/datasets/master/king-county-bureaucracy/data/kc-bureaucracy.csv}{kc-bureaucracy.csv}
#'       }
#'     \item{Washington State data download: \href{https://raw.githubusercontent.com/tiernanmartin/datasets/master/wa-bureaucracy/data/wa-bureaucracy.csv}{wa-bureaucracy.csv}
#'       }
#'     \item{Federal Government data download: \href{https://raw.githubusercontent.com/tiernanmartin/datasets/master/us-bureaucracy/data/us-bureaucracy.csv}{us-bureaucracy.csv}
#'       }
#'     \item{Places data source: \code{\link[tigris:places]{tigris::places}()}
#'       }
#'     \item{Tribes data download: \href{https://raw.githubusercontent.com/tiernanmartin/datasets/master/wa-tribes/data/wa-tribes.csv}{wa-tribes.csv}
#'       }
#'     \item{Housing Authorities data download: \href{https://raw.githubusercontent.com/tiernanmartin/datasets/master/wa-housing-authorities/data/wa-housing-authorities.csv}{wa-housing-authorities.csv}
#'       }
#'     \item{Regional Transit Authorities: no external data (Sound Transit is the only RTA in King County)
#'       }
#'     \item{Special Purpose Districts & School Districts data download: \href{https://raw.githubusercontent.com/tiernanmartin/datasets/master/wa-special-purpose-districts/data/wa-special-purpose-districts.csv}{wa-special-purpose-districts.csv}
#'       }
#'     \item{Higher-Education Providers data download: \href{https://raw.githubusercontent.com/tiernanmartin/datasets/master/wa-higher-ed-providers/data/wa-higher-ed-providers.csv}{wa-higher-ed-providers.csv}
#'       }
#'     \item{Hospitals data download: \href{https://raw.githubusercontent.com/tiernanmartin/datasets/master/wa-hospitals/data/wa-hospitals.csv}{wa-hospitals.csv}
#'       }
#'    }


# ALL OFFICIAL NAMES ------------------------------------------------------
#' @rdname official_names
#' @export
make_official_names <- function(official_names_seattle,
      official_names_kc,
      official_names_wa,
      official_names_us,
      official_names_places,
      official_names_tribes,
      official_names_housing_authorities,
      official_names_regional_transit_authorities,
      official_names_special_purpose_districts,
      official_names_school_districts,
      official_names_higher_ed_providers,
      official_names_hospitals){

  official_names_nested <- tibble::tribble(
    ~ NAME, ~ CATEGORY, ~ ORGANIZATION,
    official_names_seattle, "city", "City of Seattle",
    official_names_kc,"county","King County",
    official_names_wa, "state", "Washington State",
    official_names_us, "federal","U.S. Federal Government",
    official_names_places, "city", NA_character_,
    official_names_tribes, "tribal",NA_character_,
    official_names_housing_authorities, "housing authority",NA_character_,
    official_names_regional_transit_authorities, "regional transit authority","Sound Transit",
    official_names_special_purpose_districts, "special purpose district",NA_character_,
    official_names_school_districts, "school district",NA_character_,
    official_names_higher_ed_providers, "higher-education provider",NA_character_,
    official_names_hospitals, "hospital", NA_character_
  )

  official_names_tidy <- official_names_nested %>%
    tidyr::unnest() %>%
    dplyr::transmute(OWNER_NAME_OFFICIAL = dplyr::case_when(!is.na(AGENCY_NAME) ~ AGENCY_NAME,
                                                            !is.na(NAME) ~ NAME,
                                                            TRUE ~ DEPARTMENT),
                     OWNER_NAME_DEPT = DEPARTMENT,
                     OWNER_NAME_CATEGORY = CATEGORY,
                     OWNER_NAME_ORG = dplyr::case_when(is.na(ORGANIZATION) ~ NAME,TRUE ~ ORGANIZATION))

  official_names <- official_names_tidy


# RETURN ------------------------------------------------------------------

  return(official_names)
}

# SEATTLE -----------------------------------------------------------------


#' @param path
#' @rdname official_names
#' @export
prepare_official_names_seattle <- function(path){

  # GET DATA ----------------------------------------------------------------

  url <- "https://raw.githubusercontent.com/tiernanmartin/datasets/master/seattle-bureaucracy/data/seattle-bureaucracy.csv"

  downloader::download(url, destfile = path, mode = "wb")

  # RETURN ------------------------------------------------------------------

  official_names_seattle_prep_status <- get_modified_time(path)


  return(official_names_seattle_prep_status)
}

#' @rdname official_names
#' @export
make_official_names_seattle <- function(path){

  official_names_seattle <- readr::read_csv(path)

  return(official_names_seattle)

}


# KING COUNTY -------------------------------------------------------------


#' @param path
#' @rdname official_names
#' @export
prepare_official_names_kc <- function(path){

  # GET DATA ----------------------------------------------------------------

  url <- "https://raw.githubusercontent.com/tiernanmartin/datasets/master/king-county-bureaucracy/data/kc-bureaucracy.csv"

  downloader::download(url, destfile = path, mode = "wb")

  # RETURN ------------------------------------------------------------------

  official_names_kc_prep_status <- get_modified_time(path)


  return(official_names_kc_prep_status)
}

#' @rdname official_names
#' @export
make_official_names_kc <- function(path){

  official_names_kc <- readr::read_csv(path)

  return(official_names_kc)

}



# WASHINGTON STATE --------------------------------------------------------


#' @param path
#' @rdname official_names
#' @export
prepare_official_names_wa <- function(path){

  # GET DATA ----------------------------------------------------------------

  url <- "https://raw.githubusercontent.com/tiernanmartin/datasets/master/wa-bureaucracy/data/wa-bureaucracy.csv"

  downloader::download(url, destfile = path, mode = "wb")

  # RETURN ------------------------------------------------------------------

  official_names_wa_prep_status <- get_modified_time(path)


  return(official_names_wa_prep_status)
}

#' @rdname official_names
#' @export
make_official_names_wa <- function(path){

  official_names_wa <- readr::read_csv(path)

  return(official_names_wa)

}



# FEDERAL GOVERNMENT ------------------------------------------------------


#' @param path
#' @rdname official_names
#' @export
prepare_official_names_us <- function(path){

  # GET DATA ----------------------------------------------------------------

  url <- "https://raw.githubusercontent.com/tiernanmartin/datasets/master/us-bureaucracy/data/us-bureaucracy.csv"

  downloader::download(url, destfile = path, mode = "wb")

  # RETURN ------------------------------------------------------------------

  official_names_us_prep_status <- get_modified_time(path)


  return(official_names_us_prep_status)
}

#' @rdname official_names
#' @export
make_official_names_us <- function(path){

  official_names_us <- readr::read_csv(path)

  return(official_names_us)

}



# PLACES ------------------------------------------------------------------


#' @param path
#' @rdname official_names
#' @export
prepare_official_names_places <- function(path){

  # GET DATA ----------------------------------------------------------------

  official_names_places <- tigris::places(state = "WA",cb = FALSE) %>%
    sf::st_drop_geometry() %>%
    tibble::as_tibble() %>%
    dplyr::transmute(NAME = dplyr::case_when(
      stringr::str_detect(NAMELSAD,"city$") ~ stringr::str_c("City of ",stringr::str_extract(NAMELSAD,".+(?=\\scity$)")),
      stringr::str_detect(NAMELSAD,"town$") ~ stringr::str_c("Town of ",stringr::str_extract(NAMELSAD,".+(?=\\stown$)")),
      stringr::str_detect(NAMELSAD,"CDP$") ~ stringr::str_c(stringr::str_extract(NAMELSAD,".+(?=\\sCDP$)")),
      TRUE ~ NA_character_
    ))


  # WRITE DATA --------------------------------------------------------------

  readr::write_csv(official_names_places, path)

  # RETURN ------------------------------------------------------------------

  official_names_places_prep_status <- get_modified_time(path)


  return(official_names_places_prep_status)
}

#' @rdname official_names
#' @export
make_official_names_places <- function(path){

  official_names_places <- readr::read_csv(path)

  return(official_names_places)

}


# TRIBES ------------------------------------------------------------------

#' @param path
#' @rdname official_names
#' @export
prepare_official_names_tribes <- function(path){

  # GET DATA ----------------------------------------------------------------

  url <- "https://raw.githubusercontent.com/tiernanmartin/datasets/master/wa-tribes/data/wa-tribes.csv"

  downloader::download(url, destfile = path, mode = "wb")

  # RETURN ------------------------------------------------------------------

  official_names_tribes_prep_status <- get_modified_time(path)


  return(official_names_tribes_prep_status)
}

#' @rdname official_names
#' @export
make_official_names_tribes <- function(path){

  official_names_tribes <- readr::read_csv(path)

  return(official_names_tribes)

}


# HOUSING AUTHORITIES -----------------------------------------------------


#' @param path
#' @rdname official_names
#' @export
prepare_official_names_housing_authorities <- function(path){

  # GET DATA ----------------------------------------------------------------

  url <- "https://raw.githubusercontent.com/tiernanmartin/datasets/master/wa-housing-authorities/data/wa-housing-authorities.csv"

  downloader::download(url, destfile = path, mode = "wb")

  # RETURN ------------------------------------------------------------------

  official_names_housing_authorities_prep_status <- get_modified_time(path)


  return(official_names_housing_authorities_prep_status)
}

#' @rdname official_names
#' @export
make_official_names_housing_authorities <- function(path){

  official_names_housing_authorities <- readr::read_csv(path)

  return(official_names_housing_authorities)

}


# REGIONAL TRANSIT AUTHORITIES --------------------------------------------

#' @rdname official_names
#' @export
make_official_names_regional_transit_authorities <- function(){

  official_names_regional_transit_authorities <- tibble::tibble(NAME = "Central Puget Sound Regional Transit Authority",
                                                                ORGANIZATION = "Sound Transit")

  return(official_names_regional_transit_authorities)

}


# SPECIAL PURPOSE DISTRICTS -----------------------------------------------


#' @param path
#' @rdname official_names
#' @export
prepare_official_names_special_purpose_districts <- function(path){

  # GET DATA ----------------------------------------------------------------

  url <- "https://raw.githubusercontent.com/tiernanmartin/datasets/master/wa-special-purpose-districts/data/wa-special-purpose-districts.csv"

  downloader::download(url, destfile = path, mode = "wb")

  # RETURN ------------------------------------------------------------------

  official_names_special_purpose_districts_prep_status <- get_modified_time(path)


  return(official_names_special_purpose_districts_prep_status)
}

#' @rdname official_names
#' @export
make_official_names_special_purpose_districts <- function(path){

  official_names_special_purpose_districts <- readr::read_csv(path) %>%
    dplyr::filter(stringr::str_detect(COUNTIES_INCLUDED,"King")) %>%
    dplyr::filter(STATUS %in% "active") %>%
    dplyr::transmute(NAME = DISTRICT_NAME)

  return(official_names_special_purpose_districts)

}


# SCHOOL DISTRICTS --------------------------------------------------------

#' @rdname official_names
#' @export
make_official_names_school_districts <- function(official_names_special_purpose_districts){

  official_names_school_districts <- official_names_special_purpose_districts %>%
    dplyr::filter(stringr::str_detect(NAME, "School"))

  return(official_names_school_districts)

}


# HIGHER-ED PROVIDERS -----------------------------------------------------


#' @param path
#' @rdname official_names
#' @export
prepare_official_names_higher_ed_providers <- function(path){

  # GET DATA ----------------------------------------------------------------

  url <- "https://raw.githubusercontent.com/tiernanmartin/datasets/master/wa-higher-ed-providers/data/wa-higher-ed-providers.csv"

  downloader::download(url, destfile = path, mode = "wb")

  # RETURN ------------------------------------------------------------------

  official_names_higher_ed_providers_prep_status <- get_modified_time(path)


  return(official_names_higher_ed_providers_prep_status)
}

#' @rdname official_names
#' @export
make_official_names_higher_ed_providers <- function(path){

  official_names_higher_ed_providers <- readr::read_csv(path)

  return(official_names_higher_ed_providers)

}


# HOSPITALS ---------------------------------------------------------------


#' @param path
#' @rdname official_names
#' @export
prepare_official_names_hospitals <- function(path){

  # GET DATA ----------------------------------------------------------------

  url <- "https://raw.githubusercontent.com/tiernanmartin/datasets/master/wa-hospitals/data/wa-hospitals.csv"

  downloader::download(url, destfile = path, mode = "wb")

  # RETURN ------------------------------------------------------------------

  official_names_hospitals_prep_status <- get_modified_time(path)


  return(official_names_hospitals_prep_status)
}

#' @rdname official_names
#' @export
make_official_names_hospitals <- function(path){

  official_names_hospitals <- readr::read_csv(path)

  return(official_names_hospitals)

}
