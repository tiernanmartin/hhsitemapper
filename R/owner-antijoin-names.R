#' @title Present Use Recode
#' @description Desc
#' @return a character string

#' @rdname owner_antijoin_names
#' @export
prepare_owner_antijoin_names <- function(path){

  # GET DATA ----------------------------------------------------------------

  stop_words <- tibble::tibble(word = unlist(stopwords::data_stopwords_smart))


  numbers_0_500 <- tibble::tibble(word = as.character(0:500))

  ok_words <- c("STATE","STATES", "U", "S", "A", "US")

  anti <- stop_words %>%
    dplyr::transmute(word = toupper(word)) %>%
    dplyr::bind_rows(numbers_0_500) %>%
    dplyr::filter(! word %in% ok_words)

  owner_antijoin_names <- anti

  # WRITE -------------------------------------------------------------------

  readr::write_csv(owner_antijoin_names, path)


  # RETURN ------------------------------------------------------------------

  owner_antijoin_names_prep_status <- get_modified_time(path)

  return(owner_antijoin_names_prep_status)

}

#' @rdname owner_antijoin_names
#' @export
make_owner_antijoin_names <- function(path){

  owner_antijoin_names <- readr::read_csv(path, col_types = "c")

  return(owner_antijoin_names)
}
