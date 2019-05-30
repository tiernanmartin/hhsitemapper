#' @title Owner Names & Categories
#' @description The names of organizations that own public (or tax-exempt) properties
#'   and a set of categories that describe the different types of owners
#'   (e.g., city, port, school district, etc.)
#' @return Returns an \code{\link[tibble]{tbl_df}} object.
#' @note
#'   \itemize{
#'     \item{Data source: these targets are derived from the Taxpayer table of the King Couny
#'       Assessor data (restricted access). The owner names have been deduplicated and the
#'       categories are created by the package author.
#'    }

#' @rdname owner
#' @export
make_owner_name_full <- function(suitability, name_recode_key, owner_antijoin_names){



  # Drop unnecessary columns (too big if you don't)
  names <- suitability %>%
    sf::st_drop_geometry() %>%
    dplyr::transmute(PIN,
                     OWNER_NAME = stringr::str_trim(stringr::str_squish(stringr::str_replace_all(TAXPAYER_NAME,"[[:punct:]]"," "))),
                     SUIT_OWNER_TAX_E)

  names_tax_exempt <- names %>%
    dplyr::filter(SUIT_OWNER_TAX_E) %>%
    dplyr::select(-SUIT_OWNER_TAX_E)

  names_clean_ngram3_only <- names_tax_exempt %>%
    dplyr::mutate(TOKEN = OWNER_NAME) %>%
    tidytext::unnest_tokens(NGRAM3, TOKEN, token = "ngrams", n = 3, to_lower = FALSE) %>%
    tidyr::separate(NGRAM3, c("NAME_NGRAM_3A","NAME_NGRAM_3B", "NAME_NGRAM_3C"), sep = " ") %>%
    dplyr::left_join(name_recode_key) %>%
    dplyr::transmute(PIN,
                     OWNER_NAME,
                     NAME_NEW,
                     PATTERN = stringr::str_c(NAME_NGRAM_3A,NAME_NGRAM_3B,NAME_NGRAM_3C, sep = " "),
                     NEW = dplyr::case_when(
                       !is.na(NAME_NEW) ~ purrr::pmap_chr(list(OWNER_NAME,PATTERN, NAME_NEW), stringr::str_replace),
                       TRUE ~ NA_character_
                     )) %>%
    dplyr::group_by(PIN)  %>%
    dplyr::summarise(OWNER_NAME = dplyr::if_else(all(is.na(NEW)), dplyr::first(OWNER_NAME), first_not_na(NEW)))

  names_clean_ngram3_all <- names_tax_exempt %>%
    dplyr::filter(! PIN %in% names_clean_ngram3_only$PIN) %>%
    dplyr::union(names_clean_ngram3_only)

  names_clean_ngram2_only <- names_clean_ngram3_all %>%
    dplyr::mutate(TOKEN = OWNER_NAME) %>%
    tidytext::unnest_tokens(NGRAM2, TOKEN, token = "ngrams", n = 2, to_lower = FALSE) %>%
    tidyr::separate(NGRAM2, c("NAME_NGRAM_2A","NAME_NGRAM_2B"), sep = " ") %>%
    dplyr::left_join(name_recode_key) %>%
    dplyr::transmute(PIN,
                     OWNER_NAME,
                     PATTERN = purrr::map2_chr(NAME_NGRAM_2A,NAME_NGRAM_2B, stringr::str_c, sep = " "),
                     NEW = dplyr::if_else(is.na(NAME_NEW), NA_character_, purrr::pmap_chr(list(OWNER_NAME, PATTERN, NAME_NEW), stringr::str_replace))) %>%
    dplyr::group_by(PIN)  %>%
    dplyr::summarise(OWNER_NAME = dplyr::if_else(all(is.na(NEW)), dplyr::first(OWNER_NAME), first_not_na(NEW)))

  names_clean_ngram2_all <- names_tax_exempt %>%
    dplyr::filter(! PIN %in% names_clean_ngram2_only$PIN) %>%
    dplyr::union(names_clean_ngram2_only)

  names_clean_ngram1_only <- names_clean_ngram2_all %>%
    tidytext::unnest_tokens(ORIG, OWNER_NAME, token = "ngrams", n = 1, to_lower = FALSE) %>%
    dplyr::left_join(name_recode_key, by = c(ORIG = "NAME_NGRAM_1")) %>%
    dplyr::mutate(OWNER_NAME = dplyr::if_else(is.na(NAME_NEW),ORIG,NAME_NEW)) %>%
    dplyr::group_by(PIN) %>%
    dplyr::summarise(OWNER_NAME = stringr::str_c(OWNER_NAME, collapse = " "))

  names_clean_ngram1_all <- names_tax_exempt %>%
    dplyr::filter(! PIN %in% names_clean_ngram1_only$PIN) %>%
    dplyr::union(names_clean_ngram1_only)

  # Fix inverted city names (Seattle City of -> City of Seattle)
  # also fix ports and state of wa
  names_clean_all_ngrams <- names_clean_ngram1_all %>%
    dplyr::mutate(NO_XXX_OF = stringr::str_replace_all(OWNER_NAME, "CITY OF","") %>% stringr::str_replace_all("PORT OF", ""),
                  OWNER_NAME = dplyr::case_when(
                    stringr::str_detect(OWNER_NAME, "CITY OF") ~  str_clean_upper(stringr::str_c("CITY OF ", NO_XXX_OF)),
                    stringr::str_detect(OWNER_NAME, "PORT OF") ~  str_clean_upper(stringr::str_c("PORT OF ", NO_XXX_OF)),
                    stringr::str_detect(OWNER_NAME, "WASHINGTON STATE") ~ stringr::str_replace(OWNER_NAME, "WASHINGTON STATE", "STATE OF WASHINGTON"),
                    TRUE ~ OWNER_NAME
                  )) %>%
    dplyr::select(PIN,
                  OWNER_NAME)

  # names_clean_all_ngrams %>%
  #   dplyr::count(OWNER_NAME, sort = TRUE) %>%
  #   dplyr::filter(!stringr::str_detect(OWNER_NAME, "CITY OF")) %>%
  #   dplyr::filter(n>5) %>%
  #   print(n=Inf)

  # Remove typo duplicates
  common_owner_names <-
    names_clean_all_ngrams %>%
    dplyr::count(OWNER_NAME, sort = TRUE) %>%
    dplyr::filter(n >= 5 ) %>%
    dplyr::transmute(ID = row_number(),
                     OWNER_NAME,
                     TOKEN = OWNER_NAME) %>%
    tidytext::unnest_tokens(WORD, TOKEN, token = "ngrams", n = 1, to_lower = FALSE) %>%
    dplyr::anti_join(owner_antijoin_names, by = c("WORD" = "word")) %>%
    dplyr::group_by(ID) %>%
    dplyr::mutate(OWNER_NAME = dplyr::first(OWNER_NAME),
                  OWNER_NAME_TRIM = stringr::str_c(WORD, collapse = " ")) %>%
    dplyr::filter(!duplicated(OWNER_NAME_TRIM)) %>%
    dplyr::transmute(OWNER_NAME_NO_TYPO = OWNER_NAME,
                     OWNER_NAME_TRIM_NO_TYPO = OWNER_NAME_TRIM) %>%
    dplyr::ungroup() %>%
    dplyr::select(-ID)

  # Pull out the parcels with digits in the OWNER_NAME (these don't work for the typo join step)
  names_with_digits <- names_clean_all_ngrams %>%
    dplyr::filter(stringr::str_detect(OWNER_NAME, "[[:digit:]]"))

  names_without_digits <- names_clean_all_ngrams %>%
    dplyr::filter(! PIN %in% names_with_digits$PIN)

  names_without_typos <-
    names_without_digits %>%
    dplyr::transmute(ID = dplyr::row_number(),
                     PIN,
                     OWNER_NAME,
                     TOKEN = OWNER_NAME) %>%
    tidytext::unnest_tokens(WORD, TOKEN, token = "ngrams", n = 1, to_lower = FALSE) %>%
    dplyr::anti_join(owner_antijoin_names, by = c("WORD" = "word")) %>%
    dplyr::group_by(ID) %>%
    dplyr::mutate(PIN = dplyr::first(PIN),
                  OWNER_NAME = dplyr::first(OWNER_NAME),
                  OWNER_NAME_TRIM = stringr::str_c(WORD, collapse = " ")) %>%
    fuzzyjoin::stringdist_left_join(common_owner_names, by = c("OWNER_NAME_TRIM" = "OWNER_NAME_TRIM_NO_TYPO"),method = "jw",distance_col = "DIST",p=0.1, max_dist = .04) %>%
    dplyr::arrange(DIST) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::bind_rows(names_with_digits) %>%
    dplyr::transmute(PIN,
                     OWNER_NAME_FULL = dplyr::if_else(is.na(OWNER_NAME_TRIM_NO_TYPO), OWNER_NAME, OWNER_NAME_NO_TYPO))

  # Check what was removed
  # names_clean_all_ngrams %>%
  #   dplyr::filter(OWNER_NAME_FULL %!in% names_without_typos$OWNER_NAME) %>%
  #   dplyr::count(OWNER_NAME_FULL, sort = TRUE) %>%
  #   print(n=Inf)


  owner_name_full <- names %>%
    dplyr::left_join(names_without_typos, by = "PIN") %>%
    dplyr::transmute(PIN,
                     OWNER_NAME_FULL = dplyr::if_else(is.na(OWNER_NAME_FULL), OWNER_NAME, OWNER_NAME_FULL))

  # Check out the result
  # owner_name_full %>%
  #   dplyr::filter(PIN %in% names_tax_exempt$PIN) %>%
  #   dplyr::count(OWNER_NAME_FULL, sort = TRUE) %>% print(n=100)


  return(owner_name_full)

}

#' @rdname owner
#' @export
make_owner_category <- function(owner_name_full, public_owner_name_category_key, other_exempt_owner_name_category_key){

# 1. join public owners
# 2. join other tax exempt (regex) and remove duplicates
# 3. case_when() to condense owners whose names show up in public and other tax exempt

category_types <- tibble::tibble(OWNER_CATEGORY = c("federal","state","county","city","special purpose district","housing authority","regional transit authority","school district", "higher-education provider", "tribal","religious association","residential association","uncategorized")) %>%
  dplyr::mutate(OWNER_CATEGORY_TYPE = dplyr::case_when(
    OWNER_CATEGORY %in% "higher-education provider" ~ "uncertain",
    OWNER_CATEGORY %in% c("tribal","religious association","residential association","uncategorized") ~ "private",
    TRUE ~ "public"
  ))


owner_public <- owner_name_full %>%
  dplyr::left_join(public_owner_name_category_key, by = "OWNER_NAME_FULL")

public_pins <- owner_name_full %>%
  dplyr::semi_join(public_owner_name_category_key, by = "OWNER_NAME_FULL") %>%
  purrr::pluck("PIN")

# ~10 minute operation
owner_tax_exempt <- owner_public %>%
  fuzzyjoin::regex_left_join(other_exempt_owner_name_category_key, by = c(OWNER_NAME_FULL = "OWNER_NAME_FULL_OTHER")) %>%
  dplyr::group_by(PIN) %>%
  dplyr::summarise_all(first_not_na)

owner_category <- owner_tax_exempt %>%
  dplyr::transmute(PIN,
                   OWNER_NAME_FULL = toupper(OWNER_NAME_FULL),
                   OWNER_CATEGORY = dplyr::case_when(
                     !is.na(OWNER_CATEGORY) ~ OWNER_CATEGORY,
                     !is.na(OWNER_NAME_FULL_OTHER) ~ OWNER_CATEGORY_OTHER,
                     TRUE ~ "uncategorized"
                   ),
                   OWNER_NAME_ORG = dplyr::case_when(
                     !is.na(OWNER_NAME_ORG) ~ toupper(OWNER_NAME_ORG),
                     TRUE ~ toupper(OWNER_NAME_FULL)
                   ),
                   OWNER_NAME_DEPT = toupper(OWNER_NAME_DEPT)) %>%
  dplyr::left_join(category_types, by = "OWNER_CATEGORY") %>%
  dplyr::mutate(OWNER_PUBLIC_LGL = dplyr::case_when(
    OWNER_CATEGORY_TYPE %in% "uncertain" & PIN %in% public_pins ~ TRUE,
    OWNER_CATEGORY_TYPE %in% "public" ~ TRUE,
    TRUE ~ FALSE
  )
  ) %>%
  dplyr::mutate(OWNER_CATEGORY = dplyr::case_when(
    OWNER_CATEGORY %in% "special purpose district" ~ "special purpose district (including utilities)",  # relable "special purpose district"
    TRUE ~ OWNER_CATEGORY
  )) %>%
  dplyr::select(PIN,
                OWNER_NAME_FULL,
                OWNER_PUBLIC_LGL,
                OWNER_CATEGORY,
                OWNER_NAME_ORG,
                OWNER_NAME_DEPT)


# RETURN ------------------------------------------------------------------

  return(owner_category)

}
