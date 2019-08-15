library(tidyverse)
library(snakecase)

# path <- "path to site-inventory-20190623/inventory_suitable_table.csv"
inventory_suitable <- read_csv(path, guess_max = 10000, na = "NA")


inventory_suitable2 <- inventory_suitable %>%
  mutate(FILTER_PLACE_NAME = FILTER_PLACE_NAME %>%
           to_title_case(sep_in = NULL) %>%
           str_replace("Kc", "KC ")) %>%
  mutate_at(vars(matches("OWNER_CATEGORY")), 
            ~.x %>%
              to_title_case(sep_in = NULL) %>%
              str_replace("\\(", " \\(")) %>%
  mutate(FILTER_PUBLIC_OWNER = FILTER_PUBLIC_OWNER %>%
           to_title_case(sep_in = NULL) %>%
           str_replace("(No\\.)(?=[:digit:])", "No\\. ") %>%
           str_replace("\\(", " \\("))

write_csv(inventory_suitable2, 
          path,
          na = "")
