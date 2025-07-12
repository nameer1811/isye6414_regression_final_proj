library(dplyr)
library(janitor)
library(purrr)
library(glue)
library(stringr)

load_and_combine_data <- function(directory_path) {
  
  if (FALSE){
    directory_path <- "data/mean_income"
  }
  
  file_pattern <- glue(".*-Data\\.csv$")
  
  file_paths <- list.files(path = directory_path, pattern = file_pattern, full.names = TRUE)
  
  list_of_dfs <- map(file_paths, function(path) { 
    
    year_match_result <- regmatches(path, regexpr("Y(\\d{4})\\.*", path))
    
    year_extracted <- sub("Y(\\d{4})\\.*", "\\1", year_match_result)
    
    read.csv(path, skip = 1, na.strings = c("N", "-", "(X)")) %>% 
      clean_names() %>%
      mutate(year = as.numeric(year_extracted)) %>%
      select(-contains(c("margin_of_error", "ratio", "unit", "percent", "x"))) %>%
      mutate(state = str_trim(str_extract(geographic_area_name, ",\\s*([^,]+)$"), side = "left"),
             state = str_replace(state, "^,\\s*", ""))
      }
    )
  
  common_columns <- names(list_of_dfs[[1]])
  
  for (i in 2:length(list_of_dfs)) {
    common_columns <- intersect(common_columns, names(list_of_dfs[[i]]))
  }
  
  list_of_dfs_common_cols <- map(list_of_dfs, ~ .x %>% select(all_of(common_columns)))
  
  for (col_name in common_columns) {
    first_col_type <- class(list_of_dfs_common_cols[[1]][[col_name]])
    
    if (first_col_type %in% c("numeric", "integer")) {
      list_of_dfs_common_cols <- map(list_of_dfs_common_cols, function(df) {
        df[[col_name]] <- as.numeric(df[[col_name]])
        df
      })
    } else if (first_col_type == "factor") {
      list_of_dfs_common_cols <- map(list_of_dfs_common_cols, function(df) {
        df[[col_name]] <- as.character(df[[col_name]])
        df
      })
    }
  }
  
  yearly_data <- bind_rows(list_of_dfs_common_cols)
  
  yearly_data
}

dem_data <- load_and_combine_data("data/dem")
economic_data <- load_and_combine_data("data/economic")
mean_income_data <- load_and_combine_data("data/mean_income")
social_char_data <- load_and_combine_data("data/social_char")

combined_data <- dem_data %>%
  left_join(economic_data, by = c("geography", "geographic_area_name", "state", "year")) %>%
  left_join(mean_income_data, by = c("geography", "geographic_area_name", "state", "year")) %>%
  left_join(social_char_data, by = c("geography", "geographic_area_name", "state", "year")) %>%
  relocate(state, year) %>%
  select(-geography, -geographic_area_name, -estimate_mean_income_dollars_household_income_all_households_with_cash_public_assistance_income_or_food_stamps_snap)  %>%
  mutate(across(-c(state, year), as.numeric))

cleaned_data <- combined_data %>%
  group_by(state, year) %>%
  summarise(across(everything(), \(x) sum(x))) %>%
  ungroup()

cleaned_data %>%
  select(-state, -year) %>%
  gtsummary::tbl_summary()

write.csv(cleaned_data, "data/cleaned_data.csv")

