library(dplyr)
library(janitor)
library(purrr)
library(glue)
library(stringr)
library(yaml)

load_and_combine_data <- function(directory_path) {
  
  file_pattern <- glue(".*-Data\\.csv$")
  
  file_paths <- list.files(path = directory_path, pattern = file_pattern, full.names = TRUE)
  
  list_of_dfs <- map(file_paths, function(path) { 
    
    year_match_result <- regmatches(path, regexpr("Y(\\d{4})\\.*", path))
    
    year_extracted <- sub("Y(\\d{4})\\.*", "\\1", year_match_result)
    
    read.csv(path, skip = 1, na.strings = c("N", "-", "(X)", "null")) %>% 
      clean_names() %>%
      mutate(year = as.numeric(year_extracted)) %>%
      mutate(state = str_trim(str_extract(geographic_area_name, ",\\s*([^,]+)$"), side = "left"),
             state = str_replace(state, "^,\\s*", "")) %>%
      select(-contains(c("margin_of_error", "ratio", "unit", "percent", "x", "geograph")))
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

load_borrower_data <- function(sheet, year){
  borrower_data <- readxl::read_excel("data/Student-loan-update-2025-Mangrum.xlsx", sheet = sheet, skip = 7) %>%
    clean_names() %>%
    mutate(year = year)
  
  borrower_data
}

dem_data <- load_and_combine_data("data/dem") %>%
  rename(race_white = estimate_race_total_population_one_race_white,
         race_black = estimate_race_total_population_one_race_black_or_african_american,
         race_asian = estimate_race_total_population_one_race_asian,
         race_native = estimate_race_total_population_one_race_american_indian_and_alaska_native,
         race_pacific_islander = estimate_race_total_population_one_race_native_hawaiian_and_other_pacific_islander,
         race_other = estimate_race_total_population_one_race_some_other_race,
         race_two_or_more = estimate_race_total_population_two_or_more_races,
         hispanic_or_latino = estimate_hispanic_or_latino_and_race_total_population_hispanic_or_latino_of_any_race,
         population_18_or_over_male = estimate_citizen_voting_age_population_citizen_18_and_over_population_male,
         population_18_or_over_female = estimate_citizen_voting_age_population_citizen_18_and_over_population_female) %>%
  select(-contains("estimate")) %>%
  group_by(state, year) %>%
  summarise(across(everything(), \(x) sum(x, na.rm=TRUE)))

economic_data <- load_and_combine_data("data/economic") %>%
  rename(employement_total_employed = estimate_employment_status_population_16_years_and_over_in_labor_force,
         employment_total_unemployment = estimate_employment_status_population_16_years_and_over_not_in_labor_force) %>%
  select(-contains("estimate"))  %>%
  group_by(state, year) %>%
  summarise(across(everything(), \(x) sum(x, na.rm=TRUE)))

mean_income_data <- load_and_combine_data("data/mean_income") %>%
  group_by(state, year) %>%
  summarise(mean_household_income_dollars = mean(estimate_mean_income_dollars_household_income_all_households))

social_char_data <- load_and_combine_data("data/social_char") %>%
  rename(average_family_size = estimate_households_by_type_total_households_average_family_size,
         total_veteran = estimate_veteran_status_civilian_population_18_years_and_over,
         total_population_bachelors_degree = estimate_educational_attainment_population_25_years_and_over_bachelor_s_degree_or_higher,
         total_foreign_born_pop = estimate_u_s_citizenship_status_foreign_born_population,
         total_household_w_internet_and_computer = estimate_computers_and_internet_use_total_households) %>%
  select(-contains("estimate")) %>%
  group_by(state, year) %>%
  summarise(across(-average_family_size, \(x) sum(x, na.rm=TRUE)), average_family_size = mean(average_family_size))

borrower_data_2019 <- load_borrower_data(11, 2019)
borrower_data_2020 <- load_borrower_data(12, 2020)
borrower_data_2021 <- load_borrower_data(13, 2021)
borrower_data_2022 <- load_borrower_data(14, 2022)
borrower_data_2023 <- load_borrower_data(15, 2023)

borrower_data <- bind_rows(borrower_data_2019, borrower_data_2020, borrower_data_2021, borrower_data_2022, borrower_data_2023) %>%
  select(state, year, total_borrowers, total_balance_billions)

combined_data <- dem_data %>%
  left_join(economic_data, by = c("state", "year")) %>%
  # left_join(mean_income_data, by = c("state", "year")) %>%
  left_join(social_char_data, by = c("geography", "geographic_area_name", "state", "year")) %>%
  select(-contains("geograph")) %>%
  relocate(state, year) %>%
  mutate(across(-c(state, year), as.numeric))

cleaned_data <- combined_data %>%
  group_by(state, year) %>%
  summarise(across(everything(), \(x) sum(x, na.rm=TRUE))) %>%
  ungroup() %>%
  left_join(mean_income_data, by = c("state", "year")) %>%
  left_join(borrower_data, by = c("state", "year"))


cleaned_data %>%
  select(-state, -year) %>%
  gtsummary::tbl_summary()

write.csv(cleaned_data, "data/cleaned_data.csv")


