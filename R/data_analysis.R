set.seed(123)
library(dplyr)
library(janitor)
library(purrr)
library(glue)
library(stringr)
library(readxl)
library(glmnet)
library(car)
student_debt_data <- read.csv("data/combined_clean_data.csv")

train_idx <- sample(seq_len(nrow(student_debt_data)), size = floor(0.8 * nrow(borrower_data)))
train_data <- student_debt_data[train_idx, ]
test_data <- student_debt_data[-train_idx, ]
# Prep train data
train_data <- train_data %>%
  mutate(minority_share = 1 - race_white) %>%
  select(-starts_with("race_")) %>%
  select(-state, -year, -population_18_or_over_female, -employment_total_unemployed)
# Prepare test data
test_data <- test_data %>%
  mutate(minority_share = 1 - race_white) %>%
  select(-starts_with("race_")) %>%
  select(-state, -year, -population_18_or_over_female, -employment_total_unemployed)
colnames(test_data)
colnames(train_data)
#mlr_model <- lm(total_balance_billions ~ . -year, data = train_data)
mlr_model <- lm(total_balance_billions ~ ., data = train_data)

summary(mlr_model)
vif(mlr_model)