# --- K-means Clustering of States ---

cat("\n--- K-means Clustering of States ---\n")

student_debt_data <- read.csv("data/combined_clean_data.csv")

# Prepare state-level data for clustering
# We'll use the mean of all numeric features (excluding response and year) for each state
# to define state characteristics for clustering.
state_features_for_clustering <- student_debt_data %>%
  group_by(state) %>%
  summarise(
    across(
      .cols = -c(year),
      .fns = mean,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

# Handle any remaining NAs in the state_features_for_clustering (e.g., states with all NAs for a feature)
# For simplicity, we'll omit rows with NAs for clustering. In a real scenario, consider imputation.
state_features_for_clustering_clean <- na.omit(state_features_for_clustering)

# Store state names for mapping back
state_names_for_clustering <- state_features_for_clustering_clean$state

# Select only numeric columns for scaling and clustering
numeric_state_features <- state_features_for_clustering_clean %>%
  select(where(is.numeric))

# Scale the numeric features (important for K-means)
scaled_state_features <- scale(numeric_state_features)


k_clusters <- 5 # Example: Chosen number of clusters

# Perform K-means clustering
set.seed(100) # for reproducibility of clustering
kmeans_result <- kmeans(scaled_state_features, centers = k_clusters, nstart = 25)

# Create a dataframe of state and their cluster assignments
state_cluster_map <- data.frame(
  state = state_names_for_clustering,
  state_cluster = as.factor(kmeans_result$cluster) # Convert cluster numbers to factor
)

cat(paste0("K-means clustering performed with K = ", k_clusters, " clusters.\n"))
cat("\nFirst few rows of State-to-Cluster mapping:\n")
table(state_cluster_map)

combined_data_with_clusters <- student_debt_data %>%
  left_join(state_cluster_map, by = "state")

set.seed(100) # for reproducibility
train_idx <- sample(seq_len(nrow(combined_data_with_clusters)), size = floor(0.8 * nrow(combined_data_with_clusters)))
train_data_cluster <- combined_data_with_clusters[train_idx, ]
test_data_cluster <- combined_data_with_clusters[-train_idx, ]

train_data_cluster %>%
  select(-state) %>%
  tbl_summary()

train_data <- train_data_cluster %>%
  select(-state, -year, -race_other, -population_18_or_over_female, -employment_total_unemployed)

mlr_model_cluster <- lm(total_balance_billions ~ ., data = train_data)
summary(mlr_model_cluster)

mlr_predict_cluster <- predict(mlr_model_cluster, test_data, type='response')

View(as.data.frame(cbind(state = test_data_cluster$state, actual_borrowed = test_data_cluster$total_balance_billions, predicted_borrowed = round(mlr_predict_cluster,2))))

mlr_model_minimum <- lm(total_balance_billions ~ 1, data=train_data)
stepwise <- step(mlr_model_cluster, scope = list(lower = mlr_model_minimum, upper = mlr_model_cluster), direction = "forward", trace=T)
summary(stepwise)



# Plot 1: Residuals vs. Fitted values (to check for homoscedasticity and linearity)
plot(mlr_model_cluster, which = 1, main = "Residuals vs. Fitted Values (with Clusters)")

# Plot 2: Normal Q-Q plot (to check for normality of residuals)
plot(mlr_model_cluster, which = 2, main = "Normal Q-Q Plot (with Clusters)")

# Plot 3: Scale-Location plot (another check for homoscedasticity)
plot(mlr_model_cluster, which = 3, main = "Scale-Location Plot (with Clusters)")

# Plot 4: Residuals vs. Leverage (to identify influential points)
plot(mlr_model_cluster, which = 5, main = "Residuals vs. Leverage (with Clusters)")

all_var <- model.matrix(total_balance_billions ~ . -1, data = train_data) 

elastic_net_model_cv <- glmnet::cv.glmnet(all_var, train_data$total_balance_billions, family='gaussian', alpha=0.5, nfolds=10)
elastic_net_model <- glmnet::glmnet(all_var, train_data$total_balance_billions, family='gaussian',alpha=1, nlambda=100)
round(elastic_net_model_cv$lambda.min,4)
summary(elastic_net_model)
as.data.frame(as.matrix(coef(elastic_net_model, s=elastic_net_model_cv$lambda.min))) %>% 
  filter(s0 != 0) %>%
  rownames()

plot(elastic_net_model, xvar="lambda", lwd=2)
abline(v=log(elastic_net_model_cv$lambda.min), col='red', lty=2, lwd=2)
