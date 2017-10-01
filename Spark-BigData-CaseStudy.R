#Intro to R in Spark using sparklyr
#Case Study - Running ML models on Spark: Predicting year a song was released based
#on its timbre (quality of sound)

# timbre has been pre-defined
timbre

# Calculate column means
(mean_timbre <- colMeans(timbre))

# parquet_dir has been pre-defined
parquet_dir

# List the files in the parquet dir
filenames <- dir(parquet_dir, full.names = TRUE)

# Show the filenames and their sizes
data_frame(
  filename = basename(filenames),
  size_bytes = file.size(filenames)
)

# Import the data into Spark
timbre_tbl <- spark_read_parquet(spark_conn, "timbre", parquet_dir)

#reading, writing parquet files quicker than doing same for CSV files

#inner joining, changing type of data (sparklyr takes numeric, not integers)

# track_metadata_tbl, timbre_tbl pre-defined
track_metadata_tbl
timbre_tbl

track_metadata_tbl %>%
  # Inner join to timbre_tbl
  inner_join(timbre_tbl, "track_id") %>%
  # Convert year to numeric
  mutate(year = as.numeric(year))

#Partitioning data with group effect
#before running any models, need to partition data carefully (some issues with naming,,
#so use artist_id because it is more reliable)

# track_data_tbl has been pre-defined
track_data_tbl

training_testing_artist_ids <- track_data_tbl %>%
  # Select the artist ID
  select(artist_id) %>%
  # Get distinct rows
  distinct() %>%
  # Partition into training/testing sets
  sdf_partition(training = 0.7, test = 0.3)

track_data_to_model_tbl <- track_data_tbl %>%
  # Inner join to training partition
  inner_join(training_testing_artist_ids$training, "artist_id")

track_data_to_predict_tbl <- track_data_tbl %>%
  # Inner join to testing partition
  inner_join(training_testing_artist_ids$test, "artist_id")

#Gradient boosted trees - modeling
# track_data_to_model_tbl has been pre-defined
track_data_to_model_tbl

feature_colnames <- track_data_to_model_tbl %>%
  # Get the column names
  colnames() %>%
  # Limit to the timbre columns
  str_subset(fixed("timbre"))

gradient_boosted_trees_model <- track_data_to_model_tbl %>%
  # Run the gradient boosted trees model
  ml_gradient_boosted_trees("year", feature_colnames)

#Starting to predict with the model
# training, testing sets & model are pre-defined
track_data_to_model_tbl
track_data_to_predict_tbl
gradient_boosted_trees_model

responses <- track_data_to_predict_tbl %>%
  select(year) %>%
  collect() %>%
  mutate(
    predicted_year = predict(
      gradient_boosted_trees_model,
      track_data_to_predict_tbl
    )
  )

#Gradient boosted trees - visualizing
#need to calculate residuals because sparklyr still in early stages, doesn't support residuals in ML models
# responses has been pre-defined
responses

# scatterplot of predicted vs. actual
ggplot(responses, aes(actual, predicted)) +
  geom_point(alpha = 0.1) +
  geom_abline(intercept = 0, slope = 1)

residuals <- responses %>%
  transmute(residual = predicted - actual)

#density plot of residuals
ggplot(residuals, aes(residual)) +
  geom_density() +
  geom_vline(xintercept = 0)

#Random Forest: Modeling
# track_data_to_model_tbl has been pre-defined
track_data_to_model_tbl

# Get the timbre columns
feature_colnames <- track_data_to_model_tbl %>%
  colnames() %>%
  str_subset(fixed("timbre"))


# Run the random forest model
random_forest_model <- ml_random_forest(track_data_to_model_tbl, response = "year", features = feature_colnames)

#Random Forest: prediction

## training, testing sets & model are pre-defined
track_data_to_model_tbl
track_data_to_predict_tbl
random_forest_model

# Create a response vs. actual dataset
responses <- track_data_to_predict_tbl %>%
  select(year) %>%
  collect() %>%
  mutate(
    predicted_year = predict(
      random_forest_model,
      track_data_to_predict_tbl
    )
  )

#Visualizing Random Forest
# both_responses has been pre-defined
both_responses

# scatterplot of predicted vs. actual
ggplot(both_responses, aes(actual, predicted, color = model)) +
  geom_smooth() +
  geom_abline(intercept = 0, slope = 1)

# tibble of residuals
residuals <- both_responses%>%
  mutate(residual = predicted - actual)

# density plot of residuals
ggplot(residuals, aes(residual, color = model)) +
  geom_density() +
  geom_vline(xintercept = 0)
# both_responses has been pre-defined
both_responses

# residual sum of squares dataset
both_responses%>%
  mutate(residual = predicted - actual)%>%
  group_by(model)%>%
  summarise(rmse = sqrt(mean(residual^2)))