
# About this Dataset:
# Netflix movie and TV show dataset (mid-2024).  Contains over 8000+ titles.  Netflix had over 282 million subscribers globally at that time.
# This dataset provides details such as cast, directors, ratings, release year, duration, and more.

# Columns and Descriptions:
# - show_id: Unique identifier for each show (e.g., s1, s2).
# - type: "Movie" or "TV Show".
# - title: Name of the Netflix title.
# - director: Director of the title.
# - cast: Main actors.
# - country: Country of origin.
# - date_added: Date added to Netflix.
# - release_year: Original release year.
# - rating: Content rating (e.g., "PG-13", "TV-MA").
# - duration: Duration in minutes (movies) or number of seasons (TV shows).
# - listed_in: Categories/genres (e.g., "Documentaries", "TV Dramas").
# - description: Summary description.

# glimpse(data) output:
# Rows: 8,807
# Columns: 12
# $ show_id      <chr> "s1", "s2", "s3", ...
# $ type         <chr> "Movie", "TV Show", "TV Show", ...
# $ title        <chr> "Dick Johnson Is Dead", "Blood & Water", "Ganglands", ...
# $ director     <chr> "Kirsten Johnson", NA, "Julien Leclercq", ...
# $ cast         <chr> NA, "Ama Qamata, Khosi Ngema, ...", ...
# $ country      <chr> "United States", "South Africa", NA, ...
# $ date_added   <chr> "September 25, 2021", "September 24, 2021", ...
# $ release_year <dbl> 2020, 2021, 2021, ...
# $ rating       <chr> "PG-13", "TV-MA", "TV-MA", ...
# $ duration     <chr> "90 min", "2 Seasons", "1 Season", ...
# $ listed_in    <chr> "Documentaries", "International TV Shows, TV Dramas, TV Mysteries", ...
# $ description  <chr> "As her father nears the end of his life...", "After crossing paths at a party...", ...


# str(data) output:
# spc_tbl_ [8,807 × 12] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
#  $ show_id     : chr [1:8807] "s1" "s2" "s3" "s4" ...
#  $ type        : chr [1:8807] "Movie" "TV Show" "TV Show" "TV Show" ...
#  $ title       : chr [1:8807] "Dick Johnson Is Dead" "Blood & Water" "Ganglands" "Jailbirds New Orleans" ...
#  $ director    : chr [1:8807] "Kirsten Johnson" NA "Julien Leclercq" NA ...
#  $ cast        : chr [1:8807] NA "Ama Qamata, Khosi Ngema, Gail Mabalane, Thabang Molaba, Dillon Windvogel, Natasha Thahane, Arno Greeff, Xolile"| __truncated__ "Sami Bouajila, Tracy Gotoas, Samuel Jouy, Nabiha Akkari, Sofia Lesaffre, Salim Kechiouche, Noureddine Farihi, G"| __truncated__ NA ...
#  $ country     : chr [1:8807] "United States" "South Africa" NA NA ...
#  $ date_added  : chr [1:8807] "September 25, 2021" "September 24, 2021" "September 24, 2021" "September 24, 2021" ...
#  $ release_year: num [1:8807] 2020 2021 2021 2021 2021 ...
#  $ rating      : chr [1:8807] "PG-13" "TV-MA" "TV-MA" "TV-MA" ...
#  $ duration    : chr [1:8807] "90 min" "2 Seasons" "1 Season" "1 Season" ...
#  $ listed_in   : chr [1:8807] "Documentaries" "International TV Shows, TV Dramas, TV Mysteries" "Crime TV Shows, International TV Shows, TV Action & Adventure" "Docuseries, Reality TV" ...
#  $ description : chr [1:8807] "As her father nears the end of his life, filmmaker Kirsten Johnson stages his death in inventive and comical wa"| __truncated__ "After crossing paths at a party, a Cape Town teen sets out to prove whether a private-school swimming star is h"| __truncated__ "To protect his family from a powerful drug lord, skilled thief Mehdi and his expert team of robbers are pulled "| __truncated__ "Feuds, flirtations and toilet talk go down among the incarcerated women at the Orleans Justice Center in New Or"| __truncated__ ...
#  - attr(*, "spec")=List of 12
#  .. $ show_id     : chr "character"
#  .. $ type        : chr "character"
#  .. $ title       : chr "character"
#  .. $ director    : chr "character"
#  .. $ cast        : chr "character"
#  .. $ country     : chr "character"
#  .. $ date_added  : chr "character"
#  .. $ release_year: chr "double"
#  .. $ rating      : chr "character"
#  .. $ duration    : chr "character"
#  .. $ listed_in   : chr "character"
#  .. $ description : chr "character"
#  - attr(*, "problems")=<externalptr>

# Load necessary libraries
library(tidyverse)

# Read the dataset (update the path if needed)
data <- read_csv("./data/netflix_titles.csv")

# Check basic structure and first few rows
glimpse(data)
head(data)

# Check missing values
colSums(is.na(data))

# Fill missing values in categorical columns with "Unknown"
data_cleaned <- data %>%
  mutate(
    director = ifelse(is.na(director), "Unknown", director),
    cast = ifelse(is.na(cast), "Unknown", cast),
    country = ifelse(is.na(country), "Unknown", country)
  )

# Confirm changes
colSums(is.na(data_cleaned))

# Load ggplot2 for visualization
library(ggplot2)

# Plot the distribution of Movies vs. TV Shows
ggplot(data_cleaned, aes(x = type, fill = type)) +
  geom_bar() +
  labs(title = "Distribution of Movies vs. TV Shows on Netflix", x = "Type", y = "Count") +
  theme_minimal()

# Ensure date_added is properly formatted
data_cleaned <- data_cleaned %>%
  mutate(date_added = as.Date(date_added, format = "%B %d, %Y")) %>%
  mutate(year_added = format(date_added, "%Y"))

# Plot the number of titles added per year
ggplot(data_cleaned, aes(x = year_added)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Number of Titles Added to Netflix Per Year", x = "Year Added", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot distribution of content ratings
ggplot(data_cleaned, aes(x = fct_infreq(rating), fill = rating)) +
  geom_bar() +
  labs(title = "Distribution of Content Ratings on Netflix", x = "Rating", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = "none")  # Remove legend for better clarity


# Convert duration to numeric values
data_cleaned <- data_cleaned %>%
  mutate(duration_num = case_when(
    str_detect(duration, "min") ~ as.numeric(str_extract(duration, "\\d+")),
    str_detect(duration, "Season") ~ as.numeric(str_extract(duration, "\\d+")),
    TRUE ~ NA_real_
  ))


# Check unique duration values
unique(data_cleaned$duration)

# Recreate duration_num properly
data_cleaned <- data_cleaned %>%
  mutate(duration_num = case_when(
    str_detect(duration, "min") ~ as.numeric(str_extract(duration, "\\d+")),
    str_detect(duration, "Season") ~ as.numeric(str_extract(duration, "\\d+")),
    TRUE ~ NA_real_
  ))

# Check if duration_num was created
glimpse(data_cleaned)

ggplot(data_cleaned %>% filter(type == "Movie"), aes(x = duration_num)) +
  geom_histogram(fill = "steelblue", bins = 30) +
  labs(title = "Distribution of Movie Durations", x = "Duration (minutes)", y = "Count") +
  theme_minimal()

# Plot distribution of TV show seasons
ggplot(data_cleaned %>% filter(type == "TV Show"), aes(x = duration_num)) +
  geom_bar(fill = "darkred") +
  labs(title = "Distribution of TV Show Seasons", x = "Number of Seasons", y = "Count") +
  theme_minimal()

# Load necessary library
library(tidyr)

# Split the 'listed_in' column into separate genres
genre_counts <- data_cleaned %>%
  separate_rows(listed_in, sep = ", ") %>%
  count(listed_in, sort = TRUE)

# Plot the most common genres
ggplot(genre_counts[1:15,], aes(x = reorder(listed_in, n), y = n, fill = listed_in)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 15 Most Common Genres on Netflix", x = "Genre", y = "Count") +
  theme_minimal() +
  guides(fill = "none")  # Remove legend for clarity

########

# Split data into training and testing sets
set.seed(123)
train_index <- sample(1:nrow(data_cleaned), 0.7 * nrow(data_cleaned))
train_data <- data_cleaned[train_index, ]
test_data <- data_cleaned[-train_index, ]

# Prepare the data for modeling (numeric columns only)
# Convert 'type' to binary: "Movie" = 1, "TV Show" = 0
train_data <- train_data %>%
  mutate(type = ifelse(type == "Movie", 1, 0))

test_data <- test_data %>%
  mutate(type = ifelse(type == "Movie", 1, 0))

# Fit the logistic regression model again
model <- glm(type ~ release_year + duration_num + rating, data = train_data, family = "binomial")

# Summary of the model
summary(model)

# Predict on test data
predictions <- predict(model, newdata = test_data, type = "response")
predicted_class <- ifelse(predictions > 0.5, 1, 0)

# Evaluate model performance
table(predicted_class, test_data$type)

# Evaluate model performance with a confusion matrix
conf_matrix <- table(predicted_class, test_data$type)

# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)

# Calculate precision and recall for Movies (1)
precision_movie <- conf_matrix[2,2] / sum(conf_matrix[,2])
recall_movie <- conf_matrix[2,2] / sum(conf_matrix[2,])

# Calculate precision and recall for TV Shows (0)
precision_tvshow <- conf_matrix[1,1] / sum(conf_matrix[,1])
recall_tvshow <- conf_matrix[1,1] / sum(conf_matrix[1,])

# Display results
list(
  confusion_matrix = conf_matrix,
  accuracy = accuracy,
  precision_movie = precision_movie,
  recall_movie = recall_movie,
  precision_tvshow = precision_tvshow,
  recall_tvshow = recall_tvshow
)

#################

# Install and load gganimate if you haven't already
# install.packages("gganimate")
library(gganimate)

# Add predicted probabilities and predicted class to the test data
test_data <- test_data %>%
  mutate(prediction_prob = predict(model, newdata = test_data, type = "response"),
         predicted_class = ifelse(prediction_prob > 0.5, "Movie", "TV Show"))

# Random sample of test data for animation
sample_data <- test_data[sample(nrow(test_data), 30),]

# Create the animation showing prediction probability over time
animated_plot <- ggplot(sample_data, aes(x = release_year, y = prediction_prob, color = predicted_class)) +
  geom_point(size = 4) +
  geom_line(aes(group = show_id), alpha = 0.7) +
  labs(title = "Prediction Probability of Movie vs TV Show", x = "Release Year", y = "Prediction Probability") +
  theme_minimal() +
  transition_time(release_year) +
  ease_aes('linear')

# Animate
animate(animated_plot, nframes = 100, fps = 10, width = 800, height = 600)

############

# Install necessary package
# install.packages("class")

library(class)

# Preprocess data
data_cleaned$release_year <- as.numeric(data_cleaned$release_year)
data_cleaned$duration_num <- as.numeric(data_cleaned$duration_num)

# Convert categorical features to numerical values
data_cleaned$rating <- as.factor(data_cleaned$rating)
data_cleaned$rating_num <- as.numeric(data_cleaned$rating)

# Sample some data for simplicity
sample_data <- data_cleaned[sample(1:nrow(data_cleaned), 1000),]

# We will use features: release_year, duration_num, rating_num
features <- sample_data[, c("release_year", "duration_num", "rating_num")]

# Normalize the features for k-NN
features_scaled <- scale(features)

# Train the k-NN model (choose k=5 for simplicity)
k <- 5
knn_model <- knn(train = features_scaled, test = features_scaled, cl = sample_data$type, k = k)

# Add k-NN predictions to the data
sample_data$knn_prediction <- knn_model

# Let's preview the data
head(sample_data)

##########