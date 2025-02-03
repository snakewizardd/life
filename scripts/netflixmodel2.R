library(tidyverse)
library(tidymodels)
library(textrecipes)
library(umap)
library(plotly)
library(tidytext)
library(textdata)
library(keras)

# ------ GLOVE EMBEDDING IMPLEMENTATION ------
# Download GloVe embeddings (first-time will auto-download)
glove6b <- embedding_glove6b(dimensions = 50)  # From textdata package

# ------ DATA PIPELINE ------
netflix_data <- read_csv("./data/netflix_titles.csv") %>%
  filter(type == "Movie") %>%
  mutate(
    is_documentary = ifelse(grepl("Documentaries", listed_in), 1, 0),
    date_added = as.Date(date_added, format = "%B %d, %Y"),
    release_decade = factor(floor(release_year/10)*10)
  ) %>%
  filter(!is.na(description)) %>%
  mutate(is_documentary = factor(is_documentary))

# ------ ADVANCED TEXT PROCESSING ------
text_recipe <- recipe(~ description, data = netflix_data) %>%
  step_text_normalization(description) %>%
  step_tokenize(description) %>%
  step_stopwords(description) %>%
  step_word_embeddings(
    description,
    embeddings = glove6b,
    aggregation = "pca_norm"
  ) %>%
  step_normalize(all_predictors())

# ------ 3D SEMANTIC MAPPING ------
embedded_data <- prep(text_recipe) %>% bake(new_data = netflix_data)

set.seed(42)
umap3d <- umap(embedded_data, n_components = 3, 
               n_neighbors = 100, min_dist = 0.01)
umap_df <- as.data.frame(umap3d$layout) %>% 
  rename_with(~ c("Semantic_X", "Semantic_Y", "Semantic_Z"))

semantic_map <- bind_cols(netflix_data, umap_df) %>%
  mutate(highlight = case_when(
    is_documentary == 1 ~ "Documentary",
    release_year >= 2020 ~ "Recent Film",
    TRUE ~ "Other"
  ))

# ------ INTERACTIVE 3D VISUALIZATION ------
plot_ly(semantic_map, 
        x = ~Semantic_X, y = ~Semantic_Y, z = ~Semantic_Z,
        color = ~highlight,
        colors = c("#E50914", "#00B8D4", "#2E7D32"),
        text = ~paste(
          "</br><b>", title, "</b>",
          "</br>Year: ", release_year,
          "</br>Rating: ", rating,
          "</br>Duration: ", duration
        ),
        hoverinfo = "text",
        marker = list(size = 5, opacity = 0.8)
) %>%
  add_markers() %>%
  layout(
    scene = list(
      xaxis = list(title = "Narrative Structure"),
      yaxis = list(title = "Genre Similarity"),
      zaxis = list(title = "Temporal Context")
    ),
    title = "3D Semantic Space of Netflix Movies",
    legend = list(orientation = "h", y = -0.1)
  )

# ------ TRANSFORMER MODEL ------
# Custom Keras model architecture
build_model <- function(input_shape) {
  keras_model_sequential() %>%
    layer_dense(units = 128, activation = "relu", input_shape = input_shape) %>%
    layer_dropout(0.4) %>%
    layer_batch_normalization() %>%
    layer_dense(units = 64, activation = "selu") %>%
    layer_dropout(0.3) %>%
    layer_dense(units = 1, activation = "sigmoid") %>%
    compile(
      loss = "binary_crossentropy",
      optimizer = optimizer_adam(learning_rate = 0.001),
      metrics = c("accuracy")
    )
}

# Model training
set.seed(42)
split <- initial_split(netflix_data, strata = is_documentary)
train_data <- training(split)
test_data <- testing(split)

dl_recipe <- recipe(is_documentary ~ description, data = train_data) %>%
  step_text_normalization(description) %>%
  step_tokenize(description) %>%
  step_word_embeddings(description, embeddings = glove6b) %>%
  step_normalize(all_predictors())

preprocessed_train <- prep(dl_recipe) %>% bake(new_data = NULL)
preprocessed_test <- prep(dl_recipe) %>% bake(new_data = test_data)

model <- build_model(ncol(preprocessed_train) - 1)

history <- model %>% fit(
  x = as.matrix(preprocessed_train %>% select(-is_documentary)),
  y = as.numeric(as.character(train_data$is_documentary)),
  epochs = 30,
  batch_size = 128,
  validation_split = 0.2,
  callbacks = list(
    callback_early_stopping(patience = 5),
    callback_reduce_lr_on_plateau(factor = 0.1)
  )
)

# ------ MODEL EVALUATION ------
predictions <- predict(model, as.matrix(preprocessed_test %>% select(-is_documentary))) %>% 
  as.numeric()

results <- test_data %>%
  mutate(
    predicted_prob = predictions,
    predicted_class = ifelse(predicted_prob > 0.5, 1, 0)
  )

roc_curve <- roc_curve(results, is_documentary, predicted_prob)
auc_value <- roc_auc(results, is_documentary, predicted_prob)

# ------ FEATURE IMPORTANCE ------
embedding_weights <- get_weights(model)[[1]]
word_importance <- glove6b$tokens[rowSums(abs(embedding_weights))] %>%
  enframe(name = "word", value = "importance")

top_words <- word_importance %>%
  arrange(desc(importance)) %>%
  head(30) %>%
  mutate(word = fct_reorder(word, importance))

ggplot(top_words, aes(x = importance, y = word, fill = importance)) +
  geom_col(alpha = 0.8) +
  scale_fill_gradient(low = "#221F1F", high = "#E50914") +
  labs(title = "Top Predictive Words for Documentary Classification",
       x = "Feature Importance", y = "") +
  theme_minimal() +
  theme(legend.position = "none")