library(tidyverse)
library(umap)
library(viridis)
library(plotly)

# Read the dataset
data <- read_csv("./data/netflix_titles.csv")

# Preprocess genre list for each title
data_cleaned <- data %>%
  mutate(genres = strsplit(listed_in, ", ")) %>%
  unnest(genres)

# Vectorize the descriptions
analyze_descriptions <- function(texts) {
  it <- itoken(texts, tokenizer = word_tokenizer, progressbar = FALSE)
  vocab <- create_vocabulary(it, ngram = c(1L, 1L)) %>%
    prune_vocabulary(term_count_min = 10, doc_proportion_max = 0.5)
  vectorizer <- vocab_vectorizer(vocab)
  dtm <- create_dtm(it, vectorizer)
  
  tfidf <- TfIdf$new()
  dtm_tfidf <- fit_transform(dtm, tfidf)
  
  svd_result <- irlba(dtm_tfidf, nv = 50, maxit = 500)
  reduced_matrix <- svd_result$u %*% diag(svd_result$d)
  
  umap_result <- umap(reduced_matrix, n_neighbors = 15, n_components = 2, metric = "cosine")
  return(umap_result$layout)  # Return the UMAP layout (coordinates)
}

# Reduce dimensions with UMAP
semantic_map <- analyze_descriptions(data$description[1:5000])

# Create a data frame for plotting
plot_data <- as.data.frame(semantic_map)

# Merge with genres from cleaned data
plot_data$title <- data$title[1:5000]
plot_data$genres <- data_cleaned$genres[1:5000]

# Create the interactive plot
plot_ly(plot_data, x = ~V1, y = ~V2, text = ~paste("Title:", title, "<br>Genres:", genres),
        hoverinfo = "text", color = ~genres, colors = viridis(100)) %>%
  add_markers(size = 5, opacity = 0.6) %>%
  layout(title = "Interactive Netflix Genre Landscape",
         xaxis = list(title = "UMAP Dimension 1"),
         yaxis = list(title = "UMAP Dimension 2"),
         showlegend = FALSE)
