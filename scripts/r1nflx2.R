
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


library(text2vec)
library(Matrix)
library(irlba)  # Memory-efficient SVD for sparse matrices
library(uwot)

# ---- Robust Sparse Matrix Pipeline ----
analyze_descriptions <- function(texts) {
  # 1. Vocabulary-based vectorization with pruning
  it <- itoken(texts, 
               tokenizer = function(x) {
                 word_tokenizer(x) %>% 
                   lapply(function(x) x[nchar(x) > 2])  # Remove short tokens
               },
               progressbar = FALSE)
  
  vocab <- create_vocabulary(it, ngram = c(1L, 1L)) %>%
    prune_vocabulary(term_count_min = 10,
                     doc_proportion_max = 0.5)
  
  vectorizer <- vocab_vectorizer(vocab)
  dtm <- create_dtm(it, vectorizer)
  
  # 2. TF-IDF transformation
  tfidf <- TfIdf$new()
  dtm_tfidf <- fit_transform(dtm, tfidf)
  
  # 3. Memory-optimized SVD using irlba
  svd_result <- irlba(dtm_tfidf, nv = 50, maxit = 500)
  
  # 4. Dense projection for UMAP
  reduced_matrix <- svd_result$u %*% diag(svd_result$d)
  
  # 5. UMAP with sparse-friendly config
  umap(reduced_matrix, 
       n_neighbors = 15,
       n_components = 2,
       metric = "cosine")
}

# ---- Execute with Memory Checks ----
system.time({
  semantic_map <- analyze_descriptions(data$description[1:5000])  # Test subset
})

# ---- Visualize Results ----
ggplot(as.data.frame(semantic_map), aes(V1, V2)) +
  geom_point(color = "#E50914", alpha = 0.3, size = 1) +
  ggtitle("Sparse Matrix-Compatible Semantic Map") +
  theme_minimal()