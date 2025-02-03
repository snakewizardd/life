
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

# Core Tidyverse
library(ggplot2)
library(tidyverse)  # Includes ggplot2, dplyr, tidyr, readr, purrr, stringr, forcats
library(lubridate)  # Date manipulation (part of tidyverse but sometimes needs explicit loading)

# Specialized Packages
library(tidytext)    # Text mining and reordering factors in plots
library(tm)          # Text Mining
library(wordcloud)   # Word cloud visualization
library(RColorBrewer) # Color palettes for word cloud

# Install missing packages first if needed:
# install.packages(c("tidyverse", "lubridate", "tidytext", "tm", "wordcloud", "RColorBrewer"))



# 1. CONTENT TYPE DISTRIBUTION (Movie vs TV Show)
type_plot <- data %>% 
  count(type) %>% 
  mutate(percentage = n/sum(n)*100) %>%
  ggplot(aes(x = "", y = n, fill = type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(percentage), "%\n(", n, ")")), 
            position = position_stack(vjust = 0.5)) +
  labs(title = "Netflix Content Type Distribution", 
       fill = "Content Type") +
  theme_void() +
  scale_fill_manual(values = c("#E50914", "#221F1F"))  # Netflix colors
print(type_plot)

# 2. CONTENT ADDITION TIMELINE
data_clean <- data %>%
  mutate(date_added = mdy(date_added),
         year_added = year(date_added)) %>%
  filter(!is.na(date_added))

timeline_plot <- data_clean %>%
  count(year_added, type) %>%
  ggplot(aes(x = year_added, y = n, fill = type)) +
  geom_area(position = "stack", alpha = 0.8) +
  scale_fill_manual(values = c("#E50914", "#221F1F")) +
  labs(title = "Netflix Content Acquisition Timeline",
       x = "Year", y = "Titles Added", fill = "Type") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(2008, 2024, 2)) +
  theme(panel.grid.minor = element_blank())
print(timeline_plot)

# 3. CONTENT RATING ANALYSIS
rating_plot <- data %>%
  filter(!is.na(rating)) %>%
  mutate(rating = fct_lump_n(rating, 8)) %>%
  count(type, rating) %>%
  ggplot(aes(x = reorder(rating, n), y = n, fill = type)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("#E50914", "#221F1F")) +
  labs(title = "Content Ratings Distribution",
       x = "Rating Category", y = "Number of Titles") +
  theme_minimal() +
  facet_wrap(~type, scales = "free_y")
print(rating_plot)

# 4. GEOGRAPHICAL CONTENT DISTRIBUTION
country_plot <- data %>%
  separate_rows(country, sep = ",\\s*") %>%
  filter(!is.na(country) & country != "") %>%
  count(country) %>%
  mutate(country = fct_lump_n(country, 10, w = n)) %>%
  filter(country != "Other") %>%
  ggplot(aes(x = reorder(country, n), y = n)) +
  geom_col(fill = "#E50914") +
  coord_flip() +
  labs(title = "Top 10 Content Producing Countries",
       x = "", y = "Number of Titles") +
  theme_minimal()
print(country_plot)

# 5. GENRE ANALYSIS
genre_analysis <- data %>%
  separate_rows(listed_in, sep = ",\\s*") %>%
  count(listed_in, type) %>%
  group_by(type) %>%
  top_n(10, n) %>%
  ungroup() %>%
  ggplot(aes(x = reorder_within(listed_in, n, type), y = n, fill = type)) +
  geom_col() +
  coord_flip() +
  scale_x_reordered() +
  scale_fill_manual(values = c("#E50914", "#221F1F")) +
  labs(title = "Top Genres by Content Type",
       x = "", y = "Count") +
  facet_wrap(~type, scales = "free") +
  theme_minimal()
print(genre_analysis)

# 6. DURATION ANALYSIS
duration_analysis <- data %>%
  mutate(duration_type = ifelse(type == "Movie", "Minutes", "Seasons"),
         duration_num = parse_number(duration)) %>%
  filter(!is.na(duration_num)) %>%
  ggplot(aes(x = duration_num, fill = type)) +
  geom_histogram(bins = 30) +
  scale_fill_manual(values = c("#E50914", "#221F1F")) +
  labs(title = "Content Duration Distribution",
       x = "Duration", y = "Count") +
  facet_wrap(~duration_type, scales = "free") +
  theme_minimal()
print(duration_analysis)

# 7. RELEASE YEAR ANALYSIS
release_plot <- data %>%
  filter(release_year >= 2000) %>%
  ggplot(aes(x = release_year, fill = type)) +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values = c("#E50914", "#221F1F")) +
  labs(title = "Content Release Year Distribution",
       x = "Release Year", y = "Density") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(2000, 2024, 2))
print(release_plot)

# 8. DIRECTOR ANALYSIS (Top 10)
director_plot <- data %>%
  separate_rows(director, sep = ",\\s*") %>%
  filter(!is.na(director) & director != "") %>%
  count(director) %>%
  slice_max(n, n = 10) %>%
  ggplot(aes(x = reorder(director, n), y = n)) +
  geom_col(fill = "#E50914") +
  coord_flip() +
  labs(title = "Most Prolific Directors on Netflix",
       x = "", y = "Number of Titles") +
  theme_minimal()
print(director_plot)

# 9. TIME GAP ANALYSIS (Release Year vs Added Year)
gap_analysis <- data_clean %>%
  mutate(add_year = year(date_added),
         year_gap = add_year - release_year) %>%
  filter(year_gap >= 0) %>%
  ggplot(aes(x = year_gap, fill = type)) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = c("#E50914", "#221F1F")) +
  labs(title = "Time Gap Between Release and Netflix Addition",
       x = "Years Between Release and Addition", y = "Density") +
  theme_minimal() +
  scale_x_continuous(limits = c(0, 30))
print(gap_analysis)

# 10. TEXT ANALYSIS (Word Cloud of Descriptions)
library(tm)
library(wordcloud)

corpus <- Corpus(VectorSource(data$description)) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords("en")) %>%
  tm_map(stripWhitespace)

wordcloud(words = corpus, 
          max.words = 100, 
          colors = brewer.pal(8, "Dark2"),
          scale = c(3, 0.5), 
          random.order = FALSE,
          rot.per = 0.35)
