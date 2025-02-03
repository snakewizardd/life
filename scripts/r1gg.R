library(tidyverse)
library(ggraph)
library(tidygraph)

# Safe genre network analysis
genre_relations <- data %>%
  mutate(listed_in = str_split(listed_in, ",\\s*")) %>%
  unnest(listed_in) %>%
  filter(!is.na(listed_in)) %>%
  mutate(listed_in = str_trim(listed_in)) %>%
  # Filtering in stages for safety
  group_by(listed_in) %>%
  filter(n() >= 30) %>%        # Keep only common genres
  ungroup() %>%
  group_by(title) %>%
  # Critical safety check: must have ≥2 unique genres
  filter(n_distinct(listed_in) >= 2) %>%
  summarise(
    genres = list(unique(listed_in)),  # Remove duplicates first
    combinations = map(genres, ~ {
      if(length(.x) >= 2) combn(.x, 2, simplify = FALSE) else NULL
    })
  ) %>%
  unnest(combinations, keep_empty = FALSE) %>%  # Automatically skips NULLs
  mutate(
    from = map_chr(combinations, 1),
    to = map_chr(combinations, 2)
  ) %>%
  count(from, to) %>%
  filter(n > 10)

# Create graph with community detection
genre_graph <- genre_relations %>%
  as_tbl_graph(directed = FALSE) %>%
  activate(nodes) %>%
  mutate(
    centrality = centrality_degree(),
    community = group_louvain()
  )

# Visualization with error-resistant code
set.seed(2024)
genre_viz <- ggraph(genre_graph, layout = "fr") + 
  geom_edge_link(aes(width = n), color = "#E50914", alpha = 0.3) +
  geom_node_point(aes(size = centrality, color = as.factor(community)), 
                  show.legend = FALSE) +
  geom_node_label(
    aes(label = name, filter = centrality > median(centrality)),
    size = 3,
    repel = TRUE,
    label.padding = unit(0.1, "lines")
  ) +
  scale_edge_width_continuous(range = c(0.5, 2)) +
  theme_graph() +
  labs(title = "Netflix Genre Relationships",
       subtitle = "Node size shows connection frequency, colors represent communities")

print(genre_viz)

# Memory management
rm(genre_relations)
gc()