library(tidyverse)
library(lubridate)
library(igraph)
library(ggraph)
library(gganimate)

# Read and clean data
netflix_data <- read_csv("./data/netflix_titles.csv") %>%
  mutate(
    date_added = mdy(date_added),
    year_added = year(date_added)
  ) %>%
  filter(!is.na(year_added), year_added >= 2010) %>%
  separate_rows(listed_in, sep = ", ") %>%
  rename(genre = listed_in) %>%
  filter(!is.na(genre), genre != "") %>%
  distinct()

# Create cumulative edge list with years
cumulative_edges <- netflix_data %>%
  arrange(year_added) %>%
  group_by(title) %>%
  summarise(
    genres = list(unique(genre)),
    year_added = first(year_added),
    .groups = "drop"
  ) %>%
  filter(map_int(genres, length) >= 2) %>%
  mutate(pairs = map(genres, ~ combn(.x, 2, simplify = FALSE))) %>%
  unnest(pairs) %>%
  mutate(
    from = map_chr(pairs, 1),
    to = map_chr(pairs, 2)
  ) %>%
  group_by(year_added, from, to) %>%
  summarise(weight = n(), .groups = "drop")

# Create stable layout using all data
full_graph <- tbl_graph(
  nodes = tibble(name = unique(c(cumulative_edges$from, cumulative_edges$to))),
  edges = cumulative_edges,
  directed = FALSE
) %>%
  activate(nodes) %>%
  mutate(centrality = centrality_degree())

layout <- create_layout(full_graph, layout = "fr")

# Prepare animated data
animated_data <- cumulative_edges %>%
  group_by(year_added) %>%
  nest() %>%
  mutate(
    graph = map(data, ~ tbl_graph(
      nodes = tibble(name = unique(c(.x$from, .x$to))),
      edges = .x,
      directed = FALSE
    )),
    plot = map2(graph, year_added, ~ {
      ggraph(layout) + 
        geom_edge_arc(aes(width = weight, edge_colour = ..index..), 
                      alpha = 0.5, strength = 0.1) +
        geom_node_point(aes(size = centrality), colour = "#E50914") +
        geom_node_text(aes(label = name), repel = TRUE, size = 3) +
        scale_edge_width(range = c(0.2, 2)) +
        scale_edge_color_gradient(low = "#221F1F", high = "#B81D24") +
        theme_void() +
        labs(title = "Netflix Genre Network Evolution", 
             subtitle = "Year: {closest_state}") +
        transition_states(year_added, transition_length = 2, state_length = 1) +
        enter_fade() +
        exit_shrink()
    })
  )

# Render animation
animate(
  animated_data$plot[[1]],  # Use first plot with animation components
  nframes = nrow(animated_data) * 10,  # Smooth transitions
  fps = 10,
  width = 1200,
  height = 800,
  renderer = gifski_renderer("final_network_evolution.gif")
)