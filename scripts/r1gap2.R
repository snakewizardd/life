library(plotly)
library(Rtsne)
library(gapminder)
library(dplyr)

# Load data
data <- gapminder

# Prepare data for t-SNE (normalize numeric columns)
scaled_data <- data %>%
  select(lifeExp, gdpPercap, pop) %>%
  scale()

# Perform t-SNE dimensionality reduction
set.seed(42)
tsne_result <- Rtsne(scaled_data, dims = 3, perplexity = 30, verbose = TRUE)

# Add t-SNE coordinates to the original data
data$tsne_x <- tsne_result$Y[, 1]
data$tsne_y <- tsne_result$Y[, 2]
data$tsne_z <- tsne_result$Y[, 3]

# Create 3D animated scatter plot with t-SNE clusters
p <- plot_ly(data, x = ~tsne_x, y = ~tsne_y, z = ~tsne_z, color = ~continent, 
             colors = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd"),
             text = ~paste("Country: ", country, "<br>Year: ", year, "<br>LifeExp: ", lifeExp, "<br>GDP: ", gdpPercap),
             frame = ~year, type = 'scatter3d', mode = 'markers',
             marker = list(size = ~sqrt(pop)/1000, sizemode = 'diameter', opacity = 0.8)) %>%
  layout(scene = list(
    xaxis = list(title = 't-SNE 1'),
    yaxis = list(title = 't-SNE 2'),
    zaxis = list(title = 't-SNE 3'),
    camera = list(eye = list(x = 1.5, y = 1.5, z = 0.1)) # Adjust camera angle
  ),
  title = "t-SNE Clustering of Global Development Over Time",
  updatemenus = list(list(
    type = 'buttons',
    showactive = FALSE,
    buttons = list(
      list(method = "animate", args = list(NULL, list(frame = list(duration = 100, redraw = TRUE), fromcurrent = TRUE)), label = "Play")
    )
  ))
  ) %>%
  animation_opts(frame = 100, easing = "linear", redraw = TRUE)

# Render the plot
p
