library(plotly)
library(gapminder)

# Assuming 'data' is your tibble (gapminder-like data)
data <- gapminder

# Create a 3D animated scatter plot
p <- plot_ly(data, x = ~gdpPercap, y = ~lifeExp, z = ~pop, color = ~continent, 
             colors = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd"),
             text = ~paste("Country: ", country, "<br>Year: ", year),
             frame = ~year, type = 'scatter3d', mode = 'markers',
             marker = list(size = ~sqrt(pop)/1000, sizemode = 'diameter', opacity = 0.8)) %>%
  layout(scene = list(
    xaxis = list(title = 'GDP per Capita', type = 'log'),
    yaxis = list(title = 'Life Expectancy'),
    zaxis = list(title = 'Population (scaled)'),
    camera = list(eye = list(x = 1.5, y = 1.5, z = 0.1)) # Adjust camera angle
  ),
  title = "Global Development Over Time: GDP, Life Expectancy, and Population",
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