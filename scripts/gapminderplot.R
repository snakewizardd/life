library(ggplot2)
library(gganimate)
library(gapminder)

# Assuming 'data' is your tibble (gapminder-like data)
p <- ggplot(data, aes(x = gdpPercap, y = lifeExp, size = pop, color = continent)) +
  geom_point(alpha = 0.7) +
  scale_size(range = c(2, 12)) +
  labs(title = 'Year: {frame_time}', x = 'GDP per Capita', y = 'Life Expectancy') +
  theme_minimal() +
  transition_time(year) +
  ease_aes('linear')

# Render animation
animate(p, fps = 10, width = 800, height = 600)