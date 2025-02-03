library(ggplot2)
library(gganimate)
library(dplyr)
library(tidyr)
library(viridis)
library(gifski)
library(transformr)
library(pracma)  # For Fourier transforms

# Define the Fourier Epicycles function
fourier_series <- function(t, n_terms = 10) {
  x <- rep(0, length(t))
  y <- rep(0, length(t))
  
  for (k in seq(1, n_terms, by = 2)) {
    r <- 4 / (k * pi)  # Radius of the k-th epicycle
    x <- x + r * cos(k * t)
    y <- y + r * sin(k * t)
  }
  
  return(data.frame(t, x, y))
}

# Generate points for animation
t_vals <- seq(0, 2 * pi, length.out = 200)
df <- fourier_series(t_vals, n_terms = 10) %>%
  mutate(time = row_number())

# Create animated plot
p <- ggplot(df, aes(x = x, y = y, group = 1)) +
  geom_path(color = "gold", size = 1.2, alpha = 0.8) +
  geom_point(aes(size = 2), color = "white") +
  theme_void() +
  labs(title = "Fourier Epicycles", subtitle = "A dance of rotating circles tracing an elegant path") +
  transition_reveal(time) +
  ease_aes('cubic-in-out')

# Save as GIF
anim <- animate(p, renderer = gifski_renderer(), width = 800, height = 800, fps = 30, duration = 10)
anim_save("fourier_epicycles.gif", animation = anim)
