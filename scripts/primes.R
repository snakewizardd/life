library(ggplot2)
library(gganimate)
library(dplyr)
library(tidyr)
library(viridis)
library(gifski)
library(transformr)
library(pracma)  # For prime number checking

# Grid size
grid_size <- 50 

# Generate a grid of numbers
df <- expand.grid(x = 1:grid_size, y = 1:grid_size) %>%
  mutate(n = x + (y - 1) * grid_size,  # Map to a sequential number
         is_prime = isprime(n),  # Check if the number is prime
         time = ifelse(is_prime, n %% 100, NA)) %>%  # Assign "pulse" timing to primes
  drop_na()

# Animated plot
p <- ggplot(df, aes(x = x, y = y, fill = as.factor(time))) +
  geom_tile(show.legend = FALSE, color = "black", size = 0.1) +
  scale_fill_viridis_d(option = "plasma") +
  theme_void() +
  labs(title = "Prime Number Visualization", subtitle = "Numbers pulse into existence as primes are revealed") +
  transition_states(time, transition_length = 2, state_length = 1) +
  ease_aes('cubic-in-out')

# Save as GIF
anim <- animate(p, renderer = gifski_renderer(), width = 800, height = 800, fps = 20, duration = 10)
anim_save("prime_wave.gif", animation = anim)
