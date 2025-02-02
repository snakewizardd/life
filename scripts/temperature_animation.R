# Load necessary libraries
library(rgl)
library(gganimate)
library(gifski)
library(dplyr)
library(ggplot2)

# Function to generate simulated temperature data
generate_temperature_data <- function(time) {
  # Create a grid of points (x, y)
  x <- seq(-5, 5, length.out = 50)
  y <- seq(-5, 5, length.out = 50)
  z <- outer(x, y, function(x, y) {
    # Use a sine wave to simulate temperature fluctuations over time
    10 * sin(sqrt(x^2 + y^2) - time) + 20
  })
  data.frame(x = rep(x, each = length(y)), y = rep(y, length(x)), z = as.vector(z), time = time)
}

# Create data for animation over time
animation_data <- bind_rows(lapply(1:100, function(t) generate_temperature_data(t)))

# Function to create and save the animation
animate_surface <- function() {
  # Set up a plot for animation
  p <- ggplot(animation_data) + 
    aes(x, y, z = z) +
    geom_tile(aes(fill = z)) +
    scale_fill_gradient2(low = "blue", high = "red", mid = "yellow", midpoint = 20) +
    theme_minimal() +
    labs(title = "Temperature Simulation", x = "X", y = "Y", fill = "Temperature") +
    transition_time(time) +
    labs(subtitle = "Time: {frame_time}")
  
  # Save the GIF animation
  anim_save("./outputs/temperature_animation.gif", animation = p)
}

# Run the animation
animate_surface()

# Optional: Create a 3D plot to see the surface at a snapshot
x <- seq(-5, 5, length.out = 50)
y <- seq(-5, 5, length.out = 50)
z <- outer(x, y, function(x, y) {10 * sin(sqrt(x^2 + y^2)) + 20})
surface3d(x, y, z, col = rainbow(100), alpha = 0.7)
