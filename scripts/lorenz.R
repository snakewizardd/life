library(ggplot2)
library(gganimate)
library(dplyr)
library(tidyr)
library(viridis)
library(gifski)
library(transformr)
library(deSolve)  # For solving differential equations

# Define the Lorenz system
lorenz <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    dx <- sigma * (y - x)
    dy <- x * (rho - z) - y
    dz <- x * y - beta * z
    list(c(dx, dy, dz))
  })
}

# Parameters and initial state
params <- c(sigma = 10, rho = 28, beta = 8/3)
state <- c(x = 1, y = 1, z = 1)
times <- seq(0, 50, by = 0.01)

# Solve the system
out <- ode(y = state, times = times, func = lorenz, parms = params) %>% as.data.frame()

# Add an index for animation
out <- out %>%
  mutate(time = row_number()) %>%
  arrange(time)

# Plot with animation
p <- ggplot(out, aes(x = x, y = z, color = time)) +
  geom_path(size = 1) +
  scale_color_viridis_c(option = "magma") +
  theme_minimal(base_family = "mono") +
  labs(title = "Lorenz Attractor", subtitle = "Chaotic System Visualization", color = "Time Step") +
  transition_reveal(time)

# Render animation
anim <- animate(p, renderer = gifski_renderer(), width = 800, height = 600, fps = 30, duration = 10)

anim_save("lorenz_attractor.gif", animation = anim)