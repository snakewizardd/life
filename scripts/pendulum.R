library(ggplot2)
library(gganimate)
library(dplyr)
library(tidyr)
library(viridis)
library(gifski)
library(transformr)
library(deSolve)  # For solving differential equations

# Double pendulum equations
double_pendulum <- function(t, state, params) {
  with(as.list(c(state, params)), {
    d1 <- theta1_dot
    d2 <- theta2_dot
    
    delta <- theta2 - theta1
    den1 <- (2 * m1 + m2) - m2 * cos(2 * delta)
    den2 <- (l2 / l1) * den1
    
    d3 <- (m2 * g * sin(theta2) * cos(delta) - m2 * sin(delta) * 
             (l1 * theta1_dot^2 * cos(delta) + l2 * theta2_dot^2) -
             (m1 + m2) * g * sin(theta1)) / (l1 * den1)
    
    d4 <- ((m1 + m2) * (l1 * theta1_dot^2 * sin(delta) - g * sin(theta2) + g * sin(theta1) * cos(delta)) +
             m2 * l2 * theta2_dot^2 * sin(delta) * cos(delta)) / (l2 * den2)
    
    list(c(d1, d2, d3, d4))
  })
}

# Parameters
params <- c(m1 = 1, m2 = 1, l1 = 1, l2 = 1, g = 9.81)
state <- c(theta1 = pi / 2, theta2 = pi / 2 + 0.1, theta1_dot = 0, theta2_dot = 0)
times <- seq(0, 20, by = 0.01)

# Solve the system
out <- ode(y = state, times = times, func = double_pendulum, parms = params) %>% as.data.frame()

# Convert angles to Cartesian coordinates
out <- out %>%
  mutate(x1 = sin(theta1), y1 = -cos(theta1),
         x2 = x1 + sin(theta2), y2 = y1 - cos(theta2),
         time = row_number())

# Plot with animation
p <- ggplot(out, aes(x = x2, y = y2, color = time)) +
  geom_path(size = 1, alpha = 0.8) +
  scale_color_viridis_c(option = "inferno") +
  theme_minimal(base_family = "mono") +
  labs(title = "Double Pendulum Chaos", subtitle = "A Beautifully Unpredictable Dance", color = "Time Step") +
  transition_reveal(time)

# Save as GIF
anim <- animate(p, renderer = gifski_renderer(), width = 800, height = 600, fps = 60, duration = 10)
anim_save("double_pendulum.gif", animation = anim)
