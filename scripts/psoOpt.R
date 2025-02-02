library(ggplot2)
library(gifski)
library(dplyr)

# Define the Rastrigin function
rastrigin <- function(x, y) {
  20 + x^2 + y^2 - 10 * (cos(2 * pi * x) + cos(2 * pi * y))
}

# Particle Swarm Optimization (PSO) parameters
n_particles <- 30
n_iter <- 40
w <- 0.7   # inertia weight
c1 <- 1.5  # cognitive coefficient
c2 <- 1.5  # social coefficient

# Initialize particle positions and velocities
set.seed(42)
particles <- data.frame(
  id = 1:n_particles,
  x = runif(n_particles, -5, 5),
  y = runif(n_particles, -5, 5)
)
velocities <- data.frame(
  id = 1:n_particles,
  vx = runif(n_particles, -1, 1),
  vy = runif(n_particles, -1, 1)
)

# Initialize personal bests as current positions
particles <- particles %>%
  mutate(pbest_x = x,
         pbest_y = y,
         pbest_val = rastrigin(x, y))

# Determine global best
global_best <- particles %>% filter(pbest_val == min(pbest_val)) %>% slice(1)
gbest_x <- global_best$pbest_x
gbest_y <- global_best$pbest_y

# Pre-compute grid for contour plotting of the Rastrigin function
grid_seq <- seq(-5, 5, length.out = 100)
grid_df <- expand.grid(x = grid_seq, y = grid_seq)
grid_df$z <- rastrigin(grid_df$x, grid_df$y)

# Prepare to save animation frames
filenames <- character(n_iter)

# PSO simulation loop and frame capture
for (iter in 1:n_iter) {
  
  # Update velocities and positions for each particle
  r1 <- runif(n_particles)
  r2 <- runif(n_particles)
  velocities$vx <- w * velocities$vx + c1 * r1 * (particles$pbest_x - particles$x) + c2 * r2 * (gbest_x - particles$x)
  r1 <- runif(n_particles)
  r2 <- runif(n_particles)
  velocities$vy <- w * velocities$vy + c1 * r1 * (particles$pbest_y - particles$y) + c2 * r2 * (gbest_y - particles$y)
  
  particles$x <- particles$x + velocities$vx
  particles$y <- particles$y + velocities$vy
  
  # Evaluate current fitness and update personal bests
  current_val <- rastrigin(particles$x, particles$y)
  improved <- current_val < particles$pbest_val
  particles$pbest_x[improved] <- particles$x[improved]
  particles$pbest_y[improved] <- particles$y[improved]
  particles$pbest_val[improved] <- current_val[improved]
  
  # Update global best
  current_global <- particles %>% filter(pbest_val == min(particles$pbest_val)) %>% slice(1)
  if (current_global$pbest_val < rastrigin(gbest_x, gbest_y)) {
    gbest_x <- current_global$pbest_x
    gbest_y <- current_global$pbest_y
  }
  
  # Create a plot for the current iteration
  p <- ggplot() +
    geom_contour_filled(data = grid_df, aes(x = x, y = y, z = z), alpha = 0.8) +
    scale_fill_viridis_d(option = "C") +
    geom_point(data = particles, aes(x = x, y = y), color = "red", size = 3, alpha = 0.9) +
    geom_point(aes(x = gbest_x, y = gbest_y), color = "gold", size = 5, shape = 8) +
    labs(title = paste("Particle Swarm Optimization (Iteration", iter, "of", n_iter, ")"),
         subtitle = "Red dots: particles | Gold star: global best",
         x = "X", y = "Y") +
    theme_minimal() +
    coord_fixed(xlim = c(-5, 5), ylim = c(-5, 5))
  
  # Save the current frame as a PNG file
  filename <- sprintf("frame_%02d.png", iter)
  ggsave(filename, plot = p, width = 6, height = 5, dpi = 150)
  filenames[iter] <- filename
}

# Combine all frames into an animated GIF
gifski(png_files = filenames, gif_file = "./outputs/pso_optimization.gif", width = 900, height = 750, delay = 0.3)

# Optional: Clean up individual frame files
invisible(lapply(filenames, file.remove))
