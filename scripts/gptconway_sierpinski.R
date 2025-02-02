# Load necessary libraries
library(dplyr)
library(ggplot2)
library(gganimate)
library(gifski)

# Set up the grid dimensions and number of iterations
nrow <- 50
ncol <- 50
iterations <- 100

# Function to generate a Sierpinski triangle fractal pattern (with enough density to evolve)
generate_sierpinski <- function(nrow, ncol) {
  grid <- matrix(0, nrow, ncol)
  # Fill the grid in a fractal-like way (more density for evolution)
  for (i in 1:nrow) {
    for (j in 1:ncol) {
      # A basic rule for a more "dense" Sierpinski triangle pattern
      if ((i %% 2 == 0 | j %% 2 == 0) & (i %% 3 == 0 | j %% 3 == 0)) {
        grid[i, j] <- 1
      }
    }
  }
  return(grid)
}

# Generate initial fractal pattern
grid <- generate_sierpinski(nrow, ncol)

# Function to calculate the next generation
next_gen <- function(grid) {
  nrows <- nrow(grid)
  ncols <- ncol(grid)
  new_grid <- grid
  
  for (i in 1:nrows) {
    for (j in 1:ncols) {
      # Count live neighbors with wrap-around
      live_neighbors <- 0
      for (di in -1:1) {
        for (dj in -1:1) {
          if (!(di == 0 && dj == 0)) {
            ni <- ((i - 1 + di) %% nrows) + 1
            nj <- ((j - 1 + dj) %% ncols) + 1
            live_neighbors <- live_neighbors + grid[ni, nj]
          }
        }
      }
      
      # Apply Conway's rules
      if (grid[i, j] == 1) {
        if (live_neighbors < 2 || live_neighbors > 3) {
          new_grid[i, j] <- 0
        }
      } else {
        if (live_neighbors == 3) {
          new_grid[i, j] <- 1
        }
      }
    }
  }
  
  return(new_grid)
}

# Collect grid data over iterations for plotting
frames <- list()

for (iter in 1:iterations) {
  # Create a data frame from the current grid
  df <- expand.grid(x = 1:ncol, y = 1:nrow)
  df$state <- as.vector(grid)
  df <- df %>% mutate(iteration = iter)
  frames[[iter]] <- df
  
  # Move to the next generation
  grid <- next_gen(grid)
}

# Combine all the iterations into one data frame
df_all <- do.call(rbind, frames)

# Plot the grid using ggplot
p <- ggplot(df_all, aes(x = x, y = y, fill = factor(state))) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("0" = "white", "1" = "black"), guide = "none") +
  coord_equal() +
  theme_void() +
  transition_manual(iteration) +
  labs(title = 'Iteration: {current_frame}')

# Animate and save the GIF
anim <- animate(p, nframes = iterations, fps = 10, width = 500, height = 500, renderer = gifski_renderer())
anim_save("./outputs/conways_game_of_life_fractal_sierpinski_v2.gif", animation = anim)
