# load necessary libraries
library(dplyr)
library(ggplot2)
library(gganimate)
library(gifski)

# set up the grid dimensions and number of iterations
nrow <- 50
ncol <- 50
iterations <- 100

# initialize a random grid (0 = dead, 1 = alive)
set.seed(123)  # for reproducibility, bro
grid <- matrix(sample(c(0, 1), nrow * ncol, replace = TRUE, prob = c(0.8, 0.2)),
               nrow = nrow, ncol = ncol)

# function to calculate the next generation
next_gen <- function(grid) {
  nrows <- nrow(grid)
  ncols <- ncol(grid)
  new_grid <- grid
  
  for (i in 1:nrows) {
    for (j in 1:ncols) {
      # count live neighbors with wrap-around
      live_neighbors <- 0
      for (di in -1:1) {
        for (dj in -1:1) {
          if (!(di == 0 && dj == 0)) {
            # use modular arithmetic for wrapping around edges
            ni <- ((i - 1 + di) %% nrows) + 1
            nj <- ((j - 1 + dj) %% ncols) + 1
            live_neighbors <- live_neighbors + grid[ni, nj]
          }
        }
      }
      
      # apply Conway's rules
      if (grid[i, j] == 1) {
        # live cell dies if <2 or >3 neighbors
        if (live_neighbors < 2 || live_neighbors > 3) {
          new_grid[i, j] <- 0
        }
      } else {
        # dead cell becomes live if exactly 3 neighbors
        if (live_neighbors == 3) {
          new_grid[i, j] <- 1
        }
      }
    }
  }
  
  return(new_grid)
}

# collect the grid data over iterations for plotting
frames <- list()

for (iter in 1:iterations) {
  # create a data frame from the current grid
  df <- expand.grid(x = 1:ncol, y = 1:nrow)
  df$state <- as.vector(grid)
  
  # Count live neighbors for each cell to apply categories
  df$live_neighbors <- 0
  for (i in 1:nrow(df)) {
    x <- df$x[i]
    y <- df$y[i]
    
    live_neighbors <- 0
    for (di in -1:1) {
      for (dj in -1:1) {
        if (!(di == 0 && dj == 0)) {
          ni <- ((y - 1 + di) %% nrow) + 1
          nj <- ((x - 1 + dj) %% ncol) + 1
          live_neighbors <- live_neighbors + grid[ni, nj]
        }
      }
    }
    df$live_neighbors[i] <- live_neighbors
  }
  
  # Categorize cells based on live_neighbors
  df$cell_color <- ifelse(df$state == 0, "white", 
                          ifelse(df$live_neighbors <= 1, "lightgrey", 
                                 ifelse(df$live_neighbors == 2, "yellow", "darkgreen")))
  
  df <- df %>% mutate(iteration = iter)
  frames[[iter]] <- df
  
  # move to the next generation
  grid <- next_gen(grid)
}

# combine all the iterations into one data frame
df_all <- do.call(rbind, frames)

# plot using ggplot. Cells with different states are categorized into colors.
p <- ggplot(df_all, aes(x = x, y = y, fill = cell_color)) +
  geom_tile(color = "white") +
  scale_fill_identity(guide = "none") +
  coord_equal() +
  theme_void() +
  transition_manual(iteration) +
  labs(title = 'Iteration: {current_frame}')

# animate and save the GIF
anim <- animate(p, nframes = iterations, fps = 10, width = 500, height = 500, renderer = gifski_renderer())
anim_save("./outputs/conways_game_of_life_colored.gif", animation = anim)
