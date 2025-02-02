library(ggplot2)
library(gifski)

set.seed(123)

# True coin probability and simulation parameters
p_true <- 0.7
n_trials <- 50

# Simulate coin toss outcomes (1 = Heads, 0 = Tails)
outcomes <- rbinom(n_trials, 1, p_true)

# Start with a uniform prior: Beta(1,1)
alpha <- 1
beta <- 1

# Prepare to save frames
filenames <- character(n_trials)

for (i in 1:n_trials) {
  # Update the Beta parameters based on the outcome
  if (outcomes[i] == 1) {
    alpha <- alpha + 1
  } else {
    beta <- beta + 1
  }
  
  # Create a grid of probability values for the x-axis
  x_vals <- seq(0, 1, length.out = 200)
  # Compute the Beta density with the updated parameters
  density_vals <- dbeta(x_vals, alpha, beta)
  df <- data.frame(probability = x_vals, density = density_vals)
  
  # Create a plot showing the evolving posterior distribution
  p <- ggplot(df, aes(x = probability, y = density)) +
    geom_line(color = "blue", size = 1) +
    labs(
      title = sprintf("Bayesian Coin Toss Update (Trial %d of %d)", i, n_trials),
      subtitle = sprintf("Outcome: %s   |   Posterior: Beta(%.0f, %.0f)",
                         ifelse(outcomes[i] == 1, "Heads", "Tails"), alpha, beta),
      x = "Probability of Heads",
      y = "Density"
    ) +
    theme_minimal() +
    xlim(0, 1)
  
  # Save the current frame as a PNG file
  filename <- sprintf("frame_%02d.png", i)
  ggsave(filename, plot = p, width = 6, height = 4, dpi = 150)
  filenames[i] <- filename
}

# Combine all frames into an animated GIF using gifski
gifski(png_files = filenames, gif_file = "./outputs/bayesian_coin.gif", width = 900, height = 600, delay = 0.5)

# Optionally, clean up the individual frame files
invisible(lapply(filenames, file.remove))
