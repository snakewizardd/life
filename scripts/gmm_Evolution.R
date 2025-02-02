library(ggplot2)
library(dplyr)
library(gifski)
library(MASS)

set.seed(101)

# Simulate data from a two-component Gaussian mixture
n_total <- 300
n1 <- n2 <- n_total / 2

mu1_true <- c(-2, 0)
Sigma1_true <- matrix(c(1, 0.6, 0.6, 1), ncol = 2)
mu2_true <- c(2, 0)
Sigma2_true <- matrix(c(1, -0.4, -0.4, 1), ncol = 2)

data1 <- mvrnorm(n1, mu = mu1_true, Sigma = Sigma1_true)
data2 <- mvrnorm(n2, mu = mu2_true, Sigma = Sigma2_true)
data_all <- as.data.frame(rbind(data1, data2))
colnames(data_all) <- c("x", "y")

# Custom function for the bivariate normal density
dmvnorm_custom <- function(x, mu, Sigma) {
  k <- length(mu)
  denom <- sqrt((2 * pi)^k * det(Sigma))
  diff <- x - mu
  exp_val <- exp(-0.5 * t(diff) %*% solve(Sigma) %*% diff)
  as.numeric(exp_val / denom)
}

# Function to generate ellipse points for a given Gaussian component.
get_ellipse <- function(mu, Sigma, level = 0.95, npoints = 100) {
  theta <- seq(0, 2 * pi, length.out = npoints)
  circle <- cbind(cos(theta), sin(theta))
  eig <- eigen(Sigma)
  # Scale factor from chi-squared quantile for 2 degrees of freedom
  scale <- sqrt(qchisq(level, df = 2))
  ellipse_coords <- t(mu + scale * t(eig$vectors %*% diag(sqrt(eig$values)) %*% t(circle)))
  data.frame(x = ellipse_coords[,1], y = ellipse_coords[,2])
}

# Initialize EM parameters for 2 components
K <- 2
# Randomly choose initial means from the data
init_idx <- sample(1:n_total, K)
mu_est <- list(as.numeric(data_all[init_idx[1], ]),
               as.numeric(data_all[init_idx[2], ]))
# Initialize covariances to the overall sample covariance
Sigma_est <- list(cov(data_all), cov(data_all))
# Initialize weights equally
pi_est <- rep(1/K, K)

# Number of EM iterations
n_iter <- 30
filenames <- character(n_iter)

# Prepare to record the log-likelihood evolution (optional)
loglik_vec <- numeric(n_iter)

for (iter in 1:n_iter) {
  # E-step: Compute responsibilities for each data point
  responsibilities <- matrix(0, nrow = n_total, ncol = K)
  for (i in 1:n_total) {
    x_i <- as.numeric(data_all[i, ])
    probs <- numeric(K)
    for (k in 1:K) {
      probs[k] <- pi_est[k] * dmvnorm_custom(x_i, mu_est[[k]], Sigma_est[[k]])
    }
    responsibilities[i, ] <- probs / sum(probs)
  }
  
  # Compute log-likelihood
  loglik <- sum(log(apply(responsibilities, 1, sum) + 1e-10))
  loglik_vec[iter] <- loglik
  
  # M-step: Update parameters based on responsibilities
  for (k in 1:K) {
    r_k <- responsibilities[, k]
    N_k <- sum(r_k)
    # Update mean
    mu_est[[k]] <- colSums(data_all * r_k) / N_k
    # Update covariance
    diff <- as.matrix(sweep(data_all, 2, mu_est[[k]], "-"))
    Sigma_est[[k]] <- t(diff) %*% (diff * r_k) / N_k
    # Ensure covariance is positive-definite
    Sigma_est[[k]] <- Sigma_est[[k]] + diag(1e-6, 2)
    # Update weight
    pi_est[k] <- N_k / n_total
  }
  
  # For visualization: assign each data point to the component with max responsibility
  cluster_assign <- apply(responsibilities, 1, which.max)
  data_plot <- data_all %>% mutate(cluster = factor(cluster_assign))
  
  # Generate ellipse contours for each estimated component
  ellipse1 <- get_ellipse(mu_est[[1]], Sigma_est[[1]], level = 0.95)
  ellipse2 <- get_ellipse(mu_est[[2]], Sigma_est[[2]], level = 0.95)
  
  ellipse1$component <- "Component 1"
  ellipse2$component <- "Component 2"
  ellipses <- rbind(ellipse1, ellipse2)
  
  # Create the plot: show data colored by current assignment and overlay the Gaussian ellipses
  p <- ggplot(data_plot, aes(x = x, y = y, color = cluster)) +
    geom_point(size = 2, alpha = 0.8) +
    geom_path(data = ellipses, aes(x = x, y = y, linetype = component), color = "black", size = 1) +
    labs(title = sprintf("EM Algorithm for Gaussian Mixture Model: Iteration %d", iter),
         subtitle = sprintf("Log-likelihood: %.2f", loglik),
         x = "X", y = "Y", color = "Cluster") +
    theme_minimal() +
    coord_fixed(xlim = c(min(data_all$x) - 1, max(data_all$x) + 1),
                ylim = c(min(data_all$y) - 1, max(data_all$y) + 1))
  
  # Save the current frame
  filename <- sprintf("frame_%02d.png", iter)
  ggsave(filename, plot = p, width = 6, height = 5, dpi = 150)
  filenames[iter] <- filename
}

# Combine all frames into an animated GIF
gifski(png_files = filenames, gif_file = "./outputs/em_gmm_evolution.gif", width = 900, height = 750, delay = 0.5)

# Optionally, remove the temporary frame files
invisible(lapply(filenames, file.remove))
