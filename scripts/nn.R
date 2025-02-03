# Profusely Annotated R Code for Neural Network Classification

# Check if the 'neuralnet' package is installed; if not, install it
# This package provides tools to train neural networks with flexible architectures
if (!require("neuralnet")) {
  install.packages("neuralnet")  # Installs the package from CRAN
}
library(neuralnet)  # Load the package to use its functions

# Generate synthetic sample data for a binary classification problem
set.seed(123)  # Ensures reproducibility of random numbers
n <- 100  # Total number of data points
x1 <- runif(n, 0, 10)  # Generate 100 random values for x1 between 0 and 10
x2 <- runif(n, 0, 10)  # Generate 100 random values for x2 between 0 and 10
# Create binary labels: 1 if x1 + x2 > 10, otherwise 0 (simple linear decision boundary)
y <- ifelse(x1 + x2 > 10, 1, 0)
data <- data.frame(x1, x2, y)  # Combine into a dataframe for model training

# Split data into training (80%) and testing (20%) sets
set.seed(123)  # Reset seed to ensure the same split is generated each time
train_indices <- sample(1:n, 0.8 * n)  # Randomly select 80% of row indices
train_data <- data[train_indices, ]  # Training subset
test_data <- data[-train_indices, ]  # Testing subset (remaining 20%)

# Train a neural network model using the training data
# Formula: y ~ x1 + x2 specifies the target and features
# hidden = c(5, 3): Two hidden layers with 5 and 3 neurons respectively
# linear.output = FALSE: Applies logistic activation to output (for classification)
nn <- neuralnet(
  y ~ x1 + x2, 
  data = train_data, 
  hidden = c(5, 3), 
  linear.output = FALSE  # Output layer uses sigmoid activation for probabilities
)

# Generate predictions on the test set
# compute() feeds test data into the network and returns raw outputs
predictions <- compute(nn, test_data[, c("x1", "x2")])  # Extract test features
# Convert predicted probabilities (0-1) to binary classes using 0.5 threshold
predicted_classes <- ifelse(predictions$net.result > 0.5, 1, 0)

# Calculate model accuracy by comparing predictions to true labels
accuracy <- mean(predicted_classes == test_data$y)  # Proportion correct
cat("Accuracy:", accuracy, "\n")  # Print result (e.g., "Accuracy: 0.95")

# Visualize the neural network architecture
# plot() displays the network structure, nodes, and connection weights
plot(nn)  # Shows input layer, two hidden layers, and output layer
