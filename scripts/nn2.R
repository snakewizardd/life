# Load required packages
if (!require("neuralnet")) install.packages("neuralnet")
if (!require("dplyr")) install.packages("dplyr")    # For data wrangling
if (!require("caret")) install.packages("caret")    # For preprocessing
library(neuralnet)
library(dplyr)
library(caret)

# Assuming your data is stored in a variable named `data`
# str(data) shows it has country, continent, year, lifeExp, pop, gdpPercap

# ----------------------------------
# STEP 1: Preprocess the data
# ----------------------------------
# Remove high-cardinality categorical variable "country" (142 levels)
# Convert "continent" to dummy variables (one-hot encoding)
# Scale numeric features to [0, 1] for neural network stability

# Drop the 'country' column
data_prepped <- data %>% select(-country)

# Convert 'continent' to dummy variables
dummies <- dummyVars(" ~ continent", data = data_prepped)
data_encoded <- predict(dummies, newdata = data_prepped) %>% 
  as.data.frame() %>% 
  cbind(data_prepped %>% select(-continent))

# Define target (lifeExp) and features
target <- "lifeExp"
features <- setdiff(names(data_encoded), target)

# Scale numeric features (year, pop, gdpPercap) to [0, 1]
preprocessor <- preProcess(data_encoded[, features], method = "range")
data_scaled <- predict(preprocessor, data_encoded) 

# ----------------------------------
# STEP 2: Split data into train/test
# ----------------------------------
set.seed(123)
train_indices <- sample(1:nrow(data_scaled), 0.8 * nrow(data_scaled))
train_data <- data_scaled[train_indices, ]
test_data <- data_scaled[-train_indices, ]

# ----------------------------------
# STEP 3: Train the neural network
# ----------------------------------
# Formula for the neural network (predict lifeExp using all other variables)
formula <- as.formula(paste(target, "~", paste(features, collapse = " + ")))

# Train with 1 hidden layer (5 neurons) for demonstration
# Adjust hidden layers/neurons based on your needs
set.seed(123)
nn <- neuralnet(
  formula,
  data = train_data,
  hidden = c(5),       # Single hidden layer with 5 neurons
  linear.output = TRUE, # TRUE for regression (lifeExp is numeric)
  act.fct = "logistic" # Activation function for hidden layer
)

# ----------------------------------
# STEP 4: Predict and evaluate
# ----------------------------------
# Generate predictions on test data
predictions <- compute(nn, test_data[, features])

# Calculate RMSE (common metric for regression)
rmse <- sqrt(mean((test_data$lifeExp - predictions$net.result)^2))
cat("Test RMSE:", rmse, "\n") # Lower values = better performance

# ----------------------------------
# STEP 5: Plot the network
# ----------------------------------
plot(nn) # Visualize architecture (may be complex for large networks)



