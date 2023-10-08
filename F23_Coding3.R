library('ggplot2')

set.seed(939)# Walter's UIN 0939


ridgeless <- function(train_data, test_data) {
  
  # Separate response variable Y from predictors X
  X_train <- train_data[, -1]
  Y_train <- train_data[, 1]
  X_test <- test_data[, -1]
  Y_test <- test_data[, 1]
  
  # Center each column of the design matrix from the training data without scaling
  X_train_scaled <- scale(X_train, center = TRUE, scale = FALSE)
  X_test_scaled <- scale(X_test, center = TRUE, scale = FALSE)
  
  # Perform PCA on the centered training data
  pca_result <- prcomp(X_train_scaled, center = TRUE, scale = FALSE)
  
  # Calculate the number of components to keep using eigenvalues
  eigenvalues <- pca_result$sdev^2
  eps <- 1e-10  # Threshold for singular values
  num_components <- sum(eigenvalues > eps)
  
  # Use only the top num_components for both train and test data
  pca_train <- pca_result$x[, 1:num_components]
  pca_test <- predict(pca_result, newdata = X_test_scaled)[, 1:num_components]
  
  # Perform PCR 
  lam <- diag(1/diag(t(pca_train) %*% pca_train))
  beta <- lam %*% t(pca_train) %*% Y_train
  
  # Calculate training and test errors
  y_hat_train <- pca_train %*% beta
  test_predictions <- pca_test %*% beta
  
  #Mean Squared Error for test and train
  train_error <- mean((y_hat_train - Y_train)^2)
  test_error <- mean((test_predictions - Y_test)^2)
  
  return(list("Training Error" = train_error, "Test Error" = test_error))
}

#Because reading files in R sucks
f <- file.choose()

# Load your training and test datasets here (train_data and test_data)
myData = read.csv('Coding3_dataH.csv')

#Number of iterations for Simulation Study
T = 30

#d ranges from 6 to 241
num_features = 236

#Test Error Matrix
test_errors <- matrix(0, nrow = T, ncol = num_features)

# Calculate the number of rows for the test set (75%)
test_size <- round(0.75 * nrow(myData))


## Perform Simulation Study ##
for (i in 1:T) {
  
  ## Randomly Partition the data into training (25%) and test (75%) ##
  
  # Randomly select row indices for the test set
  test_indices <- sample(1:nrow(myData), test_size)
  
  # Create the test and train sets based on the selected row indices
  test_data <- myData[test_indices, ]
  train_data <- myData[-test_indices, ]
  
  ## Calculate and log the test error from the ridgeless method using the first d columns of myData
  ## d ranges from 6 to 241, since the first column represents Y ##
  
  for (d in 6:241) {
    
    # Create the train and test data with the first d columns
    train_data_sub <- train_data[, 1:d]
    test_data_sub <- test_data[, 1:d]
    
    # Call the ridgeless function
    errors <- ridgeless(train_data_sub, test_data_sub)
    
    # Log the test error for this iteration of i and d
    test_errors[i, d - 5] <- errors$`Test Error`
  }
}

## Plot the median of the test errors (collated over the 30 iterations) in log scale against the count 
## of the regression parameters, which spans from 5 to 240


# Calculate the median of test errors over the 30 iterations for each value of d
median_test_errors <- apply(log10(test_errors), 2, median)

# Create a data frame for the plot
plot_data <- data.frame(Count_of_Parameters = 6:241, Median_Test_Error = median_test_errors)

# Create the plot of points
ggplot(plot_data, aes(x = Count_of_Parameters, y = Median_Test_Error)) +
  geom_point() +  # Use geom_point() to plot individual points
  scale_x_continuous(breaks = seq(0, 240, by = 20)) +  # Adjust the x-axis breaks as needed
  scale_y_log10() +  # Use a log scale for the y-axis
  labs(x = "Count of Regression Parameters", y = "Median Test Error (log scale)") +
  ggtitle("Median Test Error vs. Count of Regression Parameters") +
  theme_minimal()

# Continuous plot
ggplot(plot_data, aes(x = Count_of_Parameters, y = Median_Test_Error)) +
  geom_line() +
  scale_x_continuous(breaks = seq(0, 240, by = 20)) +  # Adjust the x-axis breaks as needed
  scale_y_log10() +  # Use a log scale for the y-axis
  labs(x = "Count of Regression Parameters", y = "Median Test Error (log scale)") +
  ggtitle("Median Test Error vs. Count of Regression Parameters") +
  theme_minimal()
