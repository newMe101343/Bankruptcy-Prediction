# Load required libraries
library(ROSE)
library(randomForest)
library(caret)

# Load the dataset
data <- read.csv("test_data.csv")

# Convert the target variable to a factor
data$Bankrupt. <- as.factor(data$Bankrupt.)

# Check the class distribution before oversampling
print("Class distribution before oversampling:")
print(table(data$Bankrupt.))

# Get the number of instances in the minority class (bankrupt companies)
min_class_count <- min(table(data$Bankrupt.))

# Apply random oversampling to achieve a 1:1 ratio
set.seed(42)  # For reproducibility
oversampled_data <- ovun.sample(Bankrupt. ~ ., data = data, method = "over", N = 60 * min_class_count)$data

# Check the class distribution after oversampling
print("Class distribution after oversampling (1:1 ratio):")
print(table(oversampled_data$Bankrupt.))

# View the first few rows of the oversampled dataset
#print("First few rows of the oversampled dataset:")
#print(head(oversampled_data))

# Ensure the target variable is a factor in the oversampled data
oversampled_data$Bankrupt. <- as.factor(oversampled_data$Bankrupt.)

# Build the Random Forest model
random_forest_model <- randomForest(Bankrupt. ~ ., data = oversampled_data, ntree = 100, mtry = 2, importance = TRUE)

# Plot the importance of variables
varImpPlot(random_forest_model)

# Predict using the Random Forest model
predicted_values <- predict(random_forest_model, oversampled_data)

# Ensure that data and reference are factors with the same levels
data_factor <- as.factor(predicted_values)
reference_factor <- as.factor(oversampled_data$Bankrupt.)

# Check and match levels
levels(data_factor) <- levels(reference_factor)

# Evaluate the model performance
confusion_matrix <- confusionMatrix(data_factor, reference_factor)
print("Confusion Matrix:")
print(confusion_matrix)

# View the first few predictions
print("First few predictions:")
print(head(predicted_values))
