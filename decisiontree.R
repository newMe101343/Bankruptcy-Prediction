# Load required libraries
library(ROSE)
library(rpart)
library(rpart.plot)
library(caret)

# Load the dataset
data <- read.csv("test_data.csv")

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
#print(head(oversampled_data))

# Build the decision tree model
decision_tree_model <- rpart(Bankrupt. ~ ., data = oversampled_data, method = "class")

# Plot the decision tree
rpart.plot(decision_tree_model)

# Predict using the decision tree model
predicted_values <- predict(decision_tree_model, oversampled_data, type = "class")

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

