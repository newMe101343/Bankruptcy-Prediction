# Load required libraries
library(ROSE)
library(caret)
library(glmnet)  # For regularized logistic regression

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

# Prepare the features (excluding the target variable)
X <- as.matrix(oversampled_data[, -which(names(oversampled_data) == "Bankrupt.")])
y <- oversampled_data$Bankrupt.

# Fit a regularized logistic regression model (ridge) using glmnet
model <- cv.glmnet(X, y, family = "binomial", alpha = 0)  # alpha=0 for ridge regression

# Summary of the model
print(model)

# Get the best lambda (penalty term) from cross-validation
best_lambda <- model$lambda.min
cat("Best lambda value:", best_lambda, "\n")

# Make predictions using the regularized model
predicted_probabilities <- predict(model, X, type = "response", s = "lambda.min")

# Convert probabilities to binary predictions (using 0.5 as the threshold)
predicted_classes <- ifelse(predicted_probabilities > 0.5, 1, 0)

# Ensure that data and reference are factors with the same levels
data_factor <- as.factor(predicted_classes)
reference_factor <- as.factor(oversampled_data$Bankrupt.)

# Check and match levels
levels(data_factor) <- levels(reference_factor)

# Evaluate the model performance
confusion_matrix <- confusionMatrix(data_factor, reference_factor)
print("Confusion Matrix:")
print(confusion_matrix)

# View the first few predictions
print("First few predictions:")
print(head(predicted_classes))
