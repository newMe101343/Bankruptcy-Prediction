# Load required libraries
library(ROSE)
library(e1071)        # For SVM

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

# Ensure the target variable is a factor in the oversampled data
oversampled_data$Bankrupt. <- as.factor(oversampled_data$Bankrupt.)

# Split data into training and testing sets
set.seed(42)
trainIndex <- createDataPartition(oversampled_data$Bankrupt., p = 0.8, list = FALSE)
train_data <- oversampled_data[trainIndex, ]
test_data <- oversampled_data[-trainIndex, ]

# 1. Support Vector Machine (SVM) Model
svm_model <- svm(Bankrupt. ~ ., data = train_data, kernel = "linear", cost = 1)
svm_predictions <- predict(svm_model, test_data)
conf_matrix_svm <- confusionMatrix(svm_predictions, test_data$Bankrupt.)
print("SVM Model - Confusion Matrix:")
print(conf_matrix_svm)


