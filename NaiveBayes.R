library(ROSE)
library(caret)      # For confusion matrix and Naive Bayes
library(e1071)


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


naive_bayes_model <- naiveBayes(Bankrupt. ~ ., data = train_data)
nb_predictions <- predict(naive_bayes_model, test_data)
conf_matrix_nb <- confusionMatrix(nb_predictions, test_data$Bankrupt.)
print("Naive Bayes Model - Confusion Matrix:")
print(conf_matrix_nb)