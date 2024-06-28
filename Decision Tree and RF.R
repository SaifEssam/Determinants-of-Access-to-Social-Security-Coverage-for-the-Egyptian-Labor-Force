
### Decision Tree:

library(rpart)
library(rpart.plot)


# Define the formula
formula <- SocialSecurity ~ Age + Sex + DisabilityStatus + ChronicDisease +
  Father.s.Present.at.Home + HealthInsurance + Area + MaritalStatus+
  EducationalLevel+ EmploymentStatus + Sector.of.Employment +
  Occupation + Industry



# Build the decision tree
treeModel <- rpart(formula, data = trainData, method = "class", 
                   parms = list(split = "gini"),
                   control = rpart.control(minsplit = 20, cp = 0.01))

# Plot the decision tree
rpart.plot(treeModel, type = 3, extra = 104, fallen.leaves = TRUE, 
           main = "Decision Tree for Social Security Coverage", box.palette = "Blues")


# Predict on test data
predictions <- predict(treeModel, newdata = testData, type = "class")

# Create confusion matrix
confusionMatrix <- table(testData$SocialSecurity, predictions)

# Print confusion matrix and accuracy
print(confusionMatrix)

accuracy <- sum(diag(confusionMatrix)) / sum(confusionMatrix)
print(paste("Accuracy:", round(accuracy, 4)))


# Print complexity parameter table
printcp(treeModel)

# Prune the tree
prunedTree <- prune(treeModel, cp = treeModel$cptable[which.min(treeModel$cptable[,"xerror"]), "CP"])

# Plot the pruned tree
rpart.plot(prunedTree, type = 3, extra = 104, fallen.leaves = TRUE, 
           main = "Pruned Decision Tree for Social Security Coverage",
           cex = 0.6, tweak = 1.2, box.palette = "Blues")

###########################################################################
#######################################################################

library(caret)
# Define the parameter grid
tuneGrid <- expand.grid(
  cp = seq(0.01, 0.1, by = 0.01),      # Complexity parameter
  minsplit = seq(10, 50, by = 10),     # Minimum number of cases in a node
  maxdepth = seq(5, 30, by = 5),       # Maximum depth of the tree
  minbucket = seq(5, 25, by = 5)       # Minimum number of cases in a leaf
)
# Set up training control
trainControl <- trainControl(method = "cv", number = 10) # 10-fold cross-validation

# Train the decision tree model with cross-validation
set.seed(123) # For reproducibility
treeTune <- train(
  formula, 
  data = trainData, 
  method = "rpart",
  trControl = trainControl,
  tuneGrid = tuneGrid,
  control = rpart.control(minsplit = 20)
)

set.seed(123)
trainIndex <- sample(1:nrow(data), 0.7 * nrow(data))
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

formula <- SocialSecurity ~ Age + Sex + DisabilityStatus + ChronicDisease +
  Father.s.Present.at.Home + HealthInsurance + Area + MaritalStatus+
  EducationalLevel+ EmploymentStatus + Sector.of.Employment +
  Occupation + Industry

# Define the parameter grid for cp
tuneGrid <- expand.grid(cp = seq(0.01, 0.1, by = 0.01))

# Set up training control
trainControl <- trainControl(method = "cv", number = 10)

# Train the decision tree model with cross-validation
set.seed(123)
treeTune <- train(
  formula, 
  data = trainData, 
  method = "rpart",
  trControl = trainControl,
  tuneGrid = tuneGrid,
  control = rpart.control(minsplit = 20, maxdepth = 30, minbucket = 10)
)

# Print the results
print(treeTune)

# Get the best model's parameters
bestParams <- treeTune$bestTune
print(bestParams)


###########################################################
###########################################################

finalTree <- rpart(
  formula, 
  data = trainData, 
  method = "class", 
  control = rpart.control(cp = 0.01, minsplit = 20, maxdepth = 30, minbucket = 10)
)

rpart.plot(finalTree, type = 3, extra = 104, fallen.leaves = TRUE, 
           main = "Final Decision Tree for Social Security Coverage",
           cex = 0.6, tweak = 1.2, box.palette = "Blues")

# Predict on test data
predictions <- predict(finalTree, newdata = testData, type = "class")

# Create confusion matrix
confusionMatrix <- table(testData$SocialSecurity, predictions)

# Print confusion matrix and accuracy
print(confusionMatrix)

accuracy <- sum(diag(confusionMatrix)) / sum(confusionMatrix)
print(paste("Accuracy:", round(accuracy, 4)))


















library(caret)

# Compute confusion matrix
confusionMatrix <- confusionMatrix(predictions, testData$SocialSecurity)
print(confusionMatrix)

# Extract accuracy, precision, recall, F1-score
accuracy <- confusionMatrix$overall['Accuracy']
kappa <- confusionMatrix$overall['Kappa']
precision <- confusionMatrix$byClass['Pos Pred Value']
recall <- confusionMatrix$byClass['Sensitivity']
f1_score <- 2 * (precision * recall) / (precision + recall)

print(paste("Accuracy:", round(accuracy, 4)))
print(paste("Kappa:", round(kappa, 4)))
print(paste("Precision:", round(precision, 4)))
print(paste("Recall:", round(recall, 4)))
print(paste("F1-Score:", round(f1_score, 4)))

# Plot variable importance
importance <- varImp(finalTree, scale = FALSE)
print(importance)
###########################

library(pROC)
library(ggplot2)

# Predict probabilities on the test data
predictions_prob <- predict(finalTree, newdata = testData, type = "prob")

# Compute ROC curve
roc_curve <- roc(testData$SocialSecurity, predictions_prob[, 2])

# Create a data frame for the ROC curve
roc_df <- data.frame(
  TPR = roc_curve$sensitivities,  # True Positive Rate
  FPR = 1 - roc_curve$specificities  # False Positive Rate
)

# Calculate the AUC
auc_value <- auc(roc_curve)

# Plot the ROC curve using ggplot2
ggplot(roc_df, aes(x = FPR, y = TPR)) +
  geom_line(color = "blue", size = 1) +
  geom_abline(linetype = "dashed", color = "gray") +
  theme_minimal() +
  ggtitle(paste("ROC Curve (AUC =", round(auc_value, 2), ")")) +
  labs(x = "False Positive Rate (1 - Specificity)", y = "True Positive Rate (Sensitivity)") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "#333333"),
        text = element_text(color = "#333333"),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.position = "none" 
  )
#################################################
## HEat Map:

# Create the confusion matrix
confusionMatrix <- table(testData$SocialSecurity, predictions)
print(confusionMatrix)

# Convert the confusion matrix to a data frame
confusion_df <- as.data.frame(as.table(confusionMatrix))
colnames(confusion_df) <- c("Reference", "Prediction", "Frequency")

# Plot the heatmap
ggplot(confusion_df, aes(x = Reference, y = Prediction, fill = Frequency)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = Frequency), vjust = 1) +
  theme_minimal() +
  ggtitle("Confusion Matrix Heatmap") +
  labs(x = "Reference", y = "Prediction", fill = "Frequency") +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )




############################################
#############################################


# Create the confusion matrix
confusionMatrix <- table(testData$SocialSecurity, predictions)
print(confusionMatrix)

# Convert the confusion matrix to a data frame
confusion_df <- as.data.frame(as.table(confusionMatrix))
colnames(confusion_df) <- c("Reference", "Prediction", "Frequency")


ggplot(confusion_df, aes(x = Reference, y = Prediction, fill = Frequency)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = Frequency), vjust = 1) +
  theme_minimal() +
  ggtitle("Confusion Matrix Heatmap") +
  labs(x = "Actual", y = "Prediction", fill = "Frequency") +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )
#####################################################################################
#####################################################################################

### Random Forest:

library(randomForest)


# Train the Random Forest model
set.seed(123)
rfModel <- randomForest(
  SocialSecurity ~ Age + Sex + DisabilityStatus + ChronicDisease +
    Father.s.Present.at.Home + HealthInsurance + Area + MaritalStatus+
    EducationalLevel+ EmploymentStatus + Sector.of.Employment +
    Occupation + Industry,
  data = trainData,
  ntree =200 ,        # Number of trees
  mtry = 5, # Number of variables randomly sampled as candidates at each split
  maxnodes = 60,
  importance = TRUE   # Calculate variable importance
)

# Predict on test data
rfPredictions <- predict(rfModel, newdata = testData)

# Create confusion matrix
rfConfusionMatrix <- table(testData$SocialSecurity, rfPredictions)
print(rfConfusionMatrix)

# Calculate accuracy
rfAccuracy <- sum(diag(rfConfusionMatrix)) / sum(rfConfusionMatrix)
print(paste("Accuracy:", round(rfAccuracy, 4)))
################################################

library(pROC)

# Predict probabilities for ROC curve
rfProbabilities <- predict(rfModel, newdata = testData, type = "prob")

# Compute ROC curve
rfRocCurve <- roc(testData$SocialSecurity, rfProbabilities[, 2])

# Plot ROC curve
plot(rfRocCurve, col = "blue", main = paste("ROC Curve (AUC =", round(auc(rfRocCurve), 2), ")"))

# Plot variable importance
varImpPlot(rfModel)

##########################

# Calculate sensitivity and specificity
sensitivity <- rfConfusionMatrix[1, 1] / sum(rfConfusionMatrix[1, ])
specificity <- rfConfusionMatrix[2, 2] / sum(rfConfusionMatrix[2, ])
print(paste("Sensitivity:", round(sensitivity, 4)))
print(paste("Specificity:", round(specificity, 4)))

# Convert the confusion matrix to a data frame for heatmap
confusion_df <- as.data.frame(as.table(rfConfusionMatrix))
colnames(confusion_df) <- c("Reference", "Prediction", "Frequency")

# Plot the heatmap
ggplot(confusion_df, aes(x = Reference, y = Prediction, fill = Frequency)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = Frequency), vjust = 1) +
  theme_minimal() +
  ggtitle("Confusion Matrix Heatmap for Random Forest") +
  labs(x = "Prediction", y = "Actual", fill = "Frequency") +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

# Predict probabilities for ROC curve
rfProbabilities <- predict(rfModel, newdata = testData, type = "prob")

# Compute ROC curve
rfRocCurve <- roc(testData$SocialSecurity, rfProbabilities[, 2])

# Plot ROC curve
plot(rfRocCurve, col = "blue", main = paste("ROC Curve (AUC =", round(auc(rfRocCurve), 2), ")"))

# Plot variable importance

# Extract variable importance
importance_values <- importance(rfModel)
importance_df <- data.frame(
  Variable = rownames(importance_values),
  MeanDecreaseAccuracy = importance_values[, "MeanDecreaseAccuracy"]
)

# Plot variable importance using ggplot2
ggplot(importance_df, aes(x = reorder(Variable, MeanDecreaseAccuracy), y = MeanDecreaseAccuracy, fill = MeanDecreaseAccuracy)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_gradient(low = "lightblue", high = "blue") +
  theme_minimal() +
  ggtitle("Variable Importance Plot for Random Forest (Mean Decrease Accuracy)") +
  labs(x = "Variable", y = "Mean Decrease Accuracy") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "#333333"),
        text = element_text(color = "#333333"),
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

#####################################

## ROC curve:

# Predict probabilities for ROC curve
rfProbabilities <- predict(rfModel, newdata = testData, type = "prob")

# Compute ROC curve
rfRocCurve <- roc(testData$SocialSecurity, rfProbabilities[, 2])

# Create a data frame for the ROC curve
roc_df <- data.frame(
  TPR = rfRocCurve$sensitivities,  # True Positive Rate
  FPR = 1 - rfRocCurve$specificities  # False Positive Rate
)

# Calculate the AUC
auc_value <- auc(rfRocCurve)

# Plot the ROC curve using ggplot2
ggplot(roc_df, aes(x = FPR, y = TPR)) +
  geom_line(color = "blue", size = 1) +
  geom_abline(linetype = "dashed", color = "black") +
  theme_minimal() +
  ggtitle(paste("ROC Curve (AUC =", round(auc_value, 2), ")")) +
  labs(x = "False Positive Rate (1 - Specificity)", y = "True Positive Rate (Sensitivity)") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "#333333"),
        text = element_text(color = "#333333"),
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.position = "none"
  )
######################################



# Train the decision tree model with optimal cp
finalTree <- rpart(
  formula, 
  data = trainData, 
  method = "class", 
  control = rpart.control(cp = 0.01, minsplit = 20, maxdepth = 30, minbucket = 10)
)

# Plot the decision tree
rpart.plot(
  finalTree, 
  type = 3, 
  extra = 104, 
  fallen.leaves = F, 
   main = "Health Insurance",
  shadow.col = "black", 
  nn = TRUE,
)
############################

## VAR Imp.

# Extract variable importance
importance_values <- as.data.frame(varImp(finalTree))
importance_values$Variable <- rownames(importance_values)
importance_values <- importance_values[order(importance_values$Overall, decreasing = TRUE), ]


# Plot variable importance using ggplot2
ggplot(importance_values, aes(x = reorder(Variable, Overall), y = Overall, fill = Overall)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_gradient(low = "lightblue", high = "blue") +
  theme_minimal() +
  ggtitle("Variable Importance Plot for Decision Tree") +
  labs(x = "Variable", y = "Importance") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "#333333"),
        text = element_text(color = "#333333"),
    
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )
##################################################
#########################################################


###############################################################

## random forest:


# Load necessary libraries
library(caret)
library(randomForest)

# Convert the response variable to a factor
data1$SocialSecurity <- as.factor(data1$SocialSecurity)

# Split the data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(data1$SocialSecurity, p = 0.8, list = FALSE)
train_data <- data1[trainIndex, ]
test_data <- data1[-trainIndex, ]

# Train a random forest model
rf_model <- randomForest(SocialSecurity ~ Age + Sex + DisabilityStatus + ChronicDisease +
                           Father.s.Present.at.Home + HealthInsurance + Area + MaritalStatus+
                           EducationalLevel+ EmploymentStatus + Sector.of.Employment +
                           Occupation + Industry, data = trainData, importance = TRUE)

# Evaluate the model
predictions <- predict(rf_model, newdata = testData)
confusionMatrix(predictions, testData$SocialSecurity)

# Plot variable importance
varImpPlot(rf_model)
############################################################################

### COMPARING 3 MODELS:


# Create a data frame for the performance metrics
performance_data <- data.frame(
  Model = c("Logistic Regression", "Decision Tree", "Random Forest"),
  Accuracy = c(0.8709, 0.8709, 0.885),
  Sensitivity = c(0.9262, 0.9275, 0.9324),
  Specificity = c(0.8096, 0.7846, 0.8127),
  AUC = c(0.86, 0.86, 0.94)
)

library(reshape2)

# Melt the data frame
performance_data_melt <- melt(performance_data, id.vars = "Model", variable.name = "Metric", value.name = "Value")

# Plot the performance metrics
ggplot(performance_data_melt, aes(x = Model, y = Value, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Metric, scales = "free_y") +
  scale_fill_manual(values = c("skyblue", "orange", "green")) +
  theme_minimal() +
  ggtitle("Comparison of Model Performance Metrics") +
  labs(y = "Value", x = "Model") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.position = "none"
  )

# Plot the performance metrics
ggplot(performance_data_melt, aes(x = Model, y = Value, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Metric, scales = "free_y") +
  scale_fill_manual(values = c("skyblue", "orange", "green")) +
  theme_minimal() +
  ggtitle("Comparison of Model Performance Metrics") +
  labs(y = "Value", x = "Model") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.position = "none"
  )
###########################################################

library(pROC)
library(ggplot2)




# Split the data into training and testing sets
set.seed(42)
trainIndex <- sample(1:nrow(data), 0.7 * nrow(data))
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# Train logistic regression model
logitModel <- glm(SocialSecurity ~ Age + DisabilityStatus + Father.s.Present.at.Home + 
                    HealthInsurance + Area + MaritalStatus + EducationalLevel + 
                    EmploymentStatus + Sector.of.Employment + Occupation + Industry, data = trainData, family = binomial)
logitProb <- predict(logitModel, newdata = testData, type = "response")
logitRoc <- roc(testData$SocialSecurity, logitProb)

# Train decision tree model
library(rpart)
finalTree <- rpart(SocialSecurity ~ Age + Sex + DisabilityStatus + ChronicDisease +
                     Father.s.Present.at.Home + HealthInsurance + Area + MaritalStatus+
                     EducationalLevel+ EmploymentStatus + Sector.of.Employment +
                     Occupation + Industry, data = trainData, method = "class", control = rpart.control(cp = 0.01))
treeProb <- predict(finalTree, newdata = testData, type = "prob")[,2]
treeRoc <- roc(testData$SocialSecurity, treeProb)

# Train random forest model
library(randomForest)
rfModel <- randomForest(SocialSecurity ~ Age + Sex + DisabilityStatus + ChronicDisease +
                          Father.s.Present.at.Home + HealthInsurance + Area + MaritalStatus+
                          EducationalLevel+ EmploymentStatus + Sector.of.Employment +
                          Occupation + Industry, data = trainData, ntree = 500, mtry = 3, importance = TRUE)
rfProb <- predict(rfModel, newdata = testData, type = "prob")[,2]
rfRoc <- roc(testData$SocialSecurity, rfProb)


# Create data frame for plotting ROC curves
roc_data <- data.frame(
  TPR = c(logitRoc$sensitivities, treeRoc$sensitivities, rfRoc$sensitivities),
  FPR = c(1 - logitRoc$specificities, 1 - treeRoc$specificities, 1 - rfRoc$specificities),
  Model = factor(rep(c("Logistic Regression", "Decision Tree", "Random Forest"), each = length(logitRoc$sensitivities)))
)

# Plot the ROC curves
ggplot(roc_data, aes(x = FPR, y = TPR, color = Model)) +
  geom_line(size = 1) +
  geom_abline(linetype = "dashed", color = "gray") +
  theme_minimal() +
  ggtitle("Comparison of ROC Curves for Different Models") +
  labs(x = "False Positive Rate (1 - Specificity)", y = "True Positive Rate (Sensitivity)") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  ) +
  scale_color_manual(values = c("blue", "orange", "green"))
###############################################################


# Function to interpolate ROC curves
interpolate_roc <- function(roc, n = 100) {
  coords <- coords(roc, "all", ret = c("specificity", "sensitivity"), transpose = FALSE)
  interp <- approx(coords$specificity, coords$sensitivity, n = n)
  data.frame(FPR = 1 - interp$x, TPR = interp$y)
}

# Interpolate ROC curves
n_points <- 100
logitRocInterp <- interpolate_roc(logitRoc, n_points)
logitRocInterp$Model <- "Logistic Regression"
treeRocInterp <- interpolate_roc(treeRoc, n_points)
treeRocInterp$Model <- "Decision Tree"
rfRocInterp <- interpolate_roc(rfRoc, n_points)
rfRocInterp$Model <- "Random Forest"

# Combine data for plotting
roc_data <- rbind(logitRocInterp, treeRocInterp, rfRocInterp)
# Plot the ROC curves
ggplot(roc_data, aes(x = FPR, y = TPR, color = Model)) +
  geom_line(size = 1) +
  geom_abline(linetype = "dashed", color = "gray") +
  theme_minimal() +
  ggtitle("Comparison of ROC Curves for Different Models") +
  labs(x = "False Positive Rate (1 - Specificity)", y = "True Positive Rate (Sensitivity)") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "#333333"),
        text = element_text(color = "#333333"),
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  ) +
  scale_color_manual(values = c("blue", "red", "orange1"))
#####################################################################
###################################################################



## CNN:
library(keras)
library(tensorflow)
library(caTools)
library(dplyr)

################


# Encode categorical variables using one-hot encoding
data <- data %>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.factor, as.numeric)

# Separate features and target variable
X <- data %>% select(-SocialSecurity)
y <- data$SocialSecurity

# Standardize the input features
X <- scale(X)

# Split the data into training, validation, and test sets
set.seed(123)
sample <- sample.split(y, SplitRatio = 0.7)
X_train <- X[sample, ]
X_test <- X[!sample, ]
y_train <- y[sample]
y_test <- y[!sample]

# Further split the training set into training and validation sets
sample_val <- sample.split(y_train, SplitRatio = 0.8)
X_val <- X_train[!sample_val, ]
X_train <- X_train[sample_val, ]
y_val <- y_train[!sample_val]
y_train <- y_train[sample_val]

# Reshape the data to fit the CNN input requirements
X_train <- array_reshape(X_train, c(nrow(X_train), ncol(X_train), 1))
X_val <- array_reshape(X_val, c(nrow(X_val), ncol(X_val), 1))
X_test <- array_reshape(X_test, c(nrow(X_test), ncol(X_test), 1))

# Convert labels to categorical format
y_train <- to_categorical(y_train)
y_val <- to_categorical(y_val)
y_test <- to_categorical(y_test)
############################################################
#############################################################


# Define the CNN model
model <- keras_model_sequential() %>%
  layer_conv_1d(filters = 32, kernel_size = 3, activation = 'relu', input_shape = c(dim(X_train)[2], 1)) %>%
  layer_max_pooling_1d(pool_size = 2) %>%
  layer_conv_1d(filters = 64, kernel_size = 3, activation = 'relu') %>%
  layer_max_pooling_1d(pool_size = 2) %>%
  layer_flatten() %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 2, activation = 'softmax')

# Compile the model
model %>% compile(
  optimizer = optimizer_adam(),
  loss = 'categorical_crossentropy',
  metrics = c('accuracy', metric_auc())
#########################################################

library(randomForest)
library(caret)


# Define the hyperparameter grid
ntree_values <- c(200, 400, 500)
mtry_values <- c(2, 4, 5, 6)
maxnodes_values <- c(20, 40, 60)

# Initialize variables to store the best model and highest accuracy
best_model <- NULL
highest_accuracy <- 0
best_params <- list(ntree = NULL, mtry = NULL, maxnodes = NULL)

# Perform grid search
for (ntree in ntree_values) {
  for (mtry in mtry_values) {
    for (maxnodes in maxnodes_values) {
      set.seed(42)
      rf_model <- randomForest(SocialSecurity ~ Age + Sex + DisabilityStatus + ChronicDisease +
                                 Father.s.Present.at.Home + HealthInsurance + Area + MaritalStatus+
                                 EducationalLevel+ EmploymentStatus + Sector.of.Employment +
                                 Occupation + Industry, data = trainData, 
                               ntree = ntree, mtry = mtry, maxnodes = maxnodes)
      predictions <- predict(rf_model, testData)
      accuracy <- sum(predictions == testData$SocialSecurity) / nrow(testData)
      
      if (accuracy > highest_accuracy) {
        highest_accuracy <- accuracy
        best_model <- rf_model
        best_params <- list(ntree = ntree, mtry = mtry, maxnodes = maxnodes)
      }
    }
  }
}

# Output the best parameters and highest accuracy
print(paste("Best ntree:", best_params$ntree))
print(paste("Best mtry:", best_params$mtry))
print(paste("Best maxnodes:", best_params$maxnodes))
print(paste("Highest accuracy:", highest_accuracy))
#########################################################



# Load necessary libraries
library(randomForest)
library(caret)


# Define the optimal hyperparameters
best_ntree <- 200
best_mtry <- 4
best_maxnodes <- 40

# Train the final model on the entire dataset

final_model <- randomForest(SocialSecurity ~ Age + Sex + DisabilityStatus + ChronicDisease +
                              Father.s.Present.at.Home + HealthInsurance + Area + MaritalStatus+
                              EducationalLevel+ EmploymentStatus + Sector.of.Employment +
                              Occupation + Industry, data = trainData, 
                            ntree = best_ntree, mtry = best_mtry, maxnodes = best_maxnodes)

# Print the final model summary
print(final_model)



# Load necessary libraries
library(randomForest)
library(caret)

# Define the optimal hyperparameters
best_ntree <- 200
best_mtry <- 4
best_maxnodes <- 40

# Train the final model with class weights to handle imbalance
set.seed(42)
final_model <- randomForest(SocialSecurity ~ Age + Sex + DisabilityStatus + ChronicDisease +
                              Father.s.Present.at.Home + HealthInsurance + Area + MaritalStatus +
                              EducationalLevel + EmploymentStatus + Sector.of.Employment +
                              Occupation + Industry,
                            data = data,
                            ntree = best_ntree, mtry = best_mtry, maxnodes = best_maxnodes,
                            classwt = c(1, 1.5))  # Adjust class weights as needed

# Print the final model summary
print(final_model)

