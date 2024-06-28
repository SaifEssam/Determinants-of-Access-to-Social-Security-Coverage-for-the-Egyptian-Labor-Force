#### 1. Reading Data:
setwd("D:/Fourth Year (Second Term)/Graduation Project/Our Work/Current Work")

data <- read.csv("new_data8.csv", header=TRUE)

#Categorical Variables:
data$SocialSecurity <- as.factor(data$SocialSecurity)
data$Sex <- as.factor(data$Sex)
data$MaritalStatus <- as.factor(data$MaritalStatus)
data$Father.s.Present.at.Home <- as.factor(data$Father.s.Present.at.Home)
data$EducationalLevel<- as.factor(data$EducationalLevel)
data$DisabilityStatus <- as.factor(data$DisabilityStatus)
data$HealthInsurance <- as.factor(data$HealthInsurance)
data$ChronicDisease <- as.factor(data$ChronicDisease)
data$EmploymentStatus <- as.factor(data$EmploymentStatus)
data$Occupation <- as.factor(data$Occupation)
data$MainActivityStatus <- as.factor(data$MainActivityStatus)
data$Area <- as.factor(data$Area)
data$Industry <- as.factor(data$Industry)
data$Sector.of.Employment <- as.factor(data$Sector.of.Employment)

#Continuous Variables:
data$Age <- as.numeric(data$Age)
str(data)

#### 2. Libraries:
library(DescTools)
library(dplyr)
library(descr)
library(MESS) 
library(epitools)
library(vcd)
library(PerformanceAnalytics)
library(caret)
library(pROC)
library(plotROC)
library(ROCR)
library(pscl)
library(lmtest)
library(ResourceSelection)
library(ggplot2)
library(e1071)
library(caTools)
library(class)
library(rpart)
library(rpart.plot)

#################################################################################
library(dplyr)
library(caret)
library(car)
library(ggplot2)
library(pscl)
library(pROC)
##1. Fitting the Model:

# Split the data into training and testing sets
set.seed(42)
trainIndex <- createDataPartition(data$SocialSecurity, p = 0.7, list = FALSE)
trainData <- data[trainIndex,]
testData <- data[-trainIndex,]

# Fit the logistic regression model
model <- glm(SocialSecurity ~ Age + Sex + DisabilityStatus + ChronicDisease +
               Father.s.Present.at.Home + HealthInsurance + Area + MaritalStatus+
               EducationalLevel+ EmploymentStatus + Sector.of.Employment +
               Occupation + Industry, data = trainData, family = binomial)

# Summary of the model
summary(model)
###############################################################################
######
## Backward Elimination:

selection <- step(model ,direction = "backward")

########################

model2 <- glm(SocialSecurity ~ Age + DisabilityStatus + Father.s.Present.at.Home + 
                HealthInsurance + Area + MaritalStatus + EducationalLevel + 
                EmploymentStatus + Sector.of.Employment + Occupation + Industry , data = trainData, family = binomial) 
summary(model2)
# Calculate VIF for each predictor variable

vif(model2)
exp(cbind(OR = coef(model2), confint(model2)))

#########################################################################

# Perform Box-Tidwell test
# Box-Tidwell test for Age
boxTidwell(SocialSecurity ~ Age , data = trainData)

## Comparing full model with the reduced model.

#H0: new B's = 0  
library(lmtest)
lrtest(model,model2)
##################################

## Hosmer and Limshow test:
# Load necessary libraries
library(ResourceSelection)

# Perform the Hosmer-Lemeshow test
hoslem_test <- hoslem.test(model2$y, fitted(model2), g = 5)

# Display the results
print(hoslem_test)
##########################################################################

# Predict on the test data
testData$predicted <- predict(model2, newdata = testData, type = "response")
########################################################################

## The Optimal Cutoff Point:

Optimal_Cutoff_Point <- function(predicted_values, actual){
  # predicted_values: Fitted Values for the Logistic Regression.
  # actual: True Values for the Dependent Variable.
  cutoff_points <- c()
  MisspecificationError <- c()
  Counter <- 1
  for(i in seq(0,1, by = 0.01)){
    
    Pred <- ifelse(predicted_values > i, "Yes", "No")
    Table <- xtabs(~Pred + actual)
    
    Model_MisspecificationError <-
      sum(Table[!row(Table) == col(Table)]/sum(Table))
    MisspecificationError [Counter] <- Model_MisspecificationError
    cutoff_points[Counter] <- i
    Counter <- Counter + 1
  }
  
  results <- rbind(cutoff_points,MisspecificationError)
  Index <- which.min(results[2,])
  
  cat("Optimal Cutoff Point is:",results[1,Index])
  cat("  And its Model Misspecification Error is:",results[2,Index])
}

## Testing:
Optimal_Cutoff_Point(testData$predicted,as.vector(testData$SocialSecurity))

#########################################################################

# Convert predicted probabilities to binary outcomes
testData$predicted_class <- ifelse(testData$predicted > 0.47, "Yes", "No")

# Confusion matrix
confusionMatrix(factor(testData$predicted_class, levels = c("No", "Yes")), testData$SocialSecurity)

# ROC curve and AUC
roc_curve <- roc(testData$SocialSecurity, testData$predicted)
plot(roc_curve)
auc(roc_curve)


# Load the required libraries
library(pROC)
library(ggplot2)
library(ggthemes)

# Assuming you have your actual values and predicted probabilities in the following vectors
# actual_values <- c(...)  # Replace with your actual values
# predicted_probabilities <- c(...)  # Replace with your predicted probabilities

# Create the ROC curve object
roc_area <- roc(testData$SocialSecurity, testData$predicted)
roc_area


testData$default_num <- as.numeric(testData$SocialSecurity)-1

ggplot(testData,aes(m = testData$predicted, d = default_num))+ 
  geom_roc(cutoffs.at = c(seq(0,1, by = 0.1)),color = "blue")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1))+labs(
    title = "ROC Curve ",x = "1 - Specificity" ,
    y = "Sensitivity") + theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "#333333"),
    text = element_text(color = "#333333"),
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12))

##########################################################################
####################################################################################################
#Cut-off
hist(testData$predicted)
pred<-prediction(testData$predicted,testData$SocialSecurity)
roc<-performance(pred,"acc")
plot(roc)
abline(h=0.8834586,v=0.4154449)

