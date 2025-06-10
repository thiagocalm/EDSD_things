###########################################
##     Titanic ML Classification         ##
##     Author: andrea.nigri@unifg.it  ##
###########################################

set.seed(20)  # For reproducibility

#######################################
#  Load Required Libraries
#######################################
library(tidyverse)
library(caret)
library(plotROC)
library(rpart)
library(rattle)
library(tracerer) # For mode calculation

#######################################
#  Load and Inspect Data
#######################################
titanic <- read_csv("seminars/machine_learning/Code/My_first_ML_project/titanic_clean.csv")
colSums(is.na(titanic))  # Check missing values

#######################################
#  Handle Missing Data (Impute with Mode)
#######################################
# Function to impute missing values using mode
fill_mode <- function(column) {
  mode_value <- calc_mode(column)
  column[is.na(column)] <- mode_value
  return(column)
}

# Apply to necessary columns
titanic$embarked <- fill_mode(titanic$embarked)
titanic$pclass <- fill_mode(titanic$pclass)
titanic$survived <- fill_mode(titanic$survived)
titanic$sex <- fill_mode(titanic$sex)
titanic$sibsp <- fill_mode(titanic$sibsp)
titanic$fare <- fill_mode(titanic$fare)
titanic$parch <- fill_mode(titanic$parch)

colSums(is.na(titanic))  # Re-check for NA

#######################################
#  Data Cleaning and Selection
#######################################
titanic <- titanic %>%
  select(survived, pclass, sex, age, sibsp, parch, fare, embarked)

str(titanic)  # Structure of cleaned data
table(titanic$survived)  # Distribution of target variable

#######################################
# Optional: Visual Explorations (Commented)
#######################################
# titanic %>% ggplot(aes(x = age, fill = factor(survived))) +
#   geom_histogram(binwidth = 5) +
#   facet_wrap(~pclass) +
#   labs(title = "Age Distribution by Pclass")

#######################################
#  Train-Test Split (80/20)
#######################################
split_index <- createDataPartition(titanic$survived, p = 0.8, list = FALSE)
train <- titanic[split_index, ]
test <- titanic[-split_index, ]

#######################################
#  Logistic Regression Model
#######################################
glm_model <- glm(survived ~ ., data = train, family = binomial)
summary(glm_model)

# Predict on test set
logit_probs <- predict(glm_model, test, type = "response")
logit_preds <- ifelse(logit_probs > 0.5, 1, 0)

# ROC Curve
roc_data <- data.frame(logit = logit_preds, test = test$survived)

ggplot(roc_data, aes(d = logit, m = test)) +
  geom_roc() +
  labs(title = "ROC Curve: Logistic Regression",
       x = "1 - Specificity", y = "Sensitivity") +
  annotate("text", x = .6, y = .55,
           label = paste("AUC =", round(calc_auc(ggplot(roc_data, aes(d = logit, m = test)) + geom_roc())$AUC, 2)))

#######################################
#  Decision Tree with rpart
#######################################
tree_model <- rpart(survived ~ ., data = train)
fancyRpartPlot(tree_model)


# Predict and evaluate

tree_probs <- predict(tree_model, test)
tree_preds <- ifelse(tree_probs > 0.5, 1, 0)

tree_roc <- data.frame(tree = tree_preds, test = test$survived)

ggplot(tree_roc, aes(d = tree, m = test)) +
  geom_roc() +
  labs(title = "ROC Curve: Decision Tree",
       x = "1 - Specificity", y = "Sensitivity") +
  annotate("text", x = .6, y = .55,
           label = paste("AUC =", round(calc_auc(ggplot(tree_roc, aes(d = tree, m = test)) + geom_roc())$AUC, 2)))

#######################################
#  Modeling with caret (NNET, rpart, RF)
#######################################
# Convert target to factor
train$survived <- as.factor(train$survived)
test$survived <- as.factor(test$survived)

# Multilayer Perceptron (Neural Net)
nnet_model <- train(survived ~ ., data = train, method = "nnet", trace = FALSE)
nnet_preds <- predict(nnet_model, test)
confusionMatrix(nnet_preds, test$survived)

# Decision Tree with caret
tree_caret_model <- train(survived ~ ., data = train, method = "rpart")
tree_caret_preds <- predict(tree_caret_model, test)
confusionMatrix(tree_caret_preds, test$survived)
fancyRpartPlot(tree_caret_model$finalModel)
# different from rpart because of cross-validation in caret,

# Random Forest
rf_model <- train(survived ~ ., data = train, method = "rf", metric = "Accuracy")
rf_preds <- predict(rf_model, test)
confusionMatrix(rf_preds, test$survived)

