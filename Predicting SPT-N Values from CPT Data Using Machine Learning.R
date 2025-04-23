# HW#5 Solution
rm(list=ls())

library(leaps);
library(MASS);
library(lars);
library(pls)
library(corrplot)
library(PerformanceAnalytics)
library(GGally)
library(summarytools)
library(stargazer)
library(knitr)
library(kableExtra)
library(webshot)
library(ggplot2)
library(reshape2)
library(heplots)
library(stats)
library(e1071) 
library(survey)
library(caret)
library(Metrics)


#CPTraw <- read.csv(file.choose(), header = TRUE)
CPTraw <- read.table(file = "CPT-Data.csv", sep = ",", header=T);
head(CPTraw)

# Define a function to map USCS Group Names to the four categories
map_soil_type <- function(uscs) {
  if (uscs %in% c("Poorly graded gravel with sand", "Poorly graded gravel with clay", 
                  "Poorly graded gravel with silt and sand", "Poorly graded gravel", 
                  "Silty gravel with sand", "Poorly graded gravel with silt")) {
    return("Gravel")
  } else if (uscs %in% c("Silty sand", "Clayey sand", "Poorly graded sand with clay", 
                         "Poorly graded sand with silt", "Poorly graded sand", 
                         "Clayey sand with gravel", "Silty, clayey sand", 
                         "Poorly graded sand with silt and gravel", "Poorly graded sand with gravel", 
                         "Silty, clayey sand with gravel", "Silty sand with gravel")) {
    return("Sand")
  } else if (uscs %in% c("Sandy silt", "Silt", "Silt with sand")) {
    return("Silt")
  } else if (uscs %in% c("Lean clay", "Sandy lean clay", "Fat clay", "Silty clay", 
                         "Lean clay with sand", "Lean clay with gravel", 
                         "Sandy lean clay with gravel", "Silty clay with sand", 
                         "Gravelly lean clay with sand", "Organic lean clay", "Sandy silty clay")) {
    return("Clay")
  } else {
    return(NA)  # Assign NA if the soil type doesn't match
  }
}

# Apply the function to create a new column for mapped soil types
CPTraw$Mapped_Soil_Type <- sapply(CPTraw$USCS.Group.Name, map_soil_type)
head(CPTraw)

# Assign class labels
CPTraw$Soil_Class_Label <- as.numeric(factor(CPTraw$Mapped_Soil_Type, 
                                           levels = c("Gravel", "Sand", "Silt", "Clay"),
                                           labels = c(1, 2, 3, 4)))

# Check if mapping was successful
table(CPTraw$Mapped_Soil_Type, CPTraw$Soil_Class_Label)

# View first few rows after transformation
head(CPTraw)

# Remove the unnecessary columns using Base R
data <- CPTraw[, !(names(CPTraw) %in% c("USCS.Group.Name", "Mapped_Soil_Type"))]

# Check the updated dataset structure
str(data)

# View the first few rows to confirm the removal
head(data)

# remove NA's from the data
colSums(is.na(data))
# Remove rows with any missing values
data <- na.omit(data)
#-------------------Data Analysis ------------------------------
# Compute correlation matrix
cor_matrix <- cor(data[1:length(data)-1])

# Generate a heatmap correlation plot
corrplot(cor_matrix, method = "color", type = "lower",
         tl.col = "black", tl.srt = 25, addCoef.col = "black",
         col = colorRampPalette(c("blue", "white", "red"))(200))

ggpairs(data[1:length(data)],title = "Pairwise Scatterplots and Correlations")


summary_table <- dfSummary(data, 
                                     style = "grid",  # Structured table layout
                                     headings = FALSE, # Remove the extra headings
                                     varnumbers = FALSE,  # Hide variable numbers
                                     labels.col = FALSE) # Remove column labels
                           
summary_table1 <- summary(data)
summary_table1                         
# Save the summary as an HTML file
# Save to HTML file
print(summary_table, method = "browser", file = "D:/GATech/Courses/ISYE-7406/Homeworks/HW5/CPT_summary.html")
# Convert HTML to PNG
webshot("D:/GATech/Courses/ISYE-7406/Homeworks/HW5/CPT_summary.html", "D:/GATech/Courses/ISYE-7406/Homeworks/HW5/CPT_summary.png", zoom = 2)

###******----------------------- Bar Plot ---------------------------------------------
ggplot(data, aes(x = factor(Soil_Class_Label), fill = factor(Soil_Class_Label))) +
  geom_bar() +
  labs(title = "Distribution of Soil Classes", x = "Soil Class", y = "Count") +
  scale_x_discrete(labels = c("Gravel", "Sand", "Silt", "Clay")) +
  theme_minimal()

###*******----------------------- Feature Density Plots ---------------------------------------------
ggplot(data, aes(x = qc, fill = factor(Soil_Class_Label))) +
  geom_density(alpha = 0.4) +
  labs(title = "Density Plot of qc by Soil Class", x = "qc", y = "Density") +
  scale_fill_manual(values = c("red", "blue", "green", "purple"), labels = c("Gravel", "Sand", "Silt", "Clay")) +
  theme_minimal()

###----------------------- Box plot ---------------------------------------------
# Reshape data for ggplot
CPT_melted <- melt(data)

# Boxplots as subplots using facet_wrap
ggplot(CPT_melted, aes(x = "", y = value)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 1) +
  theme_minimal() +
  labs(title = "Boxplots for Outlier Detection") +
  facet_wrap(~variable, scales = "free_y", ncol = 3)  # Adjust ncol for layout

# Define file path
file_path <- "D:/GATech/Courses/ISYE-7406/Homeworks/HW5/boxplot_CPT.png"

# Save the last ggplot output as PNG
ggsave(filename = file_path, width = 12, height = 8, dpi = 300, bg='white')

# Confirm file saved
print(paste("Boxplot saved at:", file_path))


# For all numeric columns
boxplot(data[, sapply(data, is.numeric)], main = "Boxplot for All Numeric Variables")

###----------------------- linear Regression ---------------------------------------------
# Set seed for reproducibility
set.seed(123)

# Define response variable
response <- "Corrected.SPT.N"

# Automatically select all other columns as predictors
predictors <- setdiff(names(data), response)

# Split data into training (80%) and test (20%) sets
trainIndex <- createDataPartition(data[[response]], p = 0.8, list = FALSE)
train_data <- data[trainIndex, ]
test_data <- data[-trainIndex, ]

# Train the Linear Regression model
lm_formula <- as.formula(paste(response, "~ ."))
lm_model <- lm(lm_formula, data = train_data)

# Summarize the model
summary(lm_model)

# Make predictions on the test set
predictions <- predict(lm_model, newdata = test_data)

# Calculate Mean Squared Error (MSE)
mse_value <- mse(test_data[[response]], predictions)

# Calculate R-squared value
r_squared <- cor(test_data[[response]], predictions)^2

# Print performance metrics
cat("Linear Regression Model Performance:\n")
cat("MSE:", mse_value, "\n")
cat("R-squared:", r_squared, "\n")


#----------------Scalling dara---------------------------------

# Standardize predictor variables (excluding the response variable)
data_scaled <- data
data_scaled[, predictors] <- scale(data_scaled[, predictors])


# Split data into training (80%) and test (20%) sets
trainIndex_Scaled <- createDataPartition(data_scaled[[response]], p = 0.8, list = FALSE)
train_scaleddata <- data_scaled[trainIndex, ]
test_scaleddata <- data_scaled[-trainIndex, ]

# Train the Linear Regression model
lm_model_Scaled <- lm(lm_formula, data = train_scaleddata)

# Summarize the model
summary(lm_model_Scaled)

# Make predictions on the test set
predictions_scaled <- predict(lm_model_Scaled, newdata = test_scaleddata)

# Calculate Mean Squared Error (MSE)
mse_value_scaled <- mse(test_scaleddata[[response]], predictions_scaled)

# Calculate R-squared value
r_squared_scaled <- cor(test_scaleddata[[response]], predictions_scaled)^2

# Print performance metrics
cat("Linear Regression Model Performance on scaled data:\n")
cat("MSE:", mse_value_scaled, "\n")
cat("R-squared:", r_squared_scaled, "\n")

## Random Forest has several commonly known implementations 
##  in R packages (and other computing languages such as Python). 
##  Here we illustrate the R package ‘randomForest’
##  You need to install this R package first 
library(randomForest)

## Build Random Forest with the default parameters
## It can be 'classification', 'regression', or 'unsupervised'

rf1 <- randomForest(lm_formula, data=train_data, 
                    importance=TRUE)

## Check Important variables
importance(rf1)
## There are two types of importance measure 
##  (1=mean decrease in accuracy, 
##   2= mean decrease in node impurity)
importance(rf1, type=1)
varImpPlot(rf1)


## Prediction on the testing data set
rf.pred = predict(rf1, test_data, type='class')
# Calculate evaluation metrics
mse_value_rf <- mse(test_data[[response]], rf.pred)
r_squared_rf <- cor(test_data[[response]], rf.pred)^2

# Print performance metrics
cat("Random Forest Model Performance:\n")
cat("MSE:", mse_value_rf, "\n")
cat("R-squared:", r_squared_rf, "\n")


##In practice, You can fine-tune parameters in Random Forest such as 
#ntree = number of tress to grow, and the default is 500. 
#mtry = number of variables randomly sampled as candidates at each split. 
#          The default is sqrt(p) for classfication and p/3 for regression
#nodesize = minimum size of terminal nodes. 
#           The default value is 1 for classification and 5 for regression

#----- Cross Validation on Random Forest -----------------

# Define 10-fold Cross-Validation
control <- trainControl(method = "cv", number = 10)

# Train the Random Forest model with Cross-Validation
rf_cv_model <- train(lm_formula, 
                     data = train_data, 
                     method = "rf",
                     trControl = control, 
                     ntree = 500, 
                     importance = TRUE)

# Print the cross-validation results
print(rf_cv_model)

best_rf_model <- rf_cv_model$finalModel


# Feature importance plot
varImpPlot(rf_cv_model$finalModel)

# Make predictions on the test set
prediction_rf_cv <- predict(best_rf_model, newdata = test_data)

# Calculate evaluation metrics
mse_value_rf_cv <- mse(test_data[[response]], prediction_rf_cv)
r_squared_rf_cv <- cor(test_data[[response]], prediction_rf_cv)^2

# Print performance metrics
cat("Random Forest with Cross-Validation:\n")
cat("MSE:", mse_value_rf_cv, "\n")
cat("R-squared:", r_squared_rf_cv, "\n")

#----- Fine tuning RF parameters -----------------

# Define hyperparameter grid (manual tuning)
mtry_values <- c(2, 4)       # Number of features sampled at each split
ntree_values <- c(100, 300, 500)    # Number of trees
nodesize_values <- c(5, 10,20)      # Minimum node size

# Initialize an empty data frame to store results
results <- data.frame(mtry = integer(), ntree = integer(), nodesize = integer(), MSE = numeric())

# Loop through all combinations of hyperparameters
for (mtry_val in mtry_values) {
  for (ntree_val in ntree_values) {
    for (nodesize_val in nodesize_values) {
      
      # Train Random Forest model with given hyperparameters
      rf_model <- randomForest(Corrected.SPT.N ~ ., 
                               data = train_data, 
                               mtry = mtry_val, 
                               ntree = ntree_val, 
                               nodesize = nodesize_val)
      
      # Make predictions on the test set
      predictions <- predict(rf_model, newdata = test_data)
      
      # Compute Mean Squared Error (MSE)
      mse_value <- mse(test_data[[response]], predictions)
      r_squared <- cor(test_data[[response]], predictions)^2
      # Store results
      results <- rbind(results, data.frame(mtry = mtry_val, ntree = ntree_val, nodesize = nodesize_val, MSE = mse_value))
      
      # Print progress
      cat("mtry =", mtry_val, ", ntree =", ntree_val, ", nodesize =", nodesize_val, "-> MSE:", mse_value, 'and -> R-Squared:',r_squared, "\n")
    }
  }
}

# Find the best hyperparameter combination
best_params <- results[which.min(results$MSE), ]
print(best_params)


###----------------------- Decision Tree ---------------------------------------------
# Load necessary package
library(rpart)
library(rpart.plot)
library(Metrics)

dt_model_untuned <- rpart(
  Corrected.SPT.N ~ ., 
  data = train_data, 
  method = "anova",
  control = rpart.control(cp = 0.01)
)

# Summary of the untuned model
cat("\nUntuned Decision Tree Summary:\n")
summary(dt_model_untuned)

# Plot the untuned tree
rpart.plot(dt_model_untuned, main = "Untuned Decision Tree")

# Make predictions on test set
dt_predictions_untuned <- predict(dt_model_untuned, newdata = test_data)

# Compute MSE and R²
dt_mse_untuned <- mean((test_data$Corrected.SPT.N - dt_predictions_untuned)^2, na.rm = TRUE)
dt_r2_untuned <- cor(test_data$Corrected.SPT.N, dt_predictions_untuned, use = "complete.obs")^2 * 100  

# Print untuned results
cat("\nUntuned Decision Tree Model Performance:\n")
cat("MSE:", dt_mse_untuned, "\n")
cat("R-squared:", dt_r2_untuned, "%\n")

# --- Feature Importance for Model ---
dt_importance_untuned <- dt_model_untuned$variable.importance
if (!is.null(dt_importance_untuned)) {
  dt_importance_untuned <- data.frame(
    Feature = names(dt_importance_untuned),
    Importance = dt_importance_untuned / sum(dt_importance_untuned) * 100
  )
  cat("\nUntuned Decision Tree Feature Importance:\n")
  print(dt_importance_untuned[order(-dt_importance_untuned$Importance), ])
} else {
  cat("\nNo feature importance available for untuned model.\n")
}


# Parameter grid
cp_values <- c(0.001, 0.005, 0.01)
maxdepth_values <- c(3, 5, 10)
minsplit_values <- c(10, 20, 30)
minbucket_values <- c(5, 10, 15)

results <- data.frame()

set.seed(123)
# Manual grid search
for (cp in cp_values) {
  for (maxdepth in maxdepth_values) {
    for (minsplit in minsplit_values) {
      for (minbucket in minbucket_values) {
        
        model <- rpart(
          Corrected.SPT.N ~ ., 
          data = train_data,
          method = "anova",
          control = rpart.control(cp = cp, maxdepth = maxdepth, 
                                  minsplit = minsplit, minbucket = minbucket)
        )
        
        preds <- predict(model, newdata = test_data)
        
        rmse <- rmse(test_data$Corrected.SPT.N, preds)
        r2 <- cor(test_data$Corrected.SPT.N, preds, use = "complete.obs")^2
        
        results <- rbind(results, data.frame(cp, maxdepth, minsplit, minbucket, RMSE = rmse, R2 = r2))
      }
    }
  }
}

# Best parameters
best_params <- results[which.min(results$RMSE), ]
print("Best Parameters:")
print(best_params)

# Final model using best parameters
best_model <- rpart(
  Corrected.SPT.N ~ ., 
  data = train_data,
  method = "anova",
  control = rpart.control(
    cp = best_params$cp,
    maxdepth = best_params$maxdepth,
    minsplit = best_params$minsplit,
    minbucket = best_params$minbucket
  )
)

# Final predictions and plot
final_preds <- predict(best_model, newdata = test_data)
final_mse <- mean((test_data$Corrected.SPT.N - final_preds)^2, na.rm = TRUE)
final_r2 <- cor(test_data$Corrected.SPT.N, final_preds, use = "complete.obs")^2 * 100

cat("\nFinal Model Performance:\n")
cat("MSE:", final_mse, "\n")
cat("R-squared:", final_r2, "%\n")

# Plot tree
rpart.plot(best_model, main = "Best Tuned Decision Tree")

###----------------------- Naive Bayes ---------------------------------------------
# Load necessary package
library(e1071)

# Fit Naive Bayes model
nb_model <- naiveBayes(Corrected.SPT.N ~ ., data = train_data)

# Predict on training data
nb_predictions <- predict(nb_model, train_data)

# Make predictions on test set
nb_predictions <- predict(nb_model, newdata = test_data)

# Convert predictions to numeric (since Naive Bayes in e1071 may return factor levels)
nb_predictions <- as.numeric(nb_predictions)

# Compute MSE
nb_mse <- mse(test_data$Corrected.SPT.N, nb_predictions)

# Naive Bayes R-squared Calculation
nb_r2 <- postResample(nb_predictions, test_data$Corrected.SPT.N)[2]

# Print results
cat("\nNaïve Bayes Model Performance:\n")
cat("MSE:", nb_mse, "\n")
cat("R-squared:", nb_r2, "\n")

# Approximate feature importance via conditional probabilities
nb_importance <- function(model, data) {
  probs <- model$tables
  importance <- sapply(names(probs), function(feature) {
    if (is.matrix(probs[[feature]])) {
      mean(apply(probs[[feature]], 2, var, na.rm = TRUE))
    } else {
      0  
    }
  })
  importance <- importance / sum(importance, na.rm = TRUE) * 100
  data.frame(Feature = names(probs), Importance = importance)
}
nb_imp <- nb_importance(nb_model, train_data[, !names(train_data) %in% c("Corrected.SPT.N", "SPT_N_bin")])
cat("\nNaive Bayes Feature Importance (Approximate):\n")
print(nb_imp[order(-nb_imp$Importance), ])


###----------------------- SVM ---------------------------------------------
library(e1071)
# Train SVM Regression model
svm_model <- svm(Corrected.SPT.N ~ ., data = train_data, type = "eps-regression", kernel = "radial")

# Print the model summary
summary(svm_model)

# Make predictions on the test set
svm_predictions <- predict(svm_model, newdata = test_data)

# Compute Mean Squared Error (MSE)
svm_mse <- mean((test_data$Corrected.SPT.N - svm_predictions)^2)

# Compute R-squared value
svm_r2 <- cor(test_data$Corrected.SPT.N, svm_predictions)^2

# Print performance metrics
cat("SVM Regression - MSE:", svm_mse, "\n")
cat("SVM Regression - R²:", svm_r2, "\n")

# Define parameter grid
tune_result <- tune(
  svm, 
  Corrected.SPT.N ~ ., 
  data = train_data,
  ranges = list(
    cost = c(1, 10), 
    gamma = c(0.01, 0.1)
  ),
  kernel = "radial",
  type = "eps-regression",
  tunecontrol = tune.control(cross = 3)
)

# Best model
svm_best <- tune_result$best.model

# Print best parameters
cat("\nBest Parameters:\n")
print(tune_result$best.parameters)

# Make predictions
svm_predictions <- predict(svm_best, newdata = test_data)

# Compute performance metrics
svm_mse <- mean((test_data$Corrected.SPT.N - svm_predictions)^2)
svm_r2 <- cor(test_data$Corrected.SPT.N, svm_predictions)^2

# Print results
cat("\nTuned SVM Regression - MSE:", svm_mse, "\n")
cat("Tuned SVM Regression - R²:", svm_r2, "\n")

###----------------------- Lasso ---------------------------------------------
# Load necessary packages
library(glmnet)
library(caret)
library(ggplot2)

# Prepare data matrices for glmnet
x_train <- as.matrix(train_data[, !names(train_data) %in% "Corrected.SPT.N"])
y_train <- train_data$Corrected.SPT.N
x_test <- as.matrix(test_data[, !names(test_data) %in% "Corrected.SPT.N"])
y_test <- test_data$Corrected.SPT.N

# --- Untuned Lasso Model ---
set.seed(123)
lasso_untuned <- glmnet(x_train, y_train, alpha = 1, lambda = 0.1)
lasso_pred_untuned <- predict(lasso_untuned, x_test)
lasso_mse_untuned <- mean((y_test - lasso_pred_untuned)^2, na.rm = TRUE)
lasso_r2_untuned <- cor(y_test, lasso_pred_untuned, use = "complete.obs")^2

cat("\nUntuned Lasso Regression Performance:\n")
cat("MSE:", lasso_mse_untuned, "\n")
cat("R²:", lasso_r2_untuned, "\n")

# --- Tuning Grid ---
lambda_seq <- 10^seq(-4, 2, length.out = 50)
lasso_grid <- expand.grid(alpha = 1, lambda = lambda_seq)

# More robust repeated cross-validation
set.seed(123)
lasso_tune <- train(
  x = x_train, y = y_train,
  method = "glmnet",
  tuneGrid = lasso_grid,
  trControl = trainControl(
    method = "repeatedcv",
    number = 10,
    repeats = 5,
    verboseIter = FALSE
  ),
  metric = "RMSE",
  maximize = FALSE
)

# Final model with best lambda
lasso_model <- glmnet(x_train, y_train, alpha = 1, lambda = lasso_tune$bestTune$lambda)
lasso_pred <- predict(lasso_model, x_test)
lasso_mse <- mean((y_test - lasso_pred)^2, na.rm = TRUE)
lasso_r2 <- cor(y_test, lasso_pred, use = "complete.obs")^2

cat("\nTuned Lasso Regression Performance:\n")
cat("Best Lambda:", lasso_tune$bestTune$lambda, "\n")
cat("MSE:", lasso_mse, "\n")
cat("R²:", lasso_r2, "\n")

# --- Plot Tuning Results ---
ggplot(lasso_tune$results, aes(x = log10(lambda), y = RMSE)) +
  geom_line(color = "steelblue") +
  geom_point() +
  geom_vline(xintercept = log10(lasso_tune$bestTune$lambda), linetype = "dashed", color = "red") +
  labs(title = "Lasso RMSE vs log10(Lambda)", x = "log10(Lambda)", y = "RMSE") +
  theme_minimal()

###----------------------- XGBoost ---------------------------------------------
# Load necessary packages
library(xgboost)
library(caret)
library(ranger)
library(ggplot2)

# Prepare data matrices for XGBoost
x_train <- as.matrix(train_data[, !names(train_data) %in% "Corrected.SPT.N"])
y_train <- train_data$Corrected.SPT.N
x_test <- as.matrix(test_data[, !names(test_data) %in% "Corrected.SPT.N"])
y_test <- test_data$Corrected.SPT.N

# --- Untuned XGBoost Model ---
set.seed(123)
xgb_untuned <- xgboost(
  data = x_train, 
  label = y_train,
  nrounds = 100,
  max_depth = 6, 
  eta = 0.1,              
  subsample = 1
)
xgb_pred_untuned <- predict(xgb_untuned, x_test)
xgb_mse_untuned <- mean((y_test - xgb_pred_untuned)^2, na.rm = TRUE)
xgb_r2_untuned <- cor(y_test, xgb_pred_untuned, use = "complete.obs")^2

# Print untuned performance metrics
cat("\nUntuned XGBoost Performance:\n")
cat("MSE:", xgb_mse_untuned, "\n")
cat("R²:", xgb_r2_untuned, "\n")

# --- Tuned XGBoost Model ---
xgb_grid <- expand.grid(
  nrounds = c(100, 200), 
  max_depth = c(3, 6),    
  eta = c(0.01, 0.1),  
  subsample = c(0.7, 1),   
  gamma = 0,              
  colsample_bytree = 1,    
  min_child_weight = 1   
)

# Train with cross-validation
set.seed(123)
xgb_tune <- train(
  x = x_train, 
  y = y_train,
  method = "xgbTree",
  trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3),
  tuneGrid = xgb_grid,
  verbose = FALSE
)

# Final tuned XGBoost model using best hyperparameters
xgb_model <- xgboost(
  data = x_train, 
  label = y_train,
  nrounds = xgb_tune$bestTune$nrounds, 
  max_depth = xgb_tune$bestTune$max_depth,
  eta = xgb_tune$bestTune$eta, 
  gamma = 0,     
  colsample_bytree = 1,   
  min_child_weight = 1, 
  subsample = xgb_tune$bestTune$subsample,
  verbose = 0
)

# Make predictions on the test set
xgb_pred <- predict(xgb_model, x_test)

# Calculate performance metrics
xgb_mse <- mean((y_test - xgb_pred)^2, na.rm = TRUE)
xgb_r2 <- cor(y_test, xgb_pred, use = "complete.obs")^2

# Print tuned performance metrics
cat("\nTuned XGBoost Performance:\n")
cat("MSE:", xgb_mse, "\n")
cat("R²:", xgb_r2, "\n")

###----------------------- Boosting ---------------------------------------------
## You need to first install this R package before using it
library(gbm)

# 
gbm.CPT <- gbm(Corrected.SPT.N ~ .,
               data = train_data,
               distribution = "gaussian",  # Specify regression problem
               n.trees = 2000,             # Total number of boosting iterations (trees)
               shrinkage = 0.01,           # Learning rate
               interaction.depth = 1,      # Depth of each tree (stumps)
               cv.folds = 20)              # 10-fold cross-validation

## Model Inspection 
## Find the estimated optimal number of iterations
perf_gbm1 = gbm.perf(gbm.CPT, method="cv") 
perf_gbm1

## summary model
## Which variances are important
summary(gbm.CPT)


## Make Prediction
## use "predict" to find the training or testing error

## Training error
pred1gbm <- predict(gbm.CPT,newdata = train_data, n.trees=perf_gbm1, type="response")
pred1gbm[1:10]
y1hat <- ifelse(pred1gbm < 0.5, 0, 1)
y1hat[1:10]
sum(y1hat != y1)/length(y1)  ##Training error = 0.01729201

## Testing Error
y2hat <- predict(gbm.CPT, newdata = test_data, n.trees = perf_gbm1, type = "response")
# Calculate performance metrics for Boosting
mse_boosting <- mean((test_data$Corrected.SPT.N - y2hat)^2)
r_squared_boosting <- cor(test_data$Corrected.SPT.N, y2hat)^2

# Print results
cat("Boosting Model Performance:\n")
cat("Optimal Trees:", perf_gbm1, "\n")
cat("MSE:", mse_boosting, "\n")
cat("R-squared:", r_squared_boosting, "\n")


# Define hyperparameter grid
tune_grid <- expand.grid(
  n.trees = c(500, 1000, 2000),         # Number of boosting iterations
  shrinkage = c(0.001, 0.01, 0.05),      # Learning rate
  interaction.depth = c(1, 3, 5),       # Tree depth
  n.minobsinnode = c(5, 10, 20)         # Minimum node size
)

# Define cross-validation control
control <- trainControl(method = "cv", number = 5)  # 5-fold Cross-validation

# Train Boosting model with hyperparameter tuning
gbm_tuned <- train(
  Corrected.SPT.N ~ ., 
  data = train_data, 
  method = "gbm",
  trControl = control,
  tuneGrid = tune_grid,
  verbose = FALSE
)

# Print best parameters found
print(gbm_tuned$bestTune)

# Get the best number of trees using cross-validation
best_n_trees <- gbm.perf(gbm_tuned$finalModel, method = "cv")


# Make predictions on the test set using the best model
boosting_preds <- predict(gbm_tuned, newdata = test_data, n.trees = best_n_trees)

# Calculate performance metrics
mse_boosting <- mse(test_data[[response]], boosting_preds)
r_squared_boosting <- cor(test_data[[response]], boosting_preds)^2

# Print performance metrics
cat("Boosting Model Performance (Tuned):\n")
cat("Best Hyperparameters:\n", gbm_tuned$bestTune, "\n")
cat("Optimal Trees:", best_n_trees, "\n")
cat("MSE:", mse_boosting, "\n")
cat("R-squared:", r_squared_boosting, "\n")




# Define hyperparameter grid (manual tuning)
n.trees = c(500, 1000, 2000)       # Number of features sampled at each split
shrinkage = c(0.001, 0.01, 0.05)    # Number of trees
interaction.depth = c(1, 3, 5)      # Minimum node size
n.minobsinnode = c(5, 10, 20)

# Initialize an empty data frame to store results
results <- data.frame(ntrees = integer(), shrinkage = numeric(), interaction = integer(),nminobsinnode=numeric, MSE = numeric())

# Loop through all combinations of hyperparameters
for (ntree in n.trees) {
  for (shrinka in shrinkage) {
    for (interactionde in interaction.depth)
      for (minobsinnode1 in n.minobsinnode) {
      
      # Train Random Forest model with given hyperparameters
        gbm.CPT <- gbm(Corrected.SPT.N ~ .,
                       data = train_data,
                       distribution = "gaussian",  # Specify regression problem
                       n.trees = ntree,             # Total number of boosting iterations (trees)
                       shrinkage = shrinka,           # Learning rate
                       interaction.depth = interactionde,      # Depth of each tree (stumps)
                       n.minobsinnode = minobsinnode1)              # 10-fold cross-validation
      
       perf_gbm1 = gbm.perf(gbm.CPT, method="cv") 
       # Make predictions on the test set
       y2hat <- predict(gbm.CPT, newdata = test_data, n.trees = perf_gbm1, type = "response")
      
      # Compute Mean Squared Error (MSE)
      mse_value <- mse(test_data[[response]], predictions)
      r_squared <- cor(test_data[[response]], predictions)^2
      # Store results
      results <- rbind(results, data.frame(ntrees = ntree, shrinkage = shrinka, interaction = interactionde, MSE = mse_value))
      
      # Print progress
      cat("mtry =", mtry_val, ", ntree =", ntree_val, ", nodesize =", nodesize_val, "-> MSE:", mse_value, 'and -> R-Squared:',r_squared, "\n")
    }
  }
}

# Find the best hyperparameter combination
best_params <- results[which.min(results$MSE), ]
print(best_params)

