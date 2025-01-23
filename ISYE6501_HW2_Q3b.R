# load packages
library(kernlab)
library(ggplot2)
library(caret)

# clear environment
rm(list = ls()) 

# load dataset
path <- "/Users/Dylan Rivera/Desktop/OMSA/Spring 2025/ISYE 6501/HW2/"
ccData <- read.csv(file.path(path,"credit_card_data-headers.txt"), sep="")

# set seed
set.seed(1)

# split data into training, validation, and test sets
spec <- c(train = .7, validate = .15, test = .15)
g <- sample(cut(seq(nrow(ccData)), nrow(ccData)*cumsum(c(0,spec)), labels = names(spec)))
splits <- split(ccData, g)

# call ksvm for training data, loop through different values of C
results_train <- data.frame(C = numeric(), Accuracy = numeric())
C_train <- 1e-5
while (C_train <= 1e5) {
  # call ksvm function
  model <- ksvm(as.matrix(splits$train[,1:10]), as.factor(splits$train[,11]), type="C-svc", kernel="vanilladot", C = C_train, scaled=TRUE)
  
  # calculate a1…am
  a <- colSums(model@xmatrix[[1]] * model@coef[[1]])
  
  # calculate a0
  a0 <- -(model@b)
  
  # see what the model predicts
  pred <- predict(model,splits$train[,1:10])
  
  # see what fraction of the model’s predictions match the actual classification
  accuracy <- sum(pred == splits$train[,11]) / nrow(splits$train)
  
  # add value of C and model's accuracy into table
  row <- c(C_train, accuracy)
  results_train <- rbind(results_train, row)
  colnames(results_train) <- c("C", "Accuracy")
  
  # iterate to next C
  C_train = C_train*10
}

# plot graph of accuracy vs C value
plot_train <- ggplot(results_train, aes(C, Accuracy)) + geom_point() + scale_x_log10() + ylim(0,1) + labs(title = "SVM Model Accuracy (Training Set)")

# run ksvm on validation data
results_val <- data.frame(C = numeric(), Accuracy = numeric())
C_val <- 1e-5
while (C_val <= 1e5) {
  model <- ksvm(as.matrix(splits$validate[,1:10]), as.factor(splits$validate[,11]), type="C-svc", kernel="vanilladot", C = C_val, scaled=TRUE)
  
  a <- colSums(model@xmatrix[[1]] * model@coef[[1]])
  
  a0 <- -(model@b)
  
  pred <- predict(model,splits$validate[,1:10])
  
  accuracy <- sum(pred == splits$validate[,11]) / nrow(splits$validate)
  
  row <- c(C_val, accuracy)
  results_val <- rbind(results_val, row)
  colnames(results_val) <- c("C", "Accuracy")

  C_val = C_val * 10
}

plot_val <- ggplot(results_val, aes(C, Accuracy)) + geom_point() + scale_x_log10() + ylim(0,1) + labs(title = "SVM Model Accuracy (Validation Set)")

# after choosing best C, run ksvm on the test set
model <- ksvm(as.matrix(splits$test[,1:10]), as.factor(splits$test[,11]), type="C-svc", kernel="vanilladot", C = 10, scaled=TRUE)
a <- colSums(model@xmatrix[[1]] * model@coef[[1]])
a0 <- -(model@b)
pred <- predict(model,splits$test[,1:10])
accuracy_test <- sum(pred == splits$test[,11]) / nrow(splits$test)