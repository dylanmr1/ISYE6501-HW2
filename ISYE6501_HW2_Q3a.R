# load packages
library(kknn)
library(ggplot2)
library(caret)

# clear environment
rm(list = ls()) 

# load dataset
path <- "/Users/Dylan Rivera/Desktop/OMSA/Spring 2025/ISYE 6501/HW2/"
ccData <- read.csv(file.path(path,"credit_card_data-headers.txt"), sep="")

# initialize empty data frame to store accuracy
comparison <- data.frame(k = numeric(), Accuracy = numeric())

#set seed
set.seed(1)

# create training and test sets
index <- sample(seq_len(nrow(ccData)), size = floor(0.75*nrow(ccData)))
data_train <- ccData[index,]
data_test <- ccData[-index,]

# for the training set, loop through several values of k
for (j in 1:20) {
  # run cv.kknn
  results <- cv.kknn(R1~A1+A2+A3+A8+A9+A10+A11+A12+A14+A15, data_train, k = j, kcv = 5, distance = 2, kernel = "optimal", scale = TRUE)
  
  # round predictions to 0 or 1
  for (i in 1:length(results[[1]][,2])) {
    if (results[[1]][i,2] < 0.5) {
      results[[1]][i,2] = 0
    } else {
      results[[1]][i,2] = 1
    }
  }
  
  # calculate accuracy
  accuracy <- sum(results[[1]][,1] == results[[1]][,2]) / nrow(results[[1]])
  
  # add accuracy into table
  row <- c(j, accuracy)
  comparison <- rbind(comparison, row)
  colnames(comparison) <- c("k", "Accuracy")
}

# plot accuracy vs k
plot <- ggplot(comparison, aes(k, Accuracy))
plot + geom_point() + labs(title = "Comparison of k-fold cross-validation models (k = 5)") + xlab("k-neighbors")

# re-run on the test dataset
results_test <- data.frame(Prediction = numeric(), Actual = numeric())
for (x in 1:654){
  test_kknn <- kknn(R1~A1+A2+A3+A8+A9+A10+A11+A12+A14+A15, data_test[-x,], data_test[x,], k = 11, distance = 2, kernel = "optimal", scale = TRUE)

  # compare prediction with actual value
  prediction <- fitted.values(test_kknn)
  actual <- data_test[x,11]
  
  if (prediction < 0.5){
    prediction = 0
  } else {
    prediction = 1
  }
  # add results into table
  row_test <- c(prediction, actual)
  results_test <- rbind(results_test, row_test)
  colnames(results_test) <- c("Prediction", "Actual")
}

# calculate accuracy
accuracy_test <- sum(results_test[,1]==results_test[,2]) / nrow(results_test)