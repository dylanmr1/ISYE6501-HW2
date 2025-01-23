# load packages
library(ggplot2)

# clear environment
rm(list = ls())

# load dataset
path <- "/Users/Dylan Rivera/Desktop/OMSA/Spring 2025/ISYE 6501/HW2/"
iris <- read.csv(file.path(path,"iris.txt"), sep="")

# initialize table to store results of the kmeans function
tot_sum <- data.frame(k = numeric(), totss = numeric())

#scale data
iris_scaled <- scale(iris[,1:4], center = TRUE, scale = TRUE)
iris_scaled <- as.data.frame(iris_scaled)
iris_scaled['Species'] <- iris[,5]

# run kmeans algorithm, looping through for a number of clusters
for (i in 1:10){
  set.seed(1)
  cluster <- kmeans(iris_scaled[,1:4], centers = i, nstart = 10)
  
  row <- c(i, cluster$tot.withinss)
  tot_sum <- rbind(tot_sum, row)
  colnames(tot_sum) <- c("k", "tot.withinss")
}

plot <- ggplot(tot_sum, aes(k, tot.withinss)) + geom_point() + geom_line() + xlim(1,10) + labs(title = "Elbow diagram for different values of k")
plot

# run kmeans for k = 4
set.seed(1)
cluster_4 <- kmeans(iris_scaled[,1:4], centers = 4, nstart = 10)
table(cluster_4$cluster, iris$Species)


cluster_3 <- kmeans(iris_scaled[,1:4], centers = 3, nstart = 10)
table(cluster_3$cluster, iris$Species)

test <- kmeans(iris_scaled[,3:4], centers = 3, nstart = 10)
table(test$cluster, iris$Species)