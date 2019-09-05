# First, let's see how our data looks like

# see first rows
head(iris)

# see last rows
tail(iris)

# summary (min, max, median, quartiles)
summary(iris)

# summary 2 (n, missing, quartiles, etc)
library("Hmisc");
describe(iris);

# Structure (type of each variable and the whole data)
str(iris)

# histogram by group
ggplot(iris,aes(x=Sepal.Length))+geom_histogram()+facet_grid(~Species)+theme_bw()
ggplot(iris,aes(x=Sepal.Width))+geom_histogram()+facet_grid(~Species)+theme_bw()
ggplot(iris,aes(x=Petal.Length))+geom_histogram()+facet_grid(~Species)+theme_bw()
ggplot(iris,aes(x=Petal.Width))+geom_histogram()+facet_grid(~Species)+theme_bw()

# scatterplot with different colors per specie
library("ISLR")
library("ggplot2")
qplot(Sepal.Length, Sepal.Width, data=iris, color=Species)
qplot(Petal.Length, Petal.Width, data=iris, color=Species)

# Before running KNN, we need to normalize data
# Remember: we are dealing with euclidean distance
# Scale: (x - mean(x)) / sd(x)
iris_normalize = iris
iris_normalize[, -5] = scale(iris[, -5])

# train x test
set.seed(9999)
ind = sample(2, nrow(iris_normalize), replace=TRUE, prob=c(0.6,0.4))
train = iris_normalize[ind==1,]
test = iris_normalize[ind==2,]

# run KNN
library("class")

# Execution of k-NN with k=1
KnnTestPrediction_k1 <- knn(trainData[,-5], testData[,-5],
                            trainData$Species, k=1, prob=TRUE)

# Execution of k-NN with k=2
KnnTestPrediction_k2 <- knn(trainData[,-5], testData[,-5],
                            trainData$Species, k=2, prob=TRUE)

# Execution of k-NN with k=3
KnnTestPrediction_k3 <- knn(trainData[,-5], testData[,-5],
                            trainData$Species, k=3, prob=TRUE)

# now, you can check which k is better
# Confusion matrix
table(testData$Species, KnnTestPrediction_k1)

# Accuracy
sum(KnnTestPrediction_k1==testData$Species)/length(testData$Species)*100

# Note: Do the same for all k's you used

# Better method to compare diferent k's (found at kaggle)
KnnTestPrediction <- list()
accuracy <- numeric()

# testing all k's from 1 to 50
for(k in 1:50){
  
  KnnTestPrediction[[k]] <- knn(trainData[,-5], testData[,-5], trainData$Species, k, prob=TRUE)
  accuracy[k] <- sum(KnnTestPrediction[[k]]==testData$Species)/length(testData$Species)*100
  
}

# comparing
plot(accuracy, type="b", col="blue", cex=1, pch=1,
     xlab="k", ylab="Accuracy", 
     main="Accuracy by k")

# vertical line to mark the maximum
abline(v=which(accuracy==max(accuracy)), col="red", lwd=1.5)

# grey horizontal line for the maximum
abline(h=max(accuracy), col="grey", lty=2)

# grey horizontal line for the minimum
abline(h=min(accuracy), col="grey", lty=2)

