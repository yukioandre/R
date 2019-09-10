# KNN DO PREDICT WHETHER THE PATIENT HAVE DIABETES OR NOT
# Data: https://www.kaggle.com/amolbhivarkar/knn-for-classification-using-scikit-learn/data

# load data
data = read.csv('diabetes.csv')
View(data)

# (ignore this)
data[1,1:9]

# CHECK DATA

# first rows
head(data)

# last rows
tail(data)

# summary
summary(data)

# descriptive statistics (better than summary)
library(Hmisc)
describe(data) 

# data structure
str(data)

# Check Pregnancy x Outcome
freqPreg = table(data$Pregnancies, data$Outcome)
prop.table(freqPreg) # cell percentages
prop.table(freqPreg, 1) # row percentages 
prop.table(freqPreg, 2) # column percentages

# EXPLORATORY DATA ANALYSIS

# histogram all variables
library(plyr)
library(psych)
multi.hist(data)

# Histogram by Outcome (not that useful)
ggplot(data,aes(x=BMI))+geom_histogram()+facet_grid(~Outcome)+theme_bw()
ggplot(data,aes(x=Pregnancies))+geom_histogram()+facet_grid(~Outcome)+theme_bw()

# NORMALIZE DATA
dataNorm = data
dataNorm[, -9] = scale(dataNorm[, -9])

# Split Train 0.7 and Test 0.3
i = sample(1:nrow(dataNorm), size=0.7*nrow(dataNorm), replace=FALSE)
i = i[order(i)]
train = dataNorm[i,]
test = dataNorm[-i,]
View(test)

# RUN KNN

library("class")

# Execution of k-NN with k=3
KnnTestPrediction_k3 <- knn(train[,-9], test[,-9], train$Outcome, k=3, prob=TRUE)

### AVALIA QUAL MELHOR K
#########################

# Confusion matrix of KnnTestPrediction_k3
table(test$Outcome, KnnTestPrediction_k3)

# Classification accuracy of KnnTestPrediction_k3
sum(KnnTestPrediction_k3==test$Outcome)/length(test$Outcome)*100

# Predictions with this model (k=3)
# First, let's create a hypothetical dataset and then predict 
predict_test = dataNorm[1:4,-9]
predict_test <- rbind(predict_test, c(-0.844, 0.1596, -0.47, -1.28, -0.6923, -0.241, -0.371, 1.169))
predict_test <- rbind(predict_test, c(1.53, -1, -0.37, -0.129, -0.6925, -1.2, -0.99, -0.0205))
predict_test_TEST <- knn(train[,-9], predict_test[,], train$Outcome, k=3, prob=TRUE)
predict_test_TEST


## Doing without normalizing the data previously:
#nn5 <- kNN(Species ~ .,trainIris,testIris,norm=TRUE,k=5)