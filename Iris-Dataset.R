# Load data
df = iris

# Check first 5 rows
head(df)

# Variables type
sapply(df, class)

# Variables type (and some extra information)
str(df)

# Descriptive statistics
summary(df)


#################################################################
# FILTER (SUBSETTING) EXAMPLES
# Filter Sepal Length < 5 for all columns
df[df$Sepal.Length < 5,]

# Filter Sepal Length <= 5 for first 2 columns
df[df$Sepal.Length <= 5,1:2]

# Filter Sepal Length equal to 5 for columns chosen by name
df[df$Sepal.Length == 5,c("Sepal.Width", "Species")]

# Filter species different than setosa for all columns, except the first
df[df$Species != "setosa",-1]


#################################################################
# PLOTTING EXAMPLES
# Plot scatterplot all variables in the dataset, changing color accordingly to the specie
plot(df, col=df$Species)

# Plot petal lenght against width
library(RColorBrewer)
plot(df$Petal.Length, df$Petal.Width, col=df$Species)
legend(x=5.9, y=0.8, legend=levels(iris$Species), 
       col=brewer.pal(3, "Set2"), pch=1)

plot(df$Petal.Length, df$Petal.Width, col=df$Species)
legend('bottomright', legend=levels(iris$Species), 
       col=brewer.pal(3, "Set2"), pch=1, bty='n')

library(ggplot2)
ggplot(iris,aes(x = Sepal.Length, y = Sepal.Width, col= Species)) + geom_point()

# Histograms
dev.off()
par(mfrow=c(2,2))
hist(df$Sepal.Length, main="Sepal Length")
hist(df$Sepal.Width, main="Sepal Width")
hist(df$Petal.Length, main="Petal Length")
hist(df$Petal.Width, main="Petal Width")


# Check for correlation
cor(df[,-5])

# mean according to each specie
aggregate(df[, c(1,2,3,4)], list(df$Species), mean)

#################################################################
# MODELS
# Separa em treino e teste
smp_size <- floor(0.75 * nrow(df))
set.seed(123)
train_ind <- sample(seq_len(nrow(df)), size = smp_size)
train <- df[train_ind, ]
test <- df[-train_ind, ]

#################################################################
# KMEANS
# Roda o modelo com treino
exemplo_kmeans <- kmeans(train[1:4], 3);
exemplo_kmeans$size # tamanho de cada cluster gerado
exemplo_kmeans$cluster # a qual cluster pertencer cada um
table(icluster$cluster,iris$Species) # verifica acerto

# aplica na base teste
pred_test <- predict(exemplo_kmeans, newdata=test[1:4])

#################################################################
# KNN from CLASS LIBRARY
library("class")

# Execution of k-NN with k=1
KnnTestPrediction_k1 <- knn(train[,-5], test[,-5],
                            train$Species, k=1, prob=TRUE)

# Execution of k-NN with k=2
KnnTestPrediction_k2 <- knn(train[,-5], test[,-5],
                            train$Species, k=2, prob=TRUE)


# Avalia precisao
table(test$Species, KnnTestPrediction_k2)

#################################################################
# k-NN using caret with 
library(ISLR)
library(caret)

# Run k-NN:
set.seed(400)
ctrl <- trainControl(method="repeatedcv",repeats = 2)
knnFit <- train(Species ~ ., data = train, method = "knn", 
                trControl = ctrl, preProcess = c("center","scale"),tuneLength = 20)
knnFit

plot(knnFit)

knnPredict <- predict(knnFit,newdata = test )
confusionMatrix(knnPredict, test$Species )
