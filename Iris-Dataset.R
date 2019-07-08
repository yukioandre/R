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



# FILTER (SUBSETTING) EXAMPLES
# Filter Sepal Length < 5 for all columns
df[df$Sepal.Length < 5,]

# Filter Sepal Length <= 5 for first 2 columns
df[df$Sepal.Length <= 5,1:2]

# Filter Sepal Length equal to 5 for columns chosen by name
df[df$Sepal.Length == 5,c("Sepal.Width", "Species")]

# Filter species different than setosa for all columns, except the first
df[df$Species != "setosa",-1]



# PLOTTING EXAMPLES
# Plot scatterplot all variables in the dataset, changing color accordingly to the specie
plot(df, col=df$Species)

# Plot petal lenght against width
library(RColorBrewer)
plot(df$Petal.Length, df$Petal.Width, col=df$Species)
legend(x=5.9, y=0.8, legend=levels(iris$Species), col=brewer.pal(3, "Set2"), pch=1)

plot(df$Petal.Length, df$Petal.Width, col=df$Species)
legend('bottomright', legend=levels(iris$Species), col=brewer.pal(3, "Set2"), pch=1, bty='n')

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


# MODELS
# K-Means
par(mfrow=c(1,1))
k.max <- 5
wss<- sapply(1:k.max,function(k){kmeans(iris[,3:4],k,nstart = 20,iter.max = 20)$tot.withinss})
wss

plot(1:k.max,wss, type= "b", xlab = "Number of clusters(k)", ylab = "Within cluster sum of squares")

icluster <- kmeans(iris[,3:4],3,nstart = 20)
table(icluster$cluster,iris$Species)

# Logit