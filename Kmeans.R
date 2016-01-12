install.packages("fpc")
install.packages("cluser")
library(cluster)
library(fpc)
## EXAMPLE OF KMEANS ## 

data <- as.data.frame(matrix(c(rnbinom(500, mu = 100, size = 100), rnbinom(500, mu = 100, size = 100)), nrow = 500))
names(data) <- c("x", "y")
plot(data)

km <- kmeans(data, 5)
plotcluster(data, km$cluster)

## EXAMPLE OF KNN ## 
# Adding label
labels = km$cluster

data2 <- cbind(data, labels = labels)

train.index = sample(1:nrow(data2), replace = F, size = nrow(data) / 2)

train <- data2[train.index, ]
test <- data2[-train.index, ]

library(class)
model <- knn(train = train[, c("x", "y")], test = test[, c("x", "y")], cl = train[, c("labels")], k = 5) 

table(model, test[, "labels"])

sum(model == test[, "labels"]) / nrow(test)
plotcluster(test[, c("x", "y")], test[, "labels"], col = ifelse(model == test[, "labels"], "blue", "red"))

## EXAMPLE OF Decision Tree ## 
