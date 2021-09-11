library(kernlab)
library(kknn)

set.seed(42069)

# load data
data <- read.table("C:/Users/Admin/Desktop/MM/Homework 2/credit_card_data.txt")
head(data)

# question 3.1a
train.idx <- sample(nrow(data), size=nrow(data)*0.8)
train <- data[train.idx,]
test <- data[-train.idx,]

# leave-one-out cross validation method
kmax = 50
kknn.loocv <- train.kknn(V11~.,
                         train,
                         kmax=kmax,
                         scale=TRUE)
acc.list <- list()
for (k in 1:kmax){
  predicted <- round(fitted(kknn.loocv)[[k]])
  acc.list[k] <- sum(predicted == train[,11])/nrow(train)*100
}

print(paste('Highest Accuracy:', max(unlist(acc.list)), '%'))
print(paste('K-value with Highest Accuracy:', which.max(acc.list)))

# prediction on test set
kknn.loocv <- train.kknn(V11~.,
                         train,
                         ks=12,
                         scale=TRUE)
predicted <- predict(kknn.loocv, test)
acc <- sum(round(predicted) == test[,11])/nrow(test)*100

print(paste('Accuracy on test set:', acc, '%'))

# k-fold cross validation method
acc.list <- list()
for (k in 1:kmax){
  kknn.kfold <- cv.kknn(V11~.,
                        train,
                        k=k,
                        kcv=5, # 5-fold cross validation
                        scale=TRUE)
  
  predicted <- as.integer(round(kknn.kfold[[1]][,2]))
  acc <- sum(predicted == train[,11])/nrow(train)*100
  acc.list[k] <- acc
}

print(paste('Highest Accuracy:', max(unlist(acc.list)), '%'))
print(paste('K-value with Highest Accuracy:', which.max(acc.list)))

# prediction on test set
kknn.kfold <- kknn(V11~.,
                   train,
                   test,
                   k=17,
                   scale=TRUE)
predicted = predict(kknn.kfold)
acc = sum(round(predicted) == test[,11])/nrow(test)*100

print(paste('Accuracy on test set:', acc, '%'))


# question 3.1b
rm(list=ls())
library(kernlab)
library(kknn)

set.seed(42069)

# load data
data <- read.table("C:/Users/Admin/Desktop/MM/Homework 2/credit_card_data.txt")
head(data)

# train:valid:test split 70:20:10
train.idx = sample(nrow(data), size=nrow(data)*0.7)
train = data[train.idx,]

valid.test = data[-train.idx,]

valid.idx = sample(nrow(valid.test), size=nrow(valid.test)/3*2)
valid = valid.test[valid.idx,]
test = valid.test[-valid.idx,]

train.valid.idx = c(valid.idx, train.idx)
train.valid = data[train.valid.idx,]

# selecting kknn model on train and validation set
acc.list <- list()
for (k in 1:50){
  kknn.model <- kknn(V11~.,
                     train,
                     valid,
                     k=k,
                     scale=TRUE)
  predicted <- round(fitted(kknn.model))
  acc <- sum(predicted==valid[,11])/nrow(valid)*100
  acc.list[k] <- acc
}

print(paste('Highest Accuracy:', max(unlist(acc.list)), '%'))
print(paste('K-value with Highest Accuracy:', which.max(acc.list)))

# prediction on test data
kknn.model <- kknn(V11~.,
                   train,
                   test,
                   k=which.max(acc.list),
                   scale=TRUE)
predicted <- predict(kknn.model)
acc <- sum(round(predicted) == test[,11])/nrow(test)*100

print(paste('Accuracy on test set:', acc, '%'))

# selecting ksvm model on train and validation set
c <- c(0.001, 0.01, 0.1, 1, 10, 100, 1000)
acc.list <- list()
for (i in 1:length(c)){
  ksvm.model <- ksvm(x=as.matrix(train[,1:10]),
                     y=as.factor(train[,11]),
                     type='C-svc',
                     kernel='rbfdot',
                     C=c[i],
                     scaled=TRUE)
  predicted <- predict(ksvm.model, valid[,1:10])
  acc <- sum(predicted==valid[,11])/nrow(valid)*100
  acc.list[i] <- acc
}

print(paste('c value with highest accuracy:', c[which.max(acc.list)]))
print(paste('Highest accuracy:', max(unlist(acc.list)), '%'))

# prediction on test data
ksvm.model <- ksvm(x=as.matrix(valid.train[,1:10]),
                   y=as.factor(valid.train[,11]),
                   type='C-svc',
                   kernel='rbfdot',
                   C=0.1,
                   scaled=TRUE)
predicted <- predict(ksvm.model, test[,1:10])
acc <- sum(predicted==test[,11])/nrow(test)*100

print(paste('Accuracy on test set:', acc, '%'))


# question 4.2
rm(list=ls())
library(kernlab)
library(ggplot2)

set.seed(42069)

# load data
data = read.table("C:/Users/Admin/Desktop/MM/Homework 2/iris.txt")
head(data)
table(data[,5])

# visualize data
ggplot(data, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()
ggplot(data, aes(Sepal.Length, Sepal.Width, color = Species)) + geom_point()

# scaling data, (x-xmin)/(xmax-xmin)
data.scaled <- data
for (i in 1:4){
  data.scaled[i] <- (data[i]-min(data[i]))/(max(data[i]-min(data[i])))
}
head(data.scaled)
head(data)

# visualize scaled data
ggplot(data.scaled, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()
ggplot(data.scaled, aes(Sepal.Length, Sepal.Width, color = Species)) + geom_point()

# center selection
c <- c(1:10)
dist <- list()

for (i in 1:length(c)){
  model <- kmeans(as.matrix(data.scaled[,1:4]),
                  centers=i,
                  iter.max=10,
                  nstart=1)
  dist[i] <- model$tot.withinss
}

# plot elbow chart to find optimal number of clusters
plot(c,
     dist,
     pch=19,
     frame=FALSE,
     type='b',
     xlab='Centers',
     ylab='Distance between points to centers in each cluster')


# model with 3 centers
model <- kmeans(as.matrix(data.scaled[,1:4]),
                centers=3,
                iter.max=10,
                nstart=10)

data.scaled[6] <- model$cluster
colnames(data.scaled)[6] <- 'Predicted'

# visualize prediction
ggplot(data.scaled, aes(Petal.Length, Petal.Width, color = Predicted)) + geom_point()
ggplot(data.scaled, aes(Sepal.Length, Sepal.Width, color = Predicted)) + geom_point()

