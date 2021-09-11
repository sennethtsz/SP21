# Question 10.1.1
rm(list = ls())
library(DAAG)
library(tree)
set.seed(42069)

data = read.table("C:/Users/Admin/Desktop/MM/Homework 7/uscrime.txt",
                  stringsAsFactors = FALSE, 
                  header = TRUE)
head(data)

model = tree(Crime~., data = data)
summary(model)

# tree splits
model$frame

# Visualizing regression tree
plot(model)
text(model)

# validating 7-leaf node model using cross-validation
cv.model = cv.tree(model)
plot(cv.model$size, cv.model$dev, type = 'b')

# having 7 nodes yield least deviation in testing sets, no pruning required
pred = predict(model, data)

# visualizing predictions to actual
plot(data$Crime, pred)
abline(lm(pred~data$Crime))



# Question 10.1.2
rm(list = ls())
library(randomForest)
set.seed(42069)

data = read.table("C:/Users/Admin/Desktop/MM/Homework 7/uscrime.txt",
                  stringsAsFactors = FALSE, 
                  header = TRUE)
head(data)

model = randomForest(Crime~.,
                     data = data,
                     mtry = floor(ncol(data)/3), # default of p no. of cols/3
                     importance = TRUE)
model

# visualizing predictions
pred = predict(model, data)
plot(data$Crime, pred)
abline(lm(pred~data$Crime))

# r-squared
SSR = sum((pred - data$Crime)^2)
SST = sum((data$Crime - mean(data$Crime))^2)
r = 1 - SSR/SST # r=0.88

# variable importance in model
importance(model)



# Question 10.3
rm(list=ls())
library(pROC)
set.seed(42069)
data = read.table("C:/Users/Admin/Desktop/MM/Homework 7/germancredit.txt",
                  sep = " ")
head(data)

# V21 is the default indicator
data$V21[data$V21==1] = 0
data$V21[data$V21==2] = 1

# train:valid split 70:30
train.idx = sample(nrow(data), size=nrow(data)*0.7)
train = data[train.idx,]
test = data[-train.idx,]

# logistic model
model = glm(V21~.,
          family = binomial(link = "logit"),
          data = train)
summary(model)

# model with significant predictors
model = glm(V21~V1+V2+V3+V4+V5+V6+V7+V8+V14+V15+V20,
          family = binomial(link = "logit"),
          data=train)
summary(model)

# validating model on test data
pred = predict(model, test, type = "response")
pred # values between 0-1

# confusion matrix
pred_round = round(pred)
cm = table(pred_round, test$V21)
cm

# model accuracy
acc = (cm[1,1]+cm[2,2])/sum(cm)
acc

# ROC curve
rc = roc(test$V21, pred)
plot(rc, main = "ROC Curve")
rc # AUC


# part 2
# The loss of incorrectly classifying a "bad" customer is 5x
loss = c()
for(i in 1:100){
  pred_round = as.integer(pred > (i/100)) # rounding using as.integer
  cm = table(pred_round, test$V21)
  if (nrow(cm) > 1){false_pos = cm[2,1]} else {false_pos = 0}
  if (ncol(cm) > 1){false_neg = cm[1,2]*5} else {false_neg = 0}
  loss[i] = false_neg + false_pos
}

# visualizing threshold to loss
plot(c(1:100)/100,
     loss,
     xlab = "Threshold",
     ylab = "Loss",
     main = "Loss vs Threshold")

min(loss) # lowest loss score
which.min(loss) # threshold with lowest loss