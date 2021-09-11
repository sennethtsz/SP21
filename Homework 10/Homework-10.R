rm(list = ls())

library(DAAG)

set.seed(42069)

# load data
data = read.table("C:/Users/Admin/Desktop/MM/Homework 10/breast-cancer-wisconsin.data.txt",
                  stringsAsFactors = FALSE, 
                  header = FALSE,
                  sep = ",")
head(data)

# mode imputation
# find columns with missing rows
for (i in 1:11) {
  print(paste0("V",i))
  print(which(data[,i] == "?"))
}
plot(data$V7) # only v7 has missing values, categorical data

getmode = function(v) {
  uniqv = unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
mode = getmode(data$V7)

data_mode = data
index = which(data$V7 == "?")
data_mode$V7[index] = mode


# regression imputation
data_clean = data[-index,2:10]
data_clean$V7 = as.integer(data_clean$V7) # convert to integer for cv

# linear regression
model = lm(V7~., data_clean)
step(model) # step-wise variable selection

# model with significant variables
model = lm(V7~V2+V4+V5+V8, data_clean)
summary(model)

# cross validation
model_cv = cv.lm(data_clean,
                 model,
                 m = 5)
SST = sum((as.numeric(data[-index,]$V7) - mean(as.numeric(data[-index,]$V7)))^2)
r =  1 - attr(model_cv,"ms")*nrow(data[-index,])/SST
r

# prediction for missing value
pred = round(predict(model, data[index,]))
data_reg = data
data_reg[index,]$V7 = pred


# regression imputation with perturbation
pert = round(rnorm(nrow(data[index,]), 
             mean = mean(pred), 
             sd = sd(pred)))
pert
data_reg_pert = data
data_reg_pert[index,]$V7 = pert
data_reg_pert$V7[data_reg_pert$V7 < 1] = 1
data_reg_pert$V7[data_reg_pert$V7 > 10] = 10


# kknn model
library(kknn)

train.idx = sample(nrow(data), size = floor(nrow(data) * 0.7))
test.idx = setdiff(1:nrow(data), train.idx)

acc = matrix(ncol=5, nrow=5)
kknn = function(dataset, number) {
  for (k in 1:5) {
    knn_model = kknn(V11~., 
                     data[train.idx,], 
                     data[test.idx,],
                     k=k)
    pred = as.integer(round(fitted(knn_model)))
    acc[k,i] = sum(pred==data[test.idx,]$V11)/nrow(data[test.idx,])*100
  }
}

kknn(data_mode, 1)
kknn(data_reg, 2)
kknn(data_reg_pert, 3)
acc


# cluster on data without missing variables
data_clean = data[-index,]
train.idx = sample(nrow(data_clean), size = floor(nrow(data_clean) * 0.7))
test.idx = setdiff(1:nrow(data_clean), train.idx)

kknn(data_clean, 4)


# cluster on data for replaced binary missing category
data_binary = data
data_binary$V7[data_binary$V7 == "?"] = 0 # interaction term
data_binary$V12[data_binary$V7 == 0] = 0 # binary term
data_binary$V12[data_binary$V7 != 0] = 1

train.idx = sample(nrow(data_binary), size = floor(nrow(data_binary) * 0.7))
test.idx = setdiff(1:nrow(data_binary), train.idx)

kknn(data_binary, 5)
acc

# there isn't a significant difference in model performance between how missing
# data is being handled. Clusters might be more important.