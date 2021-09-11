rm(list=ls())
set.seed(42069)

library(caret)
library(glmnet)

data = read.table("C:/Users/Admin/Desktop/MM/Homework 8/uscrime.txt",
                  stringsAsFactors = FALSE, 
                  header = TRUE)
head(data)

# exclude scaling binary and target variable
data.scaled = as.data.frame(scale(data[,c(1,3,4,5,6,7,8,9,10,11,12,13,14,15)]))
data.scaled = cbind(data.scaled, data[,c(2,16)])
head(data.scaled)

# backwards stepwise
control = trainControl(method = 'repeatedcv',
                       number = 5,
                       repeats = 5)
model.backwards = train(Crime~.,
                        data = data.scaled,
                        method = 'lmStepAIC',
                        direction = 'backward',
                        trControl = control)
model = lm(Crime~M+Ed+Po1+U1+U2+Ineq+Prob, # variables from backwards stepwise
           data = data.scaled)
summary(model)

# forward stepwise
model.forward = train(Crime~.,
                      data = data.scaled,
                      method = 'glmStepAIC',
                      direction = 'forward',
                      trControl = control)
model = lm(Crime~Po1+Ineq+Ed+Prob+M, # variables from forward stepwise
           data = data.scaled)
summary(model)


# Lasso
model.lasso = cv.glmnet(x = as.matrix(data.scaled[,-16]),
                        y = as.matrix(data.scaled$Crime),
                        alpha = 1, # 1 for lasso
                        nfolds = 5,
                        type.measure = 'mse',
                        family = 'gaussian')
coef(model.lasso, s = model.lasso$lambda.min)
# dev.ratio = fraction of deviance explained, r squared, 1-dev/nulldev
r.lasso = model.lasso$glmnet.fit$dev.ratio[which(model.lasso$glmnet.fit$lambda == model.lasso$lambda.min)]
r.lasso


# Elastic net
dev = list()
for (i in 0:10){ # 0 for ridge, 1 for lasso
  model.elastic = cv.glmnet(x = as.matrix(data.scaled[,-16]),
                            y = as.matrix(data.scaled[,16]),
                            alpha = i/10,
                            nfolds = 5,
                            type.measure = 'mse',
                            family = 'gaussian')
  dev[i] = model.elastic$glmnet.fit$dev.ratio[which(model.elastic$glmnet.fit$lambda == model.elastic$lambda.min)]
}

# run model with best alpha identified
model.elastic = cv.glmnet(x = as.matrix(data.scaled[,-16]),
                          y = as.matrix(data.scaled[,16]),
                          alpha = which.max(dev)/10, # best alpha
                          nfolds = 5,
                          type.measure = 'mse',
                          family = 'gaussian')
coef(model.elastic, s = model.elastic$lambda.min)
r.elastic = model.lasso$glmnet.fit$dev.ratio[which(model.lasso$glmnet.fit$lambda == model.lasso$lambda.min)]
r.elastic
