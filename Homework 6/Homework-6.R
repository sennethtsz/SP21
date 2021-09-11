rm(list=ls())

library(GGally)
library(corrplot)
library(DAAG)

data = read.table("C:/Users/Admin/Desktop/MM/Homework 6/uscrime.txt", 
                  stringsAsFactors = FALSE,
                  header = TRUE)
head(data)

# visualizing correlated variables
corr_matrix = cor(data)
corrplot(corr_matrix, method='circle', is.corr = FALSE)
ggpairs(data, columns = c('Crime', 'Po1', 'Po2', 'Ed', 'NW', 'Wealth', 'Ineq'),
        lower=list(continuous='smooth'))

# pca
pca = prcomp(data[,1:15], scale.=TRUE)
summary(pca)
screeplot(pca, type='lines')

# building with 6 principal components
pca.data = cbind(pca$x[,1:6], data['Crime'])
model = lm(Crime~., data=pca.data)
summary(model)
SST = sum((data$Crime - mean(data$Crime))^2)
SSR = sum(model$residuals^2)

cvmodel = cv.lm(pca.data, model, m=5)
SSR.cv = attr(cvmodel, 'ms')*nrow(pca.data)

(1 - SSR/SST) - (1 - SSR.cv/SST)


# building with 4 significant principal components
pca.data = cbind(pca$x[,1:6][,c(1, 2, 4, 5)], data['Crime'])
model = lm(Crime~., data=pca.data)
summary(model)
SSR = sum(model$residuals^2)

cvmodel = cv.lm(pca.data, model, m=5)
SSR.cv = attr(cvmodel, 'ms')*nrow(pca.data)

(1 - SSR/SST) - (1 - SSR.cv/SST)


# reverse scaling
beta = model$coefficients
alpha.scaled = pca$rotation[,c(1, 2, 4, 5)] %*% beta[2:5]

alpha = alpha.scaled/sapply(data[,1:15], sd)
intercept = beta[1] - sum(alpha.scaled*sapply(data[,1:15], mean)/
                            sapply(data[,1:15], sd))
