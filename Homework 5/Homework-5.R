rm(list=ls())
library(GGally)

set.seed(42069)

# load data
data = read.table("C:/Users/Admin/Desktop/MM/Homework 5/uscrime.txt", 
                  stringsAsFactors = FALSE,
                  header = TRUE)
head(data)


# test data
test = data.frame(M=14.0, So=0, Ed=10.0, Po1=12.0, Po2=15.5, LF=0.640,
                  M.F=94.0, Pop=150, NW=1.1, U1=0.120, U2=3.6, 
                  Wealth=3200, Ineq=20.1, Prob=0.040, Time = 39.0)


# build model
model = glm(Crime~., data=data)
summary(model)
data %>% ggpairs(columns = c('Crime', 'M', 'Po1', 'U2', 'Ineq', 'Prob'),
                 lower=list(continuous='smooth'))

# model fit
tss = sum((data$Crime - mean(data$Crime))^2) # total sum of squared
rss = sum((model$residuals)^2) # residual sum of squared
rsq = 1 - rss/tss


# building model with significant variables
cmodel = glm(Crime~M+Ed+Po1+U2+Ineq+Prob, data=data)
summary(cmodel)

crss = sum((cmodel$residuals)^2) # residual sum of squared
crsq = 1 - crss/tss


# prediction
predict(cmodel, test)
