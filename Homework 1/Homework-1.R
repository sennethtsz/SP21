# install.packages("kernlab")
# install.packages("kknn")
library(kernlab)
library(kknn)

# Load data
data <- read.table("C:/Users/Admin/Desktop/MM/Homework 1/credit_card_data.txt")
head(data)

# model function
svm_model <- function(ker, c){
  model <<- ksvm(x = as.matrix(data[,1:10]),
                y = as.factor(data[,11]),
                type = "C-svc",
                kernel = ker,
                C = c,
                scaled = TRUE)
  pred <- predict(model,data[,1:10])
  acc <- (sum(pred == data[,11]) / nrow(data))*100
  print(acc)
}

# Question 2.2.1
# ran 10 models with margins C=100, 200, 300, 400, 500, 600, 700, 800, 900, 1000
for (x in seq(100, 1000, 100)){
  print(paste("For C=", x))
  svm_model("vanilladot", x)
}
# calculate a1.am at c=100
svm_model("vanilladot", 100)
a <- colSums(model@xmatrix[[1]] * model@coef[[1]])
a
# calculate a0
a0 <- model@b
a0


# Question 2.2.2
# using Radial Basis Kernel in SVM
svm_model("rbfdot", 100)

# using Polynomial Kernel in SVM
svm_model("polydot", 100)


# Question 2.2.3
kknn_model <- function(k){
  pred <- list()
  for (i in 1:nrow(data)){ # every row in data
    model <- kknn(V11~., #v11 a function of v1-10
                   data[-i,], # using all other data point to train
                   data[i,], # train every data point, every row
                   k = k,
                   scale = TRUE) 
    pred[i] <- as.integer(round(fitted(model))) # predictions given 0-1, round predictions to either 0 or 1 and insert  into list
  }
  
  acc <- sum(pred == data[,11]) / nrow(data) * 100 # compare with dataset to get accuracy
  return(acc)
}

acc_list = list()
for (i in 1:20){ # running model with 1-20 k-values
  acc_list[i] <- kknn_model(i)  # list of accuracy for every k-value
}

acc_list
max(unlist(acc_list))
which.max(acc_list)
