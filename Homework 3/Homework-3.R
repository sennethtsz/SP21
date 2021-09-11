# Question 5.1
library(outliers)
library(ggplot2)

data = read.table("C:/Users/Admin/Desktop/MM/Homework 3/uscrime.txt", 
                  stringsAsFactors = FALSE,
                  header = TRUE)
head(data)

hist(data[,'Crime'],
     main = 'Crime Rate Distribution',
     xlab = 'Crime Rate',
     breaks = nrow(data),
     xlim = c(min(data[,'Crime']), max(data[,'Crime'])))

boxplot(data[,'Crime'])

# 2-tailed test
grubbs.test(data[,'Crime'],type=11)

# 1-tailed test
grubbs.test(data[,'Crime'],type=10, opposite = FALSE)
grubbs.test(data[,'Crime'],type=10, opposite = TRUE)

data.new = data[-which.max(data[,'Crime']),]
grubbs.test(data.new[,'Crime'],type=10, opposite = FALSE)

data.new = data.new[-which.max(data.new[,'Crime']),]
grubbs.test(data.new[,'Crime'],type=10, opposite = FALSE)

# Compare distribution
b = min(c(data[,'Crime'], data.new[,'Crime']))
e = max(c(data[,'Crime'], data.new[,'Crime']))
ax = pretty(b:e, n=12)

hist1 = hist(data[,'Crime'], breaks = ax, plot = FALSE)
hist2 = hist(data.new[,'Crime'], breaks = ax, plot = FALSE)

c1 = rgb(0, 0, 255, max = 255, alpha = 70)
c2 = rgb(0, 255, 0, max = 255, alpha = 70)

plot(hist1, col = c1)
plot(hist2, col = c2, add = TRUE)


# Question 6.2
rm(list=ls())
data = read.table("C:/Users/Admin/Desktop/MM/Homework 3/temps.txt",
                  stringsAsFactors = FALSE,
                  header = TRUE)
head(data)


