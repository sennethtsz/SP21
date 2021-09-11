rm(list=ls())

library(kernlab)
library(ggplot2)
set.seed(42069)

data = read.table("C:/Users/Admin/Desktop/MM/Homework 4/temps.txt", 
                  stringsAsFactors = FALSE,
                  header = TRUE)
head(data)
plot.ts(data[,2])

model = function(a, b, g, s){
  ts = ts(unlist(data$X1996, use.names=FALSE),
          start = 6,
          end = 10,
          frequency = 31)
  data.mean = HoltWinters(ts,
                          alpha = a,
                          beta = b,
                          gamma = g,
                          seasonal = s)
  print(paste("a =", data.mean$alpha))
  print(paste("b =", data.mean$beta))
  print(paste("g =", data.mean$gamma))
  plot(ts)
  lines(data.mean$fitted[,1], col='red')
}

# testing model fit
model(NULL, FALSE, FALSE, 'additive')

# second order with trend
model(NULL, NULL, FALSE, 'additive')

# third order with seasonality
model(NULL, NULL, NULL, 'additive')

# third order with multiplicative seasonality
model(NULL, FALSE, NULL, 'multiplicative')

# lower alpha value selected since daily temp is highly varying
# trend is always close to 0 indicating no significant trend
# seasonality does not smooth the data properly
model(0.1, FALSE, FALSE, NULL)

# loading predictions into data frame
ls = list()
for (i in 2:21){
  ts = ts(unlist(data[,i], use.names=FALSE),
          start = 6,
          end = 10,
          frequency = 31)
  data.mean = HoltWinters(ts,
                          alpha = 0.1,
                          beta = FALSE,
                          gamma = FALSE)
  ls[[i]] = as.numeric(data.mean$fitted[,1])
}
mx = do.call(cbind, ls)
df = as.data.frame(mx)

# visualizing smoothed temperature change across years
df$MEAN96_00 = (df$V1 + df$V2 + df$V3 + df$V4 + df$V5)/5
df$MEAN01_05 = (df$V6 + df$V7 + df$V8 + df$V9 + df$V10)/5
df$MEAN06_10 = (df$V11 + df$V12 + df$V13 + df$V14 + df$V15)/5
df$MEAN11_15 = (df$V16 + df$V17 + df$V18 + df$V19 + df$V20)/5

ggplot(df, aes(x = (1:nrow(df)))) +
  geom_line(aes(y = MEAN96_00, color = '1')) +
  geom_line(aes(y = MEAN01_05, color = '2')) +
  geom_line(aes(y = MEAN06_10, color = '3')) +
  geom_line(aes(y = MEAN11_15, color = '4')) +
  labs(title = 'Year Comparison',
       x = 'Days',
       y = 'Temperature',
       color = "Legend") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(name = 'Years', 
                     values = c('1'='blue', '2'='yellow', '3'='orange', '4'='red'), 
                     labels = c('1996-2000', '2001-2005', '2006-2010', '2011-2015'))
