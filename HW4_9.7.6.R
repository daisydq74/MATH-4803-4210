##(a)
set.seed(1)
obs = 500
x1 <- runif(obs, min = 0, max = 10)
x2 <- runif(obs, min = 0, max = 10)
y <- ifelse(x2 > (x1-5) ^ 3+5, 0, 1)
data <- data.frame(x1 = x1, x2 = x2, y = as.factor(y))
train <- sample(obs, obs/2)
data_train <- data[train, ]
data_test <- data[-train, ]
par(mfrow = c(1,2))
plot(data_train$x1, data_train$x2, col = as.integer(data_train$y) + 1, main = 'training set')plot(data_test$x1, data_test$x2, col = as.integer(data_test$y) + 1, main = 'test set')

##(b)
library(e1071)
cost_num <- c(0.001, 0.01, 0.1, 1, 5, 10, 100, 10000)
tune_out <- tune(svm, y ~., data = data_train, kernel = 'linear', ranges = list(cost = cost_num))
summary(tune_out)
data.frame(cost = tune_out$performance$cost, misclass = tune_out$performance$error * 1100)

##(c)
##separate the two set with x1=x2
set.seed(2)
y_ <- ifelse(x2 >x1, 0, 1)
data_ <- data.frame(x1 = x1, x2 = x2, y_ = as.factor(y_))
data_train_ <- data_[train, ]
data_test_ <- data_[-train, ]
par(mfrow = c(1,2))
plot(data_train_$x1, data_train_$x2, col = as.integer(data_train_$y) + 1, main = 'training set')
plot(data_test_$x1, data_test_$x2, col = as.integer(data_test_$y) + 1, main = 'test set')

##repeat (b)
tune_out_ <- tune(svm, y_ ~., data = data_train_, kernel = 'linear', ranges = list(cost = cost_num))
summary(tune_out_)
data.frame(cost = tune_out_$performance$cost, misclass = tune_out_$performance$error * 1100)

