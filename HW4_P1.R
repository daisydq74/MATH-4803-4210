##(a)
library(MASS)
library(tree)
data(Boston)
set.seed(1)

##splitting data set
train_idx <- sample(nrow(Boston), nrow(Boston)/2)
train <- Boston[train_idx, ]
test <- Boston[-train_idx, ]

##fit and fint the mse
tree_fit <- tree(medv ~ ., data = train)
summary(tree_fit)
pred <- predict(tree_fit, newdata = test)
mse <- mean((test$medv - pred)^2)
mse

##(b)
library(randomForest)
set.seed(2)

##when the number of trees is 25
rf_25 <- randomForest(medv ~ ., data = train, mtry = 6, ntree = 25)
summary(rf_25)
pred_25 <- predict(rf_25, newdata = test)
mse_25 <- mean((test$medv - pred_25)^2)
mse_25

##when the number of trees is 100
rf_100 <- randomForest(medv ~ ., data = train, mtry = 6, ntree = 100)
summary(rf_100)
pred_100 <- predict(rf_100, newdata = test)
mse_100 <- mean((test$medv - pred_100)^2)
mse_100

##(c)
##fit and fint the mse
set.seed(3)
tree_fit2 <- tree(crim ~ ., data = train)
summary(tree_fit2)
pred2 <- predict(tree_fit2, newdata = test)
mse2 <- mean((test$crim - pred2)^2)
mse2

##(d)
set.seed(4)

##when the number of trees is 25
rf_25_2 <- randomForest(crim ~ ., data = train, mtry = 6, ntree = 25)
summary(rf_25_2)
pred_25_2 <- predict(rf_25_2, newdata = test)
mse_25_2 <- mean((test$crim - pred_25_2)^2)
mse_25_2

##when the number of trees is 100
rf_100_2 <- randomForest(crim ~ ., data = train, mtry = 6, ntree = 100)
summary(rf_100_2)
pred_100_2 <- predict(rf_100_2, newdata = test)
mse_100_2 <- mean((test$crim - pred_100_2)^2)
mse_100_2
