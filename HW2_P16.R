library(MASS)
attach(Boston)

#create a binary variable for 'crim'
#'1' if above median, '0' if below median
crim01 <- ifelse(Boston$crim > median(Boston$crim), yes = 1, no = 0)
#addd the variable into the data set
Boston <- data.frame(Boston, crim01)

##split into two random sets
smp_size <- floor(0.5 * nrow(Boston))
sample <- sample(seq_len(nrow(Boston)), size = smp_size)

##assign one set to be training and one to be testing
train <- Boston[sample, ]
test <- Boston[-sample, ]

##logistic regression
log_reg <- glm(crim01 ~ . - crim01 - crim, data = train, family = binomial)
summary(log_reg)

##predict test set
lr.pred <- predict(log_reg, test, type = "response")
lr.result <- rep(0, length(lr.pred))
lr.result[lr.pred > 0.5] <- 1
mean(lr.result != test$crim01)

##KNN
library(class)
set.seed(3)
train.x <- train[,c('zn', 'indus', 'chas', 'nox', 'rm', 'age', 'dis', 
                    'rad', 'tax', 'ptratio', 'lstat', 'medv')]
train.y <- train$crim0
test.x <- test[,c('zn', 'indus', 'chas', 'nox', 'rm', 'age', 'dis', 
                  'rad', 'tax', 'ptratio', 'lstat', 'medv')]

##k=1
knn.1 <- knn(train.x, test.x, train.y, k = 1)
mean(knn.1 != test$crim01)

##k=5
knn.5 <- knn(train.x, test.x, train.y, k = 5)
mean(knn.5 != test$crim01)

##k=20
knn.20 <- knn(train.x, test.x, train.y, k = 20)
mean(knn.20 != test$crim01)

##k=100
knn.100 <- knn(train.x, test.x, train.y, k = 100)
mean(knn.100 != test$crim01)

