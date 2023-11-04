library(ISLR2)
set.seed(1)
summary(Auto)

##(a)
mpg01 <- ifelse(Auto$mpg > median(Auto$mpg), yes = 1, no = 0)
Auto <- data.frame(Auto, mpg01) ##add the variable to Auto

##(b)
cor(Auto[,-9])
pairs(Auto[,-9])

##(c)
set.seed(2)

##split into two random sets
smp_size <- floor(0.5 * nrow(Auto))
sample <- sample(seq_len(nrow(Auto)), size = smp_size)

##assign one set to be training and one to be test
train <- Auto[sample, ]
test <- Auto[-sample, ]

##(f)
##create the modal
log_reg <- glm(mpg01 ~ cylinders + weight + displacement + horsepower,
               data = train, family = binomial)

##predict test set
lr.pred <-  predict(log_reg, test, type = "response")
lr.result <-  rep(0, length(lr.pred))
lr.result[lr.pred > 0.5] <- 1
mean(lr.result != test$mpg01)

##(h)
library(class)
set.seed(3)
train.x <- train[,c('cylinders', 'displacement', 'horsepower', 'weight', 'year')]
train.y <- train$mpg0
test.x <- test[,c('cylinders', 'displacement', 'horsepower', 'weight', 'year')]

##k=1
knn.1 <- knn(train.x, test.x, train.y, k = 1)
mean(knn.1 != test$mpg01)

##k=5
knn.5 <- knn(train.x, test.x, train.y, k = 5)
mean(knn.5 != test$mpg01)

##k=20
knn.20 <- knn(train.x, test.x, train.y, k = 20)
mean(knn.20 != test$mpg01)

##k=100
knn.100 <- knn(train.x, test.x, train.y, k = 100)
mean(knn.100 != test$mpg01)

#finding the best k=n by trying each value and create a plot
k_error <- rep(NA, 200)
for (n in 1:200){
  knn.n <-  knn(train.x, test.x, train.y, k = n)
  k_error[n] <- mean(knn.n != test$mpg01)
}

k_num <- c(1:200)
plot(k_num, k_error, type="o", pch=20, cex=0.5)
