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

##(d)
require(MASS)
#creating lda model
lda_mod <- lda(mpg01 ~ cylinders + weight + displacement + horsepower, 
               data = train)
lda_result <- predict(lda_mod, test)

#finding the error rate
mean(lda_result$class == test$mpg01)

##(e)
#creating qda model
qda_mod <- qda(mpg01 ~ cylinders + weight + displacement + horsepower,
               data = train)
qda_result <- predict(qda_mod, test)

#finding the error rate
mean(qda_result$class == test$mpg01)

##(g)
library(e1071)
##creating a naive bayes model
nai_bay_mod <- naiveBayes(mpg01 ~ cylinders + weight + displacement + horsepower,
                          data = train)
nai_bay_result <- predict(nai_bay_mod, test)

#finding the error rate
mean(nai_bay_result == test$mpg01)
