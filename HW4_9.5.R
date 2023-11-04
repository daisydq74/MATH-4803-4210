library(MASS)

##(b)
set.seed(1)
X <- matrix(rnorm(1000), ncol=10)

##(c)
library(rpart)
m <- c(1, 5, 10)
df <- matrix(0, nrow = 1, ncol = length(m))
y <- rnorm(100, mean = 0, sd = 1)

for (j in 1:3) {
  for (i in 1:10) {
    fit <- rpart(y ~ X[,i], control = rpart.control(maxdepth= m[j]))
    pred <- predict(fit)
    df[, j] <- df[, j]+ cov(y, pred)/var(y)
  }
  df[, j] <- df[, j]/10
}
df

