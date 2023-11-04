library(ISLR2)
attach(Boston)

##(a)
mu_est <- mean(medv)
mu_est

##(b)
mu_s.e <- sd(medv)/sqrt(dim(Boston)[1])
mu_s.e

##(c)
install.packages("boot")
library(boot)
set.seed(1)
boot_mean <- function(data, index) {
  mu <- mean(data[index])
  return (mu)
}
boot(Boston$medv, boot_mean, 1000)

##(d)
mu_CI <- c(mu_est-2*0.4106622, mu_est+2*0.4106622)
mu_CI
t.test(Boston$medv)

##(e)
mu_med_est <- median(Boston$medv)
mu_med_est

##(f)
##repeat (c)
boot_median <- function(data, index) {
  mu <- median(data[index])
  return (mu)
}
boot(Boston$medv, boot_median, 1000)

##(g)
mu_0.1_est <- quantile(medv, c(0.1))
mu_0.1_est

boot_quantile <- function(data, index) {
  mu <- quantile(data[index], c(0.1))
  return (mu)
}
boot(Boston$medv, boot_quantile, 1000)
