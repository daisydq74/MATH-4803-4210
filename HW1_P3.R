library(ISLR2)
set.seed(1)

##(a.)
x <- rnorm(100)

##(b.)
eps <- rnorm(100, sd= sqrt(0.25))

##(c.)
y <- -1+0.5*x+eps
length(y)

##(d.)
plot(x, y)

##(e.)
fit <- lm(y ~ x)
summary(fit)

##(f.)
abline(fit, col = "red") #prediction
abline(-1, 0.5, col = "green") #real relationship
legend("topleft", c("Prediction", "Real Model"), col = c("red", "green"), lty = c(1, 1))

##(g.)
fit_2 <- lm(y ~ x + I(x^2))
summary(fit_2)

##(h.)
##repeat the exact same steps
##except changing the variance of eps to be smaller
set.seed(1)
x <- rnorm(100)
eps <- rnorm(100, sd= sqrt(0.1))
y <- -1+0.5*x+eps
plot(x, y)
fit_3 <- lm(y ~ x)
summary(fit_3)
abline(fit_3, col = "red") #prediction
abline(-1, 0.5, col = "green") #real relationship
legend("topleft", c("Prediction", "Real Model"), col = c("red", "green"), lty = c(1, 1))

##(i.)
##repeat the exact same steps
##except changing the variance of eps to be larger
set.seed(1)
x <- rnorm(100)
eps <- rnorm(100, sd= sqrt(0.4))
y <- -1+0.5*x+eps
plot(x, y)
fit_3 <- lm(y ~ x)
summary(fit_3)
abline(fit_3, col = "red") #prediction
abline(-1, 0.5, col = "green") #real relationship
legend("topleft", c("Prediction", "Real Model"), col = c("red", "green"), lty = c(1, 1))

