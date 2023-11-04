library(ISLR2)
attach(Wage)
library(boot)

##(a)
##perform cv and find the best fit polynomial degree
set.seed(2)
cv_er <- rep(0,10)

##use for loop to find the optimal degree
for (i in 1:10){
  cv_fit <- glm(wage ~ poly(age,i), data=Wage)
  cv_er[i] <- cv.glm(Wage, cv_fit, K=10)$delta[1]
}
cv_er
min_error <- which.min(cv_er)
min_error ##the result is degree 5

##plot the database
plot(wage ~ age, data = Wage, col="grey")
age_range <- range(Wage$age)
age_grid <- seq(from = age_range[1], to = age_range[2])
fit_5 <- lm(wage ~ poly(age, 5), data = Wage)
pred <- predict(fit_5, newdata = list(age=age_grid))
lines(age_grid, pred, col = "red", lwd = 2)

##(b)
set.seed(3)
cv_er2 <- rep(NA,10)

##use for loop to find the optimal number of cuts
for (i in 2:10) {
  Wage$age_cut <- cut(Wage$age, i)
  cv_fit <- glm(wage~ age_cut, data = Wage)
  cv_er2[i] <- cv.glm(Wage, cv_fit, K=10)$delta[1]
}
cv_er2
min_error2 <- which.min(cv_er2)
min_error2

##fit the new prediction
fit_cut <- glm(wage ~ cut(age, 8), data = Wage)
pred2 <- predict(fit_cut, newdata = list(age=age_grid))
lines(age_grid, pred2, col = "blue", lwd = 2)
