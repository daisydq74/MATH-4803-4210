library(ISLR2)
attach(Boston)

##(a)
fit <- lm(nox ~ poly(dis, 3), data = Boston)
summary(fit)

plot(nox ~ dis, data = Boston, col="grey")
dis_range <- range(Boston$dis)
dis_grid <- seq(from = dis_range[1], to = dis_range[2])
pred <- predict(fit, newdata = list(dis=dis_grid))
lines(dis_grid, pred, col = "red", lwd = 2)

##(b)
##iterate the number of degree
##to create a list of models and their corresponding RSS

rss <- list()
fit_b <- list()
par(mfrow = c(2, 5))
for (i in 1:10) {
  fit_b[[i]] <- lm(nox ~ poly(dis, i), data = Boston)
  plot(nox ~ dis, data = Boston, col="grey")
  pred <- predict(fit_b[[i]], newdata = list(dis=dis_grid))
  lines(dis_grid, pred, col = "red", lwd = 2)
  rss[[i]] <- sum(fit_b[[i]]$residuals^2)
}

par(mfrow = c(1, 1))
plot(1:10, rss, xlab = "polynomial degree", ylab = "RSS")

##(c)
library(boot)
set.seed(2)
cv_er <- rep(NA,10)

##use for loop to find the optimal degree
for (i in 1:10){
  cv_fit <- glm(nox ~ poly(dis,i), data=Boston)
  cv_er[i] <- cv.glm(Boston, cv_fit, K=10)$delta[1]
}
cv_er
min_error <- which.min(cv_er)
min_error

##(d)
library(splines)
spline_fit <- lm(nox ~ bs(dis, df = 4), data = Boston)
summary(spline_fit)

plot(nox ~ dis, data = Boston, col="grey")
pred <- predict(spline_fit, newdata = list(dis=dis_grid))
lines(dis_grid, pred, col = "red", lwd = 2)

##(e)
##iterate the degree of freedom
##to create a list of models and their corresponding RSS

rss_list <- list()
fit_list <- list()
par(mfrow = c(2, 4))
for (i in 1:8) {
  fit_list[[i]] <- lm(nox ~ bs(dis, df = i+2), data = Boston)
  plot(nox ~ dis, data = Boston, col="grey")
  pred <- predict(fit_list[[i]], newdata = list(dis=dis_grid))
  lines(dis_grid, pred, col = "red", lwd = 2)
  rss_list[[i]] <- sum(fit_list[[i]]$residuals^2)
}

##plot the RSS v.s. deg of freedom
par(mfrow = c(1, 1))
plot(3:10, rss_list[3:10], xlab = "degree of freedom", ylab = "RSS")

##(f)
set.seed(3)
cv_er_list <- rep(NA,8)

##use for loop to find the optimal degree
for (i in 1:8){
  fit_list2 <- glm(nox ~ bs(dis,df=i+2), data=Boston)
  cv_er_list[i] <- cv.glm(Boston, fit_list2, K=8)$delta[1]
}
cv_er_list
min_error <- which.min(cv_er_list)
min_error
