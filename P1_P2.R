library(ISLR2)
simple <- lm(mpg ~ horsepower, data = Auto)
summary(simple) ##simple linear regression

##creating a scatterplot and adding the fitting line
plot(Auto$horsepower, Auto$mpg, main = "Scatterplot of mpg vs. horsepower", xlab = "horsepower", ylab = "mpg", col = "blue")
abline(simple, col="red")

##creating a colorplot
AutoCor = cor(Auto[1:8])
round(AutoCor, 2)
corrplot(AutoCor, method="color")
corrplot(AutoCor, method="number")

##multiple linear regression
multiple <- lm(mpg ~ . - name, data = Auto)
summary(multiple)
