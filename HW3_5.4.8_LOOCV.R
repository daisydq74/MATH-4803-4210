##(a)
set.seed(1)
x <- rnorm(100)
y <- x - 2 * x^2 + rnorm(100)

##(b) scatterplot
plot(x, y)

##(c)
library(boot)
set.seed(2)
data <- data.frame(x, y)

##i.y=β_0+β_1x+ε
fit_1 <- glm(y ~ x)
cv.glm(data, fit_1)$delta[1]

##ii.y=β_0+β_1x+β_2x^2+ε
fit_2 <- glm(y ~ poly(x, 2))
cv.glm(data, fit_2)$delta[1]

##iii.y=β_0+β_1x+β_2x^2+β_3x^3+ε
fit_3 <- glm(y ~ poly(x, 3))
cv.glm(data, fit_3)$delta[1]

##iv.y=β_0+β_1x+β_2x^2+β_3x^3+β_4x^4+ε
fit_4 <- glm(y ~ poly(x, 4))
cv.glm(data, fit_4)$delta[1]

##(d)
##repeat with a diffferent seed
set.seed(3)
data <- data.frame(x, y)

##i.y=β_0+β_1x+ε
fit_1 <- glm(y ~ x)
cv.glm(data, fit_1)$delta[1]

##ii.y=β_0+β_1x+β_2x^2+ε
fit_2 <- glm(y ~ poly(x, 2))
cv.glm(data, fit_2)$delta[1]

##iii.y=β_0+β_1x+β_2x^2+β_3x^3+ε
fit_3 <- glm(y ~ poly(x, 3))
cv.glm(data, fit_3)$delta[1]

##iv.y=β_0+β_1x+β_2x^2+β_3x^3+β_4x^4+ε
fit_4 <- glm(y ~ poly(x, 4))
cv.glm(data, fit_4)$delta[1]

##(f)
summary(fit_1)
summary(fit_2)
summary(fit_3)
summary(fit_4)

