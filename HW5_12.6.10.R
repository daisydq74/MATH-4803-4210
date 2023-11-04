library(MASS)

##(a)
set.seed(1)

dataset <- rbind(matrix(rnorm(20*50, mean = 2), nrow = 20),
                      matrix(rnorm(20*50, mean= 1.3), nrow = 20),
                      matrix(rnorm(20*50, mean=2.7), nrow = 20))

##(b)
dataset.pca = prcomp(dataset)$x
plot(dataset.pca[,1:2], col=c(rep(1,20), rep(2,20), rep(3,20)))

##(c)
km <- kmeans(dataset, centers = 3)
#comparison
table(km$cluster, rep(c(1:3), each = 20))
##    1  2  3
## 1  0  0 20
## 2  0 20  0
## 3 20  0  0 the clusters are exactly the same as the construction

##(d)
km_2 <- kmeans(dataset, centers = 2)
#comparison
table(km_2$cluster, rep(c(1:3), each = 20))
##    1  2  3
## 1 15  0 20
## 2  5 20  0
## class 2 and 3 are classified into different clusters
## but class 1 were divided into 2 different clusters with class 2 and 3

##(e)
km_4 <- kmeans(dataset, centers = 4)
#comparison
table(km_4$cluster, rep(c(1:3), each = 20))
##    1  2  3
## 1  0  0  6
## 2  0  0 14
## 3  0 20  0
## 4 20  0  0 overfit

## class 1 and 2 are classified into different clusters
## but class 1 is classified into 2 other clusters

##(f)
km_pca <- kmeans(dataset.pca[, 1:2], centers = 3)
##comparison
table(km_pca$cluster, rep(c(1:3), each = 20))
##    1  2  3
## 1  0  0 20
## 2  0 20  0
## 3 20  0  0 the clusters are exactly the same as the construction

##(g)
km_scale <- kmeans(scale(dataset), centers = 3)
##comparison
table(km_scale$cluster, rep(c(1:3), each = 20))
##    1  2  3
## 1  0  0 20
## 2 20  0  0
## 3  0 20  0 same as before, the clusters are exactly the same as the construction

