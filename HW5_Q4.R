##generate dataset
library(cluster)
set.seed(3)

## circle 1
t1 <- runif(200, 0, 2*pi)
x1 <- cbind(1*cos(t1) + rnorm(200, 0, 0.05), 1*sin(t1) + rnorm(200, 0, 0.05))

# circle 2
t2 <- runif(200, 0, 2*pi)
x2 <- cbind(2*cos(t2) + rnorm(200, 0, 0.05), 2*sin(t2) + rnorm(200, 0, 0.05))

data <- rbind(x1, x2)
plot(data)

##(a)
km <- kmeans(data, centers = 2)
plot(data, col=km$cluster)
##half of each circle belongs to one cluster

##(b)
##Construct epsilon-neighborhood graph
library(igraph)
eps <- 0.5
N <- lapply(1:nrow(data), function(i) which(D[i,] <= eps))
G <- graph.empty(n = nrow(data), directed = FALSE)
for (i in 1:nrow(data)) {
  for (j in N[[i]]) {
    if (i != j) {
      G <- add.edges(G, c(i,j))
    }
  }
}
G = G + t(G) 
G[ G == 2 ] = 1
degrees = colSums(G) 
n = nrow(G)
##Laplacian matrix
L = ( diag(n) - diag(degrees^(-1/2)) %*% G %*% diag(degrees^(-1/2)) )
##eigenvectors
eigenvectors = eigen(L, symmetric = TRUE)
n = nrow(L)
eigenvectors = eigenvectors$vectors[,(n - 2):(n - 1)]
##kmeans on eigenvectors
set.seed(11)
sc = kmeans(eigenvectors, 2)
sc_results = cbind(data, cluster = as.factor(sc$cluster))
plot(sc_results, col=sc$cluster)

##(c)
library(Matrix)
##Construct knn graph
S <- as.matrix(dist(data))
D <- matrix(0, nrow=nrow(data), ncol = nrow(data))

for (i in 1:nrow(data)) {
  #k=10
  index <- order(S[i,])[2:7]
  D[i,][index] <- 1 
}
D = D + t(D) 
D[ D == 2 ] = 1
degrees = colSums(D) 
n = nrow(D)

##Laplacian matrix
L = ( diag(n) - diag(degrees^(-1/2)) %*% D %*% diag(degrees^(-1/2)) )
##eigenvectors
eigenvectors = eigen(L, symmetric = TRUE)
n = nrow(L)
eigenvectors = eigenvectors$vectors[,(n - 2):(n - 1)]
##kmeans on eigenvectors
set.seed(12)
sc = kmeans(eigenvectors, 2)
sc_results = cbind(data, cluster = as.factor(sc$cluster))
plot(sc_results, col=sc$cluster)

##when k=10, the two circles fall into different clusters
##however, when k is smaller or larger, part of the outer circle is in the same cluster with the inner circle
##a nice range of k would be around 10

(d)
x3 <- cbind(1*cos(t1) + rnorm(200, 0, 0.2), 1*sin(t1) + rnorm(200, 0, 0.05))
x4 <- cbind(2*cos(t2) + rnorm(200, 0, 0.2), 2*sin(t2) + rnorm(200, 0, 0.05))
data2 <- rbind(x3, x4)
plot(data2)

library(igraph)
eps <- 0.5
N <- lapply(1:nrow(data2), function(i) which(D[i,] <= eps))
G <- graph.empty(n = nrow(data2), directed = FALSE)
for (i in 1:nrow(data2)) {
  for (j in N[[i]]) {
    if (i != j) {
      G <- add.edges(G, c(i,j))
    }
  }
}
G = G + t(G) 
G[ G == 2 ] = 1
degrees = colSums(G) 
n = nrow(G)
##Laplacian matrix
L = ( diag(n) - diag(degrees^(-1/2)) %*% G %*% diag(degrees^(-1/2)) )
##eigenvectors
eigenvectors = eigen(L, symmetric = TRUE)
n = nrow(L)
eigenvectors = eigenvectors$vectors[,(n - 2):(n - 1)]
##kmeans on eigenvectors
set.seed(11)
sc = kmeans(eigenvectors, 2)
sc_results = cbind(data2, cluster = as.factor(sc$cluster))
plot(sc_results, col=sc$cluster)

S <- as.matrix(dist(data2))
D <- matrix(0, nrow=nrow(data2), ncol = nrow(data2))
for (i in 1:nrow(data2)) {
  #k=10
  index <- order(S[i,])[2:11]
  D[i,][index] <- 1 
}
D = D + t(D) 
D[ D == 2 ] = 1
degrees = colSums(D) 
n = nrow(D)
##Laplacian matrix
L = ( diag(n) - diag(degrees^(-1/2)) %*% D %*% diag(degrees^(-1/2)) )
##eigenvectors
eigenvectors = eigen(L, symmetric = TRUE)
n = nrow(L)
eigenvectors = eigenvectors$vectors[,(n - 2):(n - 1)]
##kmeans on eigenvectors
set.seed(12)
sc = kmeans(eigenvectors, 2)
sc_results = cbind(data2, cluster = as.factor(sc$cluster))
plot(sc_results, col=sc$cluster)
