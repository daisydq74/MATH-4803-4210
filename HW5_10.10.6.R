##(a)
b <- seq(-6,6,.1)
R <- sin(b) + b/10
plot(x=b, y=R, type = "l")

##(b)
##The derivative is cos(beta) + 1/10

##(c)
rho = 0.1
b_gd <- rep(2.3, 101)
for (i in 1:100){
  gd <- rho*(cos(b_gd[i])+(1/10))
  b_gd[i+1] <- b_gd[i] - gd
}
plot(x=seq(1,101,1), y=b_gd, type = "l")
which.max(b_gd)
b_gd[101]
##the result is 4.612034

##(d)
##repeat (c) with 1.4
rho = 0.1
b_gd <- rep(1.4, 101)
for (i in 1:100){
  gd <- rho*(cos(b_gd[i])+(1/10))
  b_gd[i+1] <- b_gd[i] - gd
}
plot(x=seq(1,101,1), y=b_gd, type = "l")
which.min(b_gd)
b_gd[101]
##the result is -1.670452