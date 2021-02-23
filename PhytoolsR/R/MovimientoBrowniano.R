getwd()
setwd("C:/Users/UIS/Downloads/BiologiaComparada/")
install.packages("phytools")
install.packages("fastBM")

library("phytools")
library("ape")
library("maps")
library("fastBM")


#Objeto con el numero de generaciones
t <- 0:100
#Tasa de cambio. Cuando es pequeña, el estado de cambio del caracter es pequeño
sig2 <- 0.01

#Serie de cambios en estado del caracter en eltiempo t. Set de simulaciones estandar al azar.
x <- rnorm(n = length(t) - 1, sd = sqrt(sig2))
x
stepwise_values <- numeric()
stepwise_values[1] <- 0
stepwise_values[2] <- stepwise_values[1] + x[1]
stepwise_values[3] <- stepwise_values[2] + x[2]
stepwise_values[4] <- stepwise_values[3] + x[3]
stepwise_values[5] <- stepwise_values[4] + x[4]

abs_max <- max(abs(stepwise_values))
plot(1, stepwise_values[1],
     xlim = c(0,5), ylim = c(-abs_max,abs_max),
     xlab = "t", ylab = "trait value",
     pch = 20, type = "o")
points(c(2:5), stepwise_values[2:5], pch = 20)
lines(1:5, stepwise_values)

x <- c(0, cumsum(x))
plot(t, x, type = "l", ylim = c(-3, 3))
x <- rnorm(n = length(t) - 1, sd = sqrt(sig2))
x <- c(0, cumsum(x))
plot(t, x, type = "l", ylim = c(-2, 2))



#Objeto con el numero de generaciones
t <- 0:100
#Tasa de cambio. Cuando es pequeña, el estado de cambio del caracter es pequeño
sig2 <- 0.01
#Numero de simulaciones de la matriz de simulaciones
nsim <- 100

X <- matrix(rnorm(n = nsim * (length(t) - 1), sd = sqrt(sig2)), nsim, length(t) - 1)
X
sim_matrix <- cbind(rep(0, nsim), t(apply(X, 1, cumsum))) #matrix of simulations
evaluar <- apply(X, 1, cumsum)
evaluar2 <- cbind(rep(0, nsim), rep(0, nsim))
?cbind

plot(t, sim_matrix[1, ], xlab = "time", ylab = "phenotype", ylim = c(-3, 3), type = "l")
apply(sim_matrix[2:nsim, ], 1, function(x, t) lines(t, x), t = t)
lines(t, sim_matrix[1, ], xlab = "time", ylab = "phenotype", ylim = c(-3, 3), col = "red")




plot(t, sim_matrix[1, ], xlab = "time", ylab = "phenotype",
     ylim = c(-2, 2), type = "l", col = "grey")
apply(sim_matrix[2:nsim, ], 1, function(x, t) lines(t, x, col = "grey"), t = t)
points(0,0, col = "blue", pch = 20)
text(10,.5, "Ancestral \nValue", col = "blue")
points(x = rep(100, times = 5),
       y = sim_matrix[c(1:5),100],
       col = "red", pch = 20)
text(90, mean(sim_matrix[c(1:5),100]), "Extant\nTrait\nValues", col = "red")



nsim <- 1000
X <- matrix(rnorm(n = nsim * (length(t) - 1), sd = sqrt(sig2)), nsim, length(t) - 1)
sim_matrix <- cbind(rep(0, nsim), t(apply(X, 1, cumsum)))
plot(t, sim_matrix[1, ], xlab = "time", ylab = "phenotype",
     ylim = c(-5, 5), type = "l", col = "grey")
apply(sim_matrix[2:nsim, ], 1, function(x, t) lines(t, x, col = "grey"), t = t)
segments(x0 = 10, y0 = min(sim_matrix[,10]), y1 = max(sim_matrix[,10]), col = "red" )


v <- apply(sim_matrix, 2, var)
plot(t, v, type = "l", xlab = "time", ylab = "variance among simulations")





#_______________________________________________________________________


tree <- rcoal(n = 30)
x <- fastBM(tree, a=0, sig2=1.0, internal = TRUE)
?phenogram
?fastBM
phenogram(tree, x, spread.labels = TRUE)



par(mfrow=c(3,3))
x <- fastBM(tree, a=0, sig2=1.0, alpha=0.01, theta=4.0, internal = TRUE)
phenogram(tree, x, spread.labels = TRUE)
x <- fastBM(tree, a=0, sig2=1.0, alpha=0.5, theta=4.0, internal = TRUE)
phenogram(tree, x, spread.labels = TRUE)
x <- fastBM(tree, a=0, sig2=1.0, alpha=2.0, theta=4.0, internal = TRUE)
phenogram(tree, x, spread.labels = TRUE)


par(mfrow=c(2,3))
x <- fastBM(tree, a=0, sig2=1.0, alpha=0.01, theta=4.0, internal = TRUE)
phenogram(tree, x, spread.labels = TRUE)

x <- fastBM(tree, a=0, sig2=1.0, alpha=7.5, theta=4.0, internal = TRUE)
phenogram(tree, x, spread.labels = TRUE)



par(mfrow=c(2,3))
x <- fastBM(tree, a=0, sig2=1.0, alpha=0.01, theta=4.0, internal = TRUE)
phenogram(tree, x, spread.labels = TRUE)

x <- fastBM(tree, a=0, sig2=1.0, alpha=25, theta=4.0, internal = TRUE)
phenogram(tree, x, spread.labels = TRUE)
dev.off()



anole_tree<-read.tree("http://www.phytools.org/eqg2015/data/anole.tre")
svl <- read.csv("http://www.phytools.org/eqg2015/data/svl.csv", row.names=1)
svl <- as.matrix(svl)[,1]
plot(anole_tree)
anole_tree$Nnode #99
anole_tree$tip.label #100
class(svl)
head(svl)
str(svl)

fit <- fastAnc(anole_tree,svl,vars=TRUE,CI=TRUE)
?fastBM
?ace
fit_obj <- contMap(anole_tree, svl, plot=FALSE)
?contMap

plot(fit_obj, type="fan", legend=0.7*max(nodeHeights(anole_tree)), fsize=c(0.7,0.9))




tree <- rcoal(n = 100)
x <- fastBM(tree)
y = fastBM(tree)

plot(x, y)
abline(lm(y ~ x))
cor.test(x, y)

x_c <- pic(x, tree)
x_c
class(x_c)
str(x_c)
y_c <- pic(y, tree)
y_c
class(y_c)
str(y_c)
cor.test(x_c, y_c)



x <- fastBM(tree)
z <- fastBM(tree, sig2 = 0.2)
y <- 0.4 * x + z
y2 <- x + z
par(mfrow=c(1,2))
plot(x, y)
plot(x, y2)

cor.test(x, y)
cor.test(x, y2)

?lm
?fastBM
?setNames




#__________________________________________________________________________________
