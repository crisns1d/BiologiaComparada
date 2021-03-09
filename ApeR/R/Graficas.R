library(ape)
library(ade4)
library(phangorn)
library(adephylo)
library(phylobase)
library(rgl)

getwd()
setwd("C:/Users/UIS/Downloads/ApeR")




tre1 <- read.tree(text = "(((a:0.7,b:0.2):1.5,d:2):1,c:7);")

#####ARBOLES CON LONG DE RAMA Y TITULO#####
par(mfrow=c(1,3))
plot(tre1, use.edge.length = FALSE, main = "Arbol sin longitud de ramas")
plot.phylo(tre1, use.edge.length = TRUE, main="Arbol Con Longitud de Ramas")

#####Arbol con long de ramas igual y titulo
tre2 <- tre1
tre2$edge.length <- c(1, 1, 1, 1, 1, 1)
plot.phy(tre2, use.edge.length = TRUE, main="Arbol Con Longitud de Ramas Igual")
dev.off()
?plot

###Arboles como cladogramas (forma triangular), en dirección de derecha a izquierda
par(mfrow=c(1,3))
plot.phylo(tre1, type = "c", direction = "l", use.edge.length = FALSE, main = "Arbol sin longitud de ramas")
plot.phylo(tre1, type = "c", direction = "l", use.edge.length = TRUE, main="Arbol Con Longitud de Ramas")
plot.phylo(tre2, type = "c", direction = "l", use.edge.length = TRUE, main="Arbol Con Longitud de Ramas Igual")


###Arbol con titulos de los nodos justificados y distintas long de ramas
tre3 <- read.tree(text = "((((Homooooo:0.21,Pongooo:0.21):0.28,Macaca:0.49):0.13,Ateles:0.62):0.38,Galagoooo:1);")
par(mfrow=c(1,3))
plot.phylo(tre3, adj = 0, direction = "l", use.edge.length = FALSE, main = "Nombres alineados sin long de ramas")
plot.phylo(tre3, adj = 0, direction = "l", use.edge.length = TRUE, main="Nombres Alineados Con Long de Ramas")
tre4 <- tre3
tre4$edge.length <- c(1,1,1,1,1,1,1,1)
plot.phylo(tre4, adj = 0, direction = "l", use.edge.length = TRUE, main="Nombres Alineados Con Longitud de Ramas Igual")
dev.off()


###Arboles unrooted con distintas long de ramas
data(bird.orders)
plot(bird.orders, type = "u", font = 1, no.margin = TRUE, use.edge.length = FALSE, main = "Arbol sin longitud de ramas")
plot(bird.orders, type = "u", font = 1, no.margin = TRUE, use.edge.length = TRUE, main = "Arbol con longitud de ramas")
tre5 <- bird.orders$edge.length <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
plot(bird.orders, type = "u", font = 1, no.margin = TRUE, use.edge.length = TRUE, main = "Arbol con longitud de ramas iguales")


###Arboles en forma de fan con distintas long de ramas
data(bird.orders)
par(mfrow=c(1,3))
plot(bird.orders, type = "fan", font = 1, no.margin = TRUE, use.edge.length = FALSE, main = "Arbol sin longitud de ramas")
plot(bird.orders, type = "fan", font = 1, no.margin = TRUE, use.edge.length = TRUE, main = "Arbol con longitud de ramas")
plot(tre5, type = "fan", font = 1, no.margin = TRUE, use.edge.length = TRUE, main = "Arbol con longitud de ramas iguales")
dev.off()

par(mfrow=c(1,3))
plot(tre6)
tre6 <- read.tree(text = "((t5:0.7751354985,(t6:0.9835859218,(((t1:0.6528835276,t2:0.8458062934):0.8703846436,t8:0.3825760901):0.07328546001,t7:0.5932530346):0.6092662113):0.6149031692):0.6467406675,((t9:0.4991088894,t4:0.4884779244):0.7723605123,t3:0.1005979984):0.5129213275);")
plot(tre6, type = "fan", font = 1, use.edge.length = FALSE, main = "Arbol sin longitud de ramas")
plot(tre6, type = "fan", font = 1, use.edge.length = TRUE, main = "Arbol con longitud de ramas")
tre7 <- tre6
tre7$edge.length <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
plot(tre7, type = "fan", font = 1, use.edge.length = TRUE, main = "Arbol con longitud de ramas iguales")



###Arboles en forma radial con distintas lond de rama
plot.phylo(tre6, type = "radial", no.margin = TRUE, font = 1, use.edge.length = FALSE, main = "Arbol sin longitud de ramas")
plot.phylo(tre6, type = "radial", no.margin = TRUE, font = 1, use.edge.length = TRUE, main = "Arbol con longitud de ramas")
plot.phylo(tre7, type = "radial", no.margin = TRUE, font = 1, use.edge.length = TRUE, main = "Arbol con longitud de ramas iguales")

data(bird.orders)
plot.phylo(bird.orders, type = "radial", font = 1, no.margin = TRUE, use.edge.length = FALSE, main = "Arbol sin longitud de ramas")
plot.phylo(bird.orders, type = "radial", font = 1, no.margin = TRUE, use.edge.length = TRUE, main = "Arbol con longitud de ramas")
tre5 <- bird.orders$edge.length <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
plot.phylo(bird.orders, type = "radial", font = 1, no.margin = TRUE, use.edge.length = TRUE, main = "Arbol con longitud de ramas iguales")
dev.off()



###Arboles con rotulos en los nodos
?nodelabels
trape <- read.tree(text = "((Homo,Pan),Gorilla);")
plot(trape, main= "Arbol con titulos en los nodos")
nodelabels()
#rotulos específicos rectangulares
plot(trape, main= "Arbol con titulos en los nodos")
nodelabels("Raiz", 4, frame = "r", bg = "white")
nodelabels("Nodo interno", 5, frame = "r", bg = "white")
#rotulos específicos circulares
plot(trape, main= "Arbol con titulos en los nodos")
nodelabels("Raiz", 4, frame = "c", bg = "white")
nodelabels("Nodo interno", 5, frame = "c", bg = "white")



###Arbol con distintos valores en lOS nodos
valores1 <- scan()
valores2 <- scan()
valores3 <- scan()
valores1
plot(tre6, no.margin = TRUE, use.edge.length = TRUE, main = "Arbol con valores en los nodos")
#cex=tamaño letra, font= tipo de letra, adj= ajustar posición de la letra (x,y)
nodelabels(valores1, adj = c(-0.2, -0.1), frame = "n",
           cex = 0.8, font = 2, col = "purple")
nodelabels(valores2, adj = c(1.2, -0.5), frame = "n",
           cex = 0.8, font = 3, col = "green")
nodelabels(valores3, adj = c(1.2, 1.5), frame = "n", 
           cex = 0.8, col = "red")


###Arbol con termometros en los nodos
plot(tre6, no.margin = TRUE, use.edge.length = TRUE, main = "Arbol con termometros en los nodos")
nodelabels(thermo = valores3/100, piecol = c("purple","yellow"), horiz = TRUE)



###Arbol con puntos en los nodos
dev.off()
valores1
colores <- c("blue", "purple", "orange")
p <- character(length(valores1))
p[valores1 >= 90] <- colores[1]
p[valores1 < 90 & valores1 >= 70] <- colores[2]
p[valores1 < 70] <- colores[3]

plot(tre6, no.margin = TRUE, use.edge.length = TRUE, main = "Arbol con puntos en los nodos")
nodelabels(node = 10:17, pch = 21, bg = p[-1], cex = 2)
#Leyenda
points(rep(0.005, 3), 1:3, pch = 21, cex = 2, bg = colores)
text(rep(0.01, 3), 1:3, adj = 0,
     c("<= 90", "70 : 90", "< 70"))



#Arbol con terminales en cajitas
par(mar=c(3,3,3,3))
trape <- read.tree(text = "((Homo,Pan),Gorilla);")
plot(trape, main= "Arbol terminales en cajitas", cex=1.5)
tiplabels(trape$tip.label[2], 2, adj = 0,
          bg = "yellow", col = "purple", cex=1.5)
?tiplabels()


dev.off()
#Arboles con barras de secciones
par(mar=c(3,3,3,3))
data(bird.orders)
plot(bird.orders,no.margin = TRUE)
segments(38, 1, 38, 5, lwd = 3)
text(39, 3, "Proaves", srt = 270)
segments(38, 6, 38, 23, lwd = 3)
text(39, 14.5, "Neoaves", srt = 270)
?segments
?text



colors()

#Arboles con clados de distinto color
plot(bird.orders, type = "c", no.margin = FALSE, font = 1)
wh <- which.edge(bird.orders, 20:23)
wh2 <- which.edge(bird.orders, 13:19)
wh3 <- which.edge(bird.orders, 6:13)
colo <- rep("black", Nedge(bird.orders))
colo[wh] <- "turquoise3"
colo[38] <- "turquoise3"
colo[wh2] <- "hotpink2"
colo[wh3] <-  "yellow3"
plot(bird.orders, "c", FALSE, font = 1, edge.color = colo,
     edge.width = 3, no.margin = TRUE)



#Arboles con clados encerrados en cuadros
plot(bird.orders, font = 1, no.margin = TRUE)
rect(1.2, 0.5, 36, 5.4, col = "orange2")
?par
par(new = TRUE)
plot(bird.orders, font = 1, no.margin = TRUE)




#________________________________________________________________________________________

#Arbol de razgos con nombres de las terminales
?phylo4d
par(mar=c(3,3,3,3))
t1 <- rcoal(20)
?plot.phylo
x1 <- phylo4d(t1, matrix(rnorm(100), 20))
?table.phylo4d()
table.phylo4d(x1, box = FALSE, symbol = ("colors"), col = terrain.colors(4))

#Arbol de razgos sin nombres de las terminales
par(mar=c(3,3,3,3))
t2 <- rcoal(20)
x2 <- phylo4d(t2, matrix(rnorm(100), 20))
table.phylo4d(x2, box = FALSE, show.tip.label = FALSE,)



#Barras de riqueza de especies
Orders.dat <- scan()
data("bird.orders")
names(Orders.dat) <- bird.orders$tip.label
Orders.dat
plot(bird.orders, x.lim = 50, font = 1, cex = 0.8)
segments(rep(40, 23), 1:23, rep(40, 23) + log(Orders.dat), 1:23, lwd = 3)
axis(1, at = c(40, 45, 50), labels = c(0, 5, 10))
mtext("ln(species richness)", at = 45, side = 1, line = 2)
axisPhylo()


#Combinar 2 gráficas en 1
dev.off()
layout(matrix(c(2, rep(1, 8)), 3, 3))
wh <- which.edge(bird.orders, 8:23)
colo <- rep("black", Nedge(bird.orders))
colo[wh] <- "turquoise3"
plot(bird.orders, "p", FALSE, font = 1, no.margin = TRUE, edge.color=colo)
arrows(4.3, 15.5, 6.9, 12, length = 0.1)
par(mar = c(2, 2, 2, 2))
hist(rnorm(1000), main = "", col = terrain.colors(14))
?hist



#Arboles cofilogeneticos
dev.off()
trk <- bird.orders
trc <- chronopl(bird.orders, lambda = 2, age.min = 12)
layout(matrix(1:2, 1, 2))
plot(trk)
plot(trc, show.tip.label = FALSE, direction = "l")

?layout()
layout(matrix(1:2, 1, 2), width = c(1, 1))
par(mar = c(2, 0, 0, 0))

plot(trk, adj = 0.5, cex = 0.6, show.tip.label = T)
nodelabels(node = 26, "?", adj = 2, bg = "white")
axisPhylo()
plot(trc, direction = "l", show.tip.label=F)
axisPhylo()




#Coarbol de parasitos
TR <- read.tree("C:/Users/UIS/Downloads/ApeR/Data/Arboleswuuu.tre")
?matrix
A <- matrix(c("C.hispidus", "C.formosus", "C.eremicus", "C.intermedius", "C.californicus", "C.baileyi", "F.zacatecae-C.h.", "F.reducta-C.f.", "F.zacatecae-C.e.", "F.zacatecae-C.i.", "F.tribulosa-C.c.", "F.reducta-C.b."), 6, 2)
A
cophyloplot(TR[[1]], TR[[2]], A, space = 20, length.line = -3, lty = 2)
dev.off()



#Graficar multiples arboles de coalescencia
TR <- replicate(6, rcoal(10), simplify = FALSE)
kronoviz(TR, horiz = FALSE, type = "c", show.tip.label=FALSE)




#Extraer clados de una topología grande
dev.off()
data(chiroptera)
tr <- drop.tip(chiroptera, 16:916, subtree = TRUE)
plot(tr, font = c(rep(3, 15), rep(2, 3)), cex = 0.8, no.margin = TRUE)




#Hacer zoom en 2 clados
data("bird.families")
zoom(bird.families, list(1:15, 38:48),
     col = c("lightgrey", "slategrey"),
     no.margin = TRUE, font = 1, subtree = TRUE)




#Hacer un objeto gif
open3d()
plot(ntx, edge.width = 1, tip.color = "black")
play3d(spin3d())
movie3d(spin3d(), 12, fps = 1, convert = FALSE, dir = ".")



#####EJERCICIOS######

###1. Dibujar la figura 4.11 usando una escala de colores en lugar de gris. La figura debe incluir una leyenda
dev.off()
valores1 <- scan()
valores1
colores <- c("red", "pink", "white")
p <- character(length(valores1))
p[valores1 >= 90] <- colores[1]
p[valores1 < 90 & valores1 >= 70] <- colores[2]
p[valores1 < 70] <- colores[3]

par(mar=c(3,3,3,3))
plot(tre6, no.margin = FALSE, use.edge.length = TRUE, main = "Arbol con puntos en los nodos")
nodelabels(node = 10:17, pch = 21, bg = p[-1], cex = 2)
#Leyenda
?points
points(rep(0.0003, 3), 1:3, pch = 21, cex = 2, bg = colores)
text(rep(0.01, 3), 1:3, adj = 0,
     c("<= 90", "70 : 90", "< 70"))




###2. graficar la filogenia de ordenes de aves y colorear las proaves de azul. Repita esto pero solo para las ramas terminales de este clado

dev.off()
plot(bird.orders, no.margin = FALSE, font = 1)
wh <- which.edge(bird.orders, 1:5)
colo <- rep("black", Nedge(bird.orders))
colo[wh] <- "turquoise3"
colo2 <- rep("black", Ntip(bird.orders))
colo2[1:5] <- "turquoise3"
?plot
plot(bird.orders, "c", FALSE, font = 1, edge.color = colo,
     edge.width = 3, no.margin = TRUE, tip.color = colo2)

plot(bird.orders, no.margin = FALSE, font = 1)
colo <- rep("black", Nedge(bird.orders))
bird.orders$edge
colo[3:4] <- "turquoise3"
colo[7:9] <- "turquoise3"
colo2 <- rep("black", Ntip(bird.orders))
colo2[1:5] <- "turquoise3"
plot(bird.orders, "c", FALSE, font = 1, edge.color = colo,
     edge.width = 3, no.margin = TRUE, tip.color = colo2)



colors()
###3. Suponga que tiene un factor que representa un estado de caracter para cada nodo y cada terminal del arbol.
###   Encuentre una manera de asociar un color con cada rama dependiendo del estado en ambos extremos de la rama
tr <- rtree(6)
tr$edge
colo <- rep("black", Ntip(tr))
colores <- c("violetred4", "red", "pink")
colo[1:2] <- colores[1]
colo[3:4] <- colores[2]
colo[5:6] <- colores[3]

estados <- c(1, 2, 3, 4, 5)
class(estados)
f <- character(length(estados))
f[estados=5] <- colores[2]
f[estados<5] <- colores[1]
f[estados<3] <- colores[3]

colo2 <- rep("black", Nedge(tr))
colores2 <- c("violetred4", "orange2", "red", "purple", "pink")
colo2[1] <- colores2[5]
colo2[2] <- colores2[4]
colo2[3:5] <- colores2[1]
colo2[6] <- colores2[2]
colo2[7:8] <- colores2[3]
colo2[9:10] <- colores2[5]

plot(tr, no.margin = FALSE, font = 1, tip.color = colo, edge.color = colo2)
nodelabels(node = 7:11, pch = 21, bg = f, cex = 2)




t1 <- read.tree(text = "(c:2,(a:1,b:1):1);")
t2 <- read.tree(text = "(c:4,(a:2,b:2):2);")
tmax <- speciesTree(list(t1, t2))
all.equal(tmax, t1)

tsha <- speciesTree(list(t1, t2), sum)
kronoviz(list(t1, t2, tmax, tsha), type = "c")









##################CONTRASTES INDEPENDIENTES######################



#Arbol con contrastes en los nodos
par(mar=c(3,3,3,3))

TreePrimates <- read.tree(text="((((Homo:0.21,Pongo:0.21):0.28,Macaca:0.49):0.13,Ateles:0.62):0.38,Galago:1.00);")
TreePrimates
body <- c(4.09434, 3.61092, 2.37024, 2.02815, -1.46968)
longevity <- c(4.74493, 3.3322, 3.3673, 2.89037, 2.30259)
names(body) <- names(longevity) <- c("Homo", "Pongo", "Macaca", "Ateles", "Galago")
#Calculo de contrastes
pic.body <- pic(body, tree.primates)
pic.longevity <- pic(longevity, tree.primates)
pic.body
pic.longevity

plot(tree.primates)
edgelabels()
wh <- which.edge(tree.primates, 1:2)
colo <- rep("black", Nedge(tree.primates))
colo[wh] <- "pink"
plot(tree.primates, "p", FALSE, font = 1, no.margin = TRUE, edge.color=colo)
nodelabels(round(pic.body, 3), adj = c(0, -0.5), frame = "n", col = "blue")
nodelabels(round(pic.longevity, 3), adj = c(0, 1), frame = "n", col = "purple")

#Grafica de correlación de los contrastes
plot(pic.body, pic.longevity)
abline(a = 2.56, b = -0.5, lty = 1) # x = y line
?abline



lm(pic.longevity ~ pic.body - 1)
lmorigin(pic.longevity ~ pic.body, nperm = 1e4)
lmorigin(pic.longevity ~ pic.body, nperm = 100)
lmorigin(pic.longevity ~ pic.body, nperm = 1000)


#Cambio la diagonal de la matriz a 0
w <- 1/cophenetic(tree.primates)
diag(w) <- 0 # OR: w[w == Inf] <- 0
Moran.I(body, w)

#Que pasas si no cambio la diagonal
w1 <-  1/cophenetic(tree.primates)
diag(w1)
Moran.I(body, w)


#Calcular gearymoran
gearymoran(w, data.frame(body, longevity))


#Calcular Moran pero de la distancia cofenetica normal
Moran.I(longevity, cophenetic(tree.primates))

#Calculo abouheif.moran
abouheif.moran(cbind(body, longevity), w)


X <- phylo4d(tree.primates, data.frame(body, longevity))
abouheif.moran(X)


data(carnivora)
frm <- SW ~ Order/SuperFamily/Family/Genus
correl.carn <- correlogram.formula(frm, data = carnivora)
correl.carn

plot(correl.carn, col = c("white", "black"), adj = 1, cex=1)


tr <- rtree(3)
treePart(tr)
treePart(tr, "orthobasis")
as.matrix(orthobasis.phylo(tr))

B <- as.matrix(orthobasis.phylo(tree.primates))
X <- B[, 1:2]
X
anova(lm(body ~ X))
anova(lm(longevity ~ X))
par(mfrow=c(1,2))
orthogram(body, tree.primates)
orthogram(longevity, tree.primates)
dev.off()

tr <- rtree(30)
X <- matrix(rnorm(150), 30, 5)
rownames(X) <- tr$tip.label
X2 <- replicate(5, rTraitCont(tr))
dat <- phylo4d(tr, X)
dat2 <- phylo4d(tr, X2)
res <- ppca(dat)
res2 <- ppca(dat2)
plot(res)
plot(res2)



x <- cumsum(c(0, rnorm(99)))
x <- numeric(100)
for (i in 1:99)
        x[i + 1] <- x[i] - 0.2 * x[i] + rnorm(1)
X <- replicate(100, cumsum(c(0, rnorm(99))))
sim.ou <- function() {
        x <- numeric(100)
        for (i in 1:99)
                x[i + 1] <- x[i] - 0.2 * x[i] + rnorm(1)
        x # returns the value of x
}
X2 <- replicate(50, sim.ou())
var(X[100 ,])
var(X2[100 ,])
layout(matrix(1:2, 1, 2))
yl <- range(X)
matplot(X, ylim = yl, type = "l", col = 1, main = "Brownian")
matplot(X2, ylim = yl, type = "l", col = 1, main = "OU")


