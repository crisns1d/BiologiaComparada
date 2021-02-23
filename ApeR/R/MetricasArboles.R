##
## metricas comparaciones 
##
##
## DRME dmiranda@uis.edu.co
##
## Oct 23 /2017
##

## bibliotecas

library(phangorn)

library(ape)


##
##  leer arboles
##


## referencia

setwd("../trees")

arbol <- read.tree("arbol")

plot(arbol)

arbol$edge.length

arbolcopia <-  arbol

arbolcopia$edge.length <- NULL

plot(arbolcopia)


#######
arbol0 <- read.tree("arbol0")
arbol1 <- read.tree("arbol1")
arbol2 <- read.tree("arbol2")
arbol3 <- read.tree("arbol3")
arbol4 <- read.tree("arbol4")

plot(arbol4)


E11 <- read.tree("E11")
E12 <- read.tree("E12")
E12b <- read.tree("E12b")

plot(E12)
Equal <- read.tree("Equal")
Farris <- read.tree("Farris")
F15 <- read.tree("F15")
F17 <- read.tree("F17")

### unroot a algunos

E11 <- unroot(E11)
E12 <- unroot(E12)
plot(E12)

Equal <- unroot(Equal)
Farris <- unroot(Farris)
F15 <- unroot(F15)
F15 <- unroot(F15)



## algo de graficas

par(mfrow=c(2,2))

par(mfrow=c(1,1))

plot(arbol)

plot.phylo(arbol, use.edge.length=T)

plot.phylo(arbol0, use.edge.length=T)

plot.phylo(arbol1, use.edge.length=T)

plot.phylo(arbol2, use.edge.length=T)

plot.phylo(arbol3, use.edge.length=T)

plot.phylo(arbol4, use.edge.length=T)




par(mfrow = c(3, 1)) 
plot(Farris)
plot(F15)
plot(F17)

plot(Equal)
plot(E11)
plot(E12)





##
## metricas
##



?treedist()

treedist(arbol,arbol)



##
## metricas entre arboles
##

treedist(arbol,arbol)


treedist(arbol,arbolcopia)

arbolcopia$edge.length <- c(1,1,1,1,1,0,1)

treedist(arbol,arbolcopia)


treedist(arbol,arbol0)




all.equal(arbol,arbol0, use.edge.length=T)


plot(arbol)

plot(arbol0)

treedist(Equal,E12)

treedist(E12b,E12)

plot(E12b)

plot(E12)

## los dos arboles son muy parecidos
treedist(E12,E11)

treedist(Farris,F15)


treedist(Farris,F17)

treedist(F17,F15)




##
## idem sin considerar long 
##


all.equal.phylo(Equal,E12, use.edge.length=FALSE)

all.equal.phylo(Farris,F15, use.edge.length=FALSE)

all.equal.phylo(Farris,F17, use.edge.length=FALSE)

## con log

all.equal.phylo(Equal,E11, use.edge.length=TRUE)

all.equal.phylo(arbol,arbol0, use.edge.length=F)

all.equal.phylo(arbol,arbol0, use.edge.length=T)



treedist(arbol0,arbol1)

all.equal.phylo(arbol0,arbol1, use.edge.length=T)

all.equal.phylo(arbol0,arbol1, use.edge.length=F)


treedist(arbol2,arbol0)

all.equal.phylo(arbol2,arbol0, use.edge.length=F)

all.equal.phylo(arbol3,arbol, use.edge.length=F)


treedist(arbol3,arbol)

treedist(arbol4,arbol3)


treedist(arbol1,arbol2)
treedist(arbol3,arbol2)

plot(arbol3)

plot(arbol2)

plot(arbol1)

plot(arbol2)

##
## distancia topologica
##
dist.topo(arbol1,arbol2)

dist.topo(arbol3,arbol2)

##
## RF
##
RF.dist(arbol1,arbol2)
RF.dist(arbol3,arbol2)


##
##
##
##
## metricas comparaciones 
##
##
## DRME dmiranda@uis.edu.co
##
## Mayo 28- 2013
##

## bibliotecas

library(phangorn)

library(ape)


##
##  leer arboles
##


## referencia
arbol <- read.tree("arbol")

arbol0 <- read.tree("arbol0")

arbol1 <- read.tree("arbol1")

arbol2 <- read.tree("arbol2")

arbol3 <- read.tree("arbol3")

arbol4 <- read.tree("arbol4")


par(mfrow=c(2,2))

plot.phylo(arbol, use.edge.length=T)

plot.phylo(arbol0, use.edge.length=T)

plot.phylo(arbol1, use.edge.length=T)

plot.phylo(arbol2, use.edge.length=T)


plot.phylo(arbol3, use.edge.length=T)

plot.phylo(arbol4, use.edge.length=T)


treedist

treedist(arbol,arbol0)

all.equal.phylo(arbol,arbol0, use.edge.length=F)

all.equal.phylo(arbol,arbol0, use.edge.length=T)



treedist(arbol,arbol1)

all.equal.phylo(arbol,arbol1, use.edge.length=T)

all.equal.phylo(arbol,arbol1, use.edge.length=F)




treedist(arbol0,arbol1)

all.equal.phylo(arbol0,arbol1, use.edge.length=T)

all.equal.phylo(arbol0,arbol1, use.edge.length=F)




treedist(arbol2,arbol0)

all.equal.phylo(arbol2,arbol0, use.edge.length=F)

all.equal.phylo(arbol3,arbol, use.edge.length=F)


treedist(arbol3,arbol)

treedist(arbol4,arbol3)

RF.dist(arbol,arbol4)



##### paral e tiempo libre 


### arboles de tnt

arbolesTNT <- readTNTtree("arboles-topologia.tre")

numArboles <- length(arbolesTNT)

for(tmp1 in 1:numArboles-1){

for(tmp2 in tmp1:numArboles){

print(RF.dist(arbolesTNT[[tmp1]],arbolesTNT[[tmp2]]))


}
}
