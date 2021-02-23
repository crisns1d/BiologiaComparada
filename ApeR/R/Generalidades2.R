install.packages("phangorn")
install.packages("ape")
library(ape)
library(ade4)
library(phangorn)

getwd()
setwd("C:/Users/UIS/Downloads/ApeR")


tr1 <- read.tree(text = "(1,2,(3,((4,(8,(9,(10,11)))),(((5,15),(6,7)),((12,13),14)))));")
tr2 <- drop.tip(tr1, tip = c("10", "11"))
tr3 <- drop.tip(tr1, tip = c(10, 11))
#drop.tip sirve para quitar terminales. Si escribo el nombre de la terminal, lo pongo entre comillas. Si pongo el numero sin comillas, borra las terminal 10 y 11
plot(tr1)
plot(tr2)
plot(tr3)

#trim.internal: quitar ramas internas
write.tree(drop.tip(phy= tr1, tip = c("10", "11"), trim.internal = FALSE))
tr4 <- drop.tip(phy = tr1, tip = c("10", "11"), trim.internal = F)
tr5 <- drop.tip(phy = tr1, tip = c(10, 11), trim.internal = F)
plot(tr4)
plot(tr5)


#extract.clade: extrae/guarda un nodo
write.tree(extract.clade(tr1, node = c(17)))
tr5 <- extract.clade(tr1, node = c(17))
plot(tr5)
tr6 <- extract.clade(tr1, node = c(20))
plot(tr6)

plot(tr1)
tr1$Nnode #saber el numero de nodos internos (#taxa-2)
nodelabels(node = c(16:28)) #colocar nombres a los nodos



#bind.tree: combinar arboles
t1 <- read.tree(text = "(a:1,b:1):0.5;")
t2 <- read.tree(text = "(c:1,d:1):0.5;")
write.tree(bind.tree(t1, t2))
t3 <- bind.tree(t1, t2)
plot(t1)
plot(t2)
plot(t3)
t4 <- t1 + t2 
plot(t4)
write.tree(bind.tree(t1, t2, position = 1))

#Hacer que la topologia este al reves
?ladderize
plot(tr1)
tr7 <- ladderize(tr1, right = T)
plot(tr7)
tr8 <- ladderize(tr1, right = F)
plot(tr8)


#compute.brlen: modificar la long de ramas a mano
?compute.brlen
tr9 <- compute.brlen(tr1, 1)
plot(tr9)

hist(runif(1000000))
plot(runif(1000000))
tr10 <- compute.brlen(tr1, runif)
plot(tr10)
tr11 <- compute.brlen(tr1, rnorm)


#compute.brtime: calcular long de ramas con tiempo
tr12 <- compute.brtime(tr1, method = "coalescent")
plot(tr12)


#saber si un arbol esta enraizado
is.rooted(tr1)


#enraizar: a partir de un outgroup (outgroup =) o a partir de un nodo interno (node =)
?root
tr13 <- root(tr1, outgroup = c(13, 14)) #13 y 14 no son monofileticos
plot(tr13)

tr14 <- root(tr1, node = 20, resolve.root = T)
plot(tr14)
is.rooted(tr14)



#_______________________________________________________________________________________#

?multi2di #convierte arboles multitomicos en dicotomicos
?di2multi #convierte arboles dicotomicos en multitomicos 

par(mfrow=c(1,2))
tr1 <- read.tree(text = "(1,2,3,((4,8,9,10,11))),(((5,15),(6,7)),((12,13,14)));")
plot(tr1)

tr2 <- multi2di(tr1, random=TRUE)
plot(tr2)

tr3 <- compute.brlen(tr1, 1)
plot(tr3)
edgelabels(tr3$edge.length, bg="black", col="white", font=2) #ponerle etiqueta de la long de ramas
dev.off()

#convertir ramas de longitud x a ramas de long y
tr4 <- multi2di(tr3, random=TRUE)
plot(tr4)
tr4$edge.length == 0 #reviso el vector con longitud que quiero cambiar
tr4$edge.length[tr4$edge.length == 0] <- 2 #le asigno al vector la long que quiero
edgelabels(tr4$edge.length, bg="white", col="black", font=0.5)




#ejer1
  #crear arbol al azar simulando lond de ramas con dist normal, pasar ramas negativas a positivas
  #se escoje la rama más pequeña y se convierte en 0 ()
?rtree
tr5 <- rtree(n=10)
plot(tr5)

tr6 <- rtree(n=10, br=rnorm) #arbol con long de ramas por dist normal
plot(tr6)
tr6$edge.length < 0 #ver cuales ramas tienen long negativa por la dist normal
tr6$edge.length <- abs(tr6$edge.length) #quitar valores negativos por su valor absoluto
copia <- tr6
plot(copia)
plot(tr6)
edgelabels(tr6$edge.length, bg="white", col="black", font=0.1)
min(tr6$edge.length)
tr7 <- di2multi(tr6, tol=0.21) #di2multi no genera dicotomias en las ramas terminales, solo a partir de nodos internos
plot(tr7)
par(mfrow=c(1,3))
#Otra forma
tr6$edge.length[tr6$edge.length < 0.012] <- 0.0
plot(tr6)
dev.off()



#ejer2:
  #Comparar 2 arboles con la misma topología, pero uno tiene long de ramas y el otro no

par(mfrow=c(1,2))
tr8 <- rtree(n=10, br=NULL)
plot(tr8)
tr9 <- compute.brlen(tr8, 1)
plot(tr9)

all.equal(tr8, tr9) #Me mide las long de ramas, por lo que tr98 y tr9 son distintos




?as.matching
?as.hclust
?prop.part
?as.splits
?evonet
?as.network
?as.igraph
