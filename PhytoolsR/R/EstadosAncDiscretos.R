#####RECONSTRUCCION DE ESTADOS ANCESTRALES PARA CARACTERES DISCRETOS

getwd()
setwd("C:/Users/UIS/Downloads/BiologiaComparada/PhytoolsR")
install.packages("phytools")

library("phytools")


#Leer datos
X<-read.csv("Data/elopomorph.csv",row.names=1)
feed.mode<-setNames(X[,1],rownames(X))
feed.mode

#Leer arbol
eel.tree<-read.tree("Data/elopomorph.tre")
eel.tree

#Graficar datos
plotTree(eel.tree,type="fan",fsize=0.7,ftype="i",lwd=1)
cols<-setNames(c("red","blue"),levels(feed.mode))
tiplabels(pie=to.matrix(feed.mode[eel.tree$tip.label],
                        levels(feed.mode)),piecol=cols,cex=0.3)
add.simmap.legend(colors=cols,prompt=FALSE,x=0.9*par()$usr[1],
                  y=0.8*par()$usr[3],fsize=0.8)
