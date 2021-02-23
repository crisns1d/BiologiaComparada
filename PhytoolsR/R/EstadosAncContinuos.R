#####RECONSTRUCCION DE ESTADOS ANCESTRALES PARA CARACTERES CONTINUOS

getwd()
setwd("C:/Users/UIS/Downloads/BiologiaComparada/PhytoolsR")
install.packages("phytools")

library("phytools")

#Leer el arbol
tree <- read.tree("Data/Anolis.tre")
#Graficar el arbol
plotTree(tree,type="fan",ftype="i",lwd=1)

#Leer los datos
svl<-read.csv("Data/svl.csv",row.names=1)
#Cambiar de un dataframe a un vector con nombres
svl <- setNames(svl$svl, rownames(svl))
svl

#Estimacion de estados ancestrales. IC 95%
fit<-fastAnc(tree,svl,vars=TRUE,CI=TRUE)
print(fit,printlen=10)

#el IC de 95% puede ampliarse
range(svl) #comparación con el nodo raiz

#Proyección de las reconstrucciones sobre las ramas del arbol
obj<-contMap(tree,svl,plot=FALSE)
obj

#Grafico de los estados ancestrales
plot(obj,type="fan",legend=0.7*max(nodeHeights(tree)),
     sig=2,fsize=c(0.7,0.9))




###Propiedades de estados ancestrales y reconstrcción de estados ancestrales

#Simular un arbol y datos
tree<-pbtree(n=26,scale=1,tip.label=LETTERS[26:1])

# simular con los estados ancestrales
x<-fastBM(tree,internal=TRUE)

# estados ancestrales
a<-x[as.character(1:tree$Nnode+Ntip(tree))]

# datos de las terminales
x<-x[tree$tip.label]


#Estimacion de est. ancestrales con fastAnc
fit<-fastAnc(tree,x,CI=TRUE)
print(fit,printlen=6)

#Comparar las estimaciones con los valores simulados (desconocidos usualmente)
plot(a,fit$ace,xlab="true states",ylab="estimated states",bg="grey",cex=1.4,
     pch=21)
lines(range(c(x,a)),range(c(x,a)),lty="dashed",col="red") ## 1:1 line


##obtengamos IC del 95% en valores ancestrales, y luego demostremos que los 
##IC del 95% incluyen los valores generadores alrededor del 95% del tiempo

#Veamos el dataset anterior
print(fit)
mean(((a>=fit$CI95[,1]) + (a<=fit$CI95[,2]))==2)

#funcion personalizada para realizar una simulación y estimar estados ancestrales
#Y retorna en una fracción en 95% IC
foo<-function(){
  tree<-pbtree(n=100)
  x<-fastBM(tree,internal=TRUE)
  fit<-fastAnc(tree,x[1:length(tree$tip.label)],CI=TRUE)
  mean(((x[1:tree$Nnode+length(tree$tip.label)]>=fit$CI95[,1]) +
          (x[1:tree$Nnode+length(tree$tip.label)]<=fit$CI95[,2]))==2)
}

#Realizar 100 simulaciones
pp <- replicate(100,foo())
mean(pp)



###Estimacion de estados ancestrales cuando algunos nodos son conocidos

#Fijar el estado de algunos nodos internos durante la estimacion.
#Hacemos esto uniendo la rama terminal de long 0 al nodo interno que queremos fijar
#después lo tratamos como otro valor de terminal en el analisis
anc.states <- a[as.character(c(27,31,35))]
anc.states

fit<-fastAnc(tree,x,anc.states=anc.states,CI=TRUE)
plot(a,fit$ace,xlab="true states",ylab="estimated states",bg="grey",cex=1.4,
     pch=21)
lines(range(c(x,a)),range(c(x,a)),lty="dashed",col="red") ## 1:1 line


#Usar una probabilidad de densidad anterior informativa en uno o multiples estados del nodo interno
#Posteriormente se estima el caracter ancestral usando MCMC Bayes

tree<-pbtree(n=100,scale=1)

# simulamos datos con Movimiento Browniano con una tendencia
x<-fastBM(tree,internal=TRUE,mu=3)
phenogram(tree,x,ftype="off",spread.labels=FALSE)

a<-x[as.character(1:tree$Nnode+Ntip(tree))]
x<-x[tree$tip.label]

# que malo pasa si ignoramos la tendencia?
plot(a,fastAnc(tree,x),xlab="true values",
     ylab="estimated states under BM",
     bg="grey",cex=1.4,pch=21)
lines(range(c(x,a)),range(c(x,a)),lty="dashed",col="red")
title("estimated without prior information")

#Incorporar conocimiento prior
pm <- setNames(c(1000,rep(0,tree$Nnode)),
              c("sig2",1:tree$Nnode+length(tree$tip.label)))

#Escogemos al azar 2 nodos y la raiz
nn<-as.character(c(length(tree$tip.label)+1,
                   sample(2:tree$Nnode+length(tree$tip.label),2)))
pm[nn]<-a[as.character(nn)]

#varianza del prior
pv<-setNames(c(1000^2,rep(1000,length(pm)-1)),names(pm))
pv[as.character(nn)]<-1e-100

#Correr MCMC
mcmc<-anc.Bayes(tree,x,ngen=100000,
                control=list(pr.mean=pm,pr.var=pv,
                             a=pm[as.character(length(tree$tip.label)+1)],
                             y=pm[as.character(2:tree$Nnode+length(tree$tip.label))]))

#Estimacion usando prior informativo
anc.est<-colMeans(mcmc[201:1001,as.character(1:tree$Nnode+length(tree$tip.label))])
plot(a[names(anc.est)],anc.est,xlab="true values",
     ylab="estimated states using informative prior",
     bg="grey",cex=1.4,pch=21)
lines(range(c(x,a)),range(c(x,a)),lty="dashed",col="red")
title("estimated using informative prior")