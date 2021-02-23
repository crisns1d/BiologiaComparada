library(ape)
library(ade4)

TreePrimates <- read.tree(text="((((Homo:0.21,Pongo:0.21):0.28,Macaca:0.49):0.13,Ateles:0.62):0.38,Galago:1.00);")
TreePrimates
#Class: conocer la clase de vector o matriz o objeto que tengo
class(tree.primates)


body<-c(4.09434,3.61092,2.37024,2.02815,-1.46968)
class(body)
longevity<-c(4.74493,3.3322,3.3673,2.89037,2.30259)
class(longevity)


#Plot hace grÃ¡ficas eficientes y fÃ¡cil con 2 vectores X y Y
plot(body,longevity)

names(body)<-names(longevity)<-c("Homo","Pongo","Macaca","Ateles","Galago")
body
longevity


#ESTRUCTURA DEL ARBOL

#Uso '$' para conocer lo que hay dentro del arbol

TreePrimates$tip.label #terminales
class(TreePrimates$tip.label)

TreePrimates$Nnode #numero de nodos
class(TreePrimates$Nnode)

TreePrimates$edge #estructura del Ã¡rbol y conexiÃ³n entre ellos [1,]:donde inicia, [2,]:donde acaba el nodo; y el nÃºmero de terminales en el lado izquierdo, y el nÃºmero de ramas arriba 
class(TreePrimates$edge)
#clase: array=arreglo en forma de matriz ("matrix")

TreePrimates$edge.length #longitud d elas ramas que tenemos

plot(TreePrimates) #Graficar el arbol


mean(TreePrimates$edge.length)
summary(TreePrimates$edge.length)
hist(TreePrimates$edge.length)
x<-TreePrimates$edge.length

class(TreePrimates) <- NULL #quitarle la clase al objeto

class(TreePrimates) #clase: lista
TreePrimates
plot(TreePrimates) #no puedo graficarlo porque es una lista

class(TreePrimates) <- "phylo" #ponerle clase phylo para poder graficarlo
plot(TreePrimates) #ya puedo graficarlo




###EJERCICIO###

Arbol2 <- read.tree(text="((D:0.25, C:0.25):0.5,(B:0.25, A:0.6):0.25);")
Arbol2
class(arbol2)

Arbol2$tip.label
Arbol2$edge
Arbol2$Nnode
Arbol2$edge.length

plot(Arbol2)

#Cargar un arbol desde el escritorio
getwd()
Arbol3<-read.tree("../Data/pedro.R")
plot(arbol3)


#Leer secuencias de GenBank
secuenc1a1<- read.GenBank("AF310048")
?read.GenBank
class(secuenc1a1)
secuenc1a1
names(secuenc1a1) #nombre de la secuencia
str(secuenc1a1) #estructura


#Cargar un archivo desde la red
a<- "http://www.esapubs.org/archive/ecol/E084/093/"
b<-"Mammal_lifehistories_v2.txt"
ref<-paste(a, b, sep = "") #paste= pegar,juntar algo
ref
HistoriaVida<-read.delim(ref)
HistoriaVida
class(HistoriaVida)
head(HistoriaVida)



#Guardar un archivo. Save para guardar objetos de R
?save
save(TreePrimates, file="../Data/treeprimatesEjer1.Rdata")

#Guardar arboles en newick
?write.tree
write.tree(phy= TreePrimates, file = "../Data/treePrimatesEjer1")
write.tree(phy= TreePrimates)

#Guardar arboles en form nexus... LAS RUTAS VAN SIEMPRE ENTRE COMILLAS
write.nexus(TreePrimates, file = "../Data/treePrimatesNEXUS")

read.nexus(file="../Data/treePrimatesNEXUS")



#Quitar terminales
tr <- read.tree(text = "((a:1,b:1):1,(c:1,d:1):1);")
plot(tr)

?drop.tip
write.tree(drop.tip(tr, c("a", "b"), trim.internal = TRUE))
write.tree(drop.tip(tr, 1:2))

t1 <- read.tree(text = "(a:1,b:1):0.5;")
t2 <- read.tree(text = "(c:1,d:1):0.5;")
write.tree(bind.tree(t1, t2, position = 1), file = "../Data/drop")
t3 <- read.tree("../Data/drop")
plot(t3)
bind.tree(t1, t2, position = 1)


#Juntar subarboles
write.tree(t1+t2+t1, file = "../Data/drop2")
t4 <- read.tree("../Data/drop2")
t4
plot(t4)


#Ver raices y desenraizar
ta <- read.tree(text = "(a,b,c);")
plot(ta)
is.rooted(ta) #no enraizado

tb <- read.tree(text = "(a,b,c):1;")
plot(tb)
is.rooted(tb) #enraizado porque hay long de ramas

tc <- read.tree(text = "((a,b),c);")
plot(tc)
is.rooted(tc) #enraizado porque no estan todas las terminales en la misma rama
?unroot
td<-unroot(tc) #desenraizar. Solo elimina el nodo raiz, literal :V entonces crea otro nodo raiz
plot(td)
write.tree(td)
tp<-unroot(tree.primates)
plot(tp)

#Crear raiz
tr <- read.tree(text = "((a:1,b:1):1,(c:1,d:1):1);")
plot(tr)
plot(tj <- root(tr, interactive = TRUE))
plot(tj)


#Crear arboles al azar
?rtree()
ArbolAzar<-rtree(n=5)
plot(ArbolAzar)


#Mirar si un arbol es ultrametrico (sus ramas tienen la misma longitud)
is.ultrametric(TreePrimates)
plot(TreePrimates)

tcom <- rtree(n=5)
plot(tcom)
is.ultrametric(tcom)


#Comparar árboles (y vectores)
vec <- c(1:4)
vec2 <- c(4:1)
vec
vec2
vec == vec2
all.equal(vec, vec2) 

t1 <- read.tree(text = "((a:1,b:1):1,c:2);")
plot(t1)
t2 <- read.tree(text = "(c:2,(a:1,b:1):1);")
plot(t2)
all.equal(t1, t2) #Compara long, nombres, y estructura(edge)
t3 <- read.tree(text = "(f:1,(a:1,b:1):1);")
plot(t3)
all.equal(t1, t3)
all.equal(t1, t3, use.edge.length = FALSE)

vec3 <- c("a", "b", "c")
vec4 <- c("A", "B", "C")
all.equal(vec3, vec4)



#Comparación filogenética
ta <- read.tree(text = "(((a:1,b:1):1,c:2):1,d:3);")
plot(ta)
tb <- read.tree(text = "(((a:0.5,b:0.5):1.5,d:2):1,c:3);")
plot(tb)
comp <-  comparePhylo(ta, tb)
comp
class(comp)              
names(comp)
comp$messages[9] #en el corchete puedo anotar el número del mensaje que quiero leer, si quiero verlos todos a la vez no pongo ningún numero en corchetes
length(comp$messages) #en la long puedo tener cambio o tiempo, tener eso en cuenta

isTRUE(all.equal(ta, tb)) #isTRUE es más seguro para comparar
all.equal(t1, t2)
