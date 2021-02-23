## bibliotecas
library(ape)
library(vegan)
library(cluster)
getwd()
setwd("C:/Users/UIS/Documents/ApeR")

## ejercicio siempre de
## distnacia mas clustering

## set de datos

## leido desde disco
cars <- read.delim("C:/Users/UIS/Documents/ApeR/Data/cars.tab", stringsAsFactors=FALSE)
cars

## mirada al archivo
## 
head(cars)

## manipulacion de los datos

carsUse <- cars[,-c(1,2)]

head(carsUse)

## medias
?apply #aplica una función a los datos,en este caso, la media
medians = apply(carsUse,2,median) #aplica la función en 1=filas, 2=columnas
medians
sort(carsUse$Cylinders)
?sort #Ordenar de mayor a menor o al revés
length(carsUse$Cylinders)
?lenght #Saber el tamaño del vector
sort(carsUse$Cylinders)[c(19,20)]
median(carsUse$Cylinders) #la mediana de un numero par de datos es la media entre los dos numeros centrales


## standarizacion
##
mads <- apply(carsUse,2,mad) #1=filas, 2=columnas
mads
mad(carsUse$Cylinders)

## carros en uso
carsUse2 <- scale(carsUse,center=medians,scale=mads) #estandarizar desde la media
carsUse2[c(36,38),6]
carsUse[c(36,38),6]
hist(carsUse2)
names(carsUse2)
str(carsUse2)
?scale

## distancia por defecto ? cual
cars.dist1 <- dist(carsUse2)
head(cars.dist1)
?dist

## distancia euclidiana
cars.dist2 <- dist(carsUse2, method="euc")
cars
head(cars.dist1)

## los grupos
?hclust
t2<-(hclust(cars.dist2, method = "average", members=NULL))
t1<-(hclust(cars.dist1, method = "average", members=NULL))
t1
t2

## sus graficas
plot(hclust(cars.dist1, method = "average", members=NULL))

plot(hclust(cars.dist2, method = "average", members=NULL))

#matrices cofeneticas
#como se veria el cluster si fuera una matriz de distancia.
#La distancia cofenetica es la distancia entre dos datos de acuerdo al cluster
c1<-cophenetic(t1)
c2<-cophenetic(t2)

## correlaciones
cor(cars.dist1,c1)

cor(cars.dist2,c2)

?read.csv
