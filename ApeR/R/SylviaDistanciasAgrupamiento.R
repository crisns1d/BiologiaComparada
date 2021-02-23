library(ape)
install.packages("seqinr")
library(seqinr)
?ape
getwd()
setwd("C:/Users/UIS/Downloads/ApeR")
x <- paste("AJ5345", 26:49, sep = "")
x
x <- c("Z73494", x)

sylvia.seq <- read.GenBank(x)
head(sylvia.seq)
write.dna(sylvia.seq, "C:/Users/UIS/Downloads/ApeR/sylviaseq.fas", format = "fasta")
sylvia.seq.ali <- read.dna("C:/Users/UIS/Downloads/ApeR/sylviaalig.phy")
?read.dna
head(sylvia.seq.ali)
class(sylvia.seq.ali)

library(vegan)
?vegan
library(cluster)



#Distancias: Euclidiana, K80
#Agrupamiento: UPGMA, NJ

####Euclidiana:
sylvia.dist1 <- dist(sylvia.seq.ali, method = "euclidean")
head(sylvia.dist1)
##UPGMA: HASTA AHORA LA MEJOR
agr1 <- (hclust(sylvia.dist1, method = "average", members=NULL))
plot(agr1)
cop1 <- cophenetic(agr1)
cor(sylvia.dist1,cop1)
##NJ
?nj
agr2<-nj(sylvia.dist1)
plot(agr2)
cop2 <- cophenetic(agr2)
cor(sylvia.dist1,cop2)


####K80:
?dist.dna
sylvia.dist2 <- dist.dna(sylvia.seq.ali, model = "k80")
head(sylvia.dist2)
##UPGMA
agr3 <- (hclust(sylvia.dist2, method = "average", members=NULL))
plot(agr3)
cop3 <- cophenetic(agr3)
cor(sylvia.dist2,cop3)
##NJ
?nj
agr4<-nj(sylvia.dist2)
plot(agr4)
cop4 <- cophenetic(agr4) #HAY UN ERROR AQUI
cor(sylvia.dist2,cop4)
