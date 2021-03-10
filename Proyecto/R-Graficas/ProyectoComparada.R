library(ape)
library(ade4)
library(adephylo)



##########GRAFICAS ARBOLES CON RELOJ##############



getwd()
setwd("C:/Users/UIS/Downloads")


tr1 <- read.tree(text="(1:126.1,(2:126.09,((((3:19.22,4:19.22):21.49,5:40.72):48.30,((((((9:22.31,11:22.31):7.08,(10:25.69,12:25.69):3.70):17.31,13:46.71):18.62,(15:57.84,16:57.84):7.50):2.76,14:68.10):3.53,17:71.63):17.39):23.62,((6:60,7:60):21.19,8:81.19):31.46):13.43):0.001351);")
tr2 <- read.tree(text="(1:120.37,(2:119.48,((((3:18.31,4:18.31):20.31,5:38.62):45.67,(((((8:21.22,10:21.22):6.66,(9:24.41,11:24.41):3.47):15.90,12:43.79):13.73,13:57.53):4.64,(14:55.55,15:55.55):6.62):22.12):19.87,(6:59.99,7:59.99):44.18):15.30):0.88);")
tr3 <- read.tree(text="(1:97.97,(2:95.54,(((3:14.89,4:14.89):16.80,5:31.69):37.31,(6:60.00,((((7:17.21,9:17.21):5.48,(8:19.76,10:19.76):2.93):24.70,11:47.41):3.06,(12:45.19,13:45.19):5.28):9.52):9.01):26.53):2.43);")

dev.off() 
par(mar=c(1,1,1,1))
tr1$tip.label
tr1$tip.label <- c("Proganochelys_quenstedti","Chelus_fimbriata","Pelusios_castaneus","Pelusios_castanoides","Pelomedusa_subrufa","Podocnemis_lewyana","Podocnemis_erythrocephala","Podocnemis_unifilis","Podocnemis_expansa","Caninemys_tridentata","Peltocephalus_dumerilianus","Erymnochelys_madagascariensis","Carbonemys_cofrinii","Bairdemys_thalassica","Bothremys_maghrebiana","Acleistochelys_maliensis","Ummulisani_rutgersensis")
tr1$edge.length
plot(tr1, no.margin = F, use.edge.length = TRUE, cex = 0.9)
nodelabels(adj = c(-0.1, -0.2), frame = "n", cex = 0.8, font = 2, col = "black")
edgelabels()
tipLn1 <- c(tr1$edge.length[1],tr1$edge.length[3],(tr1$edge.length[28]+tr1$edge.length[32]),(tr1$edge.length[6]+tr1$edge.length[10]),tr1$edge.length[10],tr1$edge.length[9],tr1$edge.length[27],tr1$edge.length[26],(tr1$edge.length[25]+tr1$edge.length[23]),tr1$edge.length[22],(tr1$edge.length[19]+tr1$edge.length[20]),tr1$edge.length[18],tr1$edge.length[21],tr1$edge.length[25],tr1$edge.length[32]," ")


tr2$edge.length
plot.phylo(tr2, no.margin = F, use.edge.length = TRUE, cex = 0.7)
nodelabels(adj = c(-0.1, -0.2), frame = "n", cex = 0.8, font = 2, col = "black")
edgelabels()
tipLn2 <- c(tr2$edge.length[1],tr2$edge.length[3],(tr2$edge.length[28]+tr2$edge.length[26]),(tr2$edge.length[6]+tr2$edge.length[10]),tr2$edge.length[10],tr2$edge.length[9]," ",(tr2$edge.length[23]+tr2$edge.length[25]),tr2$edge.length[22],tr2$edge.length[21],(tr2$edge.length[18]+tr2$edge.length[20]),tr2$edge.length[17],tr2$edge.length[19],tr2$edge.length[25],"","","")


tr3$edge.length
plot.phylo(tr3, no.margin = F, use.edge.length = TRUE, cex = 0.7)
nodelabels(adj = c(-0.1, -0.2), frame = "n", cex = 0.8, font = 2, col = "black")
edgelabels()
tipLn3 <- c(tr3$edge.length[1],tr3$edge.length[3]," ",(tr3$edge.length[5]+tr3$edge.length[9]),tr3$edge.length[9],tr3$edge.length[8],"",tr3$edge.length[21],(tr3$edge.length[22]+tr3$edge.length[23]),"",(tr3$edge.length[20]+tr3$edge.length[18]),tr3$edge.length[17],tr3$edge.length[19],tr3$edge.length[24],"","","")
tr3$edge.length[21]


nodoCalib <- c("","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",tr1$edge.length[31])


plot(tr1, type = "phylogram", no.margin = F, use.edge.length = TRUE, cex = 1)
nodelabels(tipLn1, adj = c(-0.1, -0.9), frame = "n", cex = 0.8, font = 2, col = "black")
nodelabels(tipLn2, adj = c(-0.1, 0.5), frame = "n", cex = 0.8, font = 2, col = "green4")
nodelabels(tipLn3, adj = c(-0.1, 1.7), frame = "n", cex = 0.8, font = 2, col = "purple3")
nodelabels(nodoCalib, adj = c(-0.5, 0.4), frame = "n", cex = 0.8, font = 2, col = "red4")

text(-7, 17.2, adj = 0, font = 3, cex = 1, "Time with:")
points(-5, 16.4, pch = 21, cex = 2, bg = "black")
text(-3, 16.5, adj = 0, cex = 0.9, "7 fossils")
points(-5, 15.7, pch = 21, cex = 2, bg = "green4")
text(-3, 15.8, adj = 0, cex = 0.9, "5 fossils")
points(-5, 15, pch = 21, cex = 2, bg = "purple3")
text(-3, 15.1, adj = 0, cex = 0.9, "3 fossils")
points(-5, 14.3, pch = 21, cex = 2, bg = "red4")
text(-3, 14.5, adj = 0, cex = 0.8, "Calibration")
text(-3, 14.1, adj = 0, cex = 0.8, "node")







#############################################################################



##########GRAFICAS ARBOLES SIN RELOJ##############



tr4 <- read.tree(text="(1:157.83,(2:141.91,(((3:21.09,4:21.09):37.89,5:58.99):57.71,(((6:60.00,7:60.00):39.88,8:99.88):10.33,(((((((9:11.49,11:11.49):6.42,10:17.91):5.36,12:23.28):12.62,(13:11.66,17:11.66):24.23):11.46,16:47.37):17.95,15:65.33):8.80,14:74.13):36.07):6.49):25.20):15.92);")
tr5 <- read.tree(text="(1:163.10,(2:147.85,(((3:21.59,4:21.59):39.25,5:60.84):58.82,((6:60.00,7:60.00):52.52,((((((8:13.78,10:13.78):11.60,9:25.38):10.25,11:35.64):15.97,12:51.61):8.64,13:60.25):35.65,(14:70.01,15:70.01):25.90):16.61):7.14):28.18):15.25);")
tr6 <- read.tree(text="(1:163.48,(2:145.53,(((3:21.39,4:21.39):39.03,5:60.43):56.72,(6:60.00,(((((7:12.57,9:12.57):8.56,8:21.14):4.34,10:25.48):19.17,11:44.66):9.43,(12:35.15,13:35.15):18.94):5.90):57.16):28.37):17.94);")

dev.off()
par(mar=c(1,1,1,1))
tr4$tip.label
tr4$tip.label <- c("Proganochelys_quenstedti","Chelus_fimbriata","Pelusios_castaneus","Pelusios_castanoides","Pelomedusa_subrufa","Bothremys_maghrebiana","Acleistochelys_maliensis","Ummulisani_rutgersensis","Podocnemis_lewyana","Podocnemis_erythrocephala","Podocnemis_unifilis","Podocnemis_expansa","Caninemys_tridentata","Bairdemys_thalassica","Erymnochelys_madagascariensis","Peltocephalus_dumerilianus","Carbonemys_cofrinii")
tr4$edge.length
plot(tr4, no.margin = F, use.edge.length = TRUE, cex = 0.7)
nodelabels(adj = c(-0.1, -0.2), frame = "n", cex = 1, font = 2, col = "black")
edgelabels()
tipLn4 <- c(tr4$edge.length[1],tr4$edge.length[3],(tr4$edge.length[5]+tr4$edge.length[9]),tr4$edge.length[9],tr4$edge.length[8],(tr4$edge.length[16]+tr4$edge.length[32]),tr4$edge.length[15],"",tr4$edge.length[32],tr4$edge.length[31],tr4$edge.length[30],(tr4$edge.length[27]+tr4$edge.length[29]),tr4$edge.length[26],tr4$edge.length[25],tr4$edge.length[24],tr4$edge.length[29])


tr5$edge.length
plot.phylo(tr5, no.margin = F, use.edge.length = TRUE, cex = 0.7)
edgelabels()
nodelabels(adj = c(-0.1, -0.2), frame = "n", cex = 1, font = 2, col = "black")
tipLn5 <- c(tr5$edge.length[1],tr5$edge.length[3],(tr5$edge.length[5]+tr5$edge.length[9]),tr5$edge.length[9],tr5$edge.length[8],(tr5$edge.length[11]+tr5$edge.length[13]),tr5$edge.length[12],"",tr5$edge.length[25],tr5$edge.length[28],tr5$edge.length[24],"",tr5$edge.length[23],tr5$edge.length[22],tr5$edge.length[21],"","")


tr6$edge.length
plot.phylo(tr6, no.margin = F, use.edge.length = TRUE, cex = 0.7)
edgelabels()
nodelabels(adj = c(-0.1, -0.2), frame = "n", cex = 1, font = 2, col = "black")
tipLn6 <- c(tr6$edge.length[1],tr6$edge.length[3],(tr6$edge.length[5]+tr6$edge.length[9]),tr6$edge.length[9],tr6$edge.length[8],tr6$edge.length[11],"","",tr6$edge.length[21],tr6$edge.length[23],(tr6$edge.length[22]+tr6$edge.length[24]),"",tr6$edge.length[20],tr6$edge.length[19],tr6$edge.length[18],"")
tr6$edge.length[13]


nodoCalib2 <- c("","","","","","","",60,"","","","","","","","")

plot(tr4, type = "phylogram", no.margin = F, use.edge.length = TRUE, cex = 1)
nodelabels(tipLn4, adj = c(-0.1, -0.9), frame = "n", cex = 0.8, font = 2, col = "black")
nodelabels(tipLn5, adj = c(-0.1, 0.5), frame = "n", cex = 0.8, font = 2, col = "green4")
nodelabels(tipLn6, adj = c(-0.1, 1.7), frame = "n", cex = 0.8, font = 2, col = "purple3")
nodelabels(nodoCalib2, adj = c(-0.4, 0.4), frame = "n", cex = 0.8, font = 2, col = "red4")


text(-7, 17.2, adj = 0, font = 3, cex = 1, "Time with:")
points(-5, 16.4, pch = 21, cex = 2, bg = "black")
text(-1, 16.5, adj = 0, cex = 0.9, "7 fossils")
points(-5, 15.7, pch = 21, cex = 2, bg = "green4")
text(-1, 15.8, adj = 0, cex = 0.9, "5 fossils")
points(-5, 15, pch = 21, cex = 2, bg = "purple3")
text(-1, 15.1, adj = 0, cex = 0.9, "3 fossils")
points(-5, 14.3, pch = 21, cex = 2, bg = "red4")
text(-1, 14.5, adj = 0, cex = 0.8, "Calibration")
text(-1, 14.1, adj = 0, cex = 0.8, "node")



