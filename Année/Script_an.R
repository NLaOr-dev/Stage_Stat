#Prérequis
setwd("~/Traitement de donnée/Data grooves/R_file/FloreMousse_pollution")
library(readr)
#Partie flore
#Récupération des traits sur l'année 2019
Flore_2019 <- read_delim("~/Traitement de donnée/Data grooves/R_file/FloreMousse_pollution/DataFlore_2019.csv", ";", escape_double = FALSE, trim_ws = TRUE)
Flore_Traits <- read_delim("~/Traitement de donnée/Data grooves/R_file/FloreMousse_pollution/DataFlore_Traits2.csv", ";", escape_double = FALSE, trim_ws = TRUE)
Etiquette<-paste(Flore_2019$`nom court2019`, Flore_2019$Site) #créer une étiquette espece/site
Flore_2019<-cbind(Flore_2019, Etiquette)
doublon<-which(duplicated(Flore_2019$Etiquette)) #suprimer les doublons pour avoir une espece par toit (perte de la donnée abondance)
F_2019<-Flore_2019[-doublon,] 
doublon2<-which(duplicated(Flore_Traits$`nom court2019`)) #supprimr les doublons des traits
F_T<-Flore_Traits[-doublon2,]
#Fusion des traits et espèces de 2019 
library(dplyr)
Flore_pollution<-left_join(F_2019, F_T, by="nom court2019", na_matches="never")
#Flore_pollution<-semi_join(F_2019, F_T, by="nom court2019",)
summary(is.na(Flore_pollution$Res_ETM))#Nombre de ligne avec aucune REs_ETM référencée
is.na(Flore_pollution$Res_ETM)->NB
Verif<-cbind(Flore_pollution$"nom court2019", NB) #34 espece NA
write.csv(Flore_pollution, "FlorePollution_2019.csv",  na="NA") 
#Récupération des traits 2018 et 2017 (erreur dans Flore Amandine il y a l'echantillon global)
Flore_78 <- read_delim("~/Traitement de donnée/Data grooves/R_file/FloreMousse_pollution/DataFlore_1718.csv", ";", escape_double = FALSE, trim_ws = TRUE)
Flore_Traits <- read_delim("~/Traitement de donnée/Data grooves/R_file/FloreMousse_pollution/DataFlore_Traits2.csv", ";", escape_double = FALSE, trim_ws = TRUE)
Etiquette<-paste(Flore_78$`nom court2019`, Flore_78$Site)
Flore_78<-cbind(Flore_78, Etiquette)
doublon<-which(duplicated(Flore_78$Etiquette))
F_78<-Flore_78[-doublon,] 
doublon2<-which(duplicated(Flore_Traits$`nom court2019`))
F_T<-Flore_Traits[-doublon2,]
library(dplyr)
FPolt<-left_join(F_78, F_T, by="nom court2019", na_matches="never")
#Flore_pollution<-semi_join(F_2019, F_T, by="nom court2019",)
Pola<-FPolt
summary(is.na(FPolt$Res_ETM))#Nombre de ligne avecaucune REs_ETM référencée
is.na(FPolt$Res_ETM)->NB2
Verif2<-cbind(FPolt$"nom court2019", NB2) #34 espece NA
write.csv(FPolt, "FlorePollution_TotQuadrat.csv",  na="NA") 
#Fusion des trois années
TPres<-rbind(Pola, Flore_pollution)
write.csv(TPres, "TableauBrut_Complet.csv",  na="NA") 


#dfférence entre les troiq années
setwd("~/Traitement de donnée/Data grooves/R_file/Année")
# On teste les présences absences selon les 3 années
library(vegan)
library(reshape2)
Eti<-paste(TPres$Site, TPres$Annee, sep="-")
TPres2<-cbind(TPres, Eti)
Pres3<-table(TPres2$Eti, TPres2$"nom court2019")
Pres4<-decostand(Pres3, method="pa")
Pres5<-(vegdist(Pres4, method = "jaccard", binary = FALSE))
hc<-hclust(Pres5, "ward.D")
Mat<-plot(as.dendrogram(hc), horiz=F, cex=2)
library(dendextend)
Mat<-set(Dend, "labels_cex", 0.25)
Mat<-Mat%>%color_branches(k=36)
plot(Mat)

library(permute)
library(lattice)
Mat1<-Pres3[typo1=="1",]

Pres5<-quasieuclid(Pres5)
PCO<-dudi.pco(Pres5, scan=F)

An<-data.frame(table(TPres$"Annee", TPres$"Site"))
colnames(An)<-c("Annee", "Site", "Freq")
Eti<-paste(An$Site, An$Annee, sep="-")
An<-cbind(An, Eti)
An<-subset(An, An$Freq !=0)

AOV<-aov(PCO$li[,1]~An$Site)#chelou
Tu<-TukeyHSD(AOV, "An$Site", ordered = TRUE )
plot(TukeyHSD(AOV, "An$Site", ordered = TRUE ))
AOV<-aov(PCO$li[,1]~An$Annee)#illisible
Tu<-TukeyHSD(AOV, "An$Annee", ordered = TRUE )
plot(TukeyHSD(AOV, "An$Annee", ordered = TRUE ))
library(FactoMineR)
library(factoextra)

fviz_ca_row(PCO, repel=TRUE)
g3<-s.class(PCO$li, fac = "An$Site")

HCPC(PCO$li, nb.clust=4, graph=TRUE)

AOV<-aov(PCO$li[,1]~R_Gestoit$Type_Toiture)#chelou
Tu<-TukeyHSD(AOV, "An$Site", ordered = TRUE )
plot(TukeyHSD(AOV, "An$Site", ordered = TRUE ))

#Test avec indice de sokal-Michener (symétrique)
#library(clusterSim)
#dist.SM(Pres2)

#Classes d'âge
par(mfrow=c(1,1)) 
Annee_Toit <- read_delim("~/Traitement de donnée/Data grooves/R_file/Année/Annee_Toit.csv", ";", escape_double = FALSE, trim_ws = TRUE)
hist(Annee_Toit$Livraison)