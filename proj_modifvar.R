

rm(list=ls(all=TRUE));gc();
repertoire="~/Desktop/L3 MIASHS/SEMESTRE 6/DataSciences/PROJETS6";
setwd(repertoire)
vehicules2014= read.csv2('mars-2014-complete.csv', header = TRUE, sep=';' , dec='.')


vehicules2014 <- vehicules2014[,-c(2,3,5,6)]
vehicules2014 <- vehicules2014[,-c(12,13,14,15,16)]
vehicules2014 <- vehicules2014[,-c(14)]
vehicules2014 <- vehicules2014[,-c(14)]


colnames(vehicules2014)[1] <- 'MARQUE'
colnames(vehicules2014)[2] <- 'MODELE'
colnames(vehicules2014)[3] <- 'CARBURANT'
colnames(vehicules2014)[4] <- 'Hybride'
colnames(vehicules2014)[7] <- 'nbr_Rapport'
colnames(vehicules2014)[2] <- 'MODELE'
colnames(vehicules2014)[2] <- 'MODELE'

n = length(vehicules2014$MARQUE)

for (i in 1:n){
  if (vehicules2014$gamme[i]=='LUXE'){
    vehicules2014$gamme[i] <- 1
  }
  if (vehicules2014$gamme[i]=='SUPERIEURE'){
    vehicules2014$gamme[i] <- 2
  }
  if (vehicules2014$gamme[i]=='MOY-SUPER'){
    vehicules2014$gamme[i] <- 3
  }
  if (vehicules2014$gamme[i]=='MOY-INFER'){
    vehicules2014$gamme[i] <- 4
  }
  if (vehicules2014$gamme[i]=='INFERIEURE'){
    vehicules2014$gamme[i] <- 5
  }
  if (vehicules2014$gamme[i]=='ECONOMIQUE'){
    vehicules2014$gamme[i] <- 6
  }
}

unique(vehicules2014$Carrosserie)
unique(vehicules2014$gamme)
for (i in 1:n){
  if (vehicules2014$Carrosserie[i]=='COMBISPACE'){
    vehicules2014$Carrosserie[i] <- 'UTILITAIRE'
  }
}

indice2= which(vehicules2014$Carrosserie =='MINIBUS')

vehicules2014 <- vehicules2014[-c(indice2),]
