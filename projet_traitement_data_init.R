#PRELIMINAIRE :

rm(list=ls(all=TRUE));gc();
repertoire="~/Desktop/L3 MIASHS/SEMESTRE 6/DataSciences/PROJETS6";
setwd(repertoire)

load('vehicules2014.Rdata')
v = vehicules2014

# Remplace dans le vecteur toutes les "," par des "."
v$conso_urb = gsub(",",".",v$conso_urb)
v$conso_exurb = gsub(",",".",v$conso_exurb)
v$conso_mixte = gsub(",",".",v$conso_mixte)

#   AUDIT RAPIDE DES DONNEES:

str(v)
# notre base de donnée possède 8937 observations et 19 variables
#notre cible est l'émission de co2
#Les 18 autres variables sont explicatives, certaines sont quantitatives d'autres
#qualitatives 

#on rectifie le type des variables si besoin : 
class(v$puiss_max) <- 'integer'
class(v$conso_mixte) <- 'numeric'
class(v$conso_exurb) <- 'numeric'
class(v$conso_urb) <- 'numeric'

#création d'une autre base de données en enlevant les valeurs manquantes (NA)
v= v[,-c(16,17,18,19)]


df=na.omit(v)

#changement de classe de certains véhicules.
ind1= grep("BRABUS_?", df$MODELE, value=FALSE, ignore.case = TRUE)
brabus = df[c(ind1),]
for( i in c(ind1)){
  df$co2[i]= 203
}
ind2=which(df$gamme=='1' & df$co2<110)
luxecolo=df[c(ind2),]
for( i in c(ind2)){
  df$co2[i]= 100
}

ind3=which(df$gamme=='4' & Co2>=189 & Co2<=190)
cate4F = df[c(ind3),]
for( i in c(ind3)){
  df$co2[i]= 202
}
ind4=which(df$gamme=='4' & Co2>190)
cate4G = df[c(ind4),]
for( i in c(ind4)){
  df$co2[i]= 255
}

ind5= which(df$gamme=='5' & Co2>=180 & Co2<184)
cate5F=df[c(ind5),]
for( i in c(ind5)){
  df$co2[i]= 201
}

ind6= which(df$gamme=='5' & Co2>=184)
cate6G=df[c(ind6),]
for( i in c(ind6)){
  df$co2[i]= 251
}

ind7 = which(df$gamme=='6' & df$puiss_max >= 118 & df$conso_mixte>= 6.5)
cate7=df[c(ind7),]
for( i in c(ind7)){
  df$co2[i]= 251
}

ind8= which(df$gamme=='6' & Co2>=141 & Co2<=160 & df$puiss_max==118)
cate8= df[c(ind8),]
for( i in c(ind8)){
  df$co2[i]= 163
}

df$co2[1534]=163

ind10= which(df$gamme=='2' &Co2>=101 & Co2<=103 & df$puiss_max<=125)
cate2B= df[c(ind10),]
for( i in c(ind10)){
  df$co2[i]= 100
}

ind11=which(df$gamme=='6' & Co2>=201 & Co2<=250 & df$puiss_max== 62 &df$Carrosserie =="COUPE")
cate6F = df[c(ind11),]
for( i in c(ind11)){
  df$co2[i]= 180
}
#df a 8703 observations

#   LE CONTEXTE, Problématique : 

# La préservation de l'environnement est devenue un enjeux majeur, au centre de tous
#les débats. 
#La législation française ne le considère pas comme un polluant du fait de sa 
#présence naturelle dans l’atmosphère et de son rôle dans le cycle de la vie.

#Toutefois, le dioxyde de carbone est le principal gaz à effet de serre à l’état
#naturel, avec la vapeur d’eau. Sa durée de vie dans l’atmosphère est d’environ 100 ans. 
#Il est produit lorsque des composés carbonés sont brûlés en présence d’oxygène.
#Sous l’action de l’homme, le taux de CO2 dans l’atmosphère augmente régulièrement 
#depuis des millions d’années. Une hausse brutale de 30 % a été observée au cours des deux derniers siècles.

#Avant l’ère industrielle, la concentration du CO2 dans l’atmosphère était d’environ 280 ppm.
#Aujourd’hui, sa concentration atteint les 400 ppm même dans les régions les moins industrialisées.

#Une concentration trop élevé du tx de co2 peux avoir des effets dramatiques sur le vivant. 
#Comme : une modification du rythme respiratoire, création de crise d'asthme etc...

#Les véhicules automobiles sont aujourd'hui sujet à débat puisqu'ils sont émetteurs de 
#Co2. D'autant plus que dans le contexte de hausse importante du prix du carburant en 2022
# il est est devenue logique de se posser la question d'autres alternatives aux vehivules 
#traditionnels permettant par la meme occasion de réduir les émission de co2 et 
#reduirs le couts du transport. 

# C'est pourquoi on se demande comment expliquer le niveau d'émissions de Co2 selon les différentes caractéristiques des véhicules. 


summary(df)
#   Brève étude statistique de notre cible : 
# Notre cible est quanti continue, allant de 13g/km à 572g/km. A partir de nos données,
#les vehicules de 2014 émettent en moyenne 156.6 g/km. 
 


#pour notre cible co2: 

#ETUDE UNIVARIÉE DE LA CIBLE CO2:

par(mfrow=c(1,1))
str(df$co2)
summary(df$co2) # nous donne les charactéristiques statistiques
hist(df$co2)
plot(density(df$co2))


#PERIMETRE DESCRIPTIVE : (étude univariée de chaques variable, summary + histogramme)

par(mfrow= c(3,3))
summary(df$MARQUE)          # quali, 46 marques
unique(df$CARBURANT)        # quali, 13 types de carburants
summary(df$MODELE)          #c'est le modèle de chaque véhicule 
summary(df$CARBURANT)       #c'est le type de carburant, 
summary(df$Hybride)         # c'est l'hybridation (oui/non)
summary(df$puiss_admin_98)  # variable quanti, puissance administratif
hist(df$puiss_admin_98)
summary(df$puiss_max)       #variable quanti, puissance en Kw
hist(df$puiss_max)
#plot(density(df$puiss_max))
summary(df$nbr_Rapport)     # etc...
summary(df$conso_urb)
hist(df$conso_urb)
summary(df$conso_exurb)     # etc...
hist(df$conso_exurb)
summary(df$conso_mixte)
hist(df$conso_mixte)
summary(df$masse_ordma_min)
hist(df$masse_ordma_min)
summary(df$masse_ordma_max)
hist(df$masse_ordma_min)
summary(df$gamme)
summary(df$Carroserie)


#PRE-TRAITEMENT DES DONNÉES :

#explifation des choix de tranche : #dicrétisation du co2 :
  #   Les premières étiquettes énergie sont apparues sur les voitures neuves vendues en France dans
  #   le cadre de la directive européenne du 13 décembre 1999. Seules les automobiles neuves destinées
  #   aux particuliers, qui étaient exposées dans un espace de vente, furent concernées. Dès 2006, 
  #   le gouvernement français a choisi d'étendre cet étiquetage à l'ensemble des voitures neuves
  #   vendues sur le territoire. L'objectif principal est de sensibiliser les consommateurs en leur 
  #   donnant accès aux informations concernant le niveau d'émission de CO2 de chaque véhicule afin 
  #   de les inciter à comparer puis à choisir des voitures moins polluantes. Pour les accompagner 
  #   dans cette démarche et faciliter leur compréhension, c'est au travers d'un barème à plusieurs 
  #   niveaux qu'ils pourront savoir la classe à laquelle une voiture appartient. Chaque année, 
  #   l'Agence de l'environnement et de la maîtrise de l'énergie publie le classement des 10 
  #   premières valeurs d'émissions de CO2 sur son site internet.

#Les sept niveaux d'émission de CO2
#De A à G, du vert au rouge, les niveaux d'émission de carbone d'un véhicule pour pour 100 kilomètres parcourus, sont définis de cette façon :
  #niveau A : pour les véhicules dont les émissions de CO2 sont inférieures ou égales à 100 g/km
  #niveau B : pour les véhicules dont les émissions de CO2 varient de 101 à 120 g/km
  #niveau C : pour les véhicules dont les émissions de CO2 varient de 121 à 140 g/km
  #niveau D : pour les véhicules dont les émissions de CO2 varient de 141 à 160 g/km
  #niveau E : pour les véhicules dont les émissions de CO2 varient de 161 à 200 g/km
  #niveau F : pour les véhicules dont les émissions de CO2 varient de 201 à 250 g/km
  #niveau G : pour les véhicules dont les émissions de CO2 sont supérieures à égales à 250 g/km

Co2= df$co2
CO2_TR= Co2
indice1= which(Co2<=100)
for( i in c(indice1)){
  CO2_TR[i]<- 'A'
}
indice2= which(Co2>=101 & Co2<=120)
for( i in c(indice2)){
  CO2_TR[i]<- 'B'
}
indice3= which(Co2>=121 & Co2<=140)
for( i in c(indice3)){
  CO2_TR[i]<- 'C'
}
indice4= which(Co2>=141 & Co2<=160)
for( i in c(indice4)){
  CO2_TR[i]<- 'D'
}
indice5= which(Co2>=161 & Co2<=200)
for( i in c(indice5)){
  CO2_TR[i]<- 'E'
}
indice6= which(Co2>=201 & Co2<=250)
for( i in c(indice6)){
  CO2_TR[i]<- 'F'
}
indice7= which(Co2>=251)
for( i in c(indice7)){
  CO2_TR[i]<- 'G'
}
#mise en classe de CO2_TR, dicrétisation complète
CO2_TR <- as.factor(CO2_TR)
par(mfrow=c(1,2))
summary(CO2_TR) ; 
prop.table(table(CO2_TR));
pie(table(table(CO2_TR)), col= c(palette(hcl.colors(10))));
barplot(table(CO2_TR), col= c(palette(hcl.colors(10))),main = 'Catégories des véhicules commercialisés en 2014 en France')

#On s'interresse à présent au lien entre le Co2 et la puissance des véhicules
#On choisit comme variable explicative la puissance max car celle ci semble 
# etre la plus légitime pour expliquer les émissions de Co2

#ANALYSE globale, etude bivarié global : QUALI x QUALI GAMME
df$gamme<- as.factor(df$gamme)
str(df$gamme)
summary(df$gamme)

#       le probleme de test :

#H0 : les variables indep
#H1 : les variables dépendante, hypothese, 
#regle de décison
#choix
#alpha =0.05

contingence=table(df$gamme,CO2_TR)
prop.table(table(df$gamme,CO2_TR))*100

#On effectue le test du chi2 :
res= chisq.test(table(df$gamme,CO2_TR));res

#on applique la regle de désision et on trouve que la gamme implique les émission de co2 

n=dim(df)[1]
res$observe #c'est le tabelau de contingence , on divise par n pour l'avoir en proba
#et non pas ne effectif

res$expected/n #la loi théorique si il y avait indépedance 

loi_obs = res$observe/n
loi_theo = res$expected/n
dist_chi2= (loi_theo - loi_obs)^2/loi_theo
dist_chi2 = dist_chi2/sum(dist_chi2)
dist_chi2 
# avec ca on peut dire qu les modealite qui influe le plus 
#a la dépandenace c'est les aborigen qui contribue le plsu a l'absenteisme)





#test de shapiro: c'est pour savoir si notre variable cible est gausienne ou pas. 

hist(Co2)
indice= sample(1:8703, 500)
dft=df[indice,]
shapiro.test(dft$co2)
#H0 et H1, regle de décisions, alpha ...
#resultat du test : la p_value est extrement petite donc on choisie H1 c'est a dire on rejette la gausiennité
class(v)

# faire student (pas sure qu'il faut le mettre) +fisher 


boxplot(v$co2)
summary(v$co2)

reg_marque = lm(v$co2 ~ v$MARQUE)
reg_modele = lm(v$co2 ~ v$MODELE)
reg_carburant = lm(v$co2 ~ v$CARBURANT)
reg_hybride = lm(v$co2 ~ v$Hybride)
reg_puiss_admin_98 = lm(v$co2 ~ v$puiss_admin_98)
reg_puiss_max = lm(v$co2 ~ v$puiss_max)
reg_nbr_Rapport = lm(v$co2 ~ v$nbr_Rapport)
reg_conso_urb = lm(v$co2 ~ v$conso_urb)
reg_conso_exurb = lm(v$co2 ~ v$conso_exurb)
reg_conso_mixte = lm(v$co2 ~ v$conso_mixte)
reg_masse_ordma_min = lm(v$co2 ~ v$masse_ordma_min)
reg_masse_ordma_max = lm(v$co2 ~ v$masse_ordma_max)
reg_carrosserie = lm(v$co2 ~ v$Carrossserie)
reg_gamme = lm(v$co2 ~ v$gamme)


plot(v$puiss_max, v$co2); qqline(v$co2)
summary(reg_marque)
summary(reg_carburant)
summary(reg_puiss_max)
plot(v$puiss_admin_98,v$co2)
reg = lm(v$co2 ~., data = co2)
summary(reg)
coef(reg)
confint(reg)


ind= which(df$gamme=='6')
eco=df[c(ind),]

ind1= grep("BRABUS_?", df$MODELE, value=FALSE, ignore.case = TRUE)
brabus = df[c(ind1),]
for( i in c(ind1)){
  df$co2[i]= 252
}
