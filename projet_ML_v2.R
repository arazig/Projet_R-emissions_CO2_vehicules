#PROJET V2 ------------

#PRELIMINAIRE :

rm(list=ls(all=TRUE));gc();
repertoire="~/Desktop/L3 MIASHS/SEMESTRE 6/DataSciences/PROJETS6";
setwd(repertoire)

load('vehicules2014.Rdata')
df = vehicules2014

# Remplace dans le vecteur toutes les "," par des "."
df$conso_urb = gsub(",",".",df$conso_urb)
df$conso_exurb = gsub(",",".",df$conso_exurb)
df$conso_mixte = gsub(",",".",df$conso_mixte)

#----AUDIT RAPIDE DES DONNEES (+manipulation de la dataframe):-----

str(df)
# notre base de donnée possède 8937 observations et 19 variables
#notre cible est l'émission de co2
#Les 18 autres variables sont explicatives, certaines sont quantitatives d'autres
#qualitatives 

#on rectifie le type des variables si besoin : 
class(df$puiss_max) <- 'integer'
class(df$conso_mixte) <- 'numeric'
class(df$conso_exurb) <- 'numeric'
class(df$conso_urb) <- 'numeric'

#création d'une autre base de données en enlevant les valeurs manquantes (NA)
df= df[,-c(16,17,18,19)]


df=na.omit(df)




#----Séparation TRAIN/TEST----
# on utilise 80% de la data pour l'apprentissage et le reste pour le test
#Pour faire cela on agit de la maniere suivante : 
set.seed(2022)
n=8703
tx=0.8
separation= sample(1:n, tx*n)
TRAIN = df[separation,]
TEST = df[-separation,]

#Ca sert a quoi de faire ca ? /!\ important cette quetsion 


#----Test de Shapiro (gaussienne ou pas)----

#c'est pour savoir si notre variable cible est gausienne ou pas. 
df$co2= as.numeric(df$co2)
plot(density(df$Co2))
set.seed(2022)  #cette ligne permet d'avoir le meme echantillon aléatoire à chaque fois
indice= sample(1:8703, 5000)
dft=df[indice,]
shapiro.test(dft$co2)
#Probleme de test :
#H0 : Co2 suit une loi gaussienne 
#H1 : Co2 ne suit pas une loi gaussienne 
#Alpha = 0,05
#regle de décisions: si la p_value > aplha alors on choisit H0
#                    si la p_value < alpha alors on choisit H1
#resultat du test : la p_value est extrement petite donc on choisie H1 c'est a dire on rejette la gausiennité




#----ANALYSE BIVARIEE (CO2 x conso_mixte)----

plot(df$Co2,df$puiss_max)
#tout d'abord voici une représnetaion graphique ddu Co2 en fonction de la consomation mixte:
plot(df$Co2,df$conso_mixte)
# TEST DE CORRELATION 
#pour savoir si elles sont correllées on effectue un test de correlation :
cor.test(df$Co2,df$conso_mixte)
#H0 : cor=0 pas de lien 
#H1 : cor=!0 lien plus ou moins fort
#alpha =0.05
#regle de décisions: si la p_value > aplha alors on choisit H0
#                    si la p_value < alpha alors on choisit H1
#resultat du test : la p_value est extrement petite donc on choisie H1
#on conclue que les deux variable sont linéariement corélé car cor=0,98 

#l'objectif d'un modélisation c'est que l'on va faire deux cas d'usages :
#- le premier c'est l'ajustement (fit)
#- le second c'est la prediction 


XXX=c(df$puiss_max,df$conso_mixte,df$masse_ordma_max)
Co2=c(df$Co2,df$Co2,df$Co2)

coul=c(rep('blue',8703),rep('red',8703),rep('green',8703))
plot(XXX,Co2, col=coul)


#----premiere REGRESSION LINEAIRE : ----

# On effectue une premiere REGRESSION LINEAIRE : 
# nous avons choisit les regresseurs suivants car c'est ceux qui semblent le plus pertinant pour expliquer les émissions de co2
reg1=lm(df$co2~df$puiss_max+df$conso_mixte+df$masse_ordma_max)
plot(reg1)
summary(reg1)
coef(reg1)
summary(residuals(reg1)) 
#Il est important de vérifier les hypotheses:
# ui est gaussienne 
mean(reg1$residuals) # E(Ui) =0  OK!
                      # Var(Ui) = sigma^2   pour tout i homoelasticité brush pargan test
                      # E(ui*uj)=0          autocorrelation durbin watson test
#guasiannité des résidus OK  shapiro et KS
var.test(residuals(reg1),residuals(reg1))  # les deux autres hypotheses 

#    Interprétation des résultats : 
    # tout d'abord, le r2 =0,978 (presque 1), le modele explique bien
    # on obsreve 3 etoiles pour conso et masse_max
# on effectue ensuite le test de fisher pour évaluer la performance 
fisher.test()

#on utlise ensuite le pres de ALEM
# on prend la ligne i
#i=1, on l'enleve et on fait la régression
#je calcule le Fit de Ÿi on calcule la ligne i , et on regarde l'ajustement de lémission du véhicule i avec le modèle. 

#L'erreure commise (pour le véhicule i) est donc = E2= (Yi -Ÿi)^2 
#Le PRESS c'est donc la somme de toutes les erreurs comise 
#pour effectuer cela on utlise une fonction R qui se nomme: 
rstandard(reg, type='pred')
# ----> cela nous donne un vecteur avec tous les (Yi-Ÿi), il nous restera plus qu'a faire la somme des carrées 

Err= rstandard(reg1, type='pred')
Err=Err**2
Press= sum(Err)
Press
Pr = Press/sum(df$co2**2)



reg2=lm(df$co2~df$conso_mixte+df$masse_ordma_min)
summary(reg2)








#------Regression modele conso-----

# Les émissions de Co2 peuvent facilement etre mis en lien avec la consomation.La relation parrait plutot instinctive. 
#On propose le modèle de régression suivant :
rconso=lm(df$co2~df$conso_exurb+df$conso_mixte+df$conso_urb)
summary(rconso)
rconso$coefficients

#Il est important de vérifier les hypotheses:
                      #guasiannité des résidus,  shapiro et KS  NON!!!! probleme
mean(reg1$residuals)  # E(Ui) =0  OK!
                      # Var(Ui) = sigma^2   pour tout i homoelasticité brush pargan test
                      # E(ui*uj)=0          autocorrelation durbin watson test

plot(1:8703,rconso$residuals) #residus#
lines(1:8703, rep(sd(rconso$residuals)*2,8703), col='red')
lines(1:8703, rep(-sd(rconso$residuals)*2,8703),col='red')


plot(rconso)
qqline(residuals(rconso))
plot(df$co2,df$conso_urb)
plot(df$co2,df$conso_mixte)    #on voit bien que le lien entre ces variables est lineaire, pour verifier on fait le test de corrélation
plot(df$co2,df$conso_exurb)

model1=lm(df$co2~df$conso_exurb+df$conso_mixte+df$conso_urb+df$puiss_admin_98+df$puiss_max+df$masse_ordma_max+df$masse_ordma_min)
model2=lm(df$co2~df$conso_exurb+df$conso_mixte+df$conso_urb+df$puiss_admin_98+df$puiss_max+df$masse_ordma_max)
model3=lm(df$co2~df$conso_urb)
model3bis=lm(TEST$co2~TEST$conso_urb)

summary(model1)
summary(model2)
hist(model2$residuals) #OK
shapiro.test(model3bis$residuals)

summary(model3)
scatterplot(TEST$co2~TEST$conso_urb)
hist(model3$residuals) #OK
durbinWatsonTest(model3)
plot(model3,2)





bptest(model2)
bptest(model3)
#obsrevatiosn des resulats :
# le R2 est bon il est égale a 0,98
# la p_value est tres faible ce qui nous permet de conclure sur le test de Fisher => pertinance du modele
# analyse des résidus : 
#hypotheses sur les résidus :
bptest(model1)   # Var(Ui) = sigma^2   pour tout i homocedasticité brush pargan test h1 on rejette h0!

result= leaps(df$co2,c(df$conso_exurb,df$conso_mixte,df$conso_urb), method='Cp', nbest=1)


