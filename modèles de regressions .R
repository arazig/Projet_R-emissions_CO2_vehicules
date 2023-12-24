#PROJET V2 ------------

#PRELIMINAIRE :

rm(list=ls(all=TRUE));gc();
repertoire="~/Desktop/L3 MIASHS/SEMESTRE 6/DataSciences/PROJETS6";
setwd(repertoire)

load('Vehicules2014V2.Rdata')
# Afin d'aller plus loins dans la compréhension de la variable cible il est primordiale de determiner des modeles de régressions qui permettraient de mettre en évidence les liens entres régressuers et varible cible. 
# C'est pourquoi trois jeunes étudiants passionnée de voitures prosposent a leurs tours un modele qui leurs semble etre le meilleur

#######MODELE 1 (PUISSANCE) #########
r1conso=lm(df$co2~df$puiss_admin_98)#+df$puiss_max)
summary(r1conso)

t.test(df$co2,df$puiss_admin_98)
r1conso$coefficients
n=length(df$co2)

plot(1:n,r1conso$residuals, main= 'représentation graphique des résidus modèle VICTOR') #residus#
lines(1:n, rep(sd(r1conso$residuals)*2,n), col='red')
lines(1:n, rep(-sd(r1conso$residuals)*2,n),col='red')
par(mfrow=c(2,2))
plot(r1conso)
scatterplot(df$co2~df$puiss_admin_98)

#Il est important de vérifier les hypotheses:
shapiro.test(r1conso$residuals)
hist(r1conso$residuals) #NON #guasiannité des résidus,  shapiro et KS  NON!!!! probleme

mean(r1conso$residuals)    #OK    E(Ui) =0  
durbinWatsonTest(r1conso)  

#OK    E(ui*uj)=0          autocorrelation durbin watson test
ncvTest(r1conso)           #OK    Var(Ui) = sigma^2   pour tout i homoelasticité brush pargan test

#######MODELE 2 (conso) #########
rconso=lm(df$co2~df$conso_exurb+df$conso_urb)#+df$conso_mixte
summary(rconso)
rconso$coefficients
n=length(df$co2)
plot(1:n,rconso$residuals, main='résidus û du modèle de Youcef') #residus#
lines(1:n, rep(sd(rconso$residuals)*2,n), col='red')
lines(1:n, rep(-sd(rconso$residuals)*2,n),col='red')


par(mfrow=c(2,2))
plot(rconso)
scatterplot(df$co2~df$conso_urb)

#Il est important de vérifier les hypotheses:
shapiro.test(rconso$residuals)
hist(rconso$residuals) #NON #guasiannité des résidus,  shapiro et KS  NON!!!! probleme

mean(rconso$residuals)    #OK    E(Ui) =0  
durbinWatsonTest(rconso) #OK    E(ui*uj)=0          autocorrelation durbin watson test
ncvTest(rconso) #OK    Var(Ui) = sigma^2   pour tout i homoelasticité brush pargan test
anova(rconso)
#######MODELE 3 (conso + puiss + masse) ###### 
r2paul=lm(df$co2 ~ df$conso_mixte+df$puiss_max+df$puiss_admin_98+df$masse_ordma_max)
summary(r2paul)
r2paul$coefficients
n=length(df$co2)
plot(1:n,r2paul$residuals, main="résidus û du modèle de Paul") #residus#
lines(1:n, rep(sd(r2paul$residuals)*2,n), col='red')
lines(1:n, rep(-sd(r2paul$residuals)*2,n),col='red')

plot(r2paul)
scatterplot(df$co2~df$conso_urb)

#Il est important de vérifier les hypotheses:
shapiro.test(r2paul$residuals)
hist(r2paul$residuals) #NON #guasiannité des résidus,  shapiro et KS  NON!!!! probleme

mean(r2paul$residuals) # (E(Ui) =0 ) OK
durbinWatsonTest(r2paul)# OK   # E(ui*uj)=0          autocorrelation durbin watson test
ncvTest(r2paul) #NON    # Var(Ui) = sigma^2   pour tout i homoelasticité brush pargan test


qqline(residuals(r2paul))
plot(df$co2,df$conso_urb)
plot(df$co2,df$conso_mixte)    #on voit bien que le lien entre ces variables est lineaire, pour verifier on fait le test de corrélation
plot(df$co2,df$conso_exurb)

#on manipule la data frame pour retirer les valeusr abérantes qui fausse le model
df=df[-c(294,283,95,498),]

result= leaps(df$co2,c(df$conso_exurb,df$conso_mixte,df$conso_urb), method='Cp', nbest=1)

mean(r2paul$residuals)      # OK    (E(Ui) =0 ) 
durbinWatsonTest(r2paul)    # OK    #E(ui*uj)=0          autocorrelation durbin watson test
ncvTest(r2paul)             # OK    # Var(Ui) = sigma^2   pour tout i homoelasticité brush pargan test



