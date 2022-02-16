install.packages("MASS")

#EXO 1 

#Importation donnees et preparation 
simu=read.table("C:/Users/dionc/Desktop/projetf_analyse_donnees/donnees/simu.txt",header=TRUE)

#On commence par suprimer toutes les lignes relatif aux données manquantes de nos 3 variables
library(tidyr)
simu=drop_na(simu,X1,X2,Y)

#transformation Y to type factor, on spécifie rien donc automatiquement : y=1-échec     y=2-succès
simu$Y<-as.factor(simu$Y)

#1) déterminer le meilleur modèle
#EStimation modele adittif
mod1 <- glm(Y ~ X1 + X2, data=simu, family = binomial(logit))
mod1

#selection meilleur modèle 
# critère BIC  
library(MASS)
mod1.bic<- stepAIC(mod1,direction="backward",k=log(dim(simu)[1]))
mod1.bic

# critère AIC 
mod1.aic<-  stepAIC(mod1, direction="backward",k=2)
mod1.aic

# La selection du meilleur modèle avec critère BIC et AIC nous donne le même résultat -> modèle additif avec toutes les variables

# Modèle avec interaction
mod2 <- glm(Y ~ X1*X2, data=simu, family = binomial(logit))
mod2
# l'AIC de ce modèle est très légérement meilleur on peut essayer de l'améliorer

mod2.aic<-stepAIC(mod2,direction="backward",k=2)
mod2.aic
# l'étape a garder le même modèle don l'AIC ne peut pas être améliorer
#mod2.aic est le meilleur modèle


#1) Predictions

# importation echantillon test
xsimutest=read.table("C:/Users/dionc/Desktop/projetf_analyse_donnees/donnees/xsimutest.txt",header=TRUE)

#recuperation des probabilités dans xsimutest avec type="response"
predictions<-cbind(xsimutest, predict(mod2, newdata = xsimutest,type="response", se = TRUE))
head(predictions)

#prediction y (rappel : y=1-échec  y=2-succès )
predictions$pred.Y = ifelse(predictions$fit > 0.5, 2, 1)
head(predictions)
#copie predictions
write.table(predictions$pred.Y,"C:/Users/dionc/Desktop/projetf_analyse_donees/donnees/predictions.txt",row.names=F,col.names=F)

#########################################################################################################################################

#EXO2

#Chargement des données et preparation 
chiens=read.table("C:/Users/dionc/Desktop/projetf_analyse_donees/donnees/chiens",header=TRUE)

chiens$TAI=as.factor(chiens$TAI)
chiens$POI=as.factor(chiens$POI)
chiens$VEL=as.factor(chiens$VEL)
chiens$INT=as.factor(chiens$INT)
chiens$AFF=as.factor(chiens$AFF)
chiens$AGR=as.factor(chiens$AGR)
chiens$FON=as.factor(chiens$FON)


#Analyse des correspondances multiples
library(FactoMineR)
library(factoextra)


res.mca<-MCA(chiens, quali.sup=7, graph=TRUE)

plot(res.mca, choix="ind",invisible=c("var","quali.sup"))
# Le nuage de points des individus représenté dans le 1 er plan factorielle révèle une forme particulière.
# En effet, on voit 4 groupes d'individus se formé. 

plot(res.mca,choix="ind",invisible="ind")
# Le 1er axe oppose les races de chiens grand,lourds et agressive pas affectueux aux races de chiens petits, légers non agressive et affectueux. 
# Le second axe oppose les modalitées (INT_1) aux modalités (INT_2, INT_3), il oppose donc les races de chiens à l'intelligence faible aux races de chiens à l'intellignece élevé. 

#Description automatiques des axes
dimdesc(res.mca)
#Ici le 1er axe est caractérisé par les variables taille, poids, affectuosité, agressitivité et vélocité et la vraiable supplémentaire fonction
#Le 2nd  axe est caractérisé par la variable intelligennce et les variables taille, poids et vélocité.

plotellipses(res.mca)


res.mca$eig
#Le pourcentage d’inertie expliqué par le premier plan factoriel est la somme de la variance expliquée par les deux premiers facteurs soit environ 51,98 %

# Contributions en variance sur chaque axe
# Contributions relatives des individus
contrib_ind <- res.mca$ind$contrib
contrib_ind


# Qualité de représentation des indivius
cos2 <- res.mca$ind$cos2
cos2

# Contributions relatives des variables
contrib_var <- res.mca$var$contrib
contrib_var
