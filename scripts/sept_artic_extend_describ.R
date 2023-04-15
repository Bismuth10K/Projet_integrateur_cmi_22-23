################################################################################
#                                                                              #
#               Etude descriptive de la table Sept_Artic_extend                #
#                   Variable intéressante : area                               #
#                                                                              #
################################################################################

# Librairie 

library("tidyverse")
library("ggplot2")

# importation des données

SAE = read.table(file = "datasets_nasa/Sept_Artic_extend.txt", header = TRUE)
SAE$area = as.numeric(sub(",", ".", SAE$area, fixed = TRUE))

head(SAE)

# Statistique descriptives de la variable No_smoothing

summary(SAE$area)

# Boxplot des variables 

ggplot(SAE) +
  geom_boxplot(
    aes(y = area)
  )+ labs(title="Boxplot de l'aire de l'arctique",y="dizaine de millions de kilomètres carrés")

# graphiques d'analyse

# area : scatterplot + droite de régression simple

model1 = lm(SAE$area~SAE$year)
sumModel1 = summary(model1)
sumModel1

ggplot(data = SAE)  +
  geom_point(aes(x = year, y = area))+
  geom_abline(intercept = sumModel1$coefficients[1],
              slope = sumModel1$coefficients[2], color="red")+
  labs(title = "Prédiction de l'aire de l'Arctique en fonction des années",
       x = "Year", y="dizaines de millions de kilomètres carrés")

# Modèle de moins bonne qualité
