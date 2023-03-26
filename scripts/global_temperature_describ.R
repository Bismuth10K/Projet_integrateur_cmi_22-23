################################################################################
#                                                                              #
#               Etude descriptive de la table Global temperature               #
#                   Variable intéressante : No_Smoothing                       #
#                                                                              #
################################################################################

# Librairie 

library("tidyverse")
library("ggplot2")

# importation des données

GT = read.table(file = 'datasets_nasa/datasets_nasa/global_temperature.txt',
                header = TRUE, sep=';', skip = 3)
head(GT)

# Statistique descriptives de la variable No_smoothing

summary(GT$No_Smoothing)

# Boxplot des variables 

ggplot(GT) +
  geom_boxplot(
    aes(y = No_Smoothing)
  )+ labs(title="Boxplot de la quantité de dioxyde de carbone",y="ppm")

# graphiques d'analyse

# No_smoothing : scatterplot + droite de régression simple

model1 = lm(GT$No_Smoothing~GT$Year)
sumModel1 = summary(model1)
sumModel1

ggplot(data = GT)  +
  geom_point(aes(x = Year, y = No_Smoothing))+
  geom_abline(intercept = sumModel1$coefficients[1],
              slope = sumModel1$coefficients[2], color="red")+
  labs(title = "Prédiction de la température par rapport à sa valeur moyenne entre 1951 et 1980",
       x = "Year", y="degré Celsius")

# Modèle de moins bonne qualité