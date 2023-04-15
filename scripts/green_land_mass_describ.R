############################################################
#                                                          #
#       Variable importante : Greenland_mass               #
#                                                          #
############################################################


# Librairie 

library("tidyverse")
library("ggplot2")

# chargement des données

GM = read.table(file = 'datasets_nasa/greenland_mass.txt', header = FALSE, skip = 31)
header = c('TIME', 'Greenland_mass', 'Greenland_mass_1sigma_uncertainty')
colnames(GM) = header
head(GM)

# Statistiques descriptives de 

# boxplot des deux variables

ggplot(GM) +
  geom_boxplot(
    aes(y = Greenland_mass)
  )+ labs(title="Boxplot de la masse de l'antartique par rapport à 2002",y="masse (Gigatonnes)")

ggplot(GM) +


# Suppression des outlayers Greenland_mass2

GM_2 = GM[which(GM$Greenland_mass_1sigma_uncertainty<20),]

ggplot(GM_2) +
  geom_boxplot(
    aes(y = Greenland_mass_1sigma_uncertainty)
  )+ labs(title="Boxplot Greenland_mass2",y="masse (Gigatonnes)")

# plot des données + droite de régression

# Greenland_mass

model1 = lm(GM$Greenland_mass~GM$TIME)
sumModel1 = summary(model1)
sumModel1

ggplot(data = GM)  +
  geom_point(aes(x = TIME, y = Greenland_mass))+
  geom_abline(intercept = sumModel1$coefficients[1],
              slope = sumModel1$coefficients[2], color="red")+
  labs(title = "Prédiction de la variable Greenland_mass",
       x = "Années", y="Différence de Masse entre 2002 et la date actuel")

#Greenland_mass2

model2 = lm(GM_2$Greenland_mass_1sigma_uncertainty~GM_2$TIME)
sumModel2 = summary(model2)
sumModel2

ggplot(data = GM_2)  +
  geom_point(aes(x = TIME, y = Greenland_mass_1sigma_uncertainty))+
  geom_abline(intercept = sumModel2$coefficients[1],
              slope = sumModel2$coefficients[2], color="red")+
  labs(title = "Prédiction de la variable Greenland_mass_1sigma_uncertainty",
       x = "Années", "Diférence de Masse entre 2002 et la date actuel")

