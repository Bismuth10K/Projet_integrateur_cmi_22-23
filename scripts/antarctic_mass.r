############################################################
#                                                          #
#       Variable importante : Antarctic_mass               #
#                                                          #
############################################################


# Librairie

library("tidyverse")
library("ggplot2")

# chargement des données

AM = read.table(file = '../datasets_nasa/antarctica_mass.txt', header = FALSE, skip = 31)
header = c('time', 'Antarctic_mass', 'Antarctic_mass_uncertainty')
colnames(AM) = header
head(AM,5)

# boxplot des deux variables

ggplot(AM) +
  geom_boxplot(
    aes(y = Antarctic_mass)
  )+ labs(title="Boxplot de la perte de masse de l'antartique",y="masse (Gigatonnes)")

ggplot(AM) +
  geom_boxplot(
    aes(y = Antarctic_mass2)
  )+ labs(title="Boxplot Antarctic_mass2",y="masse (Gigatonnes)")

# Suppression des outlayers Antarctic_mass2

AM_2 = AM[which(AM$Antarctic_mass2<50),]

ggplot(AM_2) +
  geom_boxplot(
    aes(y = Antarctic_mass2)
  )+ labs(title="Boxplot Antarctic_mass2",y="masse (Gigatonnes)")

# plot des données + droite de régression

# Antarctic_mass

model1 = lm(AM$Antarctic_mass~AM$time)
sumModel1 = summary(model1)
sumModel1

ggplot(data = AM)  +
  geom_point(aes(x = time, y = Antarctic_mass))+
  geom_abline(intercept = sumModel1$coefficients[1],
              slope = sumModel1$coefficients[2], color="red")+
  labs(title = "Prédiction de la variable Antarctic_mass",
       x = "Années", "Diférence de Masse entre 2002 et la date actuel")

#Antarctic_mass2

model2 = lm(AM_2$Antarctic_mass2~AM_2$time)
sumModel2 = summary(model2)
sumModel2

ggplot(data = AM_2)  +
  geom_point(aes(x = time, y = Antarctic_mass2))+
  geom_abline(intercept = sumModel2$coefficients[1],
              slope = sumModel2$coefficients[2], color="red")+
  labs(title = "Prédiction de la variable Antarctic_mass2",
       x = "Années", "Diférence de Masse entre 2002 et la date actuel")
