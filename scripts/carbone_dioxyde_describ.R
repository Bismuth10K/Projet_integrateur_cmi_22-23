################################################################################
#                                                                              #
#               Etude descriptive de la table Carbone dioxyde                  #
#                   Variable intéressante : monthly average                    #
#                                                                              #
################################################################################

# Librairie 

library("tidyverse")
library("ggplot2")

# importation des données

CD = read.table(file = 'datasets_nasa/datasets_nasa/carbon-dioxyde.txt', header = FALSE, skip = 54)
CD = CD[-c(1,2)]
header = c('Date', 'monthly average', 'de-season alized', '#days', 'st.dev of days', 'unc. of mon mean')
colnames(CD) = header
head(CD)

# Statistique descriptives de la variable monthly average

summary(CD$`monthly average`)

# Boxplot des variables 

ggplot(CD) +
  geom_boxplot(
    aes(y = `monthly average`)
  )+ labs(title="Boxplot de la quantité de dioxyde de carbone",y="ppm")

ggplot(CD) +
  geom_boxplot(
    aes(y = `de-season alized`)
  )+ labs(title="Boxplot de-season alized",y="ppm")

# graphiques d'analyse

# monthly average : scatterplot + droite de régression simple

model1 = lm(CD$`monthly average`~CD$Date)
sumModel1 = summary(model1)
sumModel1

ggplot(data = CD)  +
  geom_point(aes(x = Date, y = `monthly average`))+
  geom_abline(intercept = sumModel1$coefficients[1],
              slope = sumModel1$coefficients[2], color="red")+
  labs(title = "Prédiction de la quantité de dioxyde de carbone en fonction du temps",
       x = "Date", y="ppm")
