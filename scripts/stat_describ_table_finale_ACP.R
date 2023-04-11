################################################################################
#                                                                              #
#                   Statistique descriptive de la table finale                 #
#             Pré-requis :  Avoir compilé script_importation_jointure          #
#                                                                              #
################################################################################

head(months_data)

summary(months_data)

# Corrélation

pairs(months_data, col = 'blue')
as.data.frame(as.table(cor(months_data, use = "complete.obs")))

# Toutes les variables semblent corrélées entre elles à part Area. 

library(tidyverse)

model = lm(years~months_data$`Antarctic mass (Gigatonnes)`+months_data$`monthly average` + months_data$`Greenland mass (Gigatonnes)` + months_data$No_Smoothing,data=months_data)
summary(model)$coefficients #P-value tres proche de 0, 
confint(model)
sigma(model)/mean(months_data$years) #RSE (standart residuals error) très petit, le modele est bon

#ACP
result.pca = princomp(months_data[,1:6],cor = TRUE)
biplot(result.pca)
#4
#biplot(result.pca, xlabs=nom)