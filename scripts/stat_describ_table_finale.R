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