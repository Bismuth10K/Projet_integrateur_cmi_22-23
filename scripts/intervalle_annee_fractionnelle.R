################################################################################
#                                                                              #
#                   Statistique descriptive de la table finale                 #
#             Pré-requis :  Avoir compilé script_importation_jointure          #
#                                                                              #
################################################################################
#année frac (v3)
boxplot(x=SL$V3)
fracMin = min(x=SL$V3)
fracMin
fracMinMois = as.integer((fracMin-1993)*12)+1
fracMinMois
fracMax = max(x=SL$V3)
fracMax
fracMaxMois = as.integer((fracMax-2022)*12)+1
fracMaxMois
#Resultat entre janvier 1993 et Juillet 2022