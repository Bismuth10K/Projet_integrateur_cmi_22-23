################################################################################
#                                                                              #
#                   Statistique descriptive de la table finale                 #
#             Pré-requis :  Avoir compilé script_importation_jointure          #
#                                                                              #
################################################################################
#Variable V9 = Variation GMLS (GIA on)
boxplot( x=SL$V9,names=TRUE, main="BoxPlot de la variation du GMLS(GIA on)")
plot(x = SL$V3, y=SL$V9, xlab = "Années fractionnelles", ylab = "Variation du GMLS(GIA on)")
linear = lm(V9~V3,data=SL)
summary(linear)
linear$coefficients
abline(linear$coefficients[1],linear$coefficients[2], col="red")