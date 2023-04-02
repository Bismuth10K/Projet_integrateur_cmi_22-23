################################################################################
#                                                                              #
#                   Statistique descriptive de la table finale                 #
#             Pré-requis :  Avoir compilé script_importation_jointure          #
#                                                                              #
################################################################################
#V6, variation GIA off
boxplot( x=SL$V6,names=TRUE, main="BoxPlot de la variation du GMLS(GIA off)")
plot(x = SL$V3, y=SL$V6, xlab = "Années fractionnelles", ylab = "Variation du GMLS(GIA off)")
linear = lm(V6~V3,data=SL)
summary(linear)
linear$coefficients
abline(linear$coefficients[1],linear$coefficients[2], col="red")