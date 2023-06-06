################################################################################
#                                                                              #
#                   Statistique descriptive de la table finale                 #
#             Pré-requis :  Avoir compilé script_importation_jointure          #
#                                                                              #
################################################################################

library("corrplot")
library('Rarity')
library(tidyverse)
library(leaps)
library('factoextra')
library('FactoMineR')

head(months_data)

summary(months_data)

set.seed(1)

# Corrélation

pairs(months_data, col = 'blue')
as.data.frame(as.table(cor(months_data, use = "complete.obs")))

corPlot(as.matrix(months_data),xlab='', ylb='')

# Toutes les variables semblent corrélées entre elles à part Area. 

# échantillonage + centrer et réduire les données

months_data_analyse = scale(months_data)

sample <- sample(c(TRUE, FALSE), nrow(months_data_analyse), replace=TRUE, prob=c(0.75,0.25))
train  <- as.data.frame(months_data_analyse[sample, ])
test   <- as.data.frame(months_data_analyse[!sample, ])

# modèle expliquant l'année

model = lm(years~months_data$`Antarctic mass (Gigatonnes)`+months_data$`monthly average` + months_data$`Greenland mass (Gigatonnes)` + months_data$No_Smoothing,data=months_data)
summary(model)$coefficients #P-value tres proche de 0, 
confint(model)
sigma(model)/mean(months_data$years) #RSE (standart residuals error) très petit, le modele est bon

# suppression des colonnes year et month 

months_data = months_data[c('Antarctic mass (Gigatonnes)',
                            'monthly average', 'Greenland mass (Gigatonnes)',
                            'No_Smoothing', 'area', 'V3', 'V6', 'V9')]

### modèle régression linéaire pour expliquer Atarctic_mass

# Sélection du modèle optimal

reglin_ant_opt <- regsubsets(`Antarctic mass (Gigatonnes)`~.,
                             data = train, method="exhaustive")

# technique du coude

mse=rep(NA,7)
for (i in 1:7){
  X.test = model.matrix(`Antarctic mass (Gigatonnes)`~., data=test)
  data_test = X.test[,names(coef(reglin_ant_opt,i))]
  pred =data_test%*%coef(reglin_ant_opt,i)
  mse[i]=mean((test$`Antarctic mass (Gigatonnes)`-pred)**2)
}
mse

x11()
plot(mse, xlab='Nombre de variables explicatives', main='nombre optimal de clusters')
lines(mse)

# modèle à 4 variables explicatives

plot(reglin_ant_opt, scale="bic") # monthly_average, greenland_mass, area, V6
plot(reglin_ant_opt, scale="Cp") # monthly_average, greenland_mass, area, V6

# modèle à 4 variables explicatives

reglin_bic_antarctic = lm(`Antarctic mass (Gigatonnes)`~`monthly average`+
                          `Greenland mass (Gigatonnes)`+ area+ V6,
                          data = train)
summary(reglin_bic_antarctic)

# modèle de bonne qualité : significativité des varaibles excellente, bon RSE, 
# bon R2

# Histogramme des résidus

pred = predict(reglin_bic_antarctic, test)

resid = test$`Antarctic mass (Gigatonnes)` - pred
ggplot(data = as.data.frame(resid), aes(x=resid))+
  geom_histogram(color='black')+
  labs(title="Résidus du modèle de prédiction de la masse de l'antarctique",
       x="Résidus", y = "Count")

# test de normalité

shapiro.test(pred)

# p-value 1.732e-08, l'échantillon ne suit pas une normale

### modèle régression linéaire pour expliquer monthly_average

# Sélection du modèle optimal

reglin_dc_opt <- regsubsets(`monthly average`~.,
                             data = train, method="exhaustive")

# technique du coude

mse=rep(NA,7)
for (i in 1:7){
  X.test = model.matrix(`monthly average`~., data=test)
  data_test = X.test[,names(coef(reglin_dc_opt,i))]
  pred =data_test%*%coef(reglin_dc_opt,i)
  mse[i]=mean((test$`monthly average`-pred)**2)
}
mse

x11()
plot(mse, xlab='Nombre de variables explicatives', main='nombre optimal de clusters')
lines(mse)

# modèle à 6 variables explicatives

plot(reglin_dc_opt, scale="bic") # antarctic_mass, greenland_mass, no_smoothing, area, V3, V6
plot(reglin_ant_opt, scale="Cp") # antarctic_mass, greenland_mass, area, V3, V6, V9

# modèle du BIC

reglin_bic_carbone1 = lm(`monthly average`~`Antarctic mass (Gigatonnes)`+
                            `Greenland mass (Gigatonnes)`+ No_Smoothing + area+
                            V3 + V6,
                          data = train)
summary(reglin_bic_carbone1)

# modèle de bonne qualité : significativité des varaibles excellente, bon RSE, 
# bon R2

# modèle du Cp

reglin_bic_carbone2 = lm(`monthly average`~`Antarctic mass (Gigatonnes)`+
                           `Greenland mass (Gigatonnes)` + area+
                           V3 + V6+V9,
                         data = train)
summary(reglin_bic_carbone2)

# le modèle de BIC est retenu

# Histogramme des résidus

pred = predict(reglin_bic_carbone1, test)

resid = test$`monthly average` - pred
ggplot(data = as.data.frame(resid), aes(x=resid))+
  geom_histogram(color='black')+
  labs(title="Résidus du modèle de prédiction du taux de CO2",
       x="Résidus", y = "Count")

# test de normalité

shapiro.test(pred)

# p-value 1.016e-05, l'échantillon ne suit pas une normale

### modèle régression linéaire pour expliquer la température

# Sélection du modèle optimal

reglin_temp_opt <- regsubsets(`No_Smoothing`~.,
                             data = train, method="exhaustive")

# technique du coude

mse=rep(NA,7)
for (i in 1:7){
  X.test = model.matrix(No_Smoothing~., data=test)
  data_test = X.test[,names(coef(reglin_temp_opt,i))]
  pred =data_test%*%coef(reglin_temp_opt,i)
  mse[i]=mean((test$No_Smoothing-pred)**2)
}
mse

x11()
plot(mse, xlab='Nombre de variables explicatives', main='nombre optimal de clusters')
lines(mse)

# modèle à 4 variables explicatives

plot(reglin_temp_opt, scale="bic") # monthly_average, area, V3, V9
plot(reglin_temp_opt, scale="Cp") # monthly_average, greenland_mass, area, V3, V9

# modèle à 4 variables explicatives

reglin_bic_temperature = lm(No_Smoothing~ `monthly average`+area + V3 + V9,
                          data = train)
summary(reglin_bic_temperature)

# modèle de bonne qualité : significativité des varaibles excellente, bon RSE, 
# bon R2

# Histogramme des résidus

pred = predict(reglin_bic_temperature, test)

resid = test$No_Smoothing - pred
ggplot(data = as.data.frame(resid), aes(x=resid))+
  geom_histogram(color='black')+
  labs(title="Résidus du modèle de prédiction de l'évolution de la température",
       x="Résidus", y = "Count")

# test de normalité

shapiro.test(pred)

# p-value 4.972e-10, l'échantillon ne suit pas une normale


#ACP
d.GlobalWarming = dist(months_data, "euclidian")
cah.wardCentree = hclust(d.GlobalWarming, "ward.D")
x11()
plot(cah.wardCentree, labels = FALSE, main ="dendogramme")
classe.GlobalWarming = cutree(cah.wardCentree, k=4)
classe.GlobalWarming
p4 = as.factor(classe.GlobalWarming)
levels(p4) = paste("Classe",1:4)
pca.GlobalWarming = PCA(months_data, scale= T,graph=FALSE)
fviz_pca_ind(pca.GlobalWarming,
             geom.ind = "point",
             col.ind = classe.GlobalWarming, 
             legend.title = "Groupes"
)
#plot.PCA(pca.GlobalWarming,choix = "ind", invisible ="quali")
plot.PCA(pca.GlobalWarming, choix="var")

# clustering sur les deux premières dimensions

dims = get_pca_ind(pca.GlobalWarming)$coord[,c(1,2)]

plot(dims[,1], dims[,2])

km <- kmeans(dims, centers = 4, nstart = 25)
fviz_cluster(km, data = dims)

data_class = cbind(months_data, km$cluster)

ggplot(data_class, aes(x=factor(km$cluster), y=`Antarctic mass (Gigatonnes)`,
                       color = factor(km$cluster))) + 
  geom_boxplot()+
  theme(legend.position="none")+
  labs(title = "Masse de l'antarctique en fonction de la classification",
        y = "Masse de l'antarctique", x="Classes")

ggplot(data_class, aes(x=factor(km$cluster), y=`monthly average`,
                       color = factor(km$cluster))) + 
  geom_boxplot()+
  theme(legend.position="none")+
  labs(title = "Taux de CO2 en fonction de la classification",
       y = "Taux de CO2", x="Classes")

ggplot(data_class, aes(x=factor(km$cluster), y=No_Smoothing,
                       color = factor(km$cluster))) + 
  geom_boxplot()+
  theme(legend.position="none")+
  labs(title = "Température en fonction de la classification",
       y = "Température", x="Classes")

ggplot(data_class, aes(x=factor(km$cluster), y=V3,
                       color = factor(km$cluster))) + 
  geom_boxplot()+
  theme(legend.position="none")+
  labs(title = "Date moyenne en fonction de la classification",
       y = "Date", x="Classes")
