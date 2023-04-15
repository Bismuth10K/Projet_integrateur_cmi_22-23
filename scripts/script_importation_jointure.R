# librairies 

library(rjson)
library(dplyr)

#importation

# Sept_Artic_extend

SAE = read.table(file = "datasets_nasa/datasets_nasa/Sept_Artic_extend.txt", header = TRUE)

# Antarctica_mass

AM = read.table(file = 'datasets_nasa/datasets_nasa/antarctica_mass.txt', header = FALSE, skip = 31)
header = c('TIME (year.decimal)', 'Antarctic mass (Gigatonnes)', 'Antarctic mass 1-sigma uncertainty (Gigatonnes)')
colnames(AM) = header

# Carbon-Dioxyde

CD = read.table(file = 'datasets_nasa/datasets_nasa/carbon-dioxyde.txt', header = FALSE, skip = 54)
CD = CD[-c(1,2)]
header = c('Date', 'monthly average', 'de-season alized', '#days', 'st.dev of days', 'unc. of mon mean')
colnames(CD) = header

# Global_temperature

GT = read.table(file = 'datasets_nasa/datasets_nasa/global_temperature.txt', header = TRUE, sep=';', skip = 3)

# Greenland_Mass

GM = read.table(file = 'datasets_nasa/datasets_nasa/greenland_mass.txt', header = FALSE, skip = 31)
header = c('TIME (year.decimal)', 'Greenland mass (Gigatonnes)', 'Greenland mass 1-sigma uncertainty (Gigatonnes)')
colnames(GM) = header

# Ocean-warming

OW = read.table(file = 'datasets_nasa/datasets_nasa/ocean-warming.dat', header = TRUE)

# Ocean-warming-nasa (utile sachant OW ?) (modifier le format)

OWN = fromJSON(file="datasets_nasa/datasets_nasa/ocean-warming-nasa-1993.json")

# sea_level

SL = read.table(file = 'datasets_nasa/datasets_nasa/sea_level-1993-present.txt', header = FALSE, skip=49)
header = c('altimeter', 'merged file cycle # ', 'year+fraction of year',
           'number of observations', 'number of weighted observations ', 'GMSL',
           'standard deviation of GMSL', 'smoothed GMSL variation', 'GMSL variation',
           'standart deviation of GMSL', 'smoothed GMSL variation (GIA applied)',
           'smoothed GMSL variation (GIA applied & annual and semi-annual')

# Création des variables years et months pour chaque table

changefractdate = function(table, index){
  years = rep(NA, length(table[,index]))
  months = rep(NA, length(table[,index]))
  for(i in 1:length(table[,index])){
    years[i] = as.integer(table[i,index])
    months[i] = as.integer((table[i,index]-years[i])*12)+1
  }
  table = cbind(table,years,months)
  return(table)
}

AM = changefractdate(AM, 1)
CD = changefractdate(CD,1)
GM = changefractdate(GM,1)
OW = changefractdate(OW,1)
SL = changefractdate(SL,3)

# Jointures 

months_data <- merge(x=AM,y=CD, by.x=c("years","months"), 
             by.y=c("years","months"))

months_data <- merge(x=months_data, y=GM, by.x=c("years","months"), 
              by.y=c("years","months"))

colnames(GT)[which(names(GT) == 'Year')] <- 'years'
months_data <- full_join(x=months_data, y=GT, by = 'years')[c(1:219),]

months_data <- full_join(x=months_data, y=OW, by = 'years')[c(1:219),]

colnames(SAE)[which(names(SAE) == 'year')] <- 'years'
months_data <- full_join(x=months_data, y=SAE, by = 'years')[c(1:219),]

months_data <- merge(x=months_data, y=SL, by.x=c("years","months.x"), 
                     by.y=c("years","months"))

# sort

months_data = months_data[order(months_data$`V3`),]
months_data = months_data[-c(3,6,12,17,24,25)]

