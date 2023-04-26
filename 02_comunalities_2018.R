
#  02-Communalities calculation                                                 
#                                                                               
#  Date: 20 April 2023                                                          
#                                                                               
#  Data source: a RData with survey data coding for risk factor prevalence      
#  for dementia (01_data_tidy)                                                  
#                                                                               
#  Author: Ismael Calandri                                                      
#                                                                               
#  Output: a Rdata file with the coding of the risk factors as Lancet           
#  commission 2022 definition                                                   



# 1. Data and dataorigin===================================

library(tidyverse)

#For use this dataset is requiered to run 01_Data_tidy_201
load(file="base2018_general.RData")
basecom<-base2018 %>% select(starts_with("PAF"))


library(psych)
# 2. Tetrachoric correlation ==============================
a<-tetrachoric(basecom)                             
b<-as.data.frame(a$rho)

# 3. Communalities calculation ===========
library("factoextra")
library("FactoMineR")
# PCA resolution
# Analize eigenvalues, keep Dim w/ eigenvalues >1
eigenvalues <-eigen(b)$values
eigenvalues

# Only 3 dimensions has eigenvalues > 1
eigenVectors<-eigen(b)$vectors
eigenVectors

# I use the paper criteria for caculation:
#"Communality was calculated as the sum of the square of all factor loadings"

comunalities<-as.data.frame(eigenVectors)
comunalities$RF<-names(b)

comunalities$comunality<-comunalities$V1^2+comunalities$V2^2+comunalities$V3^2

# limpio todo y nos vamos
comunalities<-comunalities %>% select(RF, comunality)

rm(a)
rm(b)
rm(eigenVectors)
rm(eigenvalues)

save(base2018, basecom, comunalities, file="base2018_general.RData")

rm(list = ls())
