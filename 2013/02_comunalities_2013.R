# 1. Data and dataorigin===================================
library(tidyverse)

rm(list = ls())
load("base2013_general.RData")

#For use this dataset is requiered to run Data_tidy_2013.r
basecom<-base2013 %>% select(starts_with("PAF"))


library(psych)
# 2. Tetrachoric correlation ==============================
a<-tetrachoric(basecom)                             
b<-as.data.frame(a$rho)

# 3. Communalitie calculation ===========
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

save(base2013, basecom, comunalities, file="base2013_general.RData")
