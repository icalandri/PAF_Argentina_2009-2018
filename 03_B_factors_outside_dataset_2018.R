

#  01-B-Data tidy other factors                                                 
#                                                                               
#  Date: 20 April 2023                                                          
#                                                                               
#  This code merge other data origins to fill the gap in the calculation of     
#  other risk factors                                                           
#                                                                               
#  Author: Ismael Calandri                                                      
#                                                                               
#  Output: a Rdata file with the coding of the risk factors as Lancet           
#  commission 2022 definition                                                   


library(survey)
library(dplyr)
library(gt)
library(readr)

#===============================Hipoacusia===========

base_discapacidad_2018 <- read_delim("2018/Discapacidad/base_estudio_discapacidad_2018.csv", 
                                             delim = ";", escape_double = FALSE, trim_ws = TRUE)


# 2. defino hipoacusia =======================================================
#...............................................................................
#                                                                              .
#  Cantidad y tipo de dificultad                                               .
#                                                                              .
#  tipo_dificultad                                                             .
#                                                                              .
#  1 = Solo motora                                                             .
#                                                                              .
#  Población de 6 años                                                         .
#                                                                              .
#  y más con dificultad                                                        .
#                                                                              .
#  2 = Solo visual                                                             .
#                                                                              .
#  3 = Solo auditiva                                                           .
#                                                                              .
#  4 = Solo mental-cognitiva                                                   .
#                                                                              .
#  5 = Solo del cuidado de sí                                                  .
#                                                                              .
#  6 = Solo del habla y la comunicación                                        .
#                                                                              .
#  7 = Dos dificultades                                                        .
#                                                                              .
#  8 = Tres dificultades o más                                                 .
#                                                                              .
#  9 = Solo certificado                                                        .
#                                                                              .
#...............................................................................


base_discapacidad_2018$PAF_hipoacusia<-as.numeric(base_discapacidad_2018$tipo_dificultad==3)
  
# 3. estimacion del diseño muestral y los pesos =======================================================
base_midlife_handicap<-base_discapacidad_2018 %>% filter(edad_inicio==4) 
n_midlife_handicap<-nrow(base_midlife_handicap)
design_midlife_handicap <- svydesign(id=~ID, weights=~pondera, data=base_midlife_handicap)


# 4. Calculo de la prevalencia ================================================================
PAF_hipoacusia<-as.data.frame(svytable(~PAF_hipoacusia, design = design_midlife_handicap))

PAF_hipoacusia<-PAF_hipoacusia %>% pivot_wider(names_from = PAF_hipoacusia, values_from =Freq)
PAF_hipoacusia$RF<-"PAF_hipoacusia"
PAF_hipoacusia$Prevalence<-PAF_hipoacusia$`1`/(PAF_hipoacusia$`1`+PAF_hipoacusia$`0`)

a<-svyciprop(~PAF_hipoacusia==1, design_midlife_handicap, method="li")

PAF_hipoacusia$LCI<-attr(a,"ci")[1]
PAF_hipoacusia$UCI<-attr(a,"ci")[2]
names(PAF_hipoacusia)
PAF_hipoacusia <- PAF_hipoacusia %>% select(RF,Prevalence, LCI, UCI)
PAF_hipoacusia$RR<-1.9 #data from lancet 2020
#===============================depression===========

PAF_depression <-data.frame(matrix(nrow = 1, ncol = 0))
PAF_depression$"RF"<-"PAF_depression"  

#data from https://doi.org/10.1007/s00127-018-1492-3

#prevalence 7.6% SE1.4
PAF_depression$"Prevalence"<-7.6/100

#Transform SE  to 95%CI using CI= mean +- SE *qnorm(0.975)
PAF_depression$LCI<-(7.6-(1.4*qnorm(0.975)))/100
PAF_depression$UCI<-(7.6+(1.4*qnorm(0.975)))/100
PAF_depression$RR<-1.9 #data from lancet 2020

#===============================TBI===========

PAF_TBI <-data.frame(matrix(nrow = 1, ncol = 0))
PAF_TBI$"RF"<-"PAF_TBI"  


PAF_TBI$"Prevalence"<-(12.1)/100

#Transform SE  to 95%CI using CI= mean +- SE *qnorm(0.975)
PAF_TBI$LCI<-NA
PAF_TBI$UCI<-NA
PAF_TBI$RR<-1.8 #data from lancet 2020

PAF_other<-rbind(PAF_depression,PAF_hipoacusia, PAF_TBI)

save(PAF_other,  file="other_risk_factor.RData")

rm(list = ls())
