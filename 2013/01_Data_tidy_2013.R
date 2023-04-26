# 1. Packages  ==============================
library(readr)
library(tidyverse)

# 2. Data origin ==============================

base2013 <- read_delim("2013/ENFR2013_baseusuario.txt", 
                                     delim = "|", escape_double = FALSE, trim_ws = TRUE)



# 3. Variables dichotomization ==============================

## 3.1 Education =========================
#####See definition!!!!!=============
base2013$PAF_education<-0
base2013$PAF_education[base2013$NIVEL_INSTRUCCION_J==1 | 
                         base2013$NIVEL_INSTRUCCION_J==2|
                         base2013$NIVEL_INSTRUCCION_J==3]<-1


summary(as.factor(base2013$PAF_education))

## 3.2 Hearing loss =========================
#####See definition!!!!!=============

## 3.3 TBI =========================
#####There is no data!!!!!=============

## 3.4 HBP =========================

#...............................................................................
#                                                                              .
#  #  biha03:¿Cuántas veces un médico, un enfermero u otro profesional de la   .
#  #  salud le dijo que tenía presión alta? 	                                 .
#                                                                              .
#  #  1: "Sólo 1 vez" 	2: "Más de 1 vez" 	3: "Ninguna" 	99: "Ns/Nc"          .                                         .                .
#...............................................................................


base2013$PAF_HBP[base2013$BIHA03==1 | base2013$BIHA03==2]<-1
base2013$PAF_HBP[base2013$BIHA03==3]<-0
base2013$PAF_HBP[base2013$BIHA03==9]<-NA


## 3.5 Alcoholism =========================

### 3.5.1 Calculating units ===============

#units according https://www.nhs.uk/live-well/alcohol-advice/calculating-alcohol-units/
# drinks per day of beer (bica04_01_b) alcohol units 2units

base2013$BICA04_01<-as.numeric(base2013$BICA04_01)
base2013$BICA04_02<-as.numeric(base2013$BICA04_02)
base2013$BICA04_03<-as.numeric(base2013$BICA04_03)

base2013<-base2013 %>% mutate(BICA04_01=na_if(BICA04_01, 888),
                              BICA04_02=na_if(BICA04_02, 888),
                              BICA04_03=na_if(BICA04_03, 888))

str(base2013$BICA04_02)


base2013$beer_units<-base2013$BICA04_01 * 2



# drinks per day of wine (bica04_02_b) alcohol units 2.1 units



base2013$wine_units<-base2013$BICA04_02 * 2.1

# drinks per day of spirit (bica04_03_b) alcohol units 1.4 units

base2013$spirit_units<-base2013$BICA04_03 * 1.4

# doses per week

base2013<-base2013 %>% replace_na(list(beer_units=0,wine_units=0,spirit_units=0))

base2013$alcohol_units_week<-(base2013$beer_units+base2013$wine_units+base2013$spirit_units)*7

### 3.5.2 Calculating alcoholism ===============

base2013$PAF_alcohol<-as.numeric(base2013$alcohol_units_week>=14)

## 3.6 Obesity =========================

base2013$PAF_obesity[base2013$IMC_AGRUPADO==1 |base2013$IMC_AGRUPADO==2]<-0
base2013$PAF_obesity[base2013$IMC_AGRUPADO==3]<-1
base2013$PAF_obesity[base2013$IMC_AGRUPADO==9]<-NA

## 3.7 Smoking =========================

#...............................................................................
#                  BITA04 Actualmente ¿fuma usted cigarrillos...
#             1 ...todos los días?
#             2 ...algunos días?
#             3 ...no fuma?                                                         .
#                                                                              .
#...............................................................................

base2013$PAF_smoking[base2013$BITA04==1 |base2013$BITA04==2]<-1
base2013$PAF_smoking[base2013$BITA04==3]<-0

## 3.8 Depression =========================
#####There is no data!!!!!=============


## 3.9 Social contact =========================
#As the Lancet comission I used "cohabitation" as a proxy 

base2013$PAF_socialisolation<-as.numeric(base2013$CANT_COMPONENTES<2)

## 3.10 Physical inactivity =========================

#Ojo! estan las variables para calcular la totalidad del IPAQ 
#(use la variable diagnostica)
#...............................................................................
#                                                                              .
#  Nivel de actividad física                                                   .
#                                                                              .
#  1. "Alto"                                                                     .
#  2. "Medio"                                                                    .
#  3. "Bajo"                                                                     .
#  99."Ns/Nc"                                                                   .
#                                                                              .
#...............................................................................

base2013$PAF_pinactivity[base2013$NIVEL_ACTIVIDAD_FISICA==3]<-1
base2013$PAF_pinactivity[base2013$NIVEL_ACTIVIDAD_FISICA==2 | base2013$NIVEL_ACTIVIDAD_FISICA==1]<-0
base2013$PAF_pinactivity[base2013$NIVEL_ACTIVIDAD_FISICA==99]<-NA

## 3.11 Diabetes =========================

#...............................................................................
#                                                                              .
#  bidi01: ¿Alguna vez un médico, un enfermero u otro profesional de la salud  .
#  le dijo   que tenía diabetes o azúcar alta en la sangre?                    . 
#  1:Sí 2:No 99:Ns/Nc                                                          .
#...............................................................................

base2013$PAF_DBT[base2013$BIDI01==1]<-1
base2013$PAF_DBT[base2013$BIDI01==2]<-0
base2013$PAF_DBT[base2013$BIDI01==99]<-NA




save(base2013, file="base2013_general.RData")


