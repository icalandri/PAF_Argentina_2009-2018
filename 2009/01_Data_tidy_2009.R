# 1. Packages  ==============================
rm(list = ls())


library(readr)
library(tidyverse)

# 2. Data origin ==============================

base2009 <- read_delim("2009/ENFR-2009 Base Usuario.txt", 
                                     delim = "|", escape_double = FALSE, trim_ws = TRUE)


# 2. Labels definition ==============================

base2009$PRVNC<-factor(base2009$PRVNC,
                               levels = c(2,
                                          6,
                                          10,
                                          14,
                                          18,
                                          22,
                                          26,
                                          30,
                                          34,
                                          38,
                                          42,
                                          46,
                                          50,
                                          54,
                                          58,
                                          62,
                                          66,
                                          70,
                                          74,
                                          78,
                                          82,
                                          86,
                                          90,
                                          94),
                               labels = c("CABA",
                                          "Buenos Aires",
                                          "Catamarca",
                                          "Cordoba",
                                          "Corrientes",
                                          "Chaco",
                                          "Chubut",
                                          "Entre Rios",
                                          "Formosa",
                                          "Jujuy",
                                          "La Pampa",
                                          "La Rioja",
                                          "Mendoza",
                                          "Misiones",
                                          "Neuquen",
                                          "Rio Negro",
                                          "Salta",
                                          "San Juan",
                                          "San Luis",
                                          "Santa Cruz",
                                          "Santa Fe",
                                          "Santiago del Estero",
                                          "Tucuman",
                                          "Tierra del Fuego"))

# 3. Variables dichotomization ==============================

## 3.1 Education =========================
#####See definition!!!!!=============


base2009$PAF_education<-0
base2009$PAF_education[base2009$NIVINSTR==1 | 
                         base2009$NIVINSTR==2|
                         base2009$NIVINSTR==3]<-1



## 3.2 Hearing loss =========================
#####See definition!!!!!=============

## 3.2 TBI =========================
#####There is no data!!!!!=============

## 3.3 HBP =========================

#...............................................................................
#                                                                              .
#  #  biha03:¿Cuántas veces un médico, un enfermero u otro profesional de la   .
#  #  salud le dijo que tenía presión alta? 	                                 .
#                                                                              .
#  #  1: "Sólo 1 vez" 	2: "Más de 1 vez" 	3: "Ninguna" 	99: "Ns/Nc"          .                                         .                .
#...............................................................................


base2009$PAF_HBP[base2009$BIHA03==1 | base2009$BIHA03==2]<-1
base2009$PAF_HBP[base2009$BIHA03==3]<-0
base2009$PAF_HBP[base2009$BIHA03==9]<-NA

## 3.4 Physical inactivity =========================

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


base2009$PAF_pinactivity[base2009$NIV_AF==3]<-1
base2009$PAF_pinactivity[base2009$NIV_AF==2 | base2009$NIV_AF==1]<-0


## 3.5 Diabetes =========================

#...............................................................................
#                                                                              .
#  bidi01: ¿Alguna vez un médico, un enfermero u otro profesional de la salud  .
#  le dijo   que tenía diabetes o azúcar alta en la sangre?                    . 
#  
# 1 Sí
# 2 No
# 9 Ns/nc.
#...............................................................................

base2009$PAF_DBT[base2009$BIDI01==1]<-1
base2009$PAF_DBT[base2009$BIDI01==2]<-0
base2009$PAF_DBT[base2009$BIDI01==9]<-NA

## 3.5 Alcoholism =========================

### 3.5.1 Calculating units ===============

#units according https://www.nhs.uk/live-well/alcohol-advice/calculating-alcohol-units/
# drinks per day of beer (bica04_01_b) alcohol units 2units

base2009$BICA04_01<-as.numeric(base2009$BICA04_01)
base2009$BICA04_02<-as.numeric(base2009$BICA04_02)
base2009$BICA04_03<-as.numeric(base2009$BICA04_03)

base2009<-base2009 %>% mutate(BICA04_01=na_if(BICA04_01, 888),
                              BICA04_02=na_if(BICA04_02, 888),
                              BICA04_03=na_if(BICA04_03, 888))



base2009$beer_units<-base2009$BICA04_01 * 2



# drinks per day of wine (bica04_02_b) alcohol units 2.1 units



base2009$wine_units<-base2009$BICA04_02 * 2.1

# drinks per day of spirit (bica04_03_b) alcohol units 1.4 units

base2009$spirit_units<-base2009$BICA04_03 * 1.4

# doses per week

base2009<-base2009 %>% replace_na(list(beer_units=0,wine_units=0,spirit_units=0))

base2009$alcohol_units_week<-(base2009$beer_units+base2009$wine_units+
                                base2009$spirit_units)*7

### 3.5.2 Calculating alcoholism ===============

base2009$PAF_alcohol<-as.numeric(base2009$alcohol_units_week>=14)


## 3.6 Obesity =========================

base2009$PAF_obesity[base2009$PC_AGR==1 |base2009$PC_AGR==2]<-0
base2009$PAF_obesity[base2009$PC_AGR==3]<-1
base2009$PAF_obesity[base2009$PC_AGR==9]<-NA


## 3.7 Smoking =========================


#...............................................................................
#                                                                              .
#  BITA04: Actualmente ¿fuma usted cigarrillos… 1 :todos los días? 2: algunos  .
#  días? 3:no fuma?                                                         .
#                                                                              .
#...............................................................................

base2009$PAF_smoking[base2009$BITA04==1 |base2009$BITA04==2]<-1
base2009$PAF_smoking[base2009$BITA04==3]<-0

## 3.8 Depression =========================
#####There is no data!!!!!=============


## 3.9 Social contact =========================
#As the Lancet comission I used "cohabitation" as a proxy 
base2009$PAF_socialisolation<-as.numeric(base2009$CANT_COMPONENTES<2)


save(base2009, file="base2009_general.RData")
