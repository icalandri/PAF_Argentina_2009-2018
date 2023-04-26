
#  01-Data tidy                                                                 
#                                                                               
#  Date: 20 April 2023                                                          
#                                                                               
#  Data source: a CSV file from Argentinian national survey                     
#  https://www.indec.gob.ar/indec/web/Institucional-Indec-BasesDeDatos-2        
#                                                                               
#  Author: Ismael Calandri                                                      
#                                                                               
#  Output: a Rdata file with the coding of the risk factors as Lancet           
#  commission 2022 definition                                                   



# 1. Packages  ==============================
library(readr)
library(tidyverse)

# 2. Data origin ==============================

base2018 <- read_delim("2018/ENFR 2018 - Base usuario.txt", 
                                     delim = "|", escape_double = FALSE, trim_ws = TRUE)


# 2. Labels definition ==============================

base2018$provincia<-factor(base2018$cod_provincia,
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
base2018$PAF_education<-0
base2018$PAF_education[base2018$nivel_instruccion_j==1 | 
                         base2018$nivel_instruccion_j==2|
                         base2018$nivel_instruccion_j==3]<-1



## 3.2 Hearing loss =========================
#####Other dataset!!!!!=============

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


base2018$PAF_HBP[(base2018$biha03==1 | base2018$biha03==2)]<-1
base2018$PAF_HBP[base2018$biha03==3]<-0


## 3.5 Alcoholism =========================

### 3.5.1 Calculating units ===============

#units according https://www.nhs.uk/live-well/alcohol-advice/calculating-alcohol-units/
# drinks per day of beer (bica04_01_b) alcohol units 2units

base2018$beer_units<-base2018$bica04_01_b * 2

# drinks per day of wine (bica04_02_b) alcohol units 2.1 units

base2018$wine_units<-base2018$bica04_02_b * 2.1

# drinks per day of spirit (bica04_03_b) alcohol units 1.4 units

base2018$spirit_units<-base2018$bica04_03_b * 1.4

# doses per week

base2018<-base2018 %>% replace_na(list(beer_units=0,wine_units=0,spirit_units=0))

base2018$alcohol_units_week<-(base2018$beer_units+base2018$wine_units+base2018$spirit_units)*7

### 3.5.2 Calculating alcoholism ===============

base2018$PAF_alcohol[(base2018$alcohol_units_week>=14)]<-1
base2018$PAF_alcohol[(base2018$alcohol_units_week<14)]<-0
## 3.6 Obesity =========================

base2018$PAF_obesity[(base2018$imc_categorias==1 |base2018$imc_categorias==2)]<-0
base2018$PAF_obesity[base2018$imc_categorias==3]<-1


## 3.7 Smoking =========================

#...............................................................................
#                                                                              .
#  bita04: Actualmente ¿fuma usted cigarrillos… 1 :todos los días? 2: algunos  .
#  días? 3:no fuma?                                                         .
#                                                                              .
#...............................................................................

base2018$PAF_smoking[(base2018$bita04==1 |base2018$bita04==2)]<-1
base2018$PAF_smoking[(base2018$bita04==3)]<-0

## 3.8 Depression =========================
#####There is no data!!!!!=============


## 3.9 Social contact =========================
#As the Lancet comission I used "cohabitation" as a proxy 

base2018$PAF_socialisolation[base2018$cant_componentes<2]<-1
base2018$PAF_socialisolation[base2018$cant_componentes>=2]<-0
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

base2018$PAF_pinactivity[(base2018$nivel_actividad_fisica==3)]<-1
base2018$PAF_pinactivity[(base2018$nivel_actividad_fisica==2 | base2018$nivel_actividad_fisica==1)]<-0

## 3.11 Diabetes =========================

#...............................................................................
#                                                                              .
#  bidi01: ¿Alguna vez un médico, un enfermero u otro profesional de la salud  .
#  le dijo   que tenía diabetes o azúcar alta en la sangre?                    . 
#  1:Sí 2:No 99:Ns/Nc                                                          .
#...............................................................................

base2018$PAF_DBT[base2018$bidi01==1]<-1
base2018$PAF_DBT[base2018$bidi01==2]<-0


## 3.12 Air pollution =========================

...............................................................................
#                                                                              .
#  #  tamanio_aglomerado	Tamaño del aglomerado                                 .
#  #                                                                           .
#  #  	1. Más de 1.500.000 habitantes                                          .
#  #                                                                           .
#  #  	2. 500.001 a 1.500.000 habitantes                                       .
#  #                                                                           .
#  #  	3. 150.001 a 500.000 habitantes                                         .
#  #                                                                           .
#  #  	4. Menos de 150.000                                                     .
#  habitantes                                                                  .
#                                                                              .
#...............................................................................

base2018$PAF_air[(base2018$tamanio_aglomerado !=4)]<-1
base2018$PAF_air[(base2018$tamanio_aglomerado ==4)]<-0

save(base2018, file="base2018_general.RData")
