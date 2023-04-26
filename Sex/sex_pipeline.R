
#  Date 21 April 2023                                                           
#                                                                               
#  Input: Output of “01_data_tidy” script                                       
#                                                                               
#  Author: Ismael L Calandri                                                    
#                                                                               
#  Output: a table with PAF calculation for females                             




# 1. Packages  ==============================
rm(list = ls())
library(readr)
library(tidyverse)
library("factoextra")
library("FactoMineR")
library(survey)
library(gt)
library(flextable)

#...............................................................................
#                                                                              .
#  SEX=FEMALE                                                                  .
#                                                                              .
#...............................................................................


# 2. Data origin (risk_factor coded) ==============================

load("base2018_general.RData")
rm(basecom, comunalities)

basefem<-base2018 %>% filter(bhch03==2)

# 3. Comunalities calculation==================

basecom_fem<-basefem %>% select(starts_with("PAF"))


library(psych)
## 1. Tetrachoric correlation ==============================
a<-tetrachoric(basecom_fem)                             
b<-as.data.frame(a$rho)

## 2. Communalitie calculation ===========

# PCA resolution
# Analize eigenvalues, keep Dim w/ eigenvalues >1
eigenvalues <-eigen(b)$values
eigenvalues

# Only 3 dimensions has eigenvalues > 1
eigenVectors<-eigen(b)$vectors
eigenVectors

comunalities<-as.data.frame(eigenVectors)
comunalities$RF<-names(b)

comunalities$comunality<-comunalities$V1^2+comunalities$V2^2+comunalities$V3^2

comunalities_fem<-comunalities %>% select(RF, comunality)

rm(base2018, a, b, eigenVectors, eigenvalues, comunalities)

# 4. PAF table ================================================================

## 1. Survey design =============================================
base_midlife<-basefem %>% filter(bhch04>=45 & bhch04<=64) 
n_midlife_fem<-nrow(base_midlife)

design_midlife <- svydesign(id=~id, weights=~wf1p, data=base_midlife)

base_latelife<-basefem %>% filter(bhch04>64) 
design_latelife <- svydesign(id=~id, weights=~wf1p, data=base_latelife)
n_latelife_fem<-nrow(base_latelife)

n_total_fem<-n_latelife_fem + n_midlife_fem

## 2. Calculate prevalence =============================================

### 2.1 Midlife factors =============================================
PAF_education<-as.data.frame(svytable(~PAF_education, design = design_midlife))
PAF_education<-PAF_education %>% pivot_wider(names_from = PAF_education, values_from =Freq)
PAF_education$RF<-"PAF_education"
PAF_education$Prevalence<-PAF_education$`1`/(PAF_education$`1`+PAF_education$`0`)
a<-svyciprop(~PAF_education==1, design_midlife, method="li")

PAF_education$LCI<-attr(a,"ci")[1]
PAF_education$UCI<-attr(a,"ci")[2]


PAF_obesity<-as.data.frame(svytable(~PAF_obesity, design = design_midlife))
PAF_obesity<-PAF_obesity %>% pivot_wider(names_from = PAF_obesity, values_from =Freq)
PAF_obesity$RF<-"PAF_obesity"
PAF_obesity$Prevalence<-PAF_obesity$`1`/(PAF_obesity$`1`+PAF_obesity$`0`)
a<-svyciprop(~PAF_obesity==1, design_midlife, method="li")
PAF_obesity$LCI<-attr(a,"ci")[1]
PAF_obesity$UCI<-attr(a,"ci")[2]


PAF_HBP<-as.data.frame(svytable(~PAF_HBP, design = design_midlife))
PAF_HBP<-PAF_HBP %>% pivot_wider(names_from = PAF_HBP, values_from =Freq)
PAF_HBP$RF<-"PAF_HBP"
PAF_HBP$Prevalence<-PAF_HBP$`1`/(PAF_HBP$`1`+PAF_HBP$`0`)
a<-svyciprop(~PAF_HBP==1, design_midlife, method="li")
PAF_HBP$LCI<-attr(a,"ci")[1]
PAF_HBP$UCI<-attr(a,"ci")[2]


PAF_alcohol<-as.data.frame(svytable(~PAF_alcohol, design = design_midlife))
PAF_alcohol<-PAF_alcohol %>% pivot_wider(names_from = PAF_alcohol, values_from =Freq)
PAF_alcohol$RF<-"PAF_alcohol"
PAF_alcohol$Prevalence<-PAF_alcohol$`1`/(PAF_alcohol$`1`+PAF_alcohol$`0`)
a<-svyciprop(~PAF_alcohol==1, design_midlife, method="li")
PAF_alcohol$LCI<-attr(a,"ci")[1]
PAF_alcohol$UCI<-attr(a,"ci")[2]

### 2.2 Later-life factors =============================================

PAF_smoking<-as.data.frame(svytable(~PAF_smoking, design = design_latelife))
PAF_smoking<-PAF_smoking %>% pivot_wider(names_from = PAF_smoking, values_from =Freq)
PAF_smoking$RF<-"PAF_smoking"
PAF_smoking$Prevalence<-PAF_smoking$`1`/(PAF_smoking$`1`+PAF_smoking$`0`)
a<-svyciprop(~PAF_smoking==1, design_latelife, method="li")
PAF_smoking$LCI<-attr(a,"ci")[1]
PAF_smoking$UCI<-attr(a,"ci")[2]


PAF_socialisolation<-as.data.frame(svytable(~PAF_socialisolation, design = design_latelife))
PAF_socialisolation<-PAF_socialisolation %>% pivot_wider(names_from = PAF_socialisolation, values_from =Freq)
PAF_socialisolation$RF<-"PAF_socialisolation"
PAF_socialisolation$Prevalence<-PAF_socialisolation$`1`/(PAF_socialisolation$`1`+PAF_socialisolation$`0`)
a<-svyciprop(~PAF_socialisolation==1, design_latelife, method="li")
PAF_socialisolation$LCI<-attr(a,"ci")[1]
PAF_socialisolation$UCI<-attr(a,"ci")[2]


PAF_pinactivity<-as.data.frame(svytable(~PAF_pinactivity, design = design_latelife))
PAF_pinactivity<-PAF_pinactivity %>% pivot_wider(names_from = PAF_pinactivity, values_from =Freq)
PAF_pinactivity$RF<-"PAF_pinactivity"
PAF_pinactivity$Prevalence<-PAF_pinactivity$`1`/(PAF_pinactivity$`1`+PAF_pinactivity$`0`)
a<-svyciprop(~PAF_pinactivity==1, design_latelife, method="li")
PAF_pinactivity$LCI<-attr(a,"ci")[1]
PAF_pinactivity$UCI<-attr(a,"ci")[2]

PAF_DBT<-as.data.frame(svytable(~PAF_DBT, design = design_latelife))
PAF_DBT<-PAF_DBT %>% pivot_wider(names_from = PAF_DBT, values_from =Freq)
PAF_DBT$RF<-"PAF_DBT"
PAF_DBT$Prevalence<-PAF_DBT$`1`/(PAF_DBT$`1`+PAF_DBT$`0`)
a<-svyciprop(~PAF_DBT==1, design_latelife, method="li")
PAF_DBT$LCI<-attr(a,"ci")[1]
PAF_DBT$UCI<-attr(a,"ci")[2]

PAF_air<-as.data.frame(svytable(~PAF_air, design = design_latelife))
PAF_air<-PAF_air %>% pivot_wider(names_from = PAF_air, values_from =Freq)
PAF_air$RF<-"PAF_air"
PAF_air$Prevalence<-PAF_air$`1`/(PAF_air$`1`+PAF_air$`0`)
a<-svyciprop(~PAF_air==1, design_latelife, method="li")
PAF_air$LCI<-attr(a,"ci")[1]
PAF_air$UCI<-attr(a,"ci")[2]


## 3. Integrating into table ==================================================

prevalences<-rbind(PAF_education,
                   PAF_obesity, 
                   PAF_HBP, 
                   PAF_alcohol, 
                   PAF_smoking, 
                   PAF_socialisolation, 
                   PAF_pinactivity, 
                   PAF_DBT,
                   PAF_air)


prevalences<-prevalences %>% select(RF, Prevalence, LCI, UCI)

RR<-c(1.6,1.6,1.6,1.2,1.6,1.6,1.4,1.5,1.1)


prevalences$RR<-RR

# comunalities<-comunalities %>% rename(RF=risk_factor)

base_summary_fem<-left_join(prevalences, comunalities_fem, by="RF")


## 4. PAF calculation=====================================

base_summary_fem$weigth<-1-base_summary_fem$comunality

base_summary_fem$PAF<-((base_summary_fem$Prevalence*
                          (base_summary_fem$RR-1)))/
  (1+base_summary_fem$Prevalence*(base_summary_fem$RR-1))

base_summary_fem$overall_PAF<-1-(base_summary_fem$comunality*base_summary_fem$PAF)

base_summary_fem$individual_PAF<-base_summary_fem$PAF/
  sum(base_summary_fem$PAF)*(1-prod(base_summary_fem$overall_PAF))

base_summary_fem$"1-PAF"<-1-base_summary_fem$PAF

unique_overall_weigthed_PAF_fem<-1-prod(base_summary_fem$overall_PAF)
unique_overall_PAF_fem<-1-prod(1-base_summary_fem$PAF)

unique_overall_weigthed_PAF_fem
unique_overall_PAF_fem


## 5. Confident intervals===========================

base_summary_fem$individual_PAF_LCI<-NA
base_summary_fem$individual_PAF_UCI<-NA


### 3.1 education ==================================
a<-prop.test((base_summary_fem$individual_PAF[base_summary_fem$RF=="PAF_education"]*n_midlife_fem),
             n_midlife_fem,
             correct=T)
base_summary_fem$individual_PAF_LCI[base_summary_fem$RF=="PAF_education"]<-a$conf.int[1]
base_summary_fem$individual_PAF_UCI[base_summary_fem$RF=="PAF_education"]<-a$conf.int[2]

### 3.2 obesity ==================================
a<-prop.test((base_summary_fem$individual_PAF[base_summary_fem$RF=="PAF_obesity"]*n_midlife_fem),
             n_midlife_fem,
             correct=T)
base_summary_fem$individual_PAF_LCI[base_summary_fem$RF=="PAF_obesity"]<-a$conf.int[1]
base_summary_fem$individual_PAF_UCI[base_summary_fem$RF=="PAF_obesity"]<-a$conf.int[2]

### 3.3 HBP ==================================
a<-prop.test((base_summary_fem$individual_PAF[base_summary_fem$RF=="PAF_HBP"]*n_midlife_fem),
             n_midlife_fem,
             correct=T)
base_summary_fem$individual_PAF_LCI[base_summary_fem$RF=="PAF_HBP"]<-a$conf.int[1]
base_summary_fem$individual_PAF_UCI[base_summary_fem$RF=="PAF_HBP"]<-a$conf.int[2]

### 3.4 Alcohol ==================================
a<-prop.test((base_summary_fem$individual_PAF[base_summary_fem$RF=="PAF_alcohol"]*n_midlife_fem),
             n_midlife_fem,
             correct=T)
base_summary_fem$individual_PAF_LCI[base_summary_fem$RF=="PAF_alcohol"]<-a$conf.int[1]
base_summary_fem$individual_PAF_UCI[base_summary_fem$RF=="PAF_alcohol"]<-a$conf.int[2]

### 3.5 Smoking ==================================
a<-prop.test((base_summary_fem$individual_PAF[base_summary_fem$RF=="PAF_smoking"]*n_latelife_fem),
             n_latelife_fem,
             correct=T)
base_summary_fem$individual_PAF_LCI[base_summary_fem$RF=="PAF_smoking"]<-a$conf.int[1]
base_summary_fem$individual_PAF_UCI[base_summary_fem$RF=="PAF_smoking"]<-a$conf.int[2]

### 3.6 Social isolation ==================================
a<-prop.test((base_summary_fem$individual_PAF[base_summary_fem$RF=="PAF_socialisolation"]*n_latelife_fem),
             n_latelife_fem,
             correct=T)
base_summary_fem$individual_PAF_LCI[base_summary_fem$RF=="PAF_socialisolation"]<-a$conf.int[1]
base_summary_fem$individual_PAF_UCI[base_summary_fem$RF=="PAF_socialisolation"]<-a$conf.int[2]

### 3.7 Physical inactivity ==================================
a<-prop.test((base_summary_fem$individual_PAF[base_summary_fem$RF=="PAF_pinactivity"]*n_latelife_fem),
             n_latelife_fem,
             correct=T)
base_summary_fem$individual_PAF_LCI[base_summary_fem$RF=="PAF_pinactivity"]<-a$conf.int[1]
base_summary_fem$individual_PAF_UCI[base_summary_fem$RF=="PAF_pinactivity"]<-a$conf.int[2]

### 3.8 Diabetes ==================================
a<-prop.test((base_summary_fem$individual_PAF[base_summary_fem$RF=="PAF_DBT"]*n_latelife_fem),
             n_latelife_fem,
             correct=T)
base_summary_fem$individual_PAF_LCI[base_summary_fem$RF=="PAF_DBT"]<-a$conf.int[1]
base_summary_fem$individual_PAF_UCI[base_summary_fem$RF=="PAF_DBT"]<-a$conf.int[2]

### 3.9 Air pollution ==================================
a<-prop.test((base_summary_fem$individual_PAF[base_summary_fem$RF=="PAF_air"]*n_latelife_fem),
             n_latelife_fem,
             correct=T)
base_summary_fem$individual_PAF_LCI[base_summary_fem$RF=="PAF_air"]<-a$conf.int[1]
base_summary_fem$individual_PAF_UCI[base_summary_fem$RF=="PAF_air"]<-a$conf.int[2]




#adjusting the table
base_summary_fem$Risk_factor<-c("Less education", "Obesity", "Hypertension", "Alcohol consumption", "Smoking", 
                                "Social Isolation", "Physical inactivity", "Diabetes",
                                "Air pollution")

base_summary_fem <- base_summary_fem[, c(14,2,3,4,5,6,7,8,9,10,11,12,13,1)]

base_summary_fem <- base_summary_fem[c(1,3,4,2,5,6,7,8,9),]


#6. Table==============================================
main_table<-base_summary_fem %>% select(-RF, -overall_PAF, -`1-PAF`) %>%
  gt() %>%
  fmt_number(
    columns = 5:9,
    decimals = 1)%>%
  fmt_percent(
    columns = c(2:4,7:11),
    decimals = 1
  )  %>%
  tab_spanner(
    label = "RF prevalence",
    columns = c(Prevalence, LCI, UCI))  %>%
  tab_spanner(
    label = "weigthed PAF",
    columns = c(individual_PAF, individual_PAF_LCI, individual_PAF_UCI))  %>%
  tab_header(title = md("PAF for dementia risk factors - fem "))%>%
  sub_missing(columns = everything(),
              rows = everything(),
              missing_text = "---"
  )
main_table
gt::gtsave(main_table, file = "Draft/annex_table_fem_2018.rtf")
#7. Calculating de overall weihed PAF with CI==============================

unique_overall_weigthed_PAF_fem_CI<-prop.test(unique_overall_weigthed_PAF_fem*n_total_fem,
                                              n_total_fem,
                                              correct=T)


unique_overall_weigthed_PAF_fem_CI

library(PropCIs)
n_latelife_fem+n_midlife_fem
(n_latelife_fem+n_midlife_fem)*0.352345


exactci(5106, 14493,
        conf.level=0.95)


save(base_summary_fem, unique_overall_PAF_fem, unique_overall_weigthed_PAF_fem,
     unique_overall_weigthed_PAF_fem_CI, 
     n_midlife_fem,
     n_latelife_fem,
     n_total_fem, file="tabla_fem.RData")

unique_overall_weigthed_PAF_fem*100
unique_overall_weigthed_PAF_fem_CI

rm(list = ls())


#...............................................................................
#                                                                              .
# 1.SEX=MALE ====================================================================                                                                   .
#                                                                              .
#...............................................................................


# 2. Data origin (risk_factor coded) ==============================

load("base2018_general.RData")
rm(basecom, comunalities)

basemasc<-base2018 %>% filter(bhch03==1)

# 3. Comunalities calculation==================

basecom_masc<-basemasc %>% select(starts_with("PAF"))


library(psych)
## 1. Tetrachoric correlation ==============================
a<-tetrachoric(basecom_masc)                             
b<-as.data.frame(a$rho)

## 2. Communalitie calculation ===========

# PCA resolution
# Analize eigenvalues, keep Dim w/ eigenvalues >1
eigenvalues <-eigen(b)$values
eigenvalues

# Only 3 dimensions has eigenvalues > 1
eigenVectors<-eigen(b)$vectors
eigenVectors

comunalities<-as.data.frame(eigenVectors)
comunalities$RF<-names(b)

comunalities$comunality<-comunalities$V1^2+comunalities$V2^2+comunalities$V3^2

comunalities_masc<-comunalities %>% select(RF, comunality)

rm(base2018, a, b, eigenVectors, eigenvalues, comunalities)

# 4. PAF table ================================================================

## 1. Survey design =============================================
base_midlife<-basemasc %>% filter(bhch04>=45 & bhch04<=64) 
n_midlife_masc<-nrow(base_midlife)

design_midlife <- svydesign(id=~id, weights=~wf1p, data=base_midlife)

base_latelife<-basemasc %>% filter(bhch04>64) 
design_latelife <- svydesign(id=~id, weights=~wf1p, data=base_latelife)
n_latelife_masc<-nrow(base_latelife)

n_total_masc<-n_latelife_masc + n_midlife_masc

## 2. Calculate prevalence =============================================

### 2.1 Midlife factors =============================================
PAF_education<-as.data.frame(svytable(~PAF_education, design = design_midlife))
PAF_education<-PAF_education %>% pivot_wider(names_from = PAF_education, values_from =Freq)
PAF_education$RF<-"PAF_education"
PAF_education$Prevalence<-PAF_education$`1`/(PAF_education$`1`+PAF_education$`0`)
a<-svyciprop(~PAF_education==1, design_midlife, method="li")

PAF_education$LCI<-attr(a,"ci")[1]
PAF_education$UCI<-attr(a,"ci")[2]


PAF_obesity<-as.data.frame(svytable(~PAF_obesity, design = design_midlife))
PAF_obesity<-PAF_obesity %>% pivot_wider(names_from = PAF_obesity, values_from =Freq)
PAF_obesity$RF<-"PAF_obesity"
PAF_obesity$Prevalence<-PAF_obesity$`1`/(PAF_obesity$`1`+PAF_obesity$`0`)
a<-svyciprop(~PAF_obesity==1, design_midlife, method="li")
PAF_obesity$LCI<-attr(a,"ci")[1]
PAF_obesity$UCI<-attr(a,"ci")[2]


PAF_HBP<-as.data.frame(svytable(~PAF_HBP, design = design_midlife))
PAF_HBP<-PAF_HBP %>% pivot_wider(names_from = PAF_HBP, values_from =Freq)
PAF_HBP$RF<-"PAF_HBP"
PAF_HBP$Prevalence<-PAF_HBP$`1`/(PAF_HBP$`1`+PAF_HBP$`0`)
a<-svyciprop(~PAF_HBP==1, design_midlife, method="li")
PAF_HBP$LCI<-attr(a,"ci")[1]
PAF_HBP$UCI<-attr(a,"ci")[2]


PAF_alcohol<-as.data.frame(svytable(~PAF_alcohol, design = design_midlife))
PAF_alcohol<-PAF_alcohol %>% pivot_wider(names_from = PAF_alcohol, values_from =Freq)
PAF_alcohol$RF<-"PAF_alcohol"
PAF_alcohol$Prevalence<-PAF_alcohol$`1`/(PAF_alcohol$`1`+PAF_alcohol$`0`)
a<-svyciprop(~PAF_alcohol==1, design_midlife, method="li")
PAF_alcohol$LCI<-attr(a,"ci")[1]
PAF_alcohol$UCI<-attr(a,"ci")[2]

### 2.2 Later-life factors =============================================

PAF_smoking<-as.data.frame(svytable(~PAF_smoking, design = design_latelife))
PAF_smoking<-PAF_smoking %>% pivot_wider(names_from = PAF_smoking, values_from =Freq)
PAF_smoking$RF<-"PAF_smoking"
PAF_smoking$Prevalence<-PAF_smoking$`1`/(PAF_smoking$`1`+PAF_smoking$`0`)
a<-svyciprop(~PAF_smoking==1, design_latelife, method="li")
PAF_smoking$LCI<-attr(a,"ci")[1]
PAF_smoking$UCI<-attr(a,"ci")[2]


PAF_socialisolation<-as.data.frame(svytable(~PAF_socialisolation, design = design_latelife))
PAF_socialisolation<-PAF_socialisolation %>% pivot_wider(names_from = PAF_socialisolation, values_from =Freq)
PAF_socialisolation$RF<-"PAF_socialisolation"
PAF_socialisolation$Prevalence<-PAF_socialisolation$`1`/(PAF_socialisolation$`1`+PAF_socialisolation$`0`)
a<-svyciprop(~PAF_socialisolation==1, design_latelife, method="li")
PAF_socialisolation$LCI<-attr(a,"ci")[1]
PAF_socialisolation$UCI<-attr(a,"ci")[2]


PAF_pinactivity<-as.data.frame(svytable(~PAF_pinactivity, design = design_latelife))
PAF_pinactivity<-PAF_pinactivity %>% pivot_wider(names_from = PAF_pinactivity, values_from =Freq)
PAF_pinactivity$RF<-"PAF_pinactivity"
PAF_pinactivity$Prevalence<-PAF_pinactivity$`1`/(PAF_pinactivity$`1`+PAF_pinactivity$`0`)
a<-svyciprop(~PAF_pinactivity==1, design_latelife, method="li")
PAF_pinactivity$LCI<-attr(a,"ci")[1]
PAF_pinactivity$UCI<-attr(a,"ci")[2]

PAF_DBT<-as.data.frame(svytable(~PAF_DBT, design = design_latelife))
PAF_DBT<-PAF_DBT %>% pivot_wider(names_from = PAF_DBT, values_from =Freq)
PAF_DBT$RF<-"PAF_DBT"
PAF_DBT$Prevalence<-PAF_DBT$`1`/(PAF_DBT$`1`+PAF_DBT$`0`)
a<-svyciprop(~PAF_DBT==1, design_latelife, method="li")
PAF_DBT$LCI<-attr(a,"ci")[1]
PAF_DBT$UCI<-attr(a,"ci")[2]

PAF_air<-as.data.frame(svytable(~PAF_air, design = design_latelife))
PAF_air<-PAF_air %>% pivot_wider(names_from = PAF_air, values_from =Freq)
PAF_air$RF<-"PAF_air"
PAF_air$Prevalence<-PAF_air$`1`/(PAF_air$`1`+PAF_air$`0`)
a<-svyciprop(~PAF_air==1, design_latelife, method="li")
PAF_air$LCI<-attr(a,"ci")[1]
PAF_air$UCI<-attr(a,"ci")[2]


## 3. Integrating into table ==================================================

prevalences<-rbind(PAF_education,
                   PAF_obesity, 
                   PAF_HBP, 
                   PAF_alcohol, 
                   PAF_smoking, 
                   PAF_socialisolation, 
                   PAF_pinactivity, 
                   PAF_DBT,
                   PAF_air)


prevalences<-prevalences %>% select(RF, Prevalence, LCI, UCI)

RR<-c(1.6,1.6,1.6,1.2,1.6,1.6,1.4,1.5,1.1)


prevalences$RR<-RR

# comunalities<-comunalities %>% rename(RF=risk_factor)

base_summary_masc<-left_join(prevalences, comunalities_masc, by="RF")


## 4. PAF calculation=====================================

base_summary_masc$weigth<-1-base_summary_masc$comunality

base_summary_masc$PAF<-((base_summary_masc$Prevalence*
                          (base_summary_masc$RR-1)))/
  (1+base_summary_masc$Prevalence*(base_summary_masc$RR-1))

base_summary_masc$overall_PAF<-1-(base_summary_masc$comunality*base_summary_masc$PAF)

base_summary_masc$individual_PAF<-base_summary_masc$PAF/
  sum(base_summary_masc$PAF)*(1-prod(base_summary_masc$overall_PAF))

base_summary_masc$"1-PAF"<-1-base_summary_masc$PAF

unique_overall_weigthed_PAF_masc<-1-prod(base_summary_masc$overall_PAF)
unique_overall_PAF_masc<-1-prod(1-base_summary_masc$PAF)

unique_overall_weigthed_PAF_masc
unique_overall_PAF_masc


## 5. Confident intervals===========================

base_summary_masc$individual_PAF_LCI<-NA
base_summary_masc$individual_PAF_UCI<-NA


### 3.1 education ==================================
a<-prop.test((base_summary_masc$individual_PAF[base_summary_masc$RF=="PAF_education"]*n_midlife_masc),
             n_midlife_masc,
             correct=T)
base_summary_masc$individual_PAF_LCI[base_summary_masc$RF=="PAF_education"]<-a$conf.int[1]
base_summary_masc$individual_PAF_UCI[base_summary_masc$RF=="PAF_education"]<-a$conf.int[2]

### 3.2 obesity ==================================
a<-prop.test((base_summary_masc$individual_PAF[base_summary_masc$RF=="PAF_obesity"]*n_midlife_masc),
             n_midlife_masc,
             correct=T)
base_summary_masc$individual_PAF_LCI[base_summary_masc$RF=="PAF_obesity"]<-a$conf.int[1]
base_summary_masc$individual_PAF_UCI[base_summary_masc$RF=="PAF_obesity"]<-a$conf.int[2]

### 3.3 HBP ==================================
a<-prop.test((base_summary_masc$individual_PAF[base_summary_masc$RF=="PAF_HBP"]*n_midlife_masc),
             n_midlife_masc,
             correct=T)
base_summary_masc$individual_PAF_LCI[base_summary_masc$RF=="PAF_HBP"]<-a$conf.int[1]
base_summary_masc$individual_PAF_UCI[base_summary_masc$RF=="PAF_HBP"]<-a$conf.int[2]

### 3.4 Alcohol ==================================
a<-prop.test((base_summary_masc$individual_PAF[base_summary_masc$RF=="PAF_alcohol"]*n_midlife_masc),
             n_midlife_masc,
             correct=T)
base_summary_masc$individual_PAF_LCI[base_summary_masc$RF=="PAF_alcohol"]<-a$conf.int[1]
base_summary_masc$individual_PAF_UCI[base_summary_masc$RF=="PAF_alcohol"]<-a$conf.int[2]

### 3.5 Smoking ==================================
a<-prop.test((base_summary_masc$individual_PAF[base_summary_masc$RF=="PAF_smoking"]*n_latelife_masc),
             n_latelife_masc,
             correct=T)
base_summary_masc$individual_PAF_LCI[base_summary_masc$RF=="PAF_smoking"]<-a$conf.int[1]
base_summary_masc$individual_PAF_UCI[base_summary_masc$RF=="PAF_smoking"]<-a$conf.int[2]

### 3.6 Social isolation ==================================
a<-prop.test((base_summary_masc$individual_PAF[base_summary_masc$RF=="PAF_socialisolation"]*n_latelife_masc),
             n_latelife_masc,
             correct=T)
base_summary_masc$individual_PAF_LCI[base_summary_masc$RF=="PAF_socialisolation"]<-a$conf.int[1]
base_summary_masc$individual_PAF_UCI[base_summary_masc$RF=="PAF_socialisolation"]<-a$conf.int[2]

### 3.7 Physical inactivity ==================================
a<-prop.test((base_summary_masc$individual_PAF[base_summary_masc$RF=="PAF_pinactivity"]*n_latelife_masc),
             n_latelife_masc,
             correct=T)
base_summary_masc$individual_PAF_LCI[base_summary_masc$RF=="PAF_pinactivity"]<-a$conf.int[1]
base_summary_masc$individual_PAF_UCI[base_summary_masc$RF=="PAF_pinactivity"]<-a$conf.int[2]

### 3.8 Diabetes ==================================
a<-prop.test((base_summary_masc$individual_PAF[base_summary_masc$RF=="PAF_DBT"]*n_latelife_masc),
             n_latelife_masc,
             correct=T)
base_summary_masc$individual_PAF_LCI[base_summary_masc$RF=="PAF_DBT"]<-a$conf.int[1]
base_summary_masc$individual_PAF_UCI[base_summary_masc$RF=="PAF_DBT"]<-a$conf.int[2]

### 3.9 Air pollution ==================================
a<-prop.test((base_summary_masc$individual_PAF[base_summary_masc$RF=="PAF_air"]*n_latelife_masc),
             n_latelife_masc,
             correct=T)
base_summary_masc$individual_PAF_LCI[base_summary_masc$RF=="PAF_air"]<-a$conf.int[1]
base_summary_masc$individual_PAF_UCI[base_summary_masc$RF=="PAF_air"]<-a$conf.int[2]




#adjusting the table
base_summary_masc$Risk_factor<-c("Less education", "Obesity", "Hypertension", "Alcohol consumption", "Smoking", 
                                "Social Isolation", "Physical inactivity", "Diabetes",
                                "Air pollution")

base_summary_masc <- base_summary_masc[, c(14,2,3,4,5,6,7,8,9,10,11,12,13,1)]

base_summary_masc <- base_summary_masc[c(1,3,4,2,5,6,7,8,9),]


#6. Table==============================================
main_table<-base_summary_masc %>% select(-RF, -overall_PAF, -`1-PAF`) %>%
  gt() %>%
  fmt_number(
    columns = 5:9,
    decimals = 1)%>%
  fmt_percent(
    columns = c(2:4,7:11),
    decimals = 1
  )  %>%
  tab_spanner(
    label = "RF prevalence",
    columns = c(Prevalence, LCI, UCI))  %>%
  tab_spanner(
    label = "weigthed PAF",
    columns = c(individual_PAF, individual_PAF_LCI, individual_PAF_UCI))  %>%
  tab_header(title = md("PAF for dementia risk factors - masc "))%>%
  sub_missing(columns = everything(),
              rows = everything(),
              missing_text = "---"
  )

main_table

gt::gtsave(main_table, file = "Draft/annex_table_masc_2018.rtf")
#7. Calculating de overall weihed PAF with CI==============================

unique_overall_weigthed_PAF_masc_CI<-prop.test(unique_overall_weigthed_PAF_masc*n_total_masc,
                                              n_total_masc,
                                              correct=T)


unique_overall_weigthed_PAF_masc_CI

library(PropCIs)
n_latelife_masc+n_midlife_masc
(n_latelife_masc+n_midlife_masc)*0.352345


exactci(5106, 14493,
        conf.level=0.95)

save(base_summary_masc, unique_overall_PAF_masc, unique_overall_weigthed_PAF_masc,
     unique_overall_weigthed_PAF_masc_CI, 
     n_midlife_masc,
     n_latelife_masc,
     n_total_masc, file="tabla_masc.RData")

unique_overall_weigthed_PAF_masc*100
unique_overall_weigthed_PAF_masc_CI

rm(list = ls())
