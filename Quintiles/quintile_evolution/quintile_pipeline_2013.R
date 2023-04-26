
#  Date 21 April 2023                                                           
#                                                                               
#  Input: Output of “01_data_tidy_2013” script                                       
#                                                                               
#  Author: Ismael L Calandri                                                    
#                                                                               
#  Output: a table with PAF calculation for 2013 quintile division                             




# 1. Packages  ==============================

rm(list = ls())
library(readr)
library(tidyverse)
library("factoextra")
library("FactoMineR")
library(survey)
library(gt)
library(flextable)

# 1. FIRST QUINTILE ==============================

#...............................................................................
#                                                                              .
#  Q1                                                                 .
#                                                                              .
#...............................................................................


# 2. Data origin (risk_factor coded) ==============================

load("base2013_general.RData")

baseq1<-base2013 %>% filter(ITF_UC_QUINTILES==1)

# 3. Comunalities calculation==================

basecom_q1<-baseq1 %>% select(starts_with("PAF"))


library(psych)
## 1. Tetrachoric correlation ==============================
a<-tetrachoric(basecom_q1)                             
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

comunalities_q1<-comunalities %>% select(RF, comunality)

rm(base2013, a, b, eigenVectors, eigenvalues, comunalities)

# 4. PAF table ================================================================


## 1. Survey design =============================================
base_midlife<-baseq1 %>% filter(BHCH05>=45 & BHCH05<=64) 
n_midlife_q1_2013<-nrow(base_midlife)

design_midlife <- svydesign(id=~ID, weights=~PONDERACION, data=base_midlife)

base_latelife<-baseq1 %>% filter(BHCH05>64) 
design_latelife <- svydesign(id=~ID, weights=~PONDERACION, data=base_latelife)
n_latelife_q1_2013<-nrow(base_latelife)

n_total_q1_2013<-n_latelife_q1_2013 + n_midlife_q1_2013

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



## 3. Integrating into table ==================================================

prevalences<-rbind(PAF_education,
                   PAF_obesity, 
                   PAF_HBP, 
                   PAF_alcohol, 
                   PAF_smoking, 
                   PAF_socialisolation, 
                   PAF_pinactivity, 
                   PAF_DBT)


prevalences<-prevalences %>% select(RF, Prevalence, LCI, UCI)

RR<-c(1.6,1.6,1.6,1.2,1.6,1.6,1.4,1.5)


prevalences$RR<-RR

# comunalities<-comunalities %>% rename(RF=risk_factor)

base_summary_q1_2013<-left_join(prevalences, comunalities_q1, by="RF")


## 4. PAF calculation=====================================

base_summary_q1_2013$weigth<-1-base_summary_q1_2013$comunality

base_summary_q1_2013$PAF<-((base_summary_q1_2013$Prevalence*
                          (base_summary_q1_2013$RR-1)))/
  (1+base_summary_q1_2013$Prevalence*(base_summary_q1_2013$RR-1))

base_summary_q1_2013$overall_PAF<-1-(base_summary_q1_2013$comunality*base_summary_q1_2013$PAF)

base_summary_q1_2013$individual_PAF<-base_summary_q1_2013$PAF/
  sum(base_summary_q1_2013$PAF)*(1-prod(base_summary_q1_2013$overall_PAF))

base_summary_q1_2013$"1-PAF"<-1-base_summary_q1_2013$PAF

unique_overall_weigthed_PAF_q1_2013<-1-prod(base_summary_q1_2013$overall_PAF)
unique_overall_PAF_q1_2013<-1-prod(1-base_summary_q1_2013$PAF)

unique_overall_weigthed_PAF_q1_2013
unique_overall_PAF_q1_2013


## 5. Confident intervals===========================

base_summary_q1_2013$individual_PAF_LCI<-NA
base_summary_q1_2013$individual_PAF_UCI<-NA


### 3.1 education ==================================
a<-prop.test((base_summary_q1_2013$individual_PAF[base_summary_q1_2013$RF=="PAF_education"]*n_midlife_q1_2013),
             n_midlife_q1_2013,
             correct=T)
base_summary_q1_2013$individual_PAF_LCI[base_summary_q1_2013$RF=="PAF_education"]<-a$conf.int[1]
base_summary_q1_2013$individual_PAF_UCI[base_summary_q1_2013$RF=="PAF_education"]<-a$conf.int[2]

### 3.2 obesity ==================================
a<-prop.test((base_summary_q1_2013$individual_PAF[base_summary_q1_2013$RF=="PAF_obesity"]*n_midlife_q1_2013),
             n_midlife_q1_2013,
             correct=T)
base_summary_q1_2013$individual_PAF_LCI[base_summary_q1_2013$RF=="PAF_obesity"]<-a$conf.int[1]
base_summary_q1_2013$individual_PAF_UCI[base_summary_q1_2013$RF=="PAF_obesity"]<-a$conf.int[2]

### 3.3 HBP ==================================
a<-prop.test((base_summary_q1_2013$individual_PAF[base_summary_q1_2013$RF=="PAF_HBP"]*n_midlife_q1_2013),
             n_midlife_q1_2013,
             correct=T)
base_summary_q1_2013$individual_PAF_LCI[base_summary_q1_2013$RF=="PAF_HBP"]<-a$conf.int[1]
base_summary_q1_2013$individual_PAF_UCI[base_summary_q1_2013$RF=="PAF_HBP"]<-a$conf.int[2]

### 3.4 Alcohol ==================================
a<-prop.test((base_summary_q1_2013$individual_PAF[base_summary_q1_2013$RF=="PAF_alcohol"]*n_midlife_q1_2013),
             n_midlife_q1_2013,
             correct=T)
base_summary_q1_2013$individual_PAF_LCI[base_summary_q1_2013$RF=="PAF_alcohol"]<-a$conf.int[1]
base_summary_q1_2013$individual_PAF_UCI[base_summary_q1_2013$RF=="PAF_alcohol"]<-a$conf.int[2]

### 3.5 Smoking ==================================
a<-prop.test((base_summary_q1_2013$individual_PAF[base_summary_q1_2013$RF=="PAF_smoking"]*n_latelife_q1_2013),
             n_latelife_q1_2013,
             correct=T)
base_summary_q1_2013$individual_PAF_LCI[base_summary_q1_2013$RF=="PAF_smoking"]<-a$conf.int[1]
base_summary_q1_2013$individual_PAF_UCI[base_summary_q1_2013$RF=="PAF_smoking"]<-a$conf.int[2]

### 3.6 Social isolation ==================================
a<-prop.test((base_summary_q1_2013$individual_PAF[base_summary_q1_2013$RF=="PAF_socialisolation"]*n_latelife_q1_2013),
             n_latelife_q1_2013,
             correct=T)
base_summary_q1_2013$individual_PAF_LCI[base_summary_q1_2013$RF=="PAF_socialisolation"]<-a$conf.int[1]
base_summary_q1_2013$individual_PAF_UCI[base_summary_q1_2013$RF=="PAF_socialisolation"]<-a$conf.int[2]

### 3.7 Physical inactivity ==================================
a<-prop.test((base_summary_q1_2013$individual_PAF[base_summary_q1_2013$RF=="PAF_pinactivity"]*n_latelife_q1_2013),
             n_latelife_q1_2013,
             correct=T)
base_summary_q1_2013$individual_PAF_LCI[base_summary_q1_2013$RF=="PAF_pinactivity"]<-a$conf.int[1]
base_summary_q1_2013$individual_PAF_UCI[base_summary_q1_2013$RF=="PAF_pinactivity"]<-a$conf.int[2]

### 3.8 Diabetes ==================================
a<-prop.test((base_summary_q1_2013$individual_PAF[base_summary_q1_2013$RF=="PAF_DBT"]*n_latelife_q1_2013),
             n_latelife_q1_2013,
             correct=T)
base_summary_q1_2013$individual_PAF_LCI[base_summary_q1_2013$RF=="PAF_DBT"]<-a$conf.int[1]
base_summary_q1_2013$individual_PAF_UCI[base_summary_q1_2013$RF=="PAF_DBT"]<-a$conf.int[2]



#adjusting the table
base_summary_q1_2013$Risk_factor<-c("Less education", "Obesity", "Hypertension", "Alcohol consumption", "Smoking", 
                                "Social Isolation", "Physical inactivity", "Diabetes")

base_summary_q1_2013 <- base_summary_q1_2013[, c(14,2,3,4,5,6,7,8,9,10,11,12,13,1)]

base_summary_q1_2013 <- base_summary_q1_2013[c(1,3,4,2,5,6,7,8),]


#6. Table==============================================
main_table<-base_summary_q1_2013 %>% select(-RF, -overall_PAF, -`1-PAF`) %>%
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
  tab_header(title = md("PAF for dementia risk factors - q1 "))%>%
  sub_missing(columns = everything(),
              rows = everything(),
              missing_text = "---"
  )
main_table
#7. Calculating de overall weihed PAF with CI==============================

unique_overall_weigthed_PAF_q1_CI_2013<-prop.test(unique_overall_weigthed_PAF_q1_2013*n_total_q1_2013,
                                              n_total_q1_2013,
                                              correct=T)


unique_overall_weigthed_PAF_q1_CI_2013

library(PropCIs)
n_latelife_q1_2013+n_midlife_q1_2013
(n_latelife_q1_2013+n_midlife_q1_2013)*0.352345


exactci(5106, 14493,
        conf.level=0.95)


save(base_summary_q1_2013, unique_overall_PAF_q1_2013, unique_overall_weigthed_PAF_q1_2013,
     unique_overall_weigthed_PAF_q1_CI_2013, 
     n_midlife_q1_2013,
     n_latelife_q1_2013,
     n_total_q1_2013, file="tabla_q1_2013.RData")

rm(list = ls())


# 1. SECOND QUINTILE ==============================

#...............................................................................
#                                                                              .
#  Quintile 2                                                                   .
#                                                                              .
#...............................................................................


# 2. Data origin (risk_factor coded) ==============================

load("base2013_general.RData")

baseq2<-base2013 %>% filter(ITF_UC_QUINTILES==2)

# 3. Comunalities calculation==================

basecom_q2<-baseq2 %>% select(starts_with("PAF"))


library(psych)
## 1. Tetrachoric correlation ==============================
a<-tetrachoric(basecom_q2)                             
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

comunalities_q2<-comunalities %>% select(RF, comunality)

rm(base2013, a, b, eigenVectors, eigenvalues, comunalities)

# 4. PAF table ================================================================

## 1. Survey design =============================================
base_midlife<-baseq2 %>% filter(BHCH05>=45 & BHCH05<=64) 
n_midlife_q2_2013<-nrow(base_midlife)

design_midlife <- svydesign(id=~ID, weights=~PONDERACION, data=base_midlife)

base_latelife<-baseq2 %>% filter(BHCH05>64) 
design_latelife <- svydesign(id=~ID, weights=~PONDERACION, data=base_latelife)
n_latelife_q2_2013<-nrow(base_latelife)

n_total_q2_2013<-n_latelife_q2_2013 + n_midlife_q2_2013

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



## 3. Integrating into table ==================================================

prevalences<-rbind(PAF_education,
                   PAF_obesity, 
                   PAF_HBP, 
                   PAF_alcohol, 
                   PAF_smoking, 
                   PAF_socialisolation, 
                   PAF_pinactivity, 
                   PAF_DBT)


prevalences<-prevalences %>% select(RF, Prevalence, LCI, UCI)

RR<-c(1.6,1.6,1.6,1.2,1.6,1.6,1.4,1.5)


prevalences$RR<-RR

# comunalities<-comunalities %>% rename(RF=risk_factor)

base_summary_q2<-left_join(prevalences, comunalities_q2, by="RF")


## 4. PAF calculation=====================================

base_summary_q2$weigth<-1-base_summary_q2$comunality

base_summary_q2$PAF<-((base_summary_q2$Prevalence*
                          (base_summary_q2$RR-1)))/
  (1+base_summary_q2$Prevalence*(base_summary_q2$RR-1))

base_summary_q2$overall_PAF<-1-(base_summary_q2$comunality*base_summary_q2$PAF)

base_summary_q2$individual_PAF<-base_summary_q2$PAF/
  sum(base_summary_q2$PAF)*(1-prod(base_summary_q2$overall_PAF))

base_summary_q2$"1-PAF"<-1-base_summary_q2$PAF

unique_overall_weigthed_PAF_q2_2013<-1-prod(base_summary_q2$overall_PAF)
unique_overall_PAF_q2_2013<-1-prod(1-base_summary_q2$PAF)

unique_overall_weigthed_PAF_q2_2013
unique_overall_PAF_q2_2013


## 5. Confident intervals===========================

base_summary_q2$individual_PAF_LCI<-NA
base_summary_q2$individual_PAF_UCI<-NA


### 3.1 education ==================================
a<-prop.test((base_summary_q2$individual_PAF[base_summary_q2$RF=="PAF_education"]*n_midlife_q2_2013),
             n_midlife_q2_2013,
             correct=T)
base_summary_q2$individual_PAF_LCI[base_summary_q2$RF=="PAF_education"]<-a$conf.int[1]
base_summary_q2$individual_PAF_UCI[base_summary_q2$RF=="PAF_education"]<-a$conf.int[2]

### 3.2 obesity ==================================
a<-prop.test((base_summary_q2$individual_PAF[base_summary_q2$RF=="PAF_obesity"]*n_midlife_q2_2013),
             n_midlife_q2_2013,
             correct=T)
base_summary_q2$individual_PAF_LCI[base_summary_q2$RF=="PAF_obesity"]<-a$conf.int[1]
base_summary_q2$individual_PAF_UCI[base_summary_q2$RF=="PAF_obesity"]<-a$conf.int[2]

### 3.3 HBP ==================================
a<-prop.test((base_summary_q2$individual_PAF[base_summary_q2$RF=="PAF_HBP"]*n_midlife_q2_2013),
             n_midlife_q2_2013,
             correct=T)
base_summary_q2$individual_PAF_LCI[base_summary_q2$RF=="PAF_HBP"]<-a$conf.int[1]
base_summary_q2$individual_PAF_UCI[base_summary_q2$RF=="PAF_HBP"]<-a$conf.int[2]

### 3.4 Alcohol ==================================
a<-prop.test((base_summary_q2$individual_PAF[base_summary_q2$RF=="PAF_alcohol"]*n_midlife_q2_2013),
             n_midlife_q2_2013,
             correct=T)
base_summary_q2$individual_PAF_LCI[base_summary_q2$RF=="PAF_alcohol"]<-a$conf.int[1]
base_summary_q2$individual_PAF_UCI[base_summary_q2$RF=="PAF_alcohol"]<-a$conf.int[2]

### 3.5 Smoking ==================================
a<-prop.test((base_summary_q2$individual_PAF[base_summary_q2$RF=="PAF_smoking"]*n_latelife_q2_2013),
             n_latelife_q2_2013,
             correct=T)
base_summary_q2$individual_PAF_LCI[base_summary_q2$RF=="PAF_smoking"]<-a$conf.int[1]
base_summary_q2$individual_PAF_UCI[base_summary_q2$RF=="PAF_smoking"]<-a$conf.int[2]

### 3.6 Social isolation ==================================
a<-prop.test((base_summary_q2$individual_PAF[base_summary_q2$RF=="PAF_socialisolation"]*n_latelife_q2_2013),
             n_latelife_q2_2013,
             correct=T)
base_summary_q2$individual_PAF_LCI[base_summary_q2$RF=="PAF_socialisolation"]<-a$conf.int[1]
base_summary_q2$individual_PAF_UCI[base_summary_q2$RF=="PAF_socialisolation"]<-a$conf.int[2]

### 3.7 Physical inactivity ==================================
a<-prop.test((base_summary_q2$individual_PAF[base_summary_q2$RF=="PAF_pinactivity"]*n_latelife_q2_2013),
             n_latelife_q2_2013,
             correct=T)
base_summary_q2$individual_PAF_LCI[base_summary_q2$RF=="PAF_pinactivity"]<-a$conf.int[1]
base_summary_q2$individual_PAF_UCI[base_summary_q2$RF=="PAF_pinactivity"]<-a$conf.int[2]

### 3.8 Diabetes ==================================
a<-prop.test((base_summary_q2$individual_PAF[base_summary_q2$RF=="PAF_DBT"]*n_latelife_q2_2013),
             n_latelife_q2_2013,
             correct=T)
base_summary_q2$individual_PAF_LCI[base_summary_q2$RF=="PAF_DBT"]<-a$conf.int[1]
base_summary_q2$individual_PAF_UCI[base_summary_q2$RF=="PAF_DBT"]<-a$conf.int[2]


#adjusting the table
base_summary_q2$Risk_factor<-c("Less education", "Obesity", "Hypertension", "Alcohol consumption", "Smoking", 
                                "Social Isolation", "Physical inactivity", "Diabetes")

base_summary_q2 <- base_summary_q2[, c(14,2,3,4,5,6,7,8,9,10,11,12,13,1)]

base_summary_q2 <- base_summary_q2[c(1,3,4,2,5,6,7,8),]


base_summary_q2_2013<-base_summary_q2
rm(base_summary_q2)

#6. Table==============================================
main_table<-base_summary_q2_2013 %>% select(-RF, -overall_PAF, -`1-PAF`) %>%
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
  tab_header(title = md("PAF for dementia risk factors - q2 "))%>%
  sub_missing(columns = everything(),
              rows = everything(),
              missing_text = "---"
  )

main_table

#7. Calculating de overall weihed PAF with CI==============================

unique_overall_weigthed_PAF_q2_CI_2013<-prop.test(unique_overall_weigthed_PAF_q2_2013*n_total_q2_2013,
                                              n_total_q2_2013,
                                              correct=T)


unique_overall_weigthed_PAF_q2_CI_2013



save(base_summary_q2_2013, unique_overall_PAF_q2_2013, unique_overall_weigthed_PAF_q2_2013,
     unique_overall_weigthed_PAF_q2_CI_2013, 
     n_midlife_q2_2013,
     n_latelife_q2_2013,
     n_total_q2_2013, file="tabla_q2_2013.RData")


rm(list = ls())


# 1. THIRD QUINTILE ==============================

#...............................................................................
#                                                                              .
#  Q3                                                                 .
#                                                                              .
#...............................................................................


# 2. Data origin (risk_factor coded) ==============================

load("base2013_general.RData")

baseq3<-base2013 %>% filter(ITF_UC_QUINTILES==3)

# 3. Comunalities calculation==================

basecom_q3<-baseq3 %>% select(starts_with("PAF"))


library(psych)
## 1. Tetrachoric correlation ==============================
a<-tetrachoric(basecom_q3)                             
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

comunalities_q3<-comunalities %>% select(RF, comunality)

rm(base2013, a, b, eigenVectors, eigenvalues, comunalities)

# 4. PAF table ================================================================


## 1. Survey design =============================================
base_midlife<-baseq3 %>% filter(BHCH05>=45 & BHCH05<=64) 
n_midlife_q3_2013<-nrow(base_midlife)

design_midlife <- svydesign(id=~ID, weights=~PONDERACION, data=base_midlife)

base_latelife<-baseq3 %>% filter(BHCH05>64) 
design_latelife <- svydesign(id=~ID, weights=~PONDERACION, data=base_latelife)
n_latelife_q3_2013<-nrow(base_latelife)

n_total_q3_2013<-n_latelife_q3_2013 + n_midlife_q3_2013

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



## 3. Integrating into table ==================================================

prevalences<-rbind(PAF_education,
                   PAF_obesity, 
                   PAF_HBP, 
                   PAF_alcohol, 
                   PAF_smoking, 
                   PAF_socialisolation, 
                   PAF_pinactivity, 
                   PAF_DBT)


prevalences<-prevalences %>% select(RF, Prevalence, LCI, UCI)

RR<-c(1.6,1.6,1.6,1.2,1.6,1.6,1.4,1.5)


prevalences$RR<-RR

# comunalities<-comunalities %>% rename(RF=risk_factor)

base_summary_q3_2013<-left_join(prevalences, comunalities_q3, by="RF")


## 4. PAF calculation=====================================

base_summary_q3_2013$weigth<-1-base_summary_q3_2013$comunality

base_summary_q3_2013$PAF<-((base_summary_q3_2013$Prevalence*
                              (base_summary_q3_2013$RR-1)))/
  (1+base_summary_q3_2013$Prevalence*(base_summary_q3_2013$RR-1))

base_summary_q3_2013$overall_PAF<-1-(base_summary_q3_2013$comunality*base_summary_q3_2013$PAF)

base_summary_q3_2013$individual_PAF<-base_summary_q3_2013$PAF/
  sum(base_summary_q3_2013$PAF)*(1-prod(base_summary_q3_2013$overall_PAF))

base_summary_q3_2013$"1-PAF"<-1-base_summary_q3_2013$PAF

unique_overall_weigthed_PAF_q3_2013<-1-prod(base_summary_q3_2013$overall_PAF)
unique_overall_PAF_q3_2013<-1-prod(1-base_summary_q3_2013$PAF)

unique_overall_weigthed_PAF_q3_2013
unique_overall_PAF_q3_2013


## 5. Confident intervals===========================

base_summary_q3_2013$individual_PAF_LCI<-NA
base_summary_q3_2013$individual_PAF_UCI<-NA


### 3.1 education ==================================
a<-prop.test((base_summary_q3_2013$individual_PAF[base_summary_q3_2013$RF=="PAF_education"]*n_midlife_q3_2013),
             n_midlife_q3_2013,
             correct=T)
base_summary_q3_2013$individual_PAF_LCI[base_summary_q3_2013$RF=="PAF_education"]<-a$conf.int[1]
base_summary_q3_2013$individual_PAF_UCI[base_summary_q3_2013$RF=="PAF_education"]<-a$conf.int[2]

### 3.2 obesity ==================================
a<-prop.test((base_summary_q3_2013$individual_PAF[base_summary_q3_2013$RF=="PAF_obesity"]*n_midlife_q3_2013),
             n_midlife_q3_2013,
             correct=T)
base_summary_q3_2013$individual_PAF_LCI[base_summary_q3_2013$RF=="PAF_obesity"]<-a$conf.int[1]
base_summary_q3_2013$individual_PAF_UCI[base_summary_q3_2013$RF=="PAF_obesity"]<-a$conf.int[2]

### 3.3 HBP ==================================
a<-prop.test((base_summary_q3_2013$individual_PAF[base_summary_q3_2013$RF=="PAF_HBP"]*n_midlife_q3_2013),
             n_midlife_q3_2013,
             correct=T)
base_summary_q3_2013$individual_PAF_LCI[base_summary_q3_2013$RF=="PAF_HBP"]<-a$conf.int[1]
base_summary_q3_2013$individual_PAF_UCI[base_summary_q3_2013$RF=="PAF_HBP"]<-a$conf.int[2]

### 3.4 Alcohol ==================================
a<-prop.test((base_summary_q3_2013$individual_PAF[base_summary_q3_2013$RF=="PAF_alcohol"]*n_midlife_q3_2013),
             n_midlife_q3_2013,
             correct=T)
base_summary_q3_2013$individual_PAF_LCI[base_summary_q3_2013$RF=="PAF_alcohol"]<-a$conf.int[1]
base_summary_q3_2013$individual_PAF_UCI[base_summary_q3_2013$RF=="PAF_alcohol"]<-a$conf.int[2]

### 3.5 Smoking ==================================
a<-prop.test((base_summary_q3_2013$individual_PAF[base_summary_q3_2013$RF=="PAF_smoking"]*n_latelife_q3_2013),
             n_latelife_q3_2013,
             correct=T)
base_summary_q3_2013$individual_PAF_LCI[base_summary_q3_2013$RF=="PAF_smoking"]<-a$conf.int[1]
base_summary_q3_2013$individual_PAF_UCI[base_summary_q3_2013$RF=="PAF_smoking"]<-a$conf.int[2]

### 3.6 Social isolation ==================================
a<-prop.test((base_summary_q3_2013$individual_PAF[base_summary_q3_2013$RF=="PAF_socialisolation"]*n_latelife_q3_2013),
             n_latelife_q3_2013,
             correct=T)
base_summary_q3_2013$individual_PAF_LCI[base_summary_q3_2013$RF=="PAF_socialisolation"]<-a$conf.int[1]
base_summary_q3_2013$individual_PAF_UCI[base_summary_q3_2013$RF=="PAF_socialisolation"]<-a$conf.int[2]

### 3.7 Physical inactivity ==================================
a<-prop.test((base_summary_q3_2013$individual_PAF[base_summary_q3_2013$RF=="PAF_pinactivity"]*n_latelife_q3_2013),
             n_latelife_q3_2013,
             correct=T)
base_summary_q3_2013$individual_PAF_LCI[base_summary_q3_2013$RF=="PAF_pinactivity"]<-a$conf.int[1]
base_summary_q3_2013$individual_PAF_UCI[base_summary_q3_2013$RF=="PAF_pinactivity"]<-a$conf.int[2]

### 3.8 Diabetes ==================================
a<-prop.test((base_summary_q3_2013$individual_PAF[base_summary_q3_2013$RF=="PAF_DBT"]*n_latelife_q3_2013),
             n_latelife_q3_2013,
             correct=T)
base_summary_q3_2013$individual_PAF_LCI[base_summary_q3_2013$RF=="PAF_DBT"]<-a$conf.int[1]
base_summary_q3_2013$individual_PAF_UCI[base_summary_q3_2013$RF=="PAF_DBT"]<-a$conf.int[2]



#adjusting the table
base_summary_q3_2013$Risk_factor<-c("Less education", "Obesity", "Hypertension", "Alcohol consumption", "Smoking", 
                                    "Social Isolation", "Physical inactivity", "Diabetes")

base_summary_q3_2013 <- base_summary_q3_2013[, c(14,2,3,4,5,6,7,8,9,10,11,12,13,1)]

base_summary_q3_2013 <- base_summary_q3_2013[c(1,3,4,2,5,6,7,8),]


#6. Table==============================================
main_table<-base_summary_q3_2013 %>% select(-RF, -overall_PAF, -`1-PAF`) %>%
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
  tab_header(title = md("PAF for dementia risk factors - q3 "))%>%
  sub_missing(columns = everything(),
              rows = everything(),
              missing_text = "---"
  )
main_table
#7. Calculating de overall weihed PAF with CI==============================

unique_overall_weigthed_PAF_q3_CI_2013<-prop.test(unique_overall_weigthed_PAF_q3_2013*n_total_q3_2013,
                                                  n_total_q3_2013,
                                                  correct=T)


unique_overall_weigthed_PAF_q3_CI_2013

library(PropCIs)
n_latelife_q3_2013+n_midlife_q3_2013
(n_latelife_q3_2013+n_midlife_q3_2013)*0.352345


exactci(5106, 14493,
        conf.level=0.95)


save(base_summary_q3_2013, unique_overall_PAF_q3_2013, unique_overall_weigthed_PAF_q3_2013,
     unique_overall_weigthed_PAF_q3_CI_2013, 
     n_midlife_q3_2013,
     n_latelife_q3_2013,
     n_total_q3_2013, file="tabla_q3_2013.RData")

rm(list = ls())

# 1. FOURTH QUINTILE ==============================

#...............................................................................
#                                                                              .
#  Q4                                                                 .
#                                                                              .
#...............................................................................


# 2. Data origin (risk_factor coded) ==============================

load("base2013_general.RData")

baseq4<-base2013 %>% filter(ITF_UC_QUINTILES==4)

# 3. Comunalities calculation==================

basecom_q4<-baseq4 %>% select(starts_with("PAF"))


library(psych)
## 1. Tetrachoric correlation ==============================
a<-tetrachoric(basecom_q4)                             
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

comunalities_q4<-comunalities %>% select(RF, comunality)

rm(base2013, a, b, eigenVectors, eigenvalues, comunalities)

# 4. PAF table ================================================================


## 1. Survey design =============================================
base_midlife<-baseq4 %>% filter(BHCH05>=45 & BHCH05<=64) 
n_midlife_q4_2013<-nrow(base_midlife)

design_midlife <- svydesign(id=~ID, weights=~PONDERACION, data=base_midlife)

base_latelife<-baseq4 %>% filter(BHCH05>64) 
design_latelife <- svydesign(id=~ID, weights=~PONDERACION, data=base_latelife)
n_latelife_q4_2013<-nrow(base_latelife)

n_total_q4_2013<-n_latelife_q4_2013 + n_midlife_q4_2013

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



## 3. Integrating into table ==================================================

prevalences<-rbind(PAF_education,
                   PAF_obesity, 
                   PAF_HBP, 
                   PAF_alcohol, 
                   PAF_smoking, 
                   PAF_socialisolation, 
                   PAF_pinactivity, 
                   PAF_DBT)


prevalences<-prevalences %>% select(RF, Prevalence, LCI, UCI)

RR<-c(1.6,1.6,1.6,1.2,1.6,1.6,1.4,1.5)


prevalences$RR<-RR

# comunalities<-comunalities %>% rename(RF=risk_factor)

base_summary_q4_2013<-left_join(prevalences, comunalities_q4, by="RF")


## 4. PAF calculation=====================================

base_summary_q4_2013$weigth<-1-base_summary_q4_2013$comunality

base_summary_q4_2013$PAF<-((base_summary_q4_2013$Prevalence*
                              (base_summary_q4_2013$RR-1)))/
  (1+base_summary_q4_2013$Prevalence*(base_summary_q4_2013$RR-1))

base_summary_q4_2013$overall_PAF<-1-(base_summary_q4_2013$comunality*base_summary_q4_2013$PAF)

base_summary_q4_2013$individual_PAF<-base_summary_q4_2013$PAF/
  sum(base_summary_q4_2013$PAF)*(1-prod(base_summary_q4_2013$overall_PAF))

base_summary_q4_2013$"1-PAF"<-1-base_summary_q4_2013$PAF

unique_overall_weigthed_PAF_q4_2013<-1-prod(base_summary_q4_2013$overall_PAF)
unique_overall_PAF_q4_2013<-1-prod(1-base_summary_q4_2013$PAF)

unique_overall_weigthed_PAF_q4_2013
unique_overall_PAF_q4_2013


## 5. Confident intervals===========================

base_summary_q4_2013$individual_PAF_LCI<-NA
base_summary_q4_2013$individual_PAF_UCI<-NA


### 3.1 education ==================================
a<-prop.test((base_summary_q4_2013$individual_PAF[base_summary_q4_2013$RF=="PAF_education"]*n_midlife_q4_2013),
             n_midlife_q4_2013,
             correct=T)
base_summary_q4_2013$individual_PAF_LCI[base_summary_q4_2013$RF=="PAF_education"]<-a$conf.int[1]
base_summary_q4_2013$individual_PAF_UCI[base_summary_q4_2013$RF=="PAF_education"]<-a$conf.int[2]

### 3.2 obesity ==================================
a<-prop.test((base_summary_q4_2013$individual_PAF[base_summary_q4_2013$RF=="PAF_obesity"]*n_midlife_q4_2013),
             n_midlife_q4_2013,
             correct=T)
base_summary_q4_2013$individual_PAF_LCI[base_summary_q4_2013$RF=="PAF_obesity"]<-a$conf.int[1]
base_summary_q4_2013$individual_PAF_UCI[base_summary_q4_2013$RF=="PAF_obesity"]<-a$conf.int[2]

### 3.3 HBP ==================================
a<-prop.test((base_summary_q4_2013$individual_PAF[base_summary_q4_2013$RF=="PAF_HBP"]*n_midlife_q4_2013),
             n_midlife_q4_2013,
             correct=T)
base_summary_q4_2013$individual_PAF_LCI[base_summary_q4_2013$RF=="PAF_HBP"]<-a$conf.int[1]
base_summary_q4_2013$individual_PAF_UCI[base_summary_q4_2013$RF=="PAF_HBP"]<-a$conf.int[2]

### 3.4 Alcohol ==================================
a<-prop.test((base_summary_q4_2013$individual_PAF[base_summary_q4_2013$RF=="PAF_alcohol"]*n_midlife_q4_2013),
             n_midlife_q4_2013,
             correct=T)
base_summary_q4_2013$individual_PAF_LCI[base_summary_q4_2013$RF=="PAF_alcohol"]<-a$conf.int[1]
base_summary_q4_2013$individual_PAF_UCI[base_summary_q4_2013$RF=="PAF_alcohol"]<-a$conf.int[2]

### 3.5 Smoking ==================================
a<-prop.test((base_summary_q4_2013$individual_PAF[base_summary_q4_2013$RF=="PAF_smoking"]*n_latelife_q4_2013),
             n_latelife_q4_2013,
             correct=T)
base_summary_q4_2013$individual_PAF_LCI[base_summary_q4_2013$RF=="PAF_smoking"]<-a$conf.int[1]
base_summary_q4_2013$individual_PAF_UCI[base_summary_q4_2013$RF=="PAF_smoking"]<-a$conf.int[2]

### 3.6 Social isolation ==================================
a<-prop.test((base_summary_q4_2013$individual_PAF[base_summary_q4_2013$RF=="PAF_socialisolation"]*n_latelife_q4_2013),
             n_latelife_q4_2013,
             correct=T)
base_summary_q4_2013$individual_PAF_LCI[base_summary_q4_2013$RF=="PAF_socialisolation"]<-a$conf.int[1]
base_summary_q4_2013$individual_PAF_UCI[base_summary_q4_2013$RF=="PAF_socialisolation"]<-a$conf.int[2]

### 3.7 Physical inactivity ==================================
a<-prop.test((base_summary_q4_2013$individual_PAF[base_summary_q4_2013$RF=="PAF_pinactivity"]*n_latelife_q4_2013),
             n_latelife_q4_2013,
             correct=T)
base_summary_q4_2013$individual_PAF_LCI[base_summary_q4_2013$RF=="PAF_pinactivity"]<-a$conf.int[1]
base_summary_q4_2013$individual_PAF_UCI[base_summary_q4_2013$RF=="PAF_pinactivity"]<-a$conf.int[2]

### 3.8 Diabetes ==================================
a<-prop.test((base_summary_q4_2013$individual_PAF[base_summary_q4_2013$RF=="PAF_DBT"]*n_latelife_q4_2013),
             n_latelife_q4_2013,
             correct=T)
base_summary_q4_2013$individual_PAF_LCI[base_summary_q4_2013$RF=="PAF_DBT"]<-a$conf.int[1]
base_summary_q4_2013$individual_PAF_UCI[base_summary_q4_2013$RF=="PAF_DBT"]<-a$conf.int[2]



#adjusting the table
base_summary_q4_2013$Risk_factor<-c("Less education", "Obesity", "Hypertension", "Alcohol consumption", "Smoking", 
                                    "Social Isolation", "Physical inactivity", "Diabetes")

base_summary_q4_2013 <- base_summary_q4_2013[, c(14,2,3,4,5,6,7,8,9,10,11,12,13,1)]

base_summary_q4_2013 <- base_summary_q4_2013[c(1,3,4,2,5,6,7,8),]


#6. Table==============================================
main_table<-base_summary_q4_2013 %>% select(-RF, -overall_PAF, -`1-PAF`) %>%
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
  tab_header(title = md("PAF for dementia risk factors - q4 "))%>%
  sub_missing(columns = everything(),
              rows = everything(),
              missing_text = "---"
  )
main_table
#7. Calculating de overall weihed PAF with CI==============================

unique_overall_weigthed_PAF_q4_CI_2013<-prop.test(unique_overall_weigthed_PAF_q4_2013*n_total_q4_2013,
                                                  n_total_q4_2013,
                                                  correct=T)


unique_overall_weigthed_PAF_q4_CI_2013

library(PropCIs)
n_latelife_q4_2013+n_midlife_q4_2013
(n_latelife_q4_2013+n_midlife_q4_2013)*0.352345


exactci(5106, 14493,
        conf.level=0.95)


save(base_summary_q4_2013, unique_overall_PAF_q4_2013, unique_overall_weigthed_PAF_q4_2013,
     unique_overall_weigthed_PAF_q4_CI_2013, 
     n_midlife_q4_2013,
     n_latelife_q4_2013,
     n_total_q4_2013, file="tabla_q4_2013.RData")

rm(list = ls())

# 1. FIFTH QUINTILE ==============================

#...............................................................................
#                                                                              .
#  Q5                                                                .
#                                                                              .
#...............................................................................


# 2. Data origin (risk_factor coded) ==============================

load("base2013_general.RData")

baseq5<-base2013 %>% filter(ITF_UC_QUINTILES==5)

# 3. Comunalities calculation==================

basecom_q5<-baseq5 %>% select(starts_with("PAF"))


library(psych)
## 1. Tetrachoric correlation ==============================
a<-tetrachoric(basecom_q5)                             
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

comunalities_q5<-comunalities %>% select(RF, comunality)

rm(base2013, a, b, eigenVectors, eigenvalues, comunalities)

# 4. PAF table ================================================================


## 1. Survey design =============================================
base_midlife<-baseq5 %>% filter(BHCH05>=45 & BHCH05<=64) 
n_midlife_q5_2013<-nrow(base_midlife)

design_midlife <- svydesign(id=~ID, weights=~PONDERACION, data=base_midlife)

base_latelife<-baseq5 %>% filter(BHCH05>64) 
design_latelife <- svydesign(id=~ID, weights=~PONDERACION, data=base_latelife)
n_latelife_q5_2013<-nrow(base_latelife)

n_total_q5_2013<-n_latelife_q5_2013 + n_midlife_q5_2013

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



## 3. Integrating into table ==================================================

prevalences<-rbind(PAF_education,
                   PAF_obesity, 
                   PAF_HBP, 
                   PAF_alcohol, 
                   PAF_smoking, 
                   PAF_socialisolation, 
                   PAF_pinactivity, 
                   PAF_DBT)


prevalences<-prevalences %>% select(RF, Prevalence, LCI, UCI)

RR<-c(1.6,1.6,1.6,1.2,1.6,1.6,1.4,1.5)


prevalences$RR<-RR

# comunalities<-comunalities %>% rename(RF=risk_factor)

base_summary_q5_2013<-left_join(prevalences, comunalities_q5, by="RF")


## 4. PAF calculation=====================================

base_summary_q5_2013$weigth<-1-base_summary_q5_2013$comunality

base_summary_q5_2013$PAF<-((base_summary_q5_2013$Prevalence*
                              (base_summary_q5_2013$RR-1)))/
  (1+base_summary_q5_2013$Prevalence*(base_summary_q5_2013$RR-1))

base_summary_q5_2013$overall_PAF<-1-(base_summary_q5_2013$comunality*base_summary_q5_2013$PAF)

base_summary_q5_2013$individual_PAF<-base_summary_q5_2013$PAF/
  sum(base_summary_q5_2013$PAF)*(1-prod(base_summary_q5_2013$overall_PAF))

base_summary_q5_2013$"1-PAF"<-1-base_summary_q5_2013$PAF

unique_overall_weigthed_PAF_q5_2013<-1-prod(base_summary_q5_2013$overall_PAF)
unique_overall_PAF_q5_2013<-1-prod(1-base_summary_q5_2013$PAF)

unique_overall_weigthed_PAF_q5_2013
unique_overall_PAF_q5_2013


## 5. Confident intervals===========================

base_summary_q5_2013$individual_PAF_LCI<-NA
base_summary_q5_2013$individual_PAF_UCI<-NA


### 3.1 education ==================================
a<-prop.test((base_summary_q5_2013$individual_PAF[base_summary_q5_2013$RF=="PAF_education"]*n_midlife_q5_2013),
             n_midlife_q5_2013,
             correct=T)
base_summary_q5_2013$individual_PAF_LCI[base_summary_q5_2013$RF=="PAF_education"]<-a$conf.int[1]
base_summary_q5_2013$individual_PAF_UCI[base_summary_q5_2013$RF=="PAF_education"]<-a$conf.int[2]

### 3.2 obesity ==================================
a<-prop.test((base_summary_q5_2013$individual_PAF[base_summary_q5_2013$RF=="PAF_obesity"]*n_midlife_q5_2013),
             n_midlife_q5_2013,
             correct=T)
base_summary_q5_2013$individual_PAF_LCI[base_summary_q5_2013$RF=="PAF_obesity"]<-a$conf.int[1]
base_summary_q5_2013$individual_PAF_UCI[base_summary_q5_2013$RF=="PAF_obesity"]<-a$conf.int[2]

### 3.3 HBP ==================================
a<-prop.test((base_summary_q5_2013$individual_PAF[base_summary_q5_2013$RF=="PAF_HBP"]*n_midlife_q5_2013),
             n_midlife_q5_2013,
             correct=T)
base_summary_q5_2013$individual_PAF_LCI[base_summary_q5_2013$RF=="PAF_HBP"]<-a$conf.int[1]
base_summary_q5_2013$individual_PAF_UCI[base_summary_q5_2013$RF=="PAF_HBP"]<-a$conf.int[2]

### 3.4 Alcohol ==================================
a<-prop.test((base_summary_q5_2013$individual_PAF[base_summary_q5_2013$RF=="PAF_alcohol"]*n_midlife_q5_2013),
             n_midlife_q5_2013,
             correct=T)
base_summary_q5_2013$individual_PAF_LCI[base_summary_q5_2013$RF=="PAF_alcohol"]<-a$conf.int[1]
base_summary_q5_2013$individual_PAF_UCI[base_summary_q5_2013$RF=="PAF_alcohol"]<-a$conf.int[2]

### 3.5 Smoking ==================================
a<-prop.test((base_summary_q5_2013$individual_PAF[base_summary_q5_2013$RF=="PAF_smoking"]*n_latelife_q5_2013),
             n_latelife_q5_2013,
             correct=T)
base_summary_q5_2013$individual_PAF_LCI[base_summary_q5_2013$RF=="PAF_smoking"]<-a$conf.int[1]
base_summary_q5_2013$individual_PAF_UCI[base_summary_q5_2013$RF=="PAF_smoking"]<-a$conf.int[2]

### 3.6 Social isolation ==================================
a<-prop.test((base_summary_q5_2013$individual_PAF[base_summary_q5_2013$RF=="PAF_socialisolation"]*n_latelife_q5_2013),
             n_latelife_q5_2013,
             correct=T)
base_summary_q5_2013$individual_PAF_LCI[base_summary_q5_2013$RF=="PAF_socialisolation"]<-a$conf.int[1]
base_summary_q5_2013$individual_PAF_UCI[base_summary_q5_2013$RF=="PAF_socialisolation"]<-a$conf.int[2]

### 3.7 Physical inactivity ==================================
a<-prop.test((base_summary_q5_2013$individual_PAF[base_summary_q5_2013$RF=="PAF_pinactivity"]*n_latelife_q5_2013),
             n_latelife_q5_2013,
             correct=T)
base_summary_q5_2013$individual_PAF_LCI[base_summary_q5_2013$RF=="PAF_pinactivity"]<-a$conf.int[1]
base_summary_q5_2013$individual_PAF_UCI[base_summary_q5_2013$RF=="PAF_pinactivity"]<-a$conf.int[2]

### 3.8 Diabetes ==================================
a<-prop.test((base_summary_q5_2013$individual_PAF[base_summary_q5_2013$RF=="PAF_DBT"]*n_latelife_q5_2013),
             n_latelife_q5_2013,
             correct=T)
base_summary_q5_2013$individual_PAF_LCI[base_summary_q5_2013$RF=="PAF_DBT"]<-a$conf.int[1]
base_summary_q5_2013$individual_PAF_UCI[base_summary_q5_2013$RF=="PAF_DBT"]<-a$conf.int[2]



#adjusting the table
base_summary_q5_2013$Risk_factor<-c("Less education", "Obesity", "Hypertension", "Alcohol consumption", "Smoking", 
                                    "Social Isolation", "Physical inactivity", "Diabetes")

base_summary_q5_2013 <- base_summary_q5_2013[, c(14,2,3,4,5,6,7,8,9,10,11,12,13,1)]

base_summary_q5_2013 <- base_summary_q5_2013[c(1,3,4,2,5,6,7,8),]


#6. Table==============================================
main_table<-base_summary_q5_2013 %>% select(-RF, -overall_PAF, -`1-PAF`) %>%
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
  tab_header(title = md("PAF for dementia risk factors - q5 "))%>%
  sub_missing(columns = everything(),
              rows = everything(),
              missing_text = "---"
  )
main_table
#7. Calculating de overall weihed PAF with CI==============================

unique_overall_weigthed_PAF_q5_CI_2013<-prop.test(unique_overall_weigthed_PAF_q5_2013*n_total_q5_2013,
                                                  n_total_q5_2013,
                                                  correct=T)


unique_overall_weigthed_PAF_q5_CI_2013

library(PropCIs)
n_latelife_q5_2013+n_midlife_q5_2013
(n_latelife_q5_2013+n_midlife_q5_2013)*0.352345


exactci(5106, 14493,
        conf.level=0.95)


save(base_summary_q5_2013, unique_overall_PAF_q5_2013, unique_overall_weigthed_PAF_q5_2013,
     unique_overall_weigthed_PAF_q5_CI_2013, 
     n_midlife_q5_2013,
     n_latelife_q5_2013,
     n_total_q5_2013, file="tabla_q5_2013.RData")

rm(list = ls())

