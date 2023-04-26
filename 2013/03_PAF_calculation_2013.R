library(survey)
library(dplyr)
library(gt)
load("base2013_general.RData")
# 1. Survey design =============================================

base_midlife<-base2013 %>% filter(BHCH05>=45 & BHCH05<=64) 
design_midlife <- svydesign(id=~ID, weights=~PONDERACION, data=base_midlife)


base_latelife<-base2013 %>% filter(BHCH05>64) 
design_latelife <- svydesign(id=~ID, weights=~PONDERACION, data=base_latelife)


n_midlife_2013<-nrow(base_midlife)
n_latelife_2013<-nrow(base_latelife)

n_total_2013<-n_latelife_2013 + n_midlife_2013



# 2. Calculate prevalence =============================================

# early-life factors (age <45 years)
##less education

# midlife (age 45–64 years), 
##Hearing loss
##Traumatic BI
## HBP "done"
## Alcohol "done"
## Obesity "done"

# later-life (age ≥65 years) risk factors

##Smoking "done"
##Depression
##social isolation "done"
##physical inactivity "done"
## air pollution
## diabetes

## 2.1 Midlife factors =============================================
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

## 2.2 Later-life factors =============================================

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

base_summary_2013<-left_join(prevalences, comunalities, by="RF")


#calculo peso
base_summary_2013$weigth<-1-base_summary_2013$comunality

#calculo PAF crudo
base_summary_2013$PAF<-((base_summary_2013$Prevalence*
                           (base_summary_2013$RR-1)))/
  (1+base_summary_2013$Prevalence*(base_summary_2013$RR-1))

#calculo de la columna para calcular el overall PAF
base_summary_2013$overall_PAF<-1-(base_summary_2013$comunality*base_summary_2013$PAF)

base_summary_2013$individual_PAF<-base_summary_2013$PAF/
  sum(base_summary_2013$PAF)*(1-prod(base_summary_2013$overall_PAF))

base_summary_2013$"1-PAF"<-1-base_summary_2013$PAF

unique_overall_weigthed_PAF_2013<-1-prod(base_summary_2013$overall_PAF)
unique_overall_PAF_2013<-1-prod(1-base_summary_2013$PAF)

unique_overall_weigthed_PAF
unique_overall_PAF

unique_overall_weigthed_PAF_2013_CI<-prop.test(unique_overall_weigthed_PAF_2013*n_total_2013,
                                          n_total_2013,
                                          correct=T)



## 3. Confident intervals===========================

base_summary_2013$individual_PAF_LCI<-c()
base_summary_2013$individual_PAF_UCI<-c()


### 3.1 education ==================================
a<-prop.test((base_summary_2013$individual_PAF[base_summary_2013$RF=="PAF_education"]*n_midlife_2013),
             n_midlife_2013,
             correct=T)
base_summary_2013$individual_PAF_LCI[base_summary_2013$RF=="PAF_education"]<-a$conf.int[1]
base_summary_2013$individual_PAF_UCI[base_summary_2013$RF=="PAF_education"]<-a$conf.int[2]

### 3.2 obesity ==================================
a<-prop.test((base_summary_2013$individual_PAF[base_summary_2013$RF=="PAF_obesity"]*n_midlife_2013),
             n_midlife_2013,
             correct=T)
base_summary_2013$individual_PAF_LCI[base_summary_2013$RF=="PAF_obesity"]<-a$conf.int[1]
base_summary_2013$individual_PAF_UCI[base_summary_2013$RF=="PAF_obesity"]<-a$conf.int[2]

### 3.3 HBP ==================================
a<-prop.test((base_summary_2013$individual_PAF[base_summary_2013$RF=="PAF_HBP"]*n_midlife_2013),
             n_midlife_2013,
             correct=T)
base_summary_2013$individual_PAF_LCI[base_summary_2013$RF=="PAF_HBP"]<-a$conf.int[1]
base_summary_2013$individual_PAF_UCI[base_summary_2013$RF=="PAF_HBP"]<-a$conf.int[2]

### 3.4 Alcohol ==================================
a<-prop.test((base_summary_2013$individual_PAF[base_summary_2013$RF=="PAF_alcohol"]*n_midlife_2013),
             n_midlife_2013,
             correct=T)
base_summary_2013$individual_PAF_LCI[base_summary_2013$RF=="PAF_alcohol"]<-a$conf.int[1]
base_summary_2013$individual_PAF_UCI[base_summary_2013$RF=="PAF_alcohol"]<-a$conf.int[2]

### 3.5 Smoking ==================================
a<-prop.test((base_summary_2013$individual_PAF[base_summary_2013$RF=="PAF_smoking"]*n_latelife_2013),
             n_latelife_2013,
             correct=T)
base_summary_2013$individual_PAF_LCI[base_summary_2013$RF=="PAF_smoking"]<-a$conf.int[1]
base_summary_2013$individual_PAF_UCI[base_summary_2013$RF=="PAF_smoking"]<-a$conf.int[2]

### 3.6 Social isolation ==================================
a<-prop.test((base_summary_2013$individual_PAF[base_summary_2013$RF=="PAF_socialisolation"]*n_latelife_2013),
             n_latelife_2013,
             correct=T)
base_summary_2013$individual_PAF_LCI[base_summary_2013$RF=="PAF_socialisolation"]<-a$conf.int[1]
base_summary_2013$individual_PAF_UCI[base_summary_2013$RF=="PAF_socialisolation"]<-a$conf.int[2]

### 3.7 Physical inactivity ==================================
a<-prop.test((base_summary_2013$individual_PAF[base_summary_2013$RF=="PAF_pinactivity"]*n_latelife_2013),
             n_latelife_2013,
             correct=T)
base_summary_2013$individual_PAF_LCI[base_summary_2013$RF=="PAF_pinactivity"]<-a$conf.int[1]
base_summary_2013$individual_PAF_UCI[base_summary_2013$RF=="PAF_pinactivity"]<-a$conf.int[2]

### 3.8 Diabetes ==================================
a<-prop.test((base_summary_2013$individual_PAF[base_summary_2013$RF=="PAF_DBT"]*n_latelife_2013),
             n_latelife_2013,
             correct=T)
base_summary_2013$individual_PAF_LCI[base_summary_2013$RF=="PAF_DBT"]<-a$conf.int[1]
base_summary_2013$individual_PAF_UCI[base_summary_2013$RF=="PAF_DBT"]<-a$conf.int[2]



base_summary_2013$Risk_factor<-c("Less education", "Obesity", "Hypertension", "Alcohol consumption", "Smoking", "Social Isolation", "Physical inactivity", "Diabetes")

base_summary_2013 <- base_summary_2013[, c(14,2,3,4,5,6,7,8,9,10,11,12,13,1)]

base_summary_2013 <- base_summary_2013[c(1,3,2,4,5,6,7,8),]


base_summary_2013 %>% select(-RF, -overall_PAF, -`1-PAF`) %>%
  gt() %>%
  fmt_percent(
    columns = c(2:4,8:11),
    decimals = 1
  )  %>%
  fmt_number(
    columns = 5:9,
    decimals = 1)%>%
  tab_spanner(
    label = "RF prevalence",
    columns = c(Prevalence, LCI, UCI))  %>%
  tab_spanner(
    label = "weigthed PAF",
    columns = c(individual_PAF, individual_PAF_LCI, individual_PAF_UCI))  %>%
  tab_header(
    title = md("PAF for dementia risk factors - 2013 "))




unique_overall_weigthed_PAF_CI<-prop.test(unique_overall_weigthed_PAF*n_total_2013,
                                          n_total_2013,
                                          correct=T)



save(base_summary_2013, unique_overall_PAF_2013, unique_overall_weigthed_PAF_2013, unique_overall_weigthed_PAF_2013_CI, n_midlife_2013, n_latelife_2013, n_total_2013, 
     unique_overall_weigthed_PAF_2013_CI,file="tabla_2013.RData")



