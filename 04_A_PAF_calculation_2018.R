library(survey)
library(dplyr)
library(gt)
load("base2018_general.RData")
# 1. Survey design =============================================
base_midlife<-base2018 %>% filter(bhch04>=45 & bhch04<=64) 
n_midlife_2018<-nrow(base_midlife)

design_midlife <- svydesign(id=~id, weights=~wf1p, data=base_midlife)

base_latelife<-base2018 %>% filter(bhch04>64) 
design_latelife <- svydesign(id=~id, weights=~wf1p, data=base_latelife)
n_latelife_2018<-nrow(base_latelife)

n_total_2018<-n_latelife_2018 + n_midlife_2018

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

PAF_air<-as.data.frame(svytable(~PAF_air, design = design_latelife))
PAF_air<-PAF_air %>% pivot_wider(names_from = PAF_air, values_from =Freq)
PAF_air$RF<-"PAF_air"
PAF_air$Prevalence<-PAF_air$`1`/(PAF_air$`1`+PAF_air$`0`)
a<-svyciprop(~PAF_air==1, design_latelife, method="li")
PAF_air$LCI<-attr(a,"ci")[1]
PAF_air$UCI<-attr(a,"ci")[2]


# 3. Integrating into table ==================================================

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

base_summary_2018<-left_join(prevalences, comunalities, by="RF")

rm("PAF_air", "PAF_alcohol", "PAF_DBT", "PAF_education", "PAF_HBP", "PAF_obesity", "PAF_pinactivity", "PAF_smoking", "PAF_socialisolation")

#rm(list=setdiff(ls(), "base_summary_2018"))

# me traigo los nuevos factores
load("other_risk_factor.RData")

# 4. PAF calculation=====================================

#uso la media de las comunalidades para cargar la "comunalidad que no tenemos"
PAF_other$comunality<-mean(base_summary_2018$comunality)

#junto todo
base_summary_2018<-rbind(base_summary_2018,PAF_other)

#calculo peso
base_summary_2018$weigth<-1-base_summary_2018$comunality

#calculo PAF crudo
base_summary_2018$PAF<-((base_summary_2018$Prevalence*
                           (base_summary_2018$RR-1)))/
  (1+base_summary_2018$Prevalence*(base_summary_2018$RR-1))

#calculo de la columna para calcular el overall PAF
base_summary_2018$overall_PAF<-1-(base_summary_2018$comunality*base_summary_2018$PAF)

base_summary_2018$individual_PAF<-base_summary_2018$PAF/
  sum(base_summary_2018$PAF)*(1-prod(base_summary_2018$overall_PAF))

base_summary_2018$"1-PAF"<-1-base_summary_2018$PAF

unique_overall_weigthed_PAF<-1-prod(base_summary_2018$overall_PAF)
unique_overall_PAF<-1-prod(1-base_summary_2018$PAF)

unique_overall_weigthed_PAF
unique_overall_PAF


# 5. Confident intervals===========================

base_summary_2018$individual_PAF_LCI<-NA
base_summary_2018$individual_PAF_UCI<-NA


### 3.1 education ==================================
a<-prop.test((base_summary_2018$individual_PAF[base_summary_2018$RF=="PAF_education"]*n_midlife_2018),
             n_midlife_2018,
             correct=T)
base_summary_2018$individual_PAF_LCI[base_summary_2018$RF=="PAF_education"]<-a$conf.int[1]
base_summary_2018$individual_PAF_UCI[base_summary_2018$RF=="PAF_education"]<-a$conf.int[2]

### 3.2 obesity ==================================
a<-prop.test((base_summary_2018$individual_PAF[base_summary_2018$RF=="PAF_obesity"]*n_midlife_2018),
             n_midlife_2018,
             correct=T)
base_summary_2018$individual_PAF_LCI[base_summary_2018$RF=="PAF_obesity"]<-a$conf.int[1]
base_summary_2018$individual_PAF_UCI[base_summary_2018$RF=="PAF_obesity"]<-a$conf.int[2]

### 3.3 HBP ==================================
a<-prop.test((base_summary_2018$individual_PAF[base_summary_2018$RF=="PAF_HBP"]*n_midlife_2018),
             n_midlife_2018,
             correct=T)
base_summary_2018$individual_PAF_LCI[base_summary_2018$RF=="PAF_HBP"]<-a$conf.int[1]
base_summary_2018$individual_PAF_UCI[base_summary_2018$RF=="PAF_HBP"]<-a$conf.int[2]

### 3.4 Alcohol ==================================
a<-prop.test((base_summary_2018$individual_PAF[base_summary_2018$RF=="PAF_alcohol"]*n_midlife_2018),
             n_midlife_2018,
             correct=T)
base_summary_2018$individual_PAF_LCI[base_summary_2018$RF=="PAF_alcohol"]<-a$conf.int[1]
base_summary_2018$individual_PAF_UCI[base_summary_2018$RF=="PAF_alcohol"]<-a$conf.int[2]

### 3.5 Smoking ==================================
a<-prop.test((base_summary_2018$individual_PAF[base_summary_2018$RF=="PAF_smoking"]*n_latelife_2018),
             n_latelife_2018,
             correct=T)
base_summary_2018$individual_PAF_LCI[base_summary_2018$RF=="PAF_smoking"]<-a$conf.int[1]
base_summary_2018$individual_PAF_UCI[base_summary_2018$RF=="PAF_smoking"]<-a$conf.int[2]

### 3.6 Social isolation ==================================
a<-prop.test((base_summary_2018$individual_PAF[base_summary_2018$RF=="PAF_socialisolation"]*n_latelife_2018),
             n_latelife_2018,
             correct=T)
base_summary_2018$individual_PAF_LCI[base_summary_2018$RF=="PAF_socialisolation"]<-a$conf.int[1]
base_summary_2018$individual_PAF_UCI[base_summary_2018$RF=="PAF_socialisolation"]<-a$conf.int[2]

### 3.7 Physical inactivity ==================================
a<-prop.test((base_summary_2018$individual_PAF[base_summary_2018$RF=="PAF_pinactivity"]*n_latelife_2018),
             n_latelife_2018,
             correct=T)
base_summary_2018$individual_PAF_LCI[base_summary_2018$RF=="PAF_pinactivity"]<-a$conf.int[1]
base_summary_2018$individual_PAF_UCI[base_summary_2018$RF=="PAF_pinactivity"]<-a$conf.int[2]

### 3.8 Diabetes ==================================
a<-prop.test((base_summary_2018$individual_PAF[base_summary_2018$RF=="PAF_DBT"]*n_latelife_2018),
             n_latelife_2018,
             correct=T)
base_summary_2018$individual_PAF_LCI[base_summary_2018$RF=="PAF_DBT"]<-a$conf.int[1]
base_summary_2018$individual_PAF_UCI[base_summary_2018$RF=="PAF_DBT"]<-a$conf.int[2]

### 3.9 Air pollution ==================================
a<-prop.test((base_summary_2018$individual_PAF[base_summary_2018$RF=="PAF_air"]*n_latelife_2018),
             n_latelife_2018,
             correct=T)
base_summary_2018$individual_PAF_LCI[base_summary_2018$RF=="PAF_air"]<-a$conf.int[1]
base_summary_2018$individual_PAF_UCI[base_summary_2018$RF=="PAF_air"]<-a$conf.int[2]


base_summary_2018$RF

#adjusting the table
base_summary_2018$Risk_factor<-c("Less education", "Obesity", "Hypertension", "Alcohol consumption", "Smoking", 
                                 "Social Isolation", "Physical inactivity", "Diabetes",
                                 "Air pollution", "Depression", "Hearing loss", "TBI" )

base_summary_2018 <- base_summary_2018[, c(14,2,3,4,5,6,7,8,9,10,11,12,13,1)]

base_summary_2018 <- base_summary_2018[c(1,11,12,3,4,2,5,10,6,7,8,9),]


#6. Table==============================================

base_summary_2018$group<-c("Early life", rep("Midlife",5), rep("Latter life",6))


main_table<-base_summary_2018 %>% select(-RF, -overall_PAF, -`1-PAF`) %>%
  gt(groupname_col = "group") %>%  
  fmt_number(
    columns = 5:8,
    decimals = 1)%>%
  fmt_percent(
    columns = 8:11,
    decimals = 1)  %>%
  fmt_percent(
    columns = 2:4,
    decimals = 1
  )  %>%
  tab_spanner(
    label = "RF prevalence",
    columns = c(Prevalence, LCI, UCI))  %>%
  tab_spanner(
    label = "weigthed PAF",
    columns = c(individual_PAF, individual_PAF_LCI, individual_PAF_UCI))  %>%
  tab_header(title = md("Population attributable fraction for dementia risk factors - 2018 "))%>%
  tab_footnote(
    footnote = "Data from `Estudio Nacional sobre el Perfil de las Personas con Discapacidad` 2018",
    locations = cells_body(
      columns = c(1),
      rows = 2))%>%
  tab_footnote(
    footnote = "Data from Livingston,2020",
    locations = cells_body(
      columns = c(1),
      rows = 3))%>%
  tab_footnote(
    footnote = "Data from Cía,2018",
    locations = cells_body(
      columns = c(1),
      rows = 8))%>%
  sub_missing(columns = everything(),
        rows = everything(),
        missing_text = "---"
      )

main_table

gt::gtsave(main_table, file = "Draft/table1.rtf")



#7. Calculating de overall weihed PAF with CI==============================

unique_overall_weigthed_PAF_CI<-prop.test(unique_overall_weigthed_PAF*n_total_2018,
                                          n_total_2018,
                                          correct=T)


unique_overall_weigthed_PAF_CI


save(base_summary_2018, unique_overall_PAF, unique_overall_weigthed_PAF,
     unique_overall_weigthed_PAF_CI, 
     n_midlife_2018,
     n_latelife_2018,
     n_total_2018, file="tabla_2018.RData")

unique_overall_weigthed_PAF*100
unique_overall_weigthed_PAF_CI


rm(list = ls())

