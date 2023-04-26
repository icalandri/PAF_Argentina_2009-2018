load("tabla_fem.RData")
rm(unique_overall_weigthed_PAF_fem_CI)
load("tabla_masc.RData")
rm(unique_overall_weigthed_PAF_masc_CI)
load("tabla_masc_2013.RData")
rm(unique_overall_weigthed_PAF_masc_CI_2013)
load("tabla_fem_2013.RData")
rm(unique_overall_weigthed_PAF_fem_CI_2013)
load("tabla_masc_2009.RData")
rm(unique_overall_weigthed_PAF_masc_CI_2009)
load("tabla_fem_2009.RData")
rm(unique_overall_weigthed_PAF_fem_CI_2009)



library(tidyverse)
library(reshape2)

### 2. Grafico =======================================================
base_summary_fem$sex<-"female"
base_summary_fem_2013$sex<-"female"
base_summary_fem_2009$sex<-"female"
base_summary_masc$sex<-"male"
base_summary_masc_2013$sex<-"male"
base_summary_masc_2009$sex<-"male"


base_summary_fem$year<-2018
base_summary_fem_2013$year<-2013
base_summary_fem_2009$year<-2009
base_summary_masc$year<-2018
base_summary_masc_2013$year<-2013
base_summary_masc_2009$year<-2009

base_summary_sex<-rbind(base_summary_fem, base_summary_masc,base_summary_fem_2013, base_summary_masc_2013, base_summary_fem_2009, base_summary_masc_2009)


basesita<-base_summary_sex %>% select(RF,individual_PAF,sex,year)

basesita$individual_PAF<-basesita$individual_PAF*100

a<-melt(basesita, id.vars = c("RF", "sex", "year"))


basesita<-base_summary_sex %>% select(RF,individual_PAF_LCI,sex, year)

basesita$individual_PAF_LCI<-basesita$individual_PAF_LCI*100

b<-melt(basesita, id.vars = c("RF", "sex", "year"))

a$LCI<-b$value

basesita<-base_summary_sex %>% select(RF,individual_PAF_UCI,sex, year)

basesita$individual_PAF_UCI<-basesita$individual_PAF_UCI*100

b<-melt(basesita, id.vars = c("RF", "sex", "year"))

a$UCI<-b$value


a$RiskFactor<-factor(a$RF,
                     levels =c("PAF_education", "PAF_HBP","PAF_alcohol" ,"PAF_obesity", "PAF_smoking","PAF_pinactivity", "PAF_socialisolation", "PAF_DBT", "PAF_air" ),
                     labels = c("Less education", "Hypertension", "Alcohol", "Obesity", "Smoking", "Physical inactivity", "Social isolation", "Diabetes", "Air pollution"))

palette_fleni<-c("#a90123","#004385","#fba31b","#578001", "grey")


a %>% filter(RiskFactor =="Less education" |
               RiskFactor =="Hypertension" |
               RiskFactor =="Alcohol" |
               RiskFactor =="Obesity") %>%
  ggplot()+
  scale_fill_manual(values = palette_fleni)+
  facet_grid(. ~ RiskFactor)+
  geom_col(aes(x=year, y=value, fill=sex),
           position = position_dodge(width = 1.8),
           show.legend = TRUE,
           alpha = 1)+
  scale_x_continuous(breaks = c(2009, 2013, 2018))+
  geom_errorbar( aes(x=year, ymin=LCI, ymax=UCI, color=sex), position =position_dodge(width = 1.8),
                 width=0.4, alpha=0.9, size=.3)+
  labs(y ="PAF (%)")+
  theme(legend.position = "none")



a %>% filter(RiskFactor !="Less education" &
               RiskFactor !="Hypertension" &
               RiskFactor !="Alcohol" &
               RiskFactor !="Obesity"&
               RiskFactor !="Air pollution") %>%
  ggplot()+
  scale_fill_manual(values = palette_fleni)+
  facet_grid(. ~ RiskFactor)+
  geom_col(aes(x=year, y=value, fill=sex),
           position = position_dodge(width = 1.8),
           show.legend = TRUE,
           alpha = 1)+
  scale_x_continuous(breaks = c(2009, 2013, 2018))+
  geom_errorbar( aes(x=year, ymin=LCI, ymax=UCI, color=sex), position =position_dodge(width = 1.8),
                 width=0.4, alpha=0.9, size=.3)+
  labs(y ="PAF (%)")+
  theme(legend.position = "none")

# tablas anexas ================================================================================

## Femenino ====================================================

base_test<-base_summary_fem %>% select(Risk_factor,individual_PAF, individual_PAF_LCI, individual_PAF_UCI) %>% rename(paf_fem_2018=individual_PAF,
                                                                                                                lci_fem_2018=individual_PAF_LCI,uci_fem_2018=individual_PAF_UCI)
base_test<-base_test[1:8,]
                                                                                                                                                                                                                                      
base_test$n_fem_2018<-c(rep(n_midlife_fem,4), rep(n_latelife_fem,4))

base_test2<-base_summary_fem_2013 %>% select(individual_PAF, individual_PAF_LCI, individual_PAF_UCI) %>% rename(paf_fem_2013=individual_PAF,
                                                                                                                      lci_fem_2013=individual_PAF_LCI,
                                                                                                                      uci_fem_2013=individual_PAF_UCI)
base_test2$n_fem_2013<-c(rep(n_midlife_fem_2013,4), rep(n_latelife_fem_2013,4))


base_test3<-base_summary_fem_2009 %>% select(individual_PAF, individual_PAF_LCI, individual_PAF_UCI) %>% rename(paf_fem_2009=individual_PAF,
                                                                                                                           lci_fem_2009=individual_PAF_LCI,
                                                                                                                           uci_fem_2009=individual_PAF_UCI)
base_test3$n_fem_2009<-c(rep(n_midlife_fem_2009,4), rep(n_latelife_fem_2009,4))


base_test<-cbind(base_test,base_test2,base_test3)

rm(base_test3,base_test2)

#=================================================
proportion_test <- data.frame(Risk_factor = character(),
                              p_value = numeric())
for (i in 1:nrow(base_test)) {
  cat("Risk Factor:", base_test$Risk_factor[i], "\n")
  
  # Perform the proportion test for the current risk factor
  prop_test <- prop.test(x = c(base_test$paf_fem_2018[i]*base_test$n_fem_2018[i], base_test$paf_fem_2013[i]*base_test$n_fem_2013[i],base_test$paf_fem_2009[i]*base_test$n_fem_2009[i]), 
                         n = c(base_test$n_fem_2018[i], base_test$n_fem_2013[i],base_test$n_fem_2009[i]),
                         correct = FALSE)
  proportion_test[i, "Risk_factor"] <- base_test$Risk_factor[i]
  proportion_test[i, "p_value"] <- prop_test$p.value
  
  cat("p-value:", proportion_test[i, "p_value"], "\n\n")
}


## Table sex ===============================================




tabla_femenina<-left_join(base_test, proportion_test, by="Risk_factor") 

tabla_femenina %>% select(-n_fem_2013, -n_fem_2009, -n_fem_2018)%>% gt()%>%
  fmt_percent(
    columns = 2:10,
    decimals = 1)  %>%
  fmt_number(columns = 11,
             decimals = 3)%>%
  tab_spanner(
    label = "2018",
    columns = c(paf_fem_2018, lci_fem_2018, uci_fem_2018))  %>%
  tab_spanner(
    label = "2013",
    columns = c(paf_fem_2013, lci_fem_2013, uci_fem_2013))  %>%
  tab_spanner(
    label = "2009",
    columns = c(paf_fem_2009, lci_fem_2009, uci_fem_2009))  %>%
  cols_label(
    paf_fem_2018="Estimate",
    lci_fem_2018="95% LCI",
    uci_fem_2018="95% UCI",
    paf_fem_2013="Estimate",
    lci_fem_2013="95% LCI",
    uci_fem_2013="95% UCI",    
    paf_fem_2009="Estimate",
    lci_fem_2009="95% LCI",
    uci_fem_2009="95% UCI",)%>%
  tab_header(title = md("PAF for dementia risk factors - female evolution ")) %>%
  gtsave(filename = "Draft/annex_table_sex_evolution_fem.rtf")


## Masculino ====================================================

base_test<-base_summary_masc %>% select(Risk_factor,individual_PAF, individual_PAF_LCI, individual_PAF_UCI) %>% rename(paf_masc_2018=individual_PAF,
                                                                                                                      lci_masc_2018=individual_PAF_LCI,uci_masc_2018=individual_PAF_UCI)
base_test<-base_test[1:8,]

base_test$n_masc_2018<-c(rep(n_midlife_masc,4), rep(n_latelife_masc,4))

base_test2<-base_summary_masc_2013 %>% select(individual_PAF, individual_PAF_LCI, individual_PAF_UCI) %>% rename(paf_masc_2013=individual_PAF,
                                                                                                                lci_masc_2013=individual_PAF_LCI,
                                                                                                                uci_masc_2013=individual_PAF_UCI)
base_test2$n_masc_2013<-c(rep(n_midlife_masc_2013,4), rep(n_latelife_masc_2013,4))


base_test3<-base_summary_masc_2009 %>% select(individual_PAF, individual_PAF_LCI, individual_PAF_UCI) %>% rename(paf_masc_2009=individual_PAF,
                                                                                                                lci_masc_2009=individual_PAF_LCI,
                                                                                                                uci_masc_2009=individual_PAF_UCI)
base_test3$n_masc_2009<-c(rep(n_midlife_masc_2009,4), rep(n_latelife_masc_2009,4))


base_test<-cbind(base_test,base_test2,base_test3)

rm(base_test3,base_test2)

#=================================================
proportion_test <- data.frame(Risk_factor = character(),
                              p_value = numeric())
for (i in 1:nrow(base_test)) {
  cat("Risk Factor:", base_test$Risk_factor[i], "\n")
  
  # Perform the proportion test for the current risk factor
  prop_test <- prop.test(x = c(base_test$paf_masc_2018[i]*base_test$n_masc_2018[i], base_test$paf_masc_2013[i]*base_test$n_masc_2013[i],base_test$paf_masc_2009[i]*base_test$n_masc_2009[i]), 
                         n = c(base_test$n_masc_2018[i], base_test$n_masc_2013[i],base_test$n_masc_2009[i]),
                         correct = FALSE)
  proportion_test[i, "Risk_factor"] <- base_test$Risk_factor[i]
  proportion_test[i, "p_value"] <- prop_test$p.value
  
  cat("p-value:", proportion_test[i, "p_value"], "\n\n")
}


## Table sex ===============================================





tabla_mascenina<-left_join(base_test, proportion_test, by="Risk_factor") 

tabla_mascenina %>% select(-n_masc_2013, -n_masc_2009, -n_masc_2018)%>% gt()%>%
  fmt_percent(
    columns = 2:10,
    decimals = 1)  %>%
  fmt_number(columns = 11,
             decimals = 3)%>%
  tab_spanner(
    label = "2018",
    columns = c(paf_masc_2018, lci_masc_2018, uci_masc_2018))  %>%
  tab_spanner(
    label = "2013",
    columns = c(paf_masc_2013, lci_masc_2013, uci_masc_2013))  %>%
  tab_spanner(
    label = "2009",
    columns = c(paf_masc_2009, lci_masc_2009, uci_masc_2009))  %>%
  cols_label(
    paf_masc_2018="Estimate",
    lci_masc_2018="95% LCI",
    uci_masc_2018="95% UCI",
    paf_masc_2013="Estimate",
    lci_masc_2013="95% LCI",
    uci_masc_2013="95% UCI",    
    paf_masc_2009="Estimate",
    lci_masc_2009="95% LCI",
    uci_masc_2009="95% UCI",)%>%
  tab_header(title = md("PAF for dementia risk factors - male evolution ")) %>%
  gtsave(filename = "Draft/annex_table_sex_evolution_masc.rtf")
