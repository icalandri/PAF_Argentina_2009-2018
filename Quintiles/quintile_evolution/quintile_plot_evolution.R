load("tabla_q1.RData")
rm(unique_overall_weigthed_PAF_q1_CI)
load("tabla_q2.RData")
rm(unique_overall_weigthed_PAF_q2_CI)
load("tabla_q2_2013.RData")
rm(unique_overall_weigthed_PAF_q2_CI_2013)
load("tabla_q1_2013.RData")
rm(unique_overall_weigthed_PAF_q1_CI_2013)
load("tabla_q2_2009.RData")
rm(unique_overall_weigthed_PAF_q2_CI_2009)
load("tabla_q1_2009.RData")
rm(unique_overall_weigthed_PAF_q1_CI_2009)
load("tabla_q3.RData")
rm(unique_overall_weigthed_PAF_q3_CI)
load("tabla_q4.RData")
rm(unique_overall_weigthed_PAF_q4_CI)
load("tabla_q4_2013.RData")
rm(unique_overall_weigthed_PAF_q4_CI_2013)
load("tabla_q3_2013.RData")
rm(unique_overall_weigthed_PAF_q3_CI_2013)
load("tabla_q4_2009.RData")
rm(unique_overall_weigthed_PAF_q4_CI_2009)
load("tabla_q3_2009.RData")
rm(unique_overall_weigthed_PAF_q3_CI_2009)
load("tabla_q5.RData")
rm(unique_overall_weigthed_PAF_q5_CI)
load("tabla_q5_2013.RData")
rm(unique_overall_weigthed_PAF_q5_CI_2013)
load("tabla_q5_2009.RData")
rm(unique_overall_weigthed_PAF_q5_CI_2009)


library(tidyverse)
library(reshape2)

### 2. Grafico =======================================================
base_summary_q1$quintil<-"q1"
base_summary_q1_2013$quintil<-"q1"
base_summary_q1_2009$quintil<-"q1"
base_summary_q2$quintil<-"q2"
base_summary_q2_2013$quintil<-"q2"
base_summary_q2_2009$quintil<-"q2"

base_summary_q3$quintil<-"q3"
base_summary_q3_2013$quintil<-"q3"
base_summary_q3_2009$quintil<-"q3"
base_summary_q4$quintil<-"q4"
base_summary_q4_2013$quintil<-"q4"
base_summary_q4_2009$quintil<-"q4"

base_summary_q5$quintil<-"q5"
base_summary_q5_2013$quintil<-"q5"
base_summary_q5_2009$quintil<-"q5"



base_summary_q1$year<-2018
base_summary_q1_2013$year<-2013
base_summary_q1_2009$year<-2009
base_summary_q2$year<-2018
base_summary_q2_2013$year<-2013
base_summary_q2_2009$year<-2009

base_summary_q3$year<-2018
base_summary_q3_2013$year<-2013
base_summary_q3_2009$year<-2009
base_summary_q4$year<-2018
base_summary_q4_2013$year<-2013
base_summary_q4_2009$year<-2009

base_summary_q5$year<-2018
base_summary_q5_2013$year<-2013
base_summary_q5_2009$year<-2009

base_summary_quintil<-rbind(base_summary_q1,base_summary_q1_2013,base_summary_q1_2009,
                        base_summary_q2,base_summary_q2_2013,base_summary_q2_2009,
                        base_summary_q3,base_summary_q3_2013,base_summary_q3_2009,
                        base_summary_q4,base_summary_q4_2013,base_summary_q4_2009,
                        base_summary_q5,base_summary_q5_2013,base_summary_q5_2009)


basesita<-base_summary_quintil %>% select(RF,individual_PAF,quintil,year)

basesita$individual_PAF<-basesita$individual_PAF*100

a<-melt(basesita, id.vars = c("RF", "quintil", "year"))


basesita<-base_summary_quintil %>% select(RF,individual_PAF_LCI,quintil, year)

basesita$individual_PAF_LCI<-basesita$individual_PAF_LCI*100

b<-melt(basesita, id.vars = c("RF", "quintil", "year"))

a$LCI<-b$value

basesita<-base_summary_quintil %>% select(RF,individual_PAF_UCI,quintil, year)

basesita$individual_PAF_UCI<-basesita$individual_PAF_UCI*100

b<-melt(basesita, id.vars = c("RF", "quintil", "year"))

a$UCI<-b$value


a$RiskFactor<-factor(a$RF,
                     levels =c("PAF_education", "PAF_HBP","PAF_alcohol" ,"PAF_obesity", "PAF_smoking","PAF_pinactivity", "PAF_socialisolation", "PAF_DBT", "PAF_air" ),
                     labels = c("Less education", "Hypertension", "Alcohol", "Obesity", "Smoking", "Physical inactivity", "Social isolation", "Diabetes", "Air pollution"))

palette_fleni<-c("#a90123","#004385","#fba31b","#578001", "grey")
palette_fleni<-viridis(5)

a %>% filter(RiskFactor =="Less education" |
               RiskFactor =="Hypertension" |
               RiskFactor =="Alcohol" |
               RiskFactor =="Obesity") %>%
  ggplot()+
  scale_fill_manual(values = palette_fleni)+
  scale_color_manual(values = palette_fleni)+
  facet_grid(. ~ RiskFactor)+
  geom_col(aes(x=year, y=value, fill=quintil),
           position = position_dodge(width = 1.8),
           show.legend = TRUE,
           alpha = .5)+
  scale_x_continuous(breaks = c(2009, 2013, 2018))+
  geom_errorbar( aes(x=year, ymin=LCI, ymax=UCI, color=quintil ), position =position_dodge(width = 1.8),
                 width=0.4, alpha=1, size=.5)+
  labs(y ="PAF (%)")+
  theme(legend.position = "none")



a %>% filter(RiskFactor !="Less education" &
               RiskFactor !="Hypertension" &
               RiskFactor !="Alcohol" &
               RiskFactor !="Obesity"&
               RiskFactor !="Air pollution") %>%
  ggplot()+
  scale_fill_manual(values = palette_fleni)+
  scale_color_manual(values = palette_fleni)+
  facet_grid(. ~ RiskFactor)+
  geom_col(aes(x=year, y=value, fill=quintil),
           position = position_dodge(width = 1.8),
           show.legend = TRUE,
           alpha = .5)+
  scale_x_continuous(breaks = c(2009, 2013, 2018))+
  geom_errorbar( aes(x=year, ymin=LCI, ymax=UCI, color=quintil), position =position_dodge(width = 1.8),
                 width=0.4, alpha=1, size=.5)+
  labs(y ="PAF (%)")+
  theme(legend.position = "none")

# tablas anexas ================================================================================

## Q1 ====================================================
base_test<-base_summary_q1 %>% select(Risk_factor,individual_PAF, individual_PAF_LCI, individual_PAF_UCI) %>% rename(paf_q1_2018=individual_PAF,
                                                                                                                lci_q1_2018=individual_PAF_LCI,uci_q1_2018=individual_PAF_UCI)
base_test<-base_test[1:8,]
                                                                                                                                                                                                                                      
base_test$n_q1_2018<-c(rep(n_midlife_q1,4), rep(n_latelife_q1,4))

base_test2<-base_summary_q1_2013 %>% select(individual_PAF, individual_PAF_LCI, individual_PAF_UCI) %>% rename(paf_q1_2013=individual_PAF,
                                                                                                                      lci_q1_2013=individual_PAF_LCI,
                                                                                                                      uci_q1_2013=individual_PAF_UCI)
base_test2$n_q1_2013<-c(rep(n_midlife_q1_2013,4), rep(n_latelife_q1_2013,4))


base_test3<-base_summary_q1_2009 %>% select(individual_PAF, individual_PAF_LCI, individual_PAF_UCI) %>% rename(paf_q1_2009=individual_PAF,
                                                                                                                           lci_q1_2009=individual_PAF_LCI,
                                                                                                                           uci_q1_2009=individual_PAF_UCI)
base_test3$n_q1_2009<-c(rep(n_midlife_q1_2009,4), rep(n_latelife_q1_2009,4))
base_test<-cbind(base_test,base_test2,base_test3)
rm(base_test3,base_test2)

###=================================================
proportion_test <- data.frame(Risk_factor = character(),
                              p_value = numeric())
for (i in 1:nrow(base_test)) {
  cat("Risk Factor:", base_test$Risk_factor[i], "\n")
  
  # Perform the proportion test for the current risk factor
  prop_test <- prop.test(x = c(base_test$paf_q1_2018[i]*base_test$n_q1_2018[i], base_test$paf_q1_2013[i]*base_test$n_q1_2013[i],base_test$paf_q1_2009[i]*base_test$n_q1_2009[i]), 
                         n = c(base_test$n_q1_2018[i], base_test$n_q1_2013[i],base_test$n_q1_2009[i]),
                         correct = FALSE)
  proportion_test[i, "Risk_factor"] <- base_test$Risk_factor[i]
  proportion_test[i, "p_value"] <- prop_test$p.value
  
  cat("p-value:", proportion_test[i, "p_value"], "\n\n")
}
### Table ===============================================
tabla_q1<-left_join(base_test, proportion_test, by="Risk_factor") 

tabla_q1 %>% select(-n_q1_2013, -n_q1_2009, -n_q1_2018)%>% gt()%>%
  fmt_percent(
    columns = 2:10,
    decimals = 1)  %>%
  fmt_number(columns = 11,
             decimals = 3)%>%
  tab_spanner(
    label = "2018",
    columns = c(paf_q1_2018, lci_q1_2018, uci_q1_2018))  %>%
  tab_spanner(
    label = "2013",
    columns = c(paf_q1_2013, lci_q1_2013, uci_q1_2013))  %>%
  tab_spanner(
    label = "2009",
    columns = c(paf_q1_2009, lci_q1_2009, uci_q1_2009))  %>%
  cols_label(
    paf_q1_2018="Estimate",
    lci_q1_2018="95% LCI",
    uci_q1_2018="95% UCI",
    paf_q1_2013="Estimate",
    lci_q1_2013="95% LCI",
    uci_q1_2013="95% UCI",    
    paf_q1_2009="Estimate",
    lci_q1_2009="95% LCI",
    uci_q1_2009="95% UCI",)%>%
  tab_header(title = md("PAF for dementia risk factors - q1ale evolution ")) %>%
  gtsave(filename = "Draft/annex_table_evolution_q1.rtf")


## q2 ====================================================
base_test<-base_summary_q2 %>% select(Risk_factor,individual_PAF, individual_PAF_LCI, individual_PAF_UCI) %>% rename(paf_q2_2018=individual_PAF,
                                                                                                                     lci_q2_2018=individual_PAF_LCI,uci_q2_2018=individual_PAF_UCI)
base_test<-base_test[1:8,]

base_test$n_q2_2018<-c(rep(n_midlife_q2,4), rep(n_latelife_q2,4))

base_test2<-base_summary_q2_2013 %>% select(individual_PAF, individual_PAF_LCI, individual_PAF_UCI) %>% rename(paf_q2_2013=individual_PAF,
                                                                                                               lci_q2_2013=individual_PAF_LCI,
                                                                                                               uci_q2_2013=individual_PAF_UCI)
base_test2$n_q2_2013<-c(rep(n_midlife_q2_2013,4), rep(n_latelife_q2_2013,4))


base_test3<-base_summary_q2_2009 %>% select(individual_PAF, individual_PAF_LCI, individual_PAF_UCI) %>% rename(paf_q2_2009=individual_PAF,
                                                                                                               lci_q2_2009=individual_PAF_LCI,
                                                                                                               uci_q2_2009=individual_PAF_UCI)
base_test3$n_q2_2009<-c(rep(n_midlife_q2_2009,4), rep(n_latelife_q2_2009,4))
base_test<-cbind(base_test,base_test2,base_test3)
rm(base_test3,base_test2)

###=================================================
proportion_test <- data.frame(Risk_factor = character(),
                              p_value = numeric())
for (i in 1:nrow(base_test)) {
  cat("Risk Factor:", base_test$Risk_factor[i], "\n")
  
  # Perform the proportion test for the current risk factor
  prop_test <- prop.test(x = c(base_test$paf_q2_2018[i]*base_test$n_q2_2018[i], base_test$paf_q2_2013[i]*base_test$n_q2_2013[i],base_test$paf_q2_2009[i]*base_test$n_q2_2009[i]), 
                         n = c(base_test$n_q2_2018[i], base_test$n_q2_2013[i],base_test$n_q2_2009[i]),
                         correct = FALSE)
  proportion_test[i, "Risk_factor"] <- base_test$Risk_factor[i]
  proportion_test[i, "p_value"] <- prop_test$p.value
  
  cat("p-value:", proportion_test[i, "p_value"], "\n\n")
}
### Table ===============================================
tabla_q2<-left_join(base_test, proportion_test, by="Risk_factor") 

tabla_q2 %>% select(-n_q2_2013, -n_q2_2009, -n_q2_2018)%>% gt()%>%
  fmt_percent(
    columns = 2:10,
    decimals = 1)  %>%
  fmt_number(columns = 11,
             decimals = 3)%>%
  tab_spanner(
    label = "2018",
    columns = c(paf_q2_2018, lci_q2_2018, uci_q2_2018))  %>%
  tab_spanner(
    label = "2013",
    columns = c(paf_q2_2013, lci_q2_2013, uci_q2_2013))  %>%
  tab_spanner(
    label = "2009",
    columns = c(paf_q2_2009, lci_q2_2009, uci_q2_2009))  %>%
  cols_label(
    paf_q2_2018="Estimate",
    lci_q2_2018="95% LCI",
    uci_q2_2018="95% UCI",
    paf_q2_2013="Estimate",
    lci_q2_2013="95% LCI",
    uci_q2_2013="95% UCI",    
    paf_q2_2009="Estimate",
    lci_q2_2009="95% LCI",
    uci_q2_2009="95% UCI",)%>%
  tab_header(title = md("PAF for dementia risk factors - q2ale evolution ")) %>%
  gtsave(filename = "Draft/annex_table_evolution_q2.rtf")

## q3 ====================================================
base_test<-base_summary_q3 %>% select(Risk_factor,individual_PAF, individual_PAF_LCI, individual_PAF_UCI) %>% rename(paf_q3_2018=individual_PAF,
                                                                                                                     lci_q3_2018=individual_PAF_LCI,uci_q3_2018=individual_PAF_UCI)
base_test<-base_test[1:8,]

base_test$n_q3_2018<-c(rep(n_midlife_q3,4), rep(n_latelife_q3,4))

base_test2<-base_summary_q3_2013 %>% select(individual_PAF, individual_PAF_LCI, individual_PAF_UCI) %>% rename(paf_q3_2013=individual_PAF,
                                                                                                               lci_q3_2013=individual_PAF_LCI,
                                                                                                               uci_q3_2013=individual_PAF_UCI)
base_test2$n_q3_2013<-c(rep(n_midlife_q3_2013,4), rep(n_latelife_q3_2013,4))


base_test3<-base_summary_q3_2009 %>% select(individual_PAF, individual_PAF_LCI, individual_PAF_UCI) %>% rename(paf_q3_2009=individual_PAF,
                                                                                                               lci_q3_2009=individual_PAF_LCI,
                                                                                                               uci_q3_2009=individual_PAF_UCI)
base_test3$n_q3_2009<-c(rep(n_midlife_q3_2009,4), rep(n_latelife_q3_2009,4))
base_test<-cbind(base_test,base_test2,base_test3)
rm(base_test3,base_test2)

###=================================================
proportion_test <- data.frame(Risk_factor = character(),
                              p_value = numeric())
for (i in 1:nrow(base_test)) {
  cat("Risk Factor:", base_test$Risk_factor[i], "\n")
  
  # Perform the proportion test for the current risk factor
  prop_test <- prop.test(x = c(base_test$paf_q3_2018[i]*base_test$n_q3_2018[i], base_test$paf_q3_2013[i]*base_test$n_q3_2013[i],base_test$paf_q3_2009[i]*base_test$n_q3_2009[i]), 
                         n = c(base_test$n_q3_2018[i], base_test$n_q3_2013[i],base_test$n_q3_2009[i]),
                         correct = FALSE)
  proportion_test[i, "Risk_factor"] <- base_test$Risk_factor[i]
  proportion_test[i, "p_value"] <- prop_test$p.value
  
  cat("p-value:", proportion_test[i, "p_value"], "\n\n")
}
### Table ===============================================
tabla_q3<-left_join(base_test, proportion_test, by="Risk_factor") 

tabla_q3 %>% select(-n_q3_2013, -n_q3_2009, -n_q3_2018)%>% gt()%>%
  fmt_percent(
    columns = 2:10,
    decimals = 1)  %>%
  fmt_number(columns = 11,
             decimals = 3)%>%
  tab_spanner(
    label = "2018",
    columns = c(paf_q3_2018, lci_q3_2018, uci_q3_2018))  %>%
  tab_spanner(
    label = "2013",
    columns = c(paf_q3_2013, lci_q3_2013, uci_q3_2013))  %>%
  tab_spanner(
    label = "2009",
    columns = c(paf_q3_2009, lci_q3_2009, uci_q3_2009))  %>%
  cols_label(
    paf_q3_2018="Estimate",
    lci_q3_2018="95% LCI",
    uci_q3_2018="95% UCI",
    paf_q3_2013="Estimate",
    lci_q3_2013="95% LCI",
    uci_q3_2013="95% UCI",    
    paf_q3_2009="Estimate",
    lci_q3_2009="95% LCI",
    uci_q3_2009="95% UCI",)%>%
  tab_header(title = md("PAF for dementia risk factors - q3ale evolution ")) %>%
  gtsave(filename = "Draft/annex_table_evolution_q3.rtf")

## q4 ====================================================
base_test<-base_summary_q4 %>% select(Risk_factor,individual_PAF, individual_PAF_LCI, individual_PAF_UCI) %>% rename(paf_q4_2018=individual_PAF,
                                                                                                                     lci_q4_2018=individual_PAF_LCI,uci_q4_2018=individual_PAF_UCI)
base_test<-base_test[1:8,]

base_test$n_q4_2018<-c(rep(n_midlife_q4,4), rep(n_latelife_q4,4))

base_test2<-base_summary_q4_2013 %>% select(individual_PAF, individual_PAF_LCI, individual_PAF_UCI) %>% rename(paf_q4_2013=individual_PAF,
                                                                                                               lci_q4_2013=individual_PAF_LCI,
                                                                                                               uci_q4_2013=individual_PAF_UCI)
base_test2$n_q4_2013<-c(rep(n_midlife_q4_2013,4), rep(n_latelife_q4_2013,4))


base_test3<-base_summary_q4_2009 %>% select(individual_PAF, individual_PAF_LCI, individual_PAF_UCI) %>% rename(paf_q4_2009=individual_PAF,
                                                                                                               lci_q4_2009=individual_PAF_LCI,
                                                                                                               uci_q4_2009=individual_PAF_UCI)
base_test3$n_q4_2009<-c(rep(n_midlife_q4_2009,4), rep(n_latelife_q4_2009,4))
base_test<-cbind(base_test,base_test2,base_test3)
rm(base_test3,base_test2)

###=================================================
proportion_test <- data.frame(Risk_factor = character(),
                              p_value = numeric())
for (i in 1:nrow(base_test)) {
  cat("Risk Factor:", base_test$Risk_factor[i], "\n")
  
  # Perform the proportion test for the current risk factor
  prop_test <- prop.test(x = c(base_test$paf_q4_2018[i]*base_test$n_q4_2018[i], base_test$paf_q4_2013[i]*base_test$n_q4_2013[i],base_test$paf_q4_2009[i]*base_test$n_q4_2009[i]), 
                         n = c(base_test$n_q4_2018[i], base_test$n_q4_2013[i],base_test$n_q4_2009[i]),
                         correct = FALSE)
  proportion_test[i, "Risk_factor"] <- base_test$Risk_factor[i]
  proportion_test[i, "p_value"] <- prop_test$p.value
  
  cat("p-value:", proportion_test[i, "p_value"], "\n\n")
}
### Table ===============================================
tabla_q4<-left_join(base_test, proportion_test, by="Risk_factor") 

tabla_q4 %>% select(-n_q4_2013, -n_q4_2009, -n_q4_2018)%>% gt()%>%
  fmt_percent(
    columns = 2:10,
    decimals = 1)  %>%
  fmt_number(columns = 11,
             decimals = 3)%>%
  tab_spanner(
    label = "2018",
    columns = c(paf_q4_2018, lci_q4_2018, uci_q4_2018))  %>%
  tab_spanner(
    label = "2013",
    columns = c(paf_q4_2013, lci_q4_2013, uci_q4_2013))  %>%
  tab_spanner(
    label = "2009",
    columns = c(paf_q4_2009, lci_q4_2009, uci_q4_2009))  %>%
  cols_label(
    paf_q4_2018="Estimate",
    lci_q4_2018="95% LCI",
    uci_q4_2018="95% UCI",
    paf_q4_2013="Estimate",
    lci_q4_2013="95% LCI",
    uci_q4_2013="95% UCI",    
    paf_q4_2009="Estimate",
    lci_q4_2009="95% LCI",
    uci_q4_2009="95% UCI",)%>%
  tab_header(title = md("PAF for dementia risk factors - q4ale evolution ")) %>%
  gtsave(filename = "Draft/annex_table_evolution_q4.rtf")


## q5 ====================================================
base_test<-base_summary_q5 %>% select(Risk_factor,individual_PAF, individual_PAF_LCI, individual_PAF_UCI) %>% rename(paf_q5_2018=individual_PAF,
                                                                                                                     lci_q5_2018=individual_PAF_LCI,uci_q5_2018=individual_PAF_UCI)
base_test<-base_test[1:8,]

base_test$n_q5_2018<-c(rep(n_midlife_q5,4), rep(n_latelife_q5,4))

base_test2<-base_summary_q5_2013 %>% select(individual_PAF, individual_PAF_LCI, individual_PAF_UCI) %>% rename(paf_q5_2013=individual_PAF,
                                                                                                               lci_q5_2013=individual_PAF_LCI,
                                                                                                               uci_q5_2013=individual_PAF_UCI)
base_test2$n_q5_2013<-c(rep(n_midlife_q5_2013,4), rep(n_latelife_q5_2013,4))


base_test3<-base_summary_q5_2009 %>% select(individual_PAF, individual_PAF_LCI, individual_PAF_UCI) %>% rename(paf_q5_2009=individual_PAF,
                                                                                                               lci_q5_2009=individual_PAF_LCI,
                                                                                                               uci_q5_2009=individual_PAF_UCI)
base_test3$n_q5_2009<-c(rep(n_midlife_q5_2009,4), rep(n_latelife_q5_2009,4))
base_test<-cbind(base_test,base_test2,base_test3)
rm(base_test3,base_test2)

###=================================================
proportion_test <- data.frame(Risk_factor = character(),
                              p_value = numeric())
for (i in 1:nrow(base_test)) {
  cat("Risk Factor:", base_test$Risk_factor[i], "\n")
  
  # Perform the proportion test for the current risk factor
  prop_test <- prop.test(x = c(base_test$paf_q5_2018[i]*base_test$n_q5_2018[i], base_test$paf_q5_2013[i]*base_test$n_q5_2013[i],base_test$paf_q5_2009[i]*base_test$n_q5_2009[i]), 
                         n = c(base_test$n_q5_2018[i], base_test$n_q5_2013[i],base_test$n_q5_2009[i]),
                         correct = FALSE)
  proportion_test[i, "Risk_factor"] <- base_test$Risk_factor[i]
  proportion_test[i, "p_value"] <- prop_test$p.value
  
  cat("p-value:", proportion_test[i, "p_value"], "\n\n")
}
### Table ===============================================
tabla_q5<-left_join(base_test, proportion_test, by="Risk_factor") 

tabla_q5 %>% select(-n_q5_2013, -n_q5_2009, -n_q5_2018)%>% gt()%>%
  fmt_percent(
    columns = 2:10,
    decimals = 1)  %>%
  fmt_number(columns = 11,
             decimals = 3)%>%
  tab_spanner(
    label = "2018",
    columns = c(paf_q5_2018, lci_q5_2018, uci_q5_2018))  %>%
  tab_spanner(
    label = "2013",
    columns = c(paf_q5_2013, lci_q5_2013, uci_q5_2013))  %>%
  tab_spanner(
    label = "2009",
    columns = c(paf_q5_2009, lci_q5_2009, uci_q5_2009))  %>%
  cols_label(
    paf_q5_2018="Estimate",
    lci_q5_2018="95% LCI",
    uci_q5_2018="95% UCI",
    paf_q5_2013="Estimate",
    lci_q5_2013="95% LCI",
    uci_q5_2013="95% UCI",    
    paf_q5_2009="Estimate",
    lci_q5_2009="95% LCI",
    uci_q5_2009="95% UCI",)%>%
  tab_header(title = md("PAF for dementia risk factors - q5ale evolution ")) %>%
  gtsave(filename = "Draft/annex_table_evolution_q5.rtf")