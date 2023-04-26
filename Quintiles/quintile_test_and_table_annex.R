library(survey)
library(tidyverse)
library(gt)
rm(list = ls())
load("tabla_q1.RData") 
load("tabla_q2.RData")
load("tabla_q3.RData") 
load("tabla_q4.RData")
load("tabla_q5.RData") 


unique_overall_weigthed_PAF_q5_CI


# D. Z test for overall PAF ===============================================================

xq1<-unique_overall_weigthed_PAF_q1*n_total_q1
xq2<-unique_overall_weigthed_PAF_q2*n_total_q2
xq3<-unique_overall_weigthed_PAF_q3*n_total_q3
xq4<-unique_overall_weigthed_PAF_q4*n_total_q4
xq5<-unique_overall_weigthed_PAF_q5*n_total_q5

prop.test(x = c(xq1,xq2,xq3,xq4,xq5),
          n = c(n_total_q1,n_total_q2,n_total_q3,n_total_q4,n_total_q5))


## Comparison with every factor ====================================================

base_test<-base_summary_q1 %>% select(Risk_factor,individual_PAF, individual_PAF_LCI, individual_PAF_UCI) %>% rename(paf_q1=individual_PAF,
                                                                                        lci_q1=individual_PAF_LCI,
                                                                                        uci_q1=individual_PAF_UCI)
base_test$n_q1<-c(rep(n_midlife_q1,4), rep(n_latelife_q1,5))
a<-base_summary_q2 %>% select(individual_PAF, individual_PAF_LCI, individual_PAF_UCI) %>% rename(paf_q2=individual_PAF,
                                                                     lci_q2=individual_PAF_LCI,
                                                                     uci_q2=individual_PAF_UCI)
base_test<-cbind(base_test,a)
base_test$n_q2<-c(rep(n_midlife_q2,4), rep(n_latelife_q2,5))


a<-base_summary_q3 %>% select(individual_PAF, individual_PAF_LCI, individual_PAF_UCI) %>% rename(paf_q3=individual_PAF,
                                                                                                 lci_q3=individual_PAF_LCI,
                                                                                                 uci_q3=individual_PAF_UCI)
base_test<-cbind(base_test,a)
base_test$n_q3<-c(rep(n_midlife_q3,4), rep(n_latelife_q3,5))

a<-base_summary_q4 %>% select(individual_PAF, individual_PAF_LCI, individual_PAF_UCI) %>% rename(paf_q4=individual_PAF,
                                                                                                 lci_q4=individual_PAF_LCI,
                                                                                                 uci_q4=individual_PAF_UCI)
base_test<-cbind(base_test,a)
base_test$n_q4<-c(rep(n_midlife_q4,4), rep(n_latelife_q4,5))

a<-base_summary_q5 %>% select(individual_PAF, individual_PAF_LCI, individual_PAF_UCI) %>% rename(paf_q5=individual_PAF,
                                                                                                 lci_q5=individual_PAF_LCI,
                                                                                                 uci_q5=individual_PAF_UCI)
base_test<-cbind(base_test,a)
base_test$n_q5<-c(rep(n_midlife_q5,4), rep(n_latelife_q5,5))



#=================================================
proportion_test <- data.frame(Risk_factor = character(),
                         p_value = numeric())
for (i in 1:nrow(base_test)) {
  cat("Risk Factor:", base_test$Risk_factor[i], "\n")
  
  # Perform the proportion test for the current risk factor
  prop_test <- prop.test(x = c(base_test$paf_q1[i]*base_test$n_q1[i], 
                               base_test$paf_q2[i]*base_test$n_q2[i],
                               base_test$paf_q3[i]*base_test$n_q3[i],
                               base_test$paf_q4[i]*base_test$n_q4[i],
                               base_test$paf_q5[i]*base_test$n_q5[i]), 
                         n = c(base_test$n_q1[i], base_test$n_q2[i],
                               base_test$n_q3[i], base_test$n_q4[i],base_test$n_q5[i]),
                         correct = FALSE)
  proportion_test[i, "Risk_factor"] <- base_test$Risk_factor[i]
  proportion_test[i, "p_value"] <- prop_test$p.value
  
  cat("p-value:", proportion_test[i, "p_value"], "\n\n")
}


## Table sex ===============================================

tabla<-left_join(base_test, proportion_test, by="Risk_factor") 

tabla %>% select(-n_q1, -n_q2, -n_q3, -n_q4, -n_q5)%>% gt()%>%
  fmt_percent(
    columns = 2:16,
    decimals = 1)   %>%
  fmt_number(columns = 17,
             decimals = 3)%>%
  tab_spanner(
    label = "Q1",
    columns = c(paf_q1, lci_q1, uci_q1))  %>%
  tab_spanner(
    label = "Q2",
    columns = c(paf_q2, lci_q2, uci_q2))  %>%
  tab_spanner(
    label = "Q3",
    columns = c(paf_q3, lci_q3, uci_q3))  %>%
  tab_spanner(
    label = "Q4",
    columns = c(paf_q4, lci_q4, uci_q4))  %>%
  tab_spanner(
    label = "Q5",
    columns = c(paf_q5, lci_q5, uci_q5))  %>%
  cols_label(
    paf_q1="Estimate",
    lci_q1="95% LCI",
    uci_q1="95% UCI",
    paf_q2="Estimate",
    lci_q2="95% LCI",
    uci_q2="95% UCI",
    paf_q3="Estimate",
    lci_q3="95% LCI",
    uci_q3="95% UCI",
    paf_q4="Estimate",
    lci_q4="95% LCI",
    uci_q4="95% UCI",
    paf_q5="Estimate",
    lci_q5="95% LCI",
    uci_q5="95% UCI")%>%
  tab_header(title = md("PAF for dementia risk factors - quintiles differences "))%>%
  gtsave(file="Draft/annex_quintile_differences.rtf")




  