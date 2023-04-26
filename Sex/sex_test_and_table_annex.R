library(survey)
library(tidyverse)
library(gt)
rm(list = ls())
load("tabla_fem.RData") 
load("tabla_masc.RData")


unique_overall_weigthed_PAF_masc_CI
# D. Z test for overall PAF ===============================================================

xmasc<-unique_overall_weigthed_PAF_masc*n_total_masc
xfem<-unique_overall_weigthed_PAF_fem*n_total_fem

prop.test(x = c(xmasc,xfem),
          n = c(n_total_masc, n_total_fem))


## Comparison with every factor ====================================================

base_test<-base_summary_fem %>% select(Risk_factor,individual_PAF, individual_PAF_LCI, individual_PAF_UCI) %>% rename(paf_fem=individual_PAF,
                                                                                        lci_fem=individual_PAF_LCI,
                                                                                        uci_fem=individual_PAF_UCI)
base_test$n_fem<-c(rep(n_midlife_fem,4), rep(n_latelife_fem,5))
a<-base_summary_masc %>% select(individual_PAF, individual_PAF_LCI, individual_PAF_UCI) %>% rename(paf_masc=individual_PAF,
                                                                     lci_masc=individual_PAF_LCI,
                                                                     uci_masc=individual_PAF_UCI)
base_test<-cbind(base_test,a)
base_test$n_masc<-c(rep(n_midlife_masc,4), rep(n_latelife_masc,5))


base_test %>% select(-Risk_factor) %>% rowwise()%>% mean() 
  

#=================================================
proportion_test <- data.frame(Risk_factor = character(),
                         p_value = numeric())
for (i in 1:nrow(base_test)) {
  cat("Risk Factor:", base_test$Risk_factor[i], "\n")
  
  # Perform the proportion test for the current risk factor
  prop_test <- prop.test(x = c(base_test$paf_masc[i]*base_test$n_masc[i], base_test$paf_fem[i]*base_test$n_fem[i]), 
                         n = c(base_test$n_masc[i], base_test$n_fem[i]),
                         correct = FALSE)
  proportion_test[i, "Risk_factor"] <- base_test$Risk_factor[i]
  proportion_test[i, "p_value"] <- prop_test$p.value
  
  cat("p-value:", proportion_test[i, "p_value"], "\n\n")
}


## Table sex ===============================================

tabla<-left_join(base_test, proportion_test, by="Risk_factor") 

tabla %>% select(-n_fem, -n_masc)%>% gt()%>%
  fmt_percent(
    columns = 2:7,
    decimals = 1)  %>%
  fmt_percent(
    columns = 2:4,
    decimals = 1
  )  %>%
  fmt_number(columns = 8,
             decimals = 3)%>%
  tab_spanner(
    label = "Male",
    columns = c(paf_masc, lci_masc, uci_masc))  %>%
  tab_spanner(
    label = "Female",
    columns = c(paf_fem, lci_fem, uci_fem))  %>%
  cols_label(
    paf_fem="Estimate",
    lci_fem="95% LCI",
    uci_fem="95% UCI",
    paf_masc="Estimate",
    lci_masc="95% LCI",
    uci_masc="95% UCI")%>%
  tab_header(title = md("PAF for dementia risk factors - sex differences ")) %>%
  gtsave(filename = "Draft/annex_table_sex_differences.rtf")




  