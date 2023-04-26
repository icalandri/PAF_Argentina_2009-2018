
#  Date: 24 April 2023                                                          
#                                                                               
#  Inpute: the outcomes of the files “04_A_PAF_calculation_2018_                
#  w_o_others.R”, “03_PAF_calculation_2013” and “03_PAF_calculation_2009”       
#                                                                               
#  Output: 1. a table comparing the overall PAF between years 2. A bar plot     
#  of that 3. A table comparing proportion by risk factor by year and 4. A      
#  bar plot of that                                                             
#                                                                               
#  Author: Ismael L Calandri                                                    



library(gt)
library(tidyverse)

# 0. Vacio la memoria ===============================
rm(list = ls())
load("tabla_2018_w_o_others.RData")
unique_overall_weigthed_PAF_CI

base_summary_2018<-na.omit(base_summary_2018)

unique_overall_weigthed_PAF_2018<-unique_overall_weigthed_PAF

load("tabla_2013.RData")

load("tabla_2009.RData")



# 1. Table year/PAF comparison=======================================================


prop.test(x = c(unique_overall_weigthed_PAF_2009*n_total_2009, 
                unique_overall_weigthed_PAF_2013*n_total_2013, 
                unique_overall_weigthed_PAF_2018*n_total_2018), 
          n = c(n_total_2009,
                n_total_2013,
                n_total_2018),
          correct = T)



## 1.1. Plot ==================================================================

year<-c("2009", "2013", "2018")
PAF<-c(unique_overall_weigthed_PAF_2009, unique_overall_weigthed_PAF_2013, unique_overall_weigthed_PAF_2018)*100
LCI<-c(unique_overall_weigthed_PAF_2009_CI$conf.int[1], unique_overall_weigthed_PAF_2013_CI$conf.int[1], unique_overall_weigthed_PAF_CI$conf.int[1])*100  
UCI<-c(unique_overall_weigthed_PAF_2009_CI$conf.int[2], unique_overall_weigthed_PAF_2013_CI$conf.int[2], unique_overall_weigthed_PAF_CI$conf.int[2])*100  

base_plot<-data.frame(year, PAF, LCI, UCI)


palette<-c("#fe7f2d","#fcca46","#619b8a")


base_plot$paf_<-as.character(round(PAF,1))
base_plot$paf_<-paste(base_plot$paf_, "%")
base_plot$CI<-paste(round(base_plot$LCI,1), "%", "-",round(base_plot$UCI,1),"%")



ggplot(base_plot)+ 
  scale_fill_manual(values = palette)+
  geom_hline( aes(yintercept = y), 
              data.frame(y = c(0,10,20,30,40)),
              color = "grey") +
  geom_col(aes(x = year,y = PAF, fill = year),
           show.legend = TRUE,
           alpha = 1)+ 
  geom_text(aes(x = year,y = PAF,label = paf_,size=5, vjust = -3))+
  geom_errorbar(aes(x=year, ymin=LCI, ymax=UCI), width=0.4, 
                            colour="gray12", 
                            alpha=0.9, size=.3)+
  geom_text(aes(x = year,y = PAF,label = CI), size=3, vjust = -1.5)+
  theme_minimal()
          

# 2. Plot ===================

base_test<-base_summary_2009 %>% select(RF,individual_PAF, individual_PAF_LCI, individual_PAF_UCI) %>% rename(paf_2009=individual_PAF,
                                                                                                                      lci_2009=individual_PAF_LCI,
                                                                                                                      uci_2009=individual_PAF_UCI)
a<-base_summary_2013 %>% select(RF,individual_PAF, individual_PAF_LCI, individual_PAF_UCI) %>% rename(paf_2013=individual_PAF,
                                                                                                   lci_2013=individual_PAF_LCI,
                                                                                                   uci_2013=individual_PAF_UCI)

b<-base_summary_2018 %>% select(Risk_factor, RF,individual_PAF, individual_PAF_LCI, individual_PAF_UCI) %>% rename(paf_2018=individual_PAF,
                                                                                                   lci_2018=individual_PAF_LCI,
                                                                                                   uci_2018=individual_PAF_UCI)


base_test<-left_join(base_test, a, by="RF")
base_test<-left_join(base_test, b, by="RF")


base_test$n_2009<-c(rep(n_midlife_2009,4), rep(n_latelife_2009,4))
base_test$n_2013<-c(rep(n_midlife_2013,4), rep(n_latelife_2013,4))
base_test$n_2018<-c(rep(n_midlife_2018,4), rep(n_latelife_2018,4))

base_test$Risk_factor<-base_summary_2018$Risk_factor


#=================================================
proportion_test <- data.frame(Risk_factor = character(),
                              p_value = numeric())
for (i in 1:nrow(base_test)) {
  cat("Risk Factor:", base_test$Risk_factor[i], "\n")
  
  # Perform the proportion test for the current risk factor
  prop_test <- prop.test(x = c(base_test$paf_2009[i]*base_test$n_2009[i], base_test$paf_2013[i]*base_test$n_2013[i], 
                         base_test$paf_2018[i]*base_test$n_2018[i]),
                         n = c(base_test$n_2009[i], base_test$n_2013[i],base_test$n_2018[i]),
                         correct = FALSE)
  proportion_test[i, "Risk_factor"] <- base_test$Risk_factor[i]
  proportion_test[i, "p_value"] <- prop_test$p.value
  
  cat("p-value:", proportion_test[i, "p_value"], "\n\n")
}


## Table===============================================

tabla<-left_join(base_test, proportion_test, by="Risk_factor") 

colnames(tabla)

tabla <- tabla[, c(8,2,3,4,5,6,7,9,10,11,12,13,14,15)]


tabla %>% select(-n_2009, -n_2013, -n_2018)%>% gt()%>%
  fmt_percent(
    columns = 2:10,
    decimals = 1)  %>%
  fmt_number(columns = 11,
             decimals = 3)%>%
  tab_spanner(
    label = "2009",
    columns = c(paf_2009, lci_2009, uci_2009))  %>%
  tab_spanner(
    label = "2013",
    columns = c(paf_2013, lci_2013, uci_2013))  %>%
  tab_spanner(
    label = "2018",
    columns = c(paf_2018, lci_2018, uci_2018))  %>%
  cols_label(
    paf_2009="Estimate",
    lci_2009="95% LCI",
    uci_2009="95% UCI",
    paf_2013="Estimate",
    lci_2013="95% LCI",
    uci_2013="95% UCI",
    paf_2018="Estimate",
    lci_2018="95% LCI",
    uci_2018="95% UCI")%>%
  tab_header(title = md("PAF for dementia risk factors - time evolution"))%>%
gtsave(filename = "Draft/annex_table_time_evolution.rtf")


# 2. error bar plot by areas ==========================================


base_summary_2018$year<-"2018"
base_summary_2013$year<-"2013"
base_summary_2009$year<-"2009"

a<-base_summary_2018 %>% select(RF,individual_PAF,individual_PAF_LCI,individual_PAF_UCI,year)
b<-base_summary_2013 %>% select(RF,individual_PAF,individual_PAF_LCI,individual_PAF_UCI,year)
c<-base_summary_2009 %>% select(RF,individual_PAF,individual_PAF_LCI,individual_PAF_UCI,year)
base_summary_year<-rbind(c,b,a)


library(reshape2)

basesita<-base_summary_year %>% select(RF,individual_PAF,year)

a<-melt(basesita, id.vars = c("RF", "year"))


basesita<-base_summary_year %>% select(RF,individual_PAF_LCI,year)

b<-melt(basesita, id.vars = c("RF", "year"))

a$LCI<-b$value

basesita<-base_summary_year %>% select(RF,individual_PAF_UCI,year)

b<-melt(basesita, id.vars = c("RF", "year"))

a$UCI<-b$value


a$RiskFactor<-factor(a$RF,
                     levels =c("PAF_education", "PAF_HBP","PAF_alcohol" ,"PAF_obesity", "PAF_smoking","PAF_pinactivity", "PAF_socialisolation", "PAF_DBT", "PAF_air" ),
                     labels = c("Less education", "Hypertension", "Alcohol", "Obesity", "Smoking", "Physical inactivity", "Social isolation", "Diabetes", "Air pollution"))


a[,4:6]<-a[,4:6]*100

palette<-c("#fe7f2d","#fcca46","#619b8a")

a %>% filter(RiskFactor =="Less education" |
               RiskFactor =="Hypertension" |
               RiskFactor =="Alcohol" |
               RiskFactor =="Obesity") %>%
  ggplot()+
  scale_fill_manual(values = palette)+
  scale_color_manual(values = palette)+
  facet_grid(. ~ RiskFactor)+
  geom_col(aes(x=year, y=value, fill=year),
           position = position_dodge(width = 1.8),
           show.legend = TRUE,
           alpha = .9)+
  geom_errorbar( aes(x=year, ymin=LCI, ymax=UCI), position =position_dodge(width = 1.8),
                 width=0.4, alpha=1, size=.5)+
  labs(y ="PAF (%)")+
  theme(legend.position = "none")



a %>% filter(RiskFactor !="Less education" &
               RiskFactor !="Hypertension" &
               RiskFactor !="Alcohol" &
               RiskFactor !="Obesity"&
               RiskFactor !="Air pollution")  %>%
  ggplot()+
  scale_fill_manual(values = palette)+
  scale_color_manual(values = palette)+
  facet_grid(. ~ RiskFactor)+
  geom_col(aes(x=year, y=value, fill=year),
           position = position_dodge(width = 1.8),
           show.legend = TRUE,
           alpha = .9)+
  geom_errorbar( aes(x=year, ymin=LCI, ymax=UCI), position =position_dodge(width = 1.8),
                 width=0.4, alpha=1, size=.5)+
  labs(y ="PAF (%)")+
  theme(legend.position = "none")

