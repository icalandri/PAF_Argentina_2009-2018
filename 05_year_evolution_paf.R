library(gt)
library(tidyverse)

# 0. load data ===============================
rm(list = ls())
load("tabla_2018_w_o_others.RData")
unique_overall_weigthed_PAF_CI

base_summary_2018<-na.omit(base_summary_2018)

unique_overall_weigthed_PAF_2018<-unique_overall_weigthed_PAF

load("tabla_2013.RData")
unique_overall_weigthed_PAF_2013_CI

load("tabla_2009.RData")
unique_overall_weigthed_PAF_2009


# 1. Table risk factor changes by year ==========================================
RF<-base_summary_2018$RF
tabla_integrada<-as.data.frame(cbind(RF,base_summary_2018$individual_PAF, 
                                     base_summary_2018$individual_PAF_LCI,
                                     base_summary_2018$individual_PAF_UCI,
                                     base_summary_2013$individual_PAF,
                                     base_summary_2013$individual_PAF_LCI,
                                     base_summary_2013$individual_PAF_UCI,
                                     base_summary_2009$individual_PAF,
                                     base_summary_2009$individual_PAF_LCI,
                                     base_summary_2009$individual_PAF_UCI))



tabla_integrada <- tabla_integrada %>% rename ("estimate 2018"=V2,
                                               "LCI 2018"=V3,
                                               "UCI 2018"=V4,
                                               "estimate 2013"=V5,
                                               "LCI 2013"=V6,
                                               "UCI 2013"=V7,
                                               "estimate 2009"=V8,
                                               "LCI 2009"=V9,
                                               "UCI 2009"=V10)



tabla_integrada[,2:10]<-lapply(tabla_integrada[,2:10], as.numeric)
tabla_integrada$Risk_factor<-c("Less education", "Obesity", "Hypertension", "Alcohol consumption", "Smoking", "Social Isolation", "Physical inactivity", "Diabetes")
names(tabla_integrada)

tabla_integrada <- tabla_integrada[, c(11,2,3,4,5,6,7,8,9,10)]


gt(tabla_integrada)%>%
  fmt_percent(columns = 2:10,
              decimals = 1)%>%
  tab_header(
    title = md("Population attributable fractions for risk factors for dementia"),
    subtitle = md("Year evolution")
  )%>%
  tab_spanner(
    label = "2018",
    columns = 2:4)%>%
  tab_spanner(
    label = "2013",
    columns = 5:7)%>%
  tab_spanner(
    label = "2009",
    columns = 8:10)%>%
  cols_label(
    "estimate 2018" = "estimate",
    "LCI 2018" = "LCI",
    "UCI 2018" = "UCI",
    "estimate 2013" = "estimate",
    "LCI 2013" = "LCI",
    "UCI 2013" = "UCI",
    "estimate 2009" = "estimate",
    "LCI 2009" = "LCI",
    "UCI 2009" = "UCI")


# 2. Table PAF total by year ==================================================

overall_PAF<-c(unique_overall_weigthed_PAF_2009_CI$estimate, unique_overall_weigthed_PAF_2013_CI$estimate, unique_overall_weigthed_PAF_CI$estimate)
Year<-c(2009,2013,2018)
LCI<-c(unique_overall_weigthed_PAF_2009_CI$conf.int[1], unique_overall_weigthed_PAF_2013_CI$conf.int[1], unique_overall_weigthed_PAF_CI$conf.int[1])
UCI<-c(unique_overall_weigthed_PAF_2009_CI$conf.int[2], unique_overall_weigthed_PAF_2013_CI$conf.int[2], unique_overall_weigthed_PAF_CI$conf.int[2])
PAF<-as.data.frame(cbind(Year, overall_PAF, LCI, UCI))

gt(PAF) %>%
  fmt_percent(2:4)


vector2018<-c(unique_overall_weigthed_PAF_CI$estimate, unique_overall_weigthed_PAF_CI$conf.int[1],unique_overall_weigthed_PAF_CI$conf.int[2])
vector2013<-c(unique_overall_weigthed_PAF_2013_CI$estimate, unique_overall_weigthed_PAF_2013_CI$conf.int[1],unique_overall_weigthed_PAF_2013_CI$conf.int[2])
vector2009<-c(unique_overall_weigthed_PAF_2009_CI$estimate, unique_overall_weigthed_PAF_2009_CI$conf.int[1],unique_overall_weigthed_PAF_2009_CI$conf.int[2])

base<-as.data.frame(rbind(vector2009, vector2013, vector2018))

base$year<-c(2009,2013,2018)

base<-base[,c(4,1,2,3)]

base<-base %>% rename (estimate=p,
                       LCI=V2,
                       UCI=V3)


gt(base)%>%
  fmt_percent(2:4,
              decimals = 1)%>%
  tab_spanner(
    label = "weigthed PAF",
    columns = c(estimate, LCI, UCI))  %>%
  tab_header(
    title = md("Population attributable fractions for risk factors"),
    subtitle = md("Year evolution"))

# 5. Comparison with economical measures ============================
library(readr)
gini <- read_csv("gini.csv")

str(gini$HDI)

base_gini<-left_join(gini,base, by ="year")

#3. Graficos ===================================================================

Year<-c(2018,2013,2009)
Risk<-c(unique_overall_weigthed_PAF_2018,
        unique_overall_weigthed_PAF_2013_CI$estimate,
        unique_overall_weigthed_PAF_2009_CI$estimate)

        
total_risk<-as.data.frame(cbind(Year, Risk))



#### grafico de barras

base_barras<- base_summary_2018 %>% select(RF, individual_PAF)
base_barras$year<-"2018"

base_barras_1<- base_summary_2013 %>% select(RF, individual_PAF)
base_barras_1$year<-"2013"


base_barras_2<- base_summary_2009 %>% select(RF, individual_PAF)
base_barras_2$year<-"2009"

base_barras<-rbind(base_barras, base_barras_1, base_barras_2)

base_barras$Risk<-round(base_barras$individual_PAF*100,2)






base_barras$RiskFactor<-factor(base_barras$RF,
                       levels =c("PAF_education", "PAF_HBP","PAF_alcohol" ,"PAF_obesity", "PAF_smoking","PAF_pinactivity", "PAF_socialisolation", "PAF_DBT" ),
                       labels = c("Less education", "Hypertension", "Alcohol", "Obesity", "Smoking", "Physical inactivity", "Social isolation", "Diabetes"))

  ggplot(base_barras,aes(x=RiskFactor, y=Risk, fill=year))+
    geom_rect(xmin=0,xmax=1.5, ymin=0, ymax=9, fill="#c7deb2", alpha=.8)+
    geom_rect(xmin=1.5,xmax=4.5, ymin=0, ymax=9, fill="#9bd4e5", alpha=.8)+
    geom_rect(xmin=4.5,xmax=8.5, ymin=0, ymax=9, fill="#d2c3e0", alpha=.8)+
  geom_bar(stat = "identity", position="dodge",
           show.legend = TRUE)+
    geom_text(aes(label=Risk), vjust=-1, color="white", position = position_dodge(width = .9),  size=2.5)+ 
  scale_fill_manual(values = c("#E09F3E","#9F2A2B", "#531013"))+
  theme_void()+
    theme(
      # Remove axis ticks and text
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text.y = element_blank(),
      # Use gray text for the region names
      axis.text.x = element_text(color = "grey12", size = 7),
      # Move the legend to the bottom
      legend.position = "bottom",
    )



total2009<- unique_overall_weigthed_PAF_2009*n_total_2009         
total2018<- unique_overall_weigthed_PAF_2018*n_total_2018

prop.test(x = c(total2009,total2018),
          n = c(n_total_2009, n_total_2018))

#4. test ===========================================================================

education2009<-base_summary_2009$individual_PAF[base_summary_2009$RF=="PAF_education"]*
  n_midlife_2009
education2018<-base_summary_2018$individual_PAF[base_summary_2018$RF=="PAF_education"]*
  n_midlife_2018


prop.test(x = c(education2009,education2018),
          n = c(n_midlife_2009, n_midlife_2018))


alcohol2009<-base_summary_2009$individual_PAF[base_summary_2009$RF=="PAF_alcohol"]*
  n_midlife_2009
alcohol2018<-base_summary_2018$individual_PAF[base_summary_2018$RF=="PAF_alcohol"]*
  n_midlife_2018
alcohol2013<-base_summary_2013$individual_PAF[base_summary_2013$RF=="PAF_alcohol"]*
  n_midlife_2013

prop.test(x = c(alcohol2009,alcohol2018),
          n = c(n_midlife_2009, n_midlife_2018))


obesity2009<-base_summary_2009$individual_PAF[base_summary_2009$RF=="PAF_obesity"]*
  n_midlife_2009
obesity2018<-base_summary_2018$individual_PAF[base_summary_2018$RF=="PAF_obesity"]*
  n_midlife_2018
obesity2013<-base_summary_2013$individual_PAF[base_summary_2013$RF=="PAF_obesity"]*
  n_midlife_2013

prop.test(x = c(obesity2009,obesity2018),
          n = c(n_midlife_2009, n_midlife_2018))



pinac2009<-base_summary_2009$individual_PAF[base_summary_2009$RF=="PAF_pinactivity"]*
  n_latelife_2009
pinac2018<-base_summary_2018$individual_PAF[base_summary_2018$RF=="PAF_pinactivity"]*
  n_latelife_2018
pinac2013<-base_summary_2013$individual_PAF[base_summary_2013$RF=="PAF_pinactivity"]*
  n_latelife_2013

prop.test(x = c(pinac2009,pinac2018),
          n = c(n_latelife_2009, n_latelife_2018))


soc2009<-base_summary_2009$individual_PAF[base_summary_2009$RF=="PAF_socialisolation"]*
  n_latelife_2009
soc2018<-base_summary_2018$individual_PAF[base_summary_2018$RF=="PAF_socialisolation"]*
  n_latelife_2018
soc2013<-base_summary_2013$individual_PAF[base_summary_2013$RF=="PAF_socialisolation"]*
  n_latelife_2013

prop.test(x = c(soc2009,soc2018),
          n = c(n_latelife_2009, n_latelife_2018))



#=============================================================

base_gini$PAF<-base_gini$estimate*100
base_gini$HDI_<-base_gini$HDI-40
names(base_gini)
str(base_gini)
base_gini$LCI<-base_gini$LCI*100
base_gini$UCI<-base_gini$UCI*100
ggplot(base_gini, aes(x=year)) + 
  geom_point(aes(y = PAF), color = "darkred")+
  geom_point(aes(y = LCI), color = "red")+
  geom_point(aes(y = UCI), color = "red")+
  geom_line(aes(y = Gini), color="steelblue")+
  geom_line(aes(y = HDI_), color="darkgreen")+
  geom_point(aes(y = `Poverty%`), color="grey")+
  xlim(2007,2022)+
  ylim(32,46)+
  theme_minimal()

base_gini2<-base_gini %>% select(year, Gini, estimate, LCI, UCI)

base_gini2<-na.omit(base_gini2)
base_gini2$PAF

cor.test(base_gini2$Gini, base_gini2$estimate, method = "spearman")

base_gini3<-base_gini %>% select(year, HDI, estimate, LCI, UCI)

base_gini3<-na.omit(base_gini3)

cor.test(base_gini3$HDI, base_gini3$estimate, method = "spearman")


) 
