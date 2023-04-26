

#  Date: 24 April 2023                                                          
#                                                                               
#  Input: the outcomes of the file “quintile_pipeline”                          
#                                                                               
#  Output: a circular barplot with the comparison of risk factors by income     
#                                                                               
#  Author: Ismael L Calandri                                                    





load("tabla_q1.RData")
load("tabla_q2.RData")
load("tabla_q3.RData")
load("tabla_q4.RData")
load("tabla_q5.RData")
library(tidyverse)
library(reshape2)

### 2. Grafico =======================================================
base_summary_q1$quintile<-"Q1"
base_summary_q2$quintile<-"Q2"
base_summary_q3$quintile<-"Q3"
base_summary_q4$quintile<-"Q4"
base_summary_q5$quintile<-"Q5"


base_summary_quintile<-rbind(base_summary_q1,
                        base_summary_q2,
                        base_summary_q3,
                        base_summary_q4,
                        base_summary_q5)


basesita<-base_summary_quintile %>% select(RF,individual_PAF,quintile)

basesita$individual_PAF<-basesita$individual_PAF*100

a<-melt(basesita, id.vars = c("RF", "quintile"))


basesita<-base_summary_quintile %>% select(RF,individual_PAF_LCI,quintile)

basesita$individual_PAF_LCI<-basesita$individual_PAF_LCI*100

b<-melt(basesita, id.vars = c("RF", "quintile"))

a$LCI<-b$value

basesita<-base_summary_quintile %>% select(RF,individual_PAF_UCI,quintile)

basesita$individual_PAF_UCI<-basesita$individual_PAF_UCI*100

b<-melt(basesita, id.vars = c("RF", "quintile"))

a$UCI<-b$value


a$RiskFactor<-factor(a$RF,
                     levels =c("PAF_education", "PAF_HBP","PAF_alcohol" ,"PAF_obesity", "PAF_smoking","PAF_pinactivity", "PAF_socialisolation", "PAF_DBT", "PAF_air" ),
                     labels = c("Less education", "Hypertension", "Alcohol", "Obesity", "Smoking", "Physical inactivity", "Social isolation", "Diabetes", "Air pollution"))

palette_fleni<-c("#383961","#dbdfac", "#5f758e", "#DB162F", "#B97995")


library(viridis)
palette_fleni<-viridis(5)

a$group<-c("Early life", "Midlife","Midlife","Midlife","Latter life","Latter life","Latter life","Latter life","Latter life")
pto_corte<-c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5)
ggplot(a)+ 
  scale_fill_manual(values = palette_fleni)+
  geom_rect(xmin=0.5,xmax=1.5, ymin=0, ymax=3.5, fill="#c7deb2")+
  geom_rect(xmin=1.5,xmax=4.5, ymin=0, ymax=3.5, fill="#9bd4e5")+
  geom_rect(xmin=4.5,xmax=9.5, ymin=0, ymax=3.5, fill="#d2c3e0")+
  geom_hline( aes(yintercept = y), 
              data.frame(y = c(0:5)),
              color = "grey") +
  geom_col(aes(x = RiskFactor,y = value, fill = quintile),
           position = position_dodge(width = 0.5),
           show.legend = TRUE,
           alpha = 0.8)+
  geom_vline(xintercept = pto_corte, col = "black", linetype = "dotted", size = 0.25)+ 
  scale_y_continuous(
    limits = c(-1.5, 9))+
  geom_errorbar( aes(x=RiskFactor, ymin=LCI, ymax=UCI, colour=quintile), position = "dodge2",
                 width=0.4, alpha=1, size=.3)+
  scale_color_manual(values = palette_fleni)+
  coord_polar()+
  theme_void()+  theme(
    # Remove axis ticks and text
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    #axis.text.y = element_blank(),
    # Use gray text for the region names
    axis.text.x = element_text(color = "gray12", size = 12),
    # Move the legend to the bottom
    legend.position = "bottom")
