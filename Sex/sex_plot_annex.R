

#  Date: 24 April 2023                                                          
#                                                                               
#  Input: the outcomes of the file “sex_pipeline”                               
#                                                                               
#  Output: a circular barplot with the comparison of risk factors by sex        
#                                                                               
#  Author: Ismael L Calandri                                                    





load("tabla_masc.RData")
load("tabla_fem.RData")


library(tidyverse)
library(reshape2)

### 2. Grafico =======================================================
base_summary_fem$sex<-"female"
base_summary_masc$sex<-"male"
base_summary_sex<-rbind(base_summary_fem, base_summary_masc)


basesita<-base_summary_sex %>% select(RF,individual_PAF,sex)

basesita$individual_PAF<-basesita$individual_PAF*100

a<-melt(basesita, id.vars = c("RF", "sex"))


basesita<-base_summary_sex %>% select(RF,individual_PAF_LCI,sex)

basesita$individual_PAF_LCI<-basesita$individual_PAF_LCI*100

b<-melt(basesita, id.vars = c("RF", "sex"))

a$LCI<-b$value

basesita<-base_summary_sex %>% select(RF,individual_PAF_UCI,sex)

basesita$individual_PAF_UCI<-basesita$individual_PAF_UCI*100

b<-melt(basesita, id.vars = c("RF", "sex"))

a$UCI<-b$value


a$RiskFactor<-factor(a$RF,
                     levels =c("PAF_education", "PAF_HBP","PAF_alcohol" ,"PAF_obesity", "PAF_smoking","PAF_pinactivity", "PAF_socialisolation", "PAF_DBT", "PAF_air" ),
                     labels = c("Less education", "Hypertension", "Alcohol", "Obesity", "Smoking", "Physical inactivity", "Social isolation", "Diabetes", "Air pollution"))

palette_fleni<-c("#a90123","#004385","#fba31b","#578001", "grey")




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
  geom_col(aes(x = RiskFactor,y = value, fill = sex),
           position = position_dodge(width = 0.5),
           show.legend = TRUE,
           alpha = 1)+
  geom_vline(xintercept = pto_corte, col = "black", linetype = "dotted", size = 0.25)+ 
  scale_y_continuous(
    limits = c(-1.5, 7))+
  geom_errorbar( aes(x=RiskFactor, ymin=LCI, ymax=UCI), position = "dodge2",
                 width=0.4, colour="gray12", alpha=0.9, size=.3)+
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
