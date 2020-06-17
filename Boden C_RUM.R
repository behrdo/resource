library(tidyr)
library(dplyr)
library(ggplot2)

#Thema erstellen
Thema_soil <- theme(panel.background = element_rect(fill = "white"),
                     axis.line = element_line())

#Vorbereitungen------------
#ursprüngliche Datentabelle einlesen
Rohdaten_soil <- read.csv2("soil data C.csv")

Rohdaten_soil$depth<-as.character(Rohdaten_soil$depth)

#Datenverarbeitung--------------

#Durchschnitt und Standardabweichung pro Parzelle und Tiefe und Sampling date
Durchschnitt_pro_Parzelle <- Rohdaten_soil %>%
  group_by(date, fieldrep,treatment,depth_class) %>%
  summarise(mean_Parameter=mean(K_kgha, na.rm=TRUE), SD_Parameter=sd(K_kgha, na.rm=TRUE))

Durchschnitt_pro_Parzelle$fieldrep<-as.character(Durchschnitt_pro_Parzelle$fieldrep)
Durchschnitt_pro_Parzelle$treatment<-as.character(Durchschnitt_pro_Parzelle$treatment)

#Durchschnitt und Standardabweichung Variante je Sampling date und Tiefe
Durchschnitt_Variante <- Durchschnitt_pro_Parzelle %>%
  group_by(date, treatment, depth_class) %>% 
  summarize(Anzahl_Parzellen=length(mean_Parameter),mean_Parameter_treat=mean(mean_Parameter, na.rm=TRUE), SD_Parameter_treat=sd(mean_Parameter, na.rm=TRUE))

Durchschnitt_Variante$treatment<-as.character(Durchschnitt_Variante$treatment)

#Abbildungen-----------

#Auswählen von Daten für die Abbildungen

Durchschnitt_Variante_date <-
  Durchschnitt_Variante[Durchschnitt_Variante$date == 4,]

Farben_Varianten <- c("3"="chartreuse3", "2"="cyan3", "1"="royalblue3","4"="royalblue4", "5"="cyan4", "6"="chartreuse4")

#Abstände und Zeiträume im Vergleich
#pro Variante der Durchschnitt der vier Parzellen dargestellt 
tiff("soil.tiff",
     width=10,height=10,unit="cm",res=200)
ggplot(Durchschnitt_Variante_date [(Durchschnitt_Variante_date$treatment == "1"| 
                                        Durchschnitt_Variante_date$treatment == "2"|
                                        Durchschnitt_Variante_date$treatment == "3"|
                                        Durchschnitt_Variante_date$treatment == "4"|
                                        Durchschnitt_Variante_date$treatment == "5"|
                                        Durchschnitt_Variante_date$treatment == "6"),], 
       aes(x=depth_class,y=mean_Parameter_treat, fill=treatment))+
  geom_bar (stat="identity",position="dodge", color="black", size=0.1)+
  ylab("K (kg*ha-1)")+
  xlab("soil depth (cm)")+
  Thema_soil+
  scale_y_continuous(position = "right", breaks = c(0, 500, 1000, 1500, 2000, 2500, 3000),
                     labels = c("0", "500", "1000", "1500", "2000", "2500", "3000"),
                     limits=c(0,3000))+
  geom_errorbar(aes(ymin=mean_Parameter_treat-SD_Parameter_treat,
                    ymax=mean_Parameter_treat+SD_Parameter_treat),position=position_dodge(width=0.9))+  
  scale_x_reverse(breaks=c(1,2,3,4), labels=c("0-30", "30-45", "45-75", "75-105"))+
  coord_flip()+
  scale_color_manual(values = Farben_Varianten, aesthetics = "fill", labels=c("Lu1", "Ch1", "Fe1", "Lu2", "Ch2", "Fe2"))+
  theme(axis.text.x = element_text(size = 10),
        axis.ticks.x=element_line(size=0.5),
        legend.title=element_blank(),
        legend.position="right",
        legend.text = element_text(size = 12),
        legend.spacing.x = unit(0.2, 'cm'),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 10))
dev.off()


#für ANOVA----------------------

Daten_aov <- Rohdaten_soil[Rohdaten_soil$date==1,]

Daten_aov_depth <- Daten_aov[Daten_aov$depth_class=="1",] %>%
  group_by(fieldrep,treatment)%>%
  summarise (mean_Parameter=mean(K_mgkg))

Daten_aov_Var <- Daten_aov_depth[(Daten_aov_depth$treatment=="1"|
                              Daten_aov_depth$treatment=="2"|
                              Daten_aov_depth$treatment=="3"|
                              Daten_aov_depth$treatment=="4"|
                              Daten_aov_depth$treatment=="5"|
                              Daten_aov_depth$treatment=="6"),]


#ANOVA einfaktoriell

library("agricolae")

Daten_aov_Var$treatment <- as.factor (Daten_aov_Var$treatment)
Daten_aov_Var$fieldrep <- as.factor (Daten_aov_Var$fieldrep)

AOV_Daten_aov <- aov(mean_Parameter ~ treatment + (fieldrep), data = Daten_aov_Var)
Tukey_Daten_aov <-HSD.test(AOV_Daten_aov, "treatment",
                           group = TRUE,
                           console = TRUE)

plot(fitted(AOV_Daten_aov), resid(AOV_Daten_aov))
qqnorm(residuals(AOV_Daten_aov))
qqline(residuals(AOV_Daten_aov))
summary(AOV_Daten_aov)


