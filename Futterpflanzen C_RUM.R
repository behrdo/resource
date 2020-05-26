library(tidyverse)
library(readxl)
library(agricolae)

#Thema shoot erstellen
Thema_shoot <- theme(panel.background = element_rect(fill = "white"),
                     axis.line = element_line())

#Vorbereitungen------------
#ursprüngliche Datentabelle einlesen
Rohdaten_shoot <- read.csv2("precrop data C.csv")

#Datenverarbeitung--------------

#Durchschnitt und Standardabweichung pro Parzelle
Durchschnitt_pro_Parzelle <- Rohdaten_shoot %>%
  group_by(fieldrep,treatment) %>%
  summarise(mean_DM=mean(DM_mean_t), SD_TM=sd(DM_mean_t))

Durchschnitt_pro_Parzelle$fieldrep<-as.character(Durchschnitt_pro_Parzelle$fieldrep)
Durchschnitt_pro_Parzelle$treatment<-as.character(Durchschnitt_pro_Parzelle$treatment)

#Durchschnitt und Standardabweichung Variante 
Durchschnitt_Variante <- Durchschnitt_pro_Parzelle %>%
  group_by(treatment) %>% 
  summarize(Anzahl_Parzellen=length(mean_DM),mean_DM_treat=mean(mean_DM), SD_DM_treat=sd(mean_DM))

Durchschnitt_Variante$treatment<-as.character(Durchschnitt_Variante$treatment)

#Abbildungen-----------

Farben_Varianten <- c("3"="chartreuse3", "2"="cyan3", "1"="royalblue3","4"="royalblue4", "5"="cyan4", "6"="chartreuse4")

#Abstände und Zeiträume im Vergleich
#pro Variante der Durchschnitt der drei Parzellen dargestellt 
tiff("shoot.tiff",
     width=10,height=10,unit="cm",res=200)

ggplot(Durchschnitt_Variante [(Durchschnitt_Variante$treatment == "1"| 
                                        Durchschnitt_Variante$treatment == "2"|
                                        Durchschnitt_Variante$treatment == "3"|
                                        Durchschnitt_Variante$treatment == "4"|
                                        Durchschnitt_Variante$treatment == "5"|
                                        Durchschnitt_Variante$treatment == "6"),], 
       aes(x=treatment,y=mean_DM_treat, fill=treatment))+
  #geom_line()+
  #geom_point()+
  geom_bar (stat="identity",position="dodge", color="black", size=0.1)+
  ylab("shoot dry matter [t*ha-1]")+
  xlab("")+
  Thema_shoot+
  scale_y_continuous(position = "right",breaks = c(0, 10, 20),
                     labels = c("0", "10", "20"),
                     limits=c(0,20))+
  geom_errorbar(aes(ymin=mean_DM_treat-SD_DM_treat,
                    ymax=mean_DM_treat+SD_DM_treat),position=position_dodge(width=0.9))+
  scale_color_manual(values = Farben_Varianten, aesthetics = "fill", labels=c("Lu1", "Ch1", "Fe1", "Lu2", "Ch2", "Fe2"))+
  theme(axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        legend.title=element_blank(),
        legend.position="right",
        legend.text = element_text(size = 12),
        legend.spacing.x = unit(0.2, 'cm'),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size=12),
        axis.text.y = element_text(size=12))

dev.off()

#für ANOVA----------------------

Daten_aov <- Rohdaten_shoot %>%
  group_by(fieldrep,treatment)%>%
  summarise (mean_DM=mean(DM_mean_mix))

Daten_aov_Var <- Daten_aov[(Daten_aov$treatment=="1"|
                              Daten_aov$treatment=="2"|
                              Daten_aov$treatment=="3"|
                              Daten_aov$treatment=="4"|
                              Daten_aov$treatment=="5"|
                              Daten_aov$treatment=="6"|
                              Daten_aov$treatment=="7"),]


#ANOVA einfaktoriell
library("agricolae")

Daten_aov_Var$treatment <- as.factor(Daten_aov_Var$treatment)
Daten_aov_Var$fieldrep <- as.factor(Daten_aov_Var$fieldrep)

AOV_Daten_aov <- aov(mean_DM ~ treatment + (fieldrep), data = Daten_aov_Var)
Tukey_Daten_aov <-HSD.test(AOV_Daten_aov, "treatment",
                           group = TRUE,
                           console = TRUE)

plot(fitted(AOV_Daten_aov), resid(AOV_Daten_aov))
qqnorm(residuals(AOV_Daten_aov))
qqline(residuals(AOV_Daten_aov))
summary(AOV_Daten_aov)


#Erwartungswert berechnen

