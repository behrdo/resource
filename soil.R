library(tidyverse)
library(readxl)
library(gridExtra)
library(ggpubr)

#rohdaten soil
Rohdaten_soil <- read.csv2("soil data C.csv")

#Thema
Thema_soil <- theme(panel.background = element_rect(fill = "white"),
                    axis.line = element_line())

#K
#Durchschnitt und Standardabweichung pro Parzelle und Tiefe und Sampling date
Durchschnitt_pro_Parzelle <- Rohdaten_soil %>%
  group_by(date, fieldrep, treatment, depth_class) %>%
  summarise(mean_Parameter = mean(K_mgkg, na.rm=TRUE), SD_Parameter = sd(K_mgkg, na.rm=TRUE))

Durchschnitt_pro_Parzelle$fieldrep<-as.character(Durchschnitt_pro_Parzelle$fieldrep)
Durchschnitt_pro_Parzelle$treatment<-as.character(Durchschnitt_pro_Parzelle$treatment)

#Durchschnitt und Standardabweichung Variante je Sampling date und Tiefe
Durchschnitt_Variante <- Durchschnitt_pro_Parzelle %>%
  group_by(date, treatment, depth_class) %>% 
  summarize(Anzahl_Parzellen=length(mean_Parameter),mean_Parameter_treat=mean(mean_Parameter, na.rm=TRUE), SD_Parameter_treat=sd(mean_Parameter, na.rm=TRUE))

Durchschnitt_Variante$treatment<-as.character(Durchschnitt_Variante$treatment)

Farben_Varianten <- c("5"="steelblue4", "6"="steelblue2")

Durchschnitt_Variante_date <-
  Durchschnitt_Variante[Durchschnitt_Variante$date == 1,]

a <- ggplot(Durchschnitt_Variante_date [(  Durchschnitt_Variante_date$treatment == "5"|
                                      Durchschnitt_Variante_date$treatment == "6"),], 
       aes(x=depth_class,y=mean_Parameter_treat, fill=treatment))+
  geom_bar (stat="identity",position="dodge", color="black", size=0.1)+
  ylab(bquote("Mean K [kg *" ~ha^-1 ~"]"))+
  xlab("soil depth (cm)")+
  Thema_soil+
  scale_y_continuous(position = "right", breaks = c(0, 100, 200, 300, 400, 500),
                     labels = c("0", "100", "200", "300", "400", "500"),
                     limits=c(0,500))+
  geom_errorbar(aes(ymin=mean_Parameter_treat-SD_Parameter_treat,
                    ymax=mean_Parameter_treat+SD_Parameter_treat),position=position_dodge(width=0.9))+  
  scale_x_reverse(breaks=c(1,2,3,4), labels=c("0-30", "30-45", "45-75", "75-105"))+
  coord_flip()+
  ggtitle("(A)") +
  scale_color_manual(values = Farben_Varianten, aesthetics = "fill", labels=c("RS2", "WW2"))+
  theme(axis.text.x = element_text(size = 10),
        axis.ticks.x=element_line(size=0.5),
        legend.title=element_blank(),
        legend.position="right",
        legend.text = element_text(size = 12),
        legend.spacing.x = unit(0.2, 'cm'),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 15, vjust = -115, hjust = 0.99))
a

Durchschnitt_Variante_date <-
  Durchschnitt_Variante[Durchschnitt_Variante$date == 2,]

b <- ggplot(Durchschnitt_Variante_date [(  Durchschnitt_Variante_date$treatment == "5"|
                                             Durchschnitt_Variante_date$treatment == "6"),], 
            aes(x=depth_class,y=mean_Parameter_treat, fill=treatment))+
  geom_bar (stat="identity",position="dodge", color="black", size=0.1)+
  ylab(bquote("Mean K [kg *" ~ha^-1 ~"]"))+
  xlab("soil depth (cm)")+
  Thema_soil+
  scale_y_continuous(position = "right", breaks = c(0, 100, 200, 300, 400, 500),
                     labels = c("0", "100", "200", "300", "400", "500"),
                     limits=c(0,500))+
  geom_errorbar(aes(ymin=mean_Parameter_treat-SD_Parameter_treat,
                    ymax=mean_Parameter_treat+SD_Parameter_treat),position=position_dodge(width=0.9))+  
  scale_x_reverse(breaks=c(1,2,3,4), labels=c("0-30", "30-45", "45-75", "75-105"))+
  coord_flip()+
  ggtitle("(B)") +
  scale_color_manual(values = Farben_Varianten, aesthetics = "fill", labels=c("RS2", "WW2"))+
  theme(axis.text.x = element_text(size = 10),
        axis.ticks.x=element_line(size=0.5),
        legend.title=element_blank(),
        legend.position="right",
        legend.text = element_text(size = 12),
        legend.spacing.x = unit(0.2, 'cm'),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 15, vjust = -115, hjust = 0.99))
b

Durchschnitt_Variante_date <-
  Durchschnitt_Variante[Durchschnitt_Variante$date == 3,]

c <- ggplot(Durchschnitt_Variante_date [(  Durchschnitt_Variante_date$treatment == "5"|
                                             Durchschnitt_Variante_date$treatment == "6"),], 
            aes(x=depth_class,y=mean_Parameter_treat, fill=treatment))+
  geom_bar (stat="identity",position="dodge", color="black", size=0.1)+
  ylab(bquote("Mean K [kg *" ~ha^-1 ~"]"))+
  xlab("soil depth (cm)")+
  Thema_soil+
  scale_y_continuous(position = "right", breaks = c(0, 100, 200, 300, 400, 500),
                     labels = c("0", "100", "200", "300", "400", "500"),
                     limits=c(0,500))+
  geom_errorbar(aes(ymin=mean_Parameter_treat-SD_Parameter_treat,
                    ymax=mean_Parameter_treat+SD_Parameter_treat),position=position_dodge(width=0.9))+  
  scale_x_reverse(breaks=c(1,2,3,4), labels=c("0-30", "30-45", "45-75", "75-105"))+
  coord_flip()+
  ggtitle("(C)") +
  scale_color_manual(values = Farben_Varianten, aesthetics = "fill", labels=c("RS2", "WW2"))+
  theme(axis.text.x = element_text(size = 10),
        axis.ticks.x=element_line(size=0.5),
        legend.title=element_blank(),
        legend.position="right",
        legend.text = element_text(size = 12),
        legend.spacing.x = unit(0.2, 'cm'),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 15, vjust = -115, hjust = 0.99))
c

Durchschnitt_Variante_date <-
  Durchschnitt_Variante[Durchschnitt_Variante$date == 4,]

d <- ggplot(Durchschnitt_Variante_date [(  Durchschnitt_Variante_date$treatment == "5"|
                                             Durchschnitt_Variante_date$treatment == "6"),], 
            aes(x=depth_class,y=mean_Parameter_treat, fill=treatment))+
  geom_bar (stat="identity",position="dodge", color="black", size=0.1)+
  ylab(bquote("Mean K [kg *" ~ha^-1 ~"]"))+
  xlab("soil depth (cm)")+
  Thema_soil+
  scale_y_continuous(position = "right", breaks = c(0, 100, 200, 300, 400, 500),
                     labels = c("0", "100", "200", "300", "400", "500"),
                     limits=c(0,500))+
  geom_errorbar(aes(ymin=mean_Parameter_treat-SD_Parameter_treat,
                    ymax=mean_Parameter_treat+SD_Parameter_treat),position=position_dodge(width=0.9))+  
  scale_x_reverse(breaks=c(1,2,3,4), labels=c("0-30", "30-45", "45-75", "75-105"))+
  coord_flip()+
  ggtitle("(D)") +
  scale_color_manual(values = Farben_Varianten, aesthetics = "fill", labels=c("RS2", "WW2"))+
  theme(axis.text.x = element_text(size = 10),
        axis.ticks.x=element_line(size=0.5),
        legend.title=element_blank(),
        legend.position="right",
        legend.text = element_text(size = 12),
        legend.spacing.x = unit(0.2, 'cm'),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 15, vjust = -115, hjust = 0.99))
d

Durchschnitt_Variante_date <-
  Durchschnitt_Variante[Durchschnitt_Variante$date == 4,]

e <- ggplot(Durchschnitt_Variante_date [(  Durchschnitt_Variante_date$treatment == "5"|
                                             Durchschnitt_Variante_date$treatment == "6"),], 
            aes(x=depth_class,y=mean_Parameter_treat, fill=treatment))+
  geom_bar (stat="identity",position="dodge", color="black", size=0.1)+
  ylab(bquote("Mean K [kg *" ~ha^-1 ~"]"))+
  xlab("soil depth (cm)")+
  Thema_soil+
  scale_y_continuous(position = "right", breaks = c(0, 100, 200, 300, 400, 500),
                     labels = c("0", "100", "200", "300", "400", "500"),
                     limits=c(0,500))+
  geom_errorbar(aes(ymin=mean_Parameter_treat-SD_Parameter_treat,
                    ymax=mean_Parameter_treat+SD_Parameter_treat),position=position_dodge(width=0.9))+  
  scale_x_reverse(breaks=c(1,2,3,4), labels=c("0-30", "30-45", "45-75", "75-105"))+
  coord_flip()+
  ggtitle("(E)") +
  scale_color_manual(values = Farben_Varianten, aesthetics = "fill", labels=c("RS2", "WW2"))+
  theme(axis.text.x = element_text(size = 10),
        axis.ticks.x=element_line(size=0.5),
        legend.title=element_blank(),
        legend.position="right",
        legend.text = element_text(size = 12),
        legend.spacing.x = unit(0.2, 'cm'),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 15, vjust = -115, hjust = 0.99))
e

K <- ggarrange(a, b, c, d, ncol = 4, nrow = 1, common.legend = TRUE, legend = "bottom")
K

#C
#Durchschnitt und Standardabweichung pro Parzelle und Tiefe und Sampling date
Durchschnitt_pro_Parzelle <- Rohdaten_soil %>%
  group_by(date, fieldrep, treatment, depth_class) %>%
  summarise(mean_Parameter = mean(C_proz, na.rm=TRUE), SD_Parameter = sd(C_proz, na.rm=TRUE))

Durchschnitt_pro_Parzelle$fieldrep<-as.character(Durchschnitt_pro_Parzelle$fieldrep)
Durchschnitt_pro_Parzelle$treatment<-as.character(Durchschnitt_pro_Parzelle$treatment)

#Durchschnitt und Standardabweichung Variante je Sampling date und Tiefe
Durchschnitt_Variante <- Durchschnitt_pro_Parzelle %>%
  group_by(date, treatment, depth_class) %>% 
  summarize(Anzahl_Parzellen=length(mean_Parameter),mean_Parameter_treat=mean(mean_Parameter, na.rm=TRUE), SD_Parameter_treat=sd(mean_Parameter, na.rm=TRUE))

Durchschnitt_Variante$treatment<-as.character(Durchschnitt_Variante$treatment)

Farben_Varianten <- c("5"="steelblue4", "6"="steelblue2")

Durchschnitt_Variante_date <-
  Durchschnitt_Variante[Durchschnitt_Variante$date == 1,]

e <- ggplot(Durchschnitt_Variante_date [(  Durchschnitt_Variante_date$treatment == "5"|
                                             Durchschnitt_Variante_date$treatment == "6"),], 
            aes(x=depth_class,y=mean_Parameter_treat, fill=treatment))+
  geom_bar (stat="identity",position="dodge", color="black", size=0.1)+
  ylab(bquote("Mean C [kg *" ~ha^-1 ~"]"))+
  xlab("soil depth (cm)")+
  Thema_soil+
  scale_y_continuous(position = "right", breaks = c(0, 0.3, 0.6, 0.9, 1.2, 1.5),
                     labels = c("0", "0.3", "0.6", "0.9", "1.2", "1.5"),
                     limits=c(0,1.6))+
  geom_errorbar(aes(ymin=mean_Parameter_treat-SD_Parameter_treat,
                    ymax=mean_Parameter_treat+SD_Parameter_treat),position=position_dodge(width=0.9))+  
  scale_x_reverse(breaks=c(1,2,3,4), labels=c("0-30", "30-45", "45-75", "75-105"))+
  coord_flip()+
  ggtitle("(E)") +
  scale_color_manual(values = Farben_Varianten, aesthetics = "fill", labels=c("RS2", "WW2"))+
  theme(axis.text.x = element_text(size = 10),
        axis.ticks.x=element_line(size=0.5),
        legend.title=element_blank(),
        legend.position="right",
        legend.text = element_text(size = 12),
        legend.spacing.x = unit(0.2, 'cm'),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 15, vjust = -115, hjust = 0.99))
e

Durchschnitt_Variante_date <-
  Durchschnitt_Variante[Durchschnitt_Variante$date == 2,]

f <- ggplot(Durchschnitt_Variante_date [(  Durchschnitt_Variante_date$treatment == "5"|
                                             Durchschnitt_Variante_date$treatment == "6"),], 
            aes(x=depth_class,y=mean_Parameter_treat, fill=treatment))+
  geom_bar (stat="identity",position="dodge", color="black", size=0.1)+
  ylab(bquote("Mean C [kg *" ~ha^-1 ~"]"))+
  xlab("soil depth (cm)")+
  Thema_soil+
  scale_y_continuous(position = "right", breaks = c(0, 0.3, 0.6, 0.9, 1.2, 1.5),
                     labels = c("0", "0.3", "0.6", "0.9", "1.2", "1.5"),
                     limits=c(0,1.6))+
  geom_errorbar(aes(ymin=mean_Parameter_treat-SD_Parameter_treat,
                    ymax=mean_Parameter_treat+SD_Parameter_treat),position=position_dodge(width=0.9))+  
  scale_x_reverse(breaks=c(1,2,3,4), labels=c("0-30", "30-45", "45-75", "75-105"))+
  coord_flip()+
  ggtitle("(F)") +
  scale_color_manual(values = Farben_Varianten, aesthetics = "fill", labels=c("RS2", "WW2"))+
  theme(axis.text.x = element_text(size = 10),
        axis.ticks.x=element_line(size=0.5),
        legend.title=element_blank(),
        legend.position="right",
        legend.text = element_text(size = 12),
        legend.spacing.x = unit(0.2, 'cm'),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 15, vjust = -115, hjust = 0.99))
f

Durchschnitt_Variante_date <-
  Durchschnitt_Variante[Durchschnitt_Variante$date == 3,]

g <- ggplot(Durchschnitt_Variante_date [(  Durchschnitt_Variante_date$treatment == "5"|
                                             Durchschnitt_Variante_date$treatment == "6"),], 
            aes(x=depth_class,y=mean_Parameter_treat, fill=treatment))+
  geom_bar (stat="identity",position="dodge", color="black", size=0.1)+
  ylab(bquote("Mean C [kg *" ~ha^-1 ~"]"))+
  xlab("soil depth (cm)")+
  Thema_soil+
  scale_y_continuous(position = "right", breaks = c(0, 0.3, 0.6, 0.9, 1.2, 1.5),
                     labels = c("0", "0.3", "0.6", "0.9", "1.2", "1.5"),
                     limits=c(0,1.6))+
  geom_errorbar(aes(ymin=mean_Parameter_treat-SD_Parameter_treat,
                    ymax=mean_Parameter_treat+SD_Parameter_treat),position=position_dodge(width=0.9))+  
  scale_x_reverse(breaks=c(1,2,3,4), labels=c("0-30", "30-45", "45-75", "75-105"))+
  coord_flip()+
  ggtitle("(G)") +
  scale_color_manual(values = Farben_Varianten, aesthetics = "fill", labels=c("RS2", "WW2"))+
  theme(axis.text.x = element_text(size = 10),
        axis.ticks.x=element_line(size=0.5),
        legend.title=element_blank(),
        legend.position="right",
        legend.text = element_text(size = 12),
        legend.spacing.x = unit(0.2, 'cm'),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 15, vjust = -115, hjust = 0.99))
g

Durchschnitt_Variante_date <-
  Durchschnitt_Variante[Durchschnitt_Variante$date == 4,]

h <- ggplot(Durchschnitt_Variante_date [(  Durchschnitt_Variante_date$treatment == "5"|
                                             Durchschnitt_Variante_date$treatment == "6"),], 
            aes(x=depth_class,y=mean_Parameter_treat, fill=treatment))+
  geom_bar (stat="identity",position="dodge", color="black", size=0.1)+
  ylab(bquote("Mean C [kg *" ~ha^-1 ~"]"))+
  xlab("soil depth (cm)")+
  Thema_soil+
  scale_y_continuous(position = "right", breaks = c(0, 0.3, 0.6, 0.9, 1.2, 1.5),
                     labels = c("0", "0.3", "0.6", "0.9", "1.2", "1.5"),
                     limits=c(0,1.6))+
  geom_errorbar(aes(ymin=mean_Parameter_treat-SD_Parameter_treat,
                    ymax=mean_Parameter_treat+SD_Parameter_treat),position=position_dodge(width=0.9))+  
  scale_x_reverse(breaks=c(1,2,3,4), labels=c("0-30", "30-45", "45-75", "75-105"))+
  coord_flip()+
  ggtitle("(H)") +
  scale_color_manual(values = Farben_Varianten, aesthetics = "fill", labels=c("RS2", "WW2"))+
  theme(axis.text.x = element_text(size = 10),
        axis.ticks.x=element_line(size=0.5),
        legend.title=element_blank(),
        legend.position="right",
        legend.text = element_text(size = 12),
        legend.spacing.x = unit(0.2, 'cm'),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 15, vjust = -115, hjust = 0.99))
h

C <- ggarrange(e, f, g, h, ncol = 4, nrow = 1, common.legend = TRUE, legend = "bottom")
C

#N
#Durchschnitt und Standardabweichung pro Parzelle und Tiefe und Sampling date
Durchschnitt_pro_Parzelle <- Rohdaten_soil %>%
  group_by(date, fieldrep, treatment, depth_class) %>%
  summarise(mean_Parameter = mean(N_proz, na.rm=TRUE), SD_Parameter = sd(N_proz, na.rm=TRUE))

Durchschnitt_pro_Parzelle$fieldrep<-as.character(Durchschnitt_pro_Parzelle$fieldrep)
Durchschnitt_pro_Parzelle$treatment<-as.character(Durchschnitt_pro_Parzelle$treatment)

#Durchschnitt und Standardabweichung Variante je Sampling date und Tiefe
Durchschnitt_Variante <- Durchschnitt_pro_Parzelle %>%
  group_by(date, treatment, depth_class) %>% 
  summarize(Anzahl_Parzellen=length(mean_Parameter),mean_Parameter_treat=mean(mean_Parameter, na.rm=TRUE), SD_Parameter_treat=sd(mean_Parameter, na.rm=TRUE))

Durchschnitt_Variante$treatment<-as.character(Durchschnitt_Variante$treatment)

Farben_Varianten <- c("5"="steelblue4", "6"="steelblue2")

Durchschnitt_Variante_date <-
  Durchschnitt_Variante[Durchschnitt_Variante$date == 1,]

i <- ggplot(Durchschnitt_Variante_date [(  Durchschnitt_Variante_date$treatment == "5"|
                                             Durchschnitt_Variante_date$treatment == "6"),], 
            aes(x=depth_class,y=mean_Parameter_treat, fill=treatment))+
  geom_bar (stat="identity",position="dodge", color="black", size=0.1)+
  ylab(bquote("Mean N [kg *" ~ha^-1 ~"]"))+
  xlab("soil depth (cm)")+
  Thema_soil+
  scale_y_continuous(position = "right", breaks = c(0, 0.03, 0.06, 0.09, 0.12, 0.15),
                     labels = c("0", "0.03", "0.06", "0.09", "0.12", "0.15"),
                     limits=c(0,0.15))+
  geom_errorbar(aes(ymin=mean_Parameter_treat-SD_Parameter_treat,
                    ymax=mean_Parameter_treat+SD_Parameter_treat),position=position_dodge(width=0.9))+  
  scale_x_reverse(breaks=c(1,2,3,4), labels=c("0-30", "30-45", "45-75", "75-105"))+
  coord_flip()+
  ggtitle("(I)") +
  scale_color_manual(values = Farben_Varianten, aesthetics = "fill", labels=c("RS2", "WW2"))+
  theme(axis.text.x = element_text(size = 10),
        axis.ticks.x=element_line(size=0.5),
        legend.title=element_blank(),
        legend.position="right",
        legend.text = element_text(size = 12),
        legend.spacing.x = unit(0.2, 'cm'),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 15, vjust = -115, hjust = 0.99))
i

Durchschnitt_Variante_date <-
  Durchschnitt_Variante[Durchschnitt_Variante$date == 2,]

j <- ggplot(Durchschnitt_Variante_date [(  Durchschnitt_Variante_date$treatment == "5"|
                                             Durchschnitt_Variante_date$treatment == "6"),], 
            aes(x=depth_class,y=mean_Parameter_treat, fill=treatment))+
  geom_bar (stat="identity",position="dodge", color="black", size=0.1)+
  ylab(bquote("Mean N [kg *" ~ha^-1 ~"]"))+
  xlab("soil depth (cm)")+
  Thema_soil+
  scale_y_continuous(position = "right", breaks = c(0, 0.03, 0.06, 0.09, 0.12, 0.15),
                     labels = c("0", "0.03", "0.06", "0.09", "0.12", "0.15"),
                     limits=c(0,0.15))+
  geom_errorbar(aes(ymin=mean_Parameter_treat-SD_Parameter_treat,
                    ymax=mean_Parameter_treat+SD_Parameter_treat),position=position_dodge(width=0.9))+  
  scale_x_reverse(breaks=c(1,2,3,4), labels=c("0-30", "30-45", "45-75", "75-105"))+
  coord_flip()+
  ggtitle("(J)") +
  scale_color_manual(values = Farben_Varianten, aesthetics = "fill", labels=c("RS2", "WW2"))+
  theme(axis.text.x = element_text(size = 10),
        axis.ticks.x=element_line(size=0.5),
        legend.title=element_blank(),
        legend.position="right",
        legend.text = element_text(size = 12),
        legend.spacing.x = unit(0.2, 'cm'),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 15, vjust = -115, hjust = 0.99))
j

Durchschnitt_Variante_date <-
  Durchschnitt_Variante[Durchschnitt_Variante$date == 3,]

k <- ggplot(Durchschnitt_Variante_date [(  Durchschnitt_Variante_date$treatment == "5"|
                                             Durchschnitt_Variante_date$treatment == "6"),], 
            aes(x=depth_class,y=mean_Parameter_treat, fill=treatment))+
  geom_bar (stat="identity",position="dodge", color="black", size=0.1)+
  ylab(bquote("Mean N [kg *" ~ha^-1 ~"]"))+
  xlab("soil depth (cm)")+
  Thema_soil+
  scale_y_continuous(position = "right", breaks = c(0, 0.03, 0.06, 0.09, 0.12, 0.15),
                     labels = c("0", "0.03", "0.06", "0.09", "0.12", "0.15"),
                     limits=c(0,0.15))+
  geom_errorbar(aes(ymin=mean_Parameter_treat-SD_Parameter_treat,
                    ymax=mean_Parameter_treat+SD_Parameter_treat),position=position_dodge(width=0.9))+  
  scale_x_reverse(breaks=c(1,2,3,4), labels=c("0-30", "30-45", "45-75", "75-105"))+
  coord_flip()+
  ggtitle("(K)") +
  scale_color_manual(values = Farben_Varianten, aesthetics = "fill", labels=c("RS2", "WW2"))+
  theme(axis.text.x = element_text(size = 10),
        axis.ticks.x=element_line(size=0.5),
        legend.title=element_blank(),
        legend.position="right",
        legend.text = element_text(size = 12),
        legend.spacing.x = unit(0.2, 'cm'),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 15, vjust = -115, hjust = 0.99))
k

Durchschnitt_Variante_date <-
  Durchschnitt_Variante[Durchschnitt_Variante$date == 4,]

l <- ggplot(Durchschnitt_Variante_date [(  Durchschnitt_Variante_date$treatment == "5"|
                                             Durchschnitt_Variante_date$treatment == "6"),], 
            aes(x=depth_class,y=mean_Parameter_treat, fill=treatment))+
  geom_bar (stat="identity",position="dodge", color="black", size=0.1)+
  ylab(bquote("Mean N [kg *" ~ha^-1 ~"]"))+
  xlab("soil depth (cm)")+
  Thema_soil+
  scale_y_continuous(position = "right", breaks = c(0, 0.03, 0.06, 0.09, 0.12, 0.15),
                     labels = c("0", "0.03", "0.06", "0.09", "0.12", "0.15"),
                     limits=c(0,0.15))+
  geom_errorbar(aes(ymin=mean_Parameter_treat-SD_Parameter_treat,
                    ymax=mean_Parameter_treat+SD_Parameter_treat),position=position_dodge(width=0.9))+  
  scale_x_reverse(breaks=c(1,2,3,4), labels=c("0-30", "30-45", "45-75", "75-105"))+
  coord_flip()+
  ggtitle("(L)") +
  scale_color_manual(values = Farben_Varianten, aesthetics = "fill", labels=c("RS2", "WW2"))+
  theme(axis.text.x = element_text(size = 10),
        axis.ticks.x=element_line(size=0.5),
        legend.title=element_blank(),
        legend.position="right",
        legend.text = element_text(size = 12),
        legend.spacing.x = unit(0.2, 'cm'),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 15, vjust = -115, hjust = 0.99))
l

N <- ggarrange(i, j, k, l, ncol = 4, nrow = 1, common.legend = TRUE, legend = "bottom")
N


#P
#Durchschnitt und Standardabweichung pro Parzelle und Tiefe und Sampling date
Durchschnitt_pro_Parzelle <- Rohdaten_soil %>%
  group_by(date, fieldrep, treatment, depth_class) %>%
  summarise(mean_Parameter = mean(P_mgkg, na.rm=TRUE), SD_Parameter = sd(P_mgkg, na.rm=TRUE))

Durchschnitt_pro_Parzelle$fieldrep<-as.character(Durchschnitt_pro_Parzelle$fieldrep)
Durchschnitt_pro_Parzelle$treatment<-as.character(Durchschnitt_pro_Parzelle$treatment)

#Durchschnitt und Standardabweichung Variante je Sampling date und Tiefe
Durchschnitt_Variante <- Durchschnitt_pro_Parzelle %>%
  group_by(date, treatment, depth_class) %>% 
  summarize(Anzahl_Parzellen=length(mean_Parameter),mean_Parameter_treat=mean(mean_Parameter, na.rm=TRUE), SD_Parameter_treat=sd(mean_Parameter, na.rm=TRUE))

Durchschnitt_Variante$treatment<-as.character(Durchschnitt_Variante$treatment)

Farben_Varianten <- c("5"="steelblue4", "6"="steelblue2")

Durchschnitt_Variante_date <-
  Durchschnitt_Variante[Durchschnitt_Variante$date == 1,]

m <- ggplot(Durchschnitt_Variante_date [(  Durchschnitt_Variante_date$treatment == "5"|
                                             Durchschnitt_Variante_date$treatment == "6"),], 
            aes(x=depth_class,y=mean_Parameter_treat, fill=treatment))+
  geom_bar (stat="identity",position="dodge", color="black", size=0.1)+
  ylab(bquote("Mean P [kg *" ~ha^-1 ~"]"))+
  xlab("soil depth (cm)")+
  Thema_soil+
  scale_y_continuous(position = "right", breaks = c(0, 30, 60, 90, 120, 150),
                     labels = c("0", "30", "60", "90", "120", "150"),
                     limits=c(0,160))+
  geom_errorbar(aes(ymin=mean_Parameter_treat-SD_Parameter_treat,
                    ymax=mean_Parameter_treat+SD_Parameter_treat),position=position_dodge(width=0.9))+  
  scale_x_reverse(breaks=c(1,2,3,4), labels=c("0-30", "30-45", "45-75", "75-105"))+
  coord_flip()+
  ggtitle("(M)") +
  scale_color_manual(values = Farben_Varianten, aesthetics = "fill", labels=c("RS2", "WW2"))+
  theme(axis.text.x = element_text(size = 10),
        axis.ticks.x=element_line(size=0.5),
        legend.title=element_blank(),
        legend.position="right",
        legend.text = element_text(size = 12),
        legend.spacing.x = unit(0.2, 'cm'),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 15, vjust = -115, hjust = 0.99))
m

Durchschnitt_Variante_date <-
  Durchschnitt_Variante[Durchschnitt_Variante$date == 2,]

n <- ggplot(Durchschnitt_Variante_date [(  Durchschnitt_Variante_date$treatment == "5"|
                                             Durchschnitt_Variante_date$treatment == "6"),], 
            aes(x=depth_class,y=mean_Parameter_treat, fill=treatment))+
  geom_bar (stat="identity",position="dodge", color="black", size=0.1)+
  ylab(bquote("Mean P [kg *" ~ha^-1 ~"]"))+
  xlab("soil depth (cm)")+
  Thema_soil+
  scale_y_continuous(position = "right", breaks = c(0, 30, 60, 90, 120, 150),
                     labels = c("0", "30", "60", "90", "120", "150"),
                     limits=c(0,160))+
  geom_errorbar(aes(ymin=mean_Parameter_treat-SD_Parameter_treat,
                    ymax=mean_Parameter_treat+SD_Parameter_treat),position=position_dodge(width=0.9))+  
  scale_x_reverse(breaks=c(1,2,3,4), labels=c("0-30", "30-45", "45-75", "75-105"))+
  coord_flip()+
  ggtitle("(N)") +
  scale_color_manual(values = Farben_Varianten, aesthetics = "fill", labels=c("RS2", "WW2"))+
  theme(axis.text.x = element_text(size = 10),
        axis.ticks.x=element_line(size=0.5),
        legend.title=element_blank(),
        legend.position="right",
        legend.text = element_text(size = 12),
        legend.spacing.x = unit(0.2, 'cm'),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 15, vjust = -115, hjust = 0.99))
n

Durchschnitt_Variante_date <-
  Durchschnitt_Variante[Durchschnitt_Variante$date == 3,]

o <- ggplot(Durchschnitt_Variante_date [(  Durchschnitt_Variante_date$treatment == "5"|
                                             Durchschnitt_Variante_date$treatment == "6"),], 
            aes(x=depth_class,y=mean_Parameter_treat, fill=treatment))+
  geom_bar (stat="identity",position="dodge", color="black", size=0.1)+
  ylab(bquote("Mean P [kg *" ~ha^-1 ~"]"))+
  xlab("soil depth (cm)")+
  Thema_soil+
  scale_y_continuous(position = "right", breaks = c(0, 30, 60, 90, 120, 150),
                     labels = c("0", "30", "60", "90", "120", "150"),
                     limits=c(0,160))+
  geom_errorbar(aes(ymin=mean_Parameter_treat-SD_Parameter_treat,
                    ymax=mean_Parameter_treat+SD_Parameter_treat),position=position_dodge(width=0.9))+  
  scale_x_reverse(breaks=c(1,2,3,4), labels=c("0-30", "30-45", "45-75", "75-105"))+
  coord_flip()+
  ggtitle("(O)") +
  scale_color_manual(values = Farben_Varianten, aesthetics = "fill", labels=c("RS2", "WW2"))+
  theme(axis.text.x = element_text(size = 10),
        axis.ticks.x=element_line(size=0.5),
        legend.title=element_blank(),
        legend.position="right",
        legend.text = element_text(size = 12),
        legend.spacing.x = unit(0.2, 'cm'),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 15, vjust = -115, hjust = 0.99))
o

Durchschnitt_Variante_date <-
  Durchschnitt_Variante[Durchschnitt_Variante$date == 4,]

p <- ggplot(Durchschnitt_Variante_date [(  Durchschnitt_Variante_date$treatment == "5"|
                                             Durchschnitt_Variante_date$treatment == "6"),], 
            aes(x=depth_class,y=mean_Parameter_treat, fill=treatment))+
  geom_bar (stat="identity",position="dodge", color="black", size=0.1)+
  ylab(bquote("Mean P [kg *" ~ha^-1 ~"]"))+
  xlab("soil depth (cm)")+
  Thema_soil+
  scale_y_continuous(position = "right", breaks = c(0, 30, 60, 90, 120, 150),
                     labels = c("0", "30", "60", "90", "120", "150"),
                     limits=c(0,160))+
  geom_errorbar(aes(ymin=mean_Parameter_treat-SD_Parameter_treat,
                    ymax=mean_Parameter_treat+SD_Parameter_treat),position=position_dodge(width=0.9))+  
  scale_x_reverse(breaks=c(1,2,3,4), labels=c("0-30", "30-45", "45-75", "75-105"))+
  coord_flip()+
  ggtitle("(P)") +
  scale_color_manual(values = Farben_Varianten, aesthetics = "fill", labels=c("RS2", "WW2"))+
  theme(axis.text.x = element_text(size = 10),
        axis.ticks.x=element_line(size=0.5),
        legend.title=element_blank(),
        legend.position="right",
        legend.text = element_text(size = 12),
        legend.spacing.x = unit(0.2, 'cm'),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 15, vjust = -115, hjust = 0.99))
p

P <- ggarrange(m, n, o, p, ncol = 4, nrow = 1, common.legend = TRUE, legend = "bottom")
P

ggarrange(C, P, N, K, ncol = 1, nrow = 4, common.legend = TRUE, legend = "bottom")












