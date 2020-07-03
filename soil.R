library(tidyverse)
library(readxl)
library(gridExtra)
library(ggpubr)

Thema_soil <- theme(panel.background = element_rect(fill = "white"),
                    axis.line = element_line())

#rohdaten soil
Rohdaten_soil <- read.csv2("soil data C.csv")

#facet_grid #####
#1. changing the data frame into something i can plot
Rohdaten_soil <- transform(Rohdaten_soil, date = as.factor(date), 
                           treatment = as.factor(treatment), 
                           plot = as.factor(plot), 
                           fieldrep = as.factor(fieldrep), 
                           depth = as.factor(depth), 
                           depth_class = as.factor(depth_class))

dp <- Rohdaten_soil %>%  group_by(date, fieldrep, treatment, depth_class) %>%
  summarize(mean_K = mean(K_mgkg, na.rm = TRUE), mean_C = mean(C_proz, na.rm = TRUE), 
            mean_N = mean(N_proz, na.rm = TRUE), mean_P = mean(P_mgkg, na.rm = TRUE))

q <- filter(dp, treatment == 5)
w <- filter(dp, treatment == 6)

dp <- bind_rows(q, w)

dp$mean_C[is.nan(as.numeric(dp$mean_C))] <- "Na"
dp$mean_N[is.nan(as.numeric(dp$mean_N))] <- "Na"

dp$mean_C<-as.numeric(dp$mean_C)
dp$mean_N<-as.numeric(dp$mean_N)
dp$fieldrep<-as.character(dp$fieldrep)
dp$treatment<-as.character(dp$treatment)

dp <- dp[complete.cases(dp[ , 6:7]),]

dv <- dp %>%  group_by(date, treatment, depth_class) %>% 
  summarize(Anzahl_Parzellen = length(mean_K), 
            mean_k = mean(mean_K, na.rm=TRUE), sd_k = sd(mean_K, na.rm=TRUE),
            mean_c = mean(mean_C, na.rm = TRUE), sd_c = sd(mean_C, na.rm=TRUE),
            mean_n = mean(mean_N, na.rm = TRUE), sd_n = sd(mean_N, na.rm=TRUE),
            mean_p = mean(mean_P, na.rm = TRUE), sd_p = sd(mean_P, na.rm=TRUE))

dv <- dv %>% mutate_at(vars(mean_k), funs(round(., 3)))
dv <- dv %>% mutate_at(vars(mean_n), funs(round(., 3)))
dv <- dv %>% mutate_at(vars(mean_c), funs(round(., 3)))
dv <- dv %>% mutate_at(vars(mean_p), funs(round(., 3)))
dv <- dv %>% mutate_at(vars(sd_k), funs(round(., 3)))
dv <- dv %>% mutate_at(vars(sd_n), funs(round(., 3)))
dv <- dv %>% mutate_at(vars(sd_c), funs(round(., 3)))
dv <- dv %>% mutate_at(vars(sd_p), funs(round(., 3)))

df1 <- gather(dv, "mean_k", "mean_c", "mean_n", "mean_p", key = "nutr", value = "mean_nutr")
df2 <- gather(dv, "sd_k", "sd_c", "sd_n", "sd_p", key = "nutr", value = "sd_nutr")

df1[5:8] <- NULL
df2[5:8] <- NULL

df <- cbind(df1, df2[!names(df2) %in% names(df1)])

df <- transform(df, date = as.character(date))

df$date[df$date == "1"] <- "2014"
df$date[df$date == "2"] <- "2015"
df$date[df$date == "3"] <- "2016"
df$date[df$date == "4"] <- "2017"

df$nutr[df$nutr == "mean_k"] <- "K"
df$nutr[df$nutr == "mean_c"] <- "C"
df$nutr[df$nutr == "mean_n"] <- "N"
df$nutr[df$nutr == "mean_p"] <- "P"

df$treatment<-as.character(df$treatment)
df$date<-as.character(df$date)
df$depth_class<-as.numeric(df$depth_class)

df_k <- filter(df, nutr == "K")
df_c <- filter(df, nutr == "C")
df_n <- filter(df, nutr == "N")
df_p <- filter(df, nutr == "P")

#2. ploting 
Farben_Varianten <- c("5" = "red3", "6" = "#0072B2")

w <- ggplot(df_k, aes(x = depth_class, y = mean_nutr, fill = treatment))+
  geom_bar(stat="identity", position="dodge", color="black", size=0.1)+
  ylab(bquote("Mean K [kg *" ~ha^-1 ~"]"))+
  xlab("Soil Depth (cm)")+
  Thema_soil +
  facet_grid(rows = vars(date))+
  geom_errorbar(aes(ymin = mean_nutr-sd_nutr, ymax = mean_nutr+sd_nutr), 
                position=position_dodge(width=0.9)) +  
  scale_x_reverse(breaks=c(1,2,3,4), labels=c("0-30", "30-45", "45-75", "75-105"))+
  scale_y_continuous(position = "right", breaks = c(0, 100, 200, 300, 400, 500),
                     labels = c("0", "100", "200", "300", "400", "500"),
                     limits=c(0,500))+
  coord_flip() +
  scale_color_manual(values = Farben_Varianten, aesthetics = "fill", labels=c("Ch", "Fe"))+
  theme(axis.text.x = element_text(size = 12),
        axis.ticks.x = element_line(size=0.5),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 13),
        legend.spacing.x = unit(0.2, "cm"),
        axis.title.x = element_text(size = 14),
        strip.text.y = element_text(size = 13), 
        strip.text.x = element_text(size = 13),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 15, vjust = -115, hjust = 0.99))
w
  
x <- ggplot(df_c, aes(x = depth_class, y = mean_nutr, fill = treatment)) +
  geom_bar(stat="identity", position="dodge", color="black", size=0.1) +
  ylab(bquote("Mean C [kg *" ~ha^-1 ~"]")) +
  xlab("Soil Depth (cm)") +
  Thema_soil +
  facet_grid(rows = vars(date)) +
  geom_errorbar(aes(ymin = mean_nutr-sd_nutr, ymax = mean_nutr+sd_nutr), 
                position=position_dodge(width=0.9)) +  
  scale_x_reverse(breaks=c(1,2,3,4), labels=c("0-30", "30-45", "45-75", "75-105"))+
  scale_y_continuous(position = "right", breaks = c(0, 0.3, 0.6, 0.9, 1.2, 1.5),
                     labels = c("0", "0.3", "0.6", "0.9", "1.2", "1.5"),
                     limits=c(0,1.6)) +
  coord_flip() +
  scale_color_manual(values = Farben_Varianten, aesthetics = "fill", labels=c("Ch", "Fe"))+
  theme(axis.text.x = element_text(size = 12),
        axis.ticks.x = element_line(size=0.5),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 13),
        legend.spacing.x = unit(0.2, "cm"),
        axis.title.x = element_text(size = 14),
        strip.text.y = element_text(size = 13), 
        strip.text.x = element_text(size = 13),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 15, vjust = -115, hjust = 0.99))
x

y <- ggplot(df_n, aes(x = depth_class, y = mean_nutr, fill = treatment))+
  geom_bar(stat="identity", position="dodge", color="black", size=0.1)+
  ylab(bquote("Mean N [kg *" ~ha^-1 ~"]"))+
  xlab("Soil Depth (cm)")+
  Thema_soil +
  facet_grid(rows = vars(date))+
  geom_errorbar(aes(ymin = mean_nutr-sd_nutr, ymax = mean_nutr+sd_nutr), 
                position=position_dodge(width=0.9)) +  
  scale_x_reverse(breaks=c(1,2,3,4), labels=c("0-30", "30-45", "45-75", "75-105"))+
  scale_y_continuous(position = "right", breaks = c(0, 0.03, 0.06, 0.09, 0.12, 0.15),
                     labels = c("0", "0.03", "0.06", "0.09", "0.12", "0.15"),
                     limits=c(0,0.15))+
  coord_flip() +
  scale_color_manual(values = Farben_Varianten, aesthetics = "fill", labels=c("Ch", "Fe"))+
  theme(axis.text.x = element_text(size = 12),
        axis.ticks.x = element_line(size=0.5),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 13),
        legend.spacing.x = unit(0.2, "cm"),
        axis.title.x = element_text(size = 14),
        strip.text.y = element_text(size = 13), 
        strip.text.x = element_text(size = 13),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 15, vjust = -115, hjust = 0.99))
y

z <- ggplot(df_p, aes(x = depth_class, y = mean_nutr, fill = treatment))+
  geom_bar(stat="identity", position="dodge", color="black", size=0.1)+
  ylab(bquote("Mean P [kg *" ~ha^-1 ~"]"))+
  xlab("Soil Depth (cm)")+
  Thema_soil +
  facet_grid(rows = vars(date))+
  geom_errorbar(aes(ymin = mean_nutr-sd_nutr, ymax = mean_nutr+sd_nutr), 
                position=position_dodge(width=0.9)) +  
  scale_x_reverse(breaks=c(1,2,3,4), labels=c("0-30", "30-45", "45-75", "75-105"))+
  scale_y_continuous(position = "right", breaks = c(0, 30, 60, 90, 120, 150),
                     labels = c("0", "30", "60", "90", "120", "150"),
                     limits=c(0,160))+
  coord_flip() +
  scale_color_manual(values = Farben_Varianten, aesthetics = "fill", labels=c("Ch", "Fe"))+
  theme(axis.text.x = element_text(size = 12),
        axis.ticks.x = element_line(size=0.5),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 13),
        legend.spacing.x = unit(0.2, "cm"),
        axis.title.x = element_text(size = 14),
        strip.text.y = element_text(size = 13), 
        strip.text.x = element_text(size = 13),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 15, vjust = -115, hjust = 0.99))
z

#plots with miriams code #####
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
  summarize(Anzahl_Parzellen=length(mean_Parameter),
            mean_Parameter_treat = mean(mean_Parameter, na.rm=TRUE), 
            SD_Parameter_treat = sd(mean_Parameter, na.rm=TRUE))

Durchschnitt_Variante$treatment<-as.character(Durchschnitt_Variante$treatment)

###
Farben_Varianten <- c("5"="steelblue4", "6"="steelblue2")

Durchschnitt_Variante_date <-
  Durchschnitt_Variante[Durchschnitt_Variante$date == 1,]

a <- ggplot(Durchschnitt_Variante_date [(  Durchschnitt_Variante_date$treatment == "5"|
                                      Durchschnitt_Variante_date$treatment == "6"),], 
       aes(x=depth_class,y=mean_Parameter_treat, fill=treatment))+
  geom_bar (stat="identity",position="dodge", color="black", size=0.1)+
  ylab(bquote("Mean K [kg *" ~ha^-1 ~"]"))+
  xlab("soil depth (cm)")+
  Thema_soil +
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
        legend.position="none",
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
        legend.position="none",
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
        legend.position="none",
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
        legend.position="none",
        legend.text = element_text(size = 12),
        legend.spacing.x = unit(0.2, 'cm'),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 15, vjust = -115, hjust = 0.99))
d

Durchschnitt_Variante_date <-
  Durchschnitt_Variante[Durchschnitt_Variante$date == 4,]

K <- ggarrange(a, b, c, d, ncol = 4, nrow = 1)
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
        legend.position="none",
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
        legend.position="none",
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
        legend.position="none",
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
        legend.position="none",
        legend.text = element_text(size = 12),
        legend.spacing.x = unit(0.2, 'cm'),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 15, vjust = -115, hjust = 0.99))
h

C <- ggarrange(e, f, g, h, ncol = 4, nrow = 1)
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
        legend.position="none",
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
        legend.position="none",
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
        legend.position="none",
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
        legend.position="none",
        legend.text = element_text(size = 12),
        legend.spacing.x = unit(0.2, 'cm'),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 15, vjust = -115, hjust = 0.99))
l

N <- ggarrange(i, j, k, l, ncol = 4, nrow = 1)
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
        legend.position="none",
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
        legend.position="none",
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
        legend.position="none",
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
        legend.position="none",
        legend.text = element_text(size = 12),
        legend.spacing.x = unit(0.2, 'cm'),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 15, vjust = -115, hjust = 0.99))
p

P <- ggarrange(m, n, o, p, ncol = 4, nrow = 1)
P

ggarrange(C, P, N, K, ncol = 1, nrow = 4, common.legend = TRUE, legend = "bottom")












