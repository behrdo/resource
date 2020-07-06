library(tidyverse)
library(readxl)
library(gridExtra)
library(ggpubr)
library(lme4)
library(chillR)
#precrop dry matter----------------------
Rohdaten_shoot <- read.csv2("precrop data C.csv")

ms1 <- Rohdaten_shoot %>% group_by(treatment) %>% summarise(mean = mean(DM_mean), sd = sd(DM_mean))

ms2 <- filter(ms1, treatment == 5)
ms3 <- filter(ms1, treatment == 6)

ms1 <- bind_rows(ms2, ms3)

ms1 <- transform(ms1, treatment = as.factor(treatment))

b <- ggplot(ms1, aes(x = treatment, y = mean, fill = treatment)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black") + 
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), width=.2,
                position=position_dodge(.9)) +
  scale_fill_manual(values = c("red3", "#0072B2")) +
  labs(x = "Treatment", 
       y = bquote("Mean dry matter [kg  " ~ha^-1 ~"]"),
       title = "B") + 
  theme_bw() +
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 11), 
        plot.title = element_text(size = 15, vjust = -10, hjust = 0.03), 
        strip.text.y = element_text(size = 10), 
        strip.text.x = element_text(size = 10), 
        legend.position = "none")
b

ggarrange(a, b, ncol = 2, nrow = 1)



#sprossdaten unsere plots: 
#5, 7, 17, 19 sind treatment ww2
#6, 8, 16, 21 sind rs2

#maincrops ---------------  
spross <- read_excel("data Trial C_2020_06_10_naemi.xlsx", 
                     sheet = "PlantNutrients Bearbeitet", 
                     col_types = c("text", "text", "text", 
                                   "text", "text", "text", "text", "text", 
                                   "date", "text", "text", "text", "text", 
                                   "text", "text", "text", "text", "text", 
                                   "text", "text", "text", "text", "text", 
                                   "text", "text", "text", "text", "text", 
                                   "text", "text", "text", "text", "text", 
                                   "text", "text", "text", "numeric", 
                                   "text", "text"))


#1. data frame vorbereiten
spross <- slice(spross, 1:360)

names(spross)[9] <- "date"
names(spross)[10] <- "spross_dm"
names(spross)[19] <- "harvest_dm"
names(spross)[28] <- "straw_dm"

spross[20:27] <- NULL
spross[21:31] <-NULL
spross <- separate(spross, date, sep = "-", into =c("Year", "Month", "Day"))

spross <- make_JDay(spross)

spross <- transform(spross, spross_dm = as.numeric(spross_dm))
spross <- transform(spross, harvest_dm = as.numeric(harvest_dm))
spross <- transform(spross, straw_dm = as.numeric(straw_dm))


spross <- spross %>% mutate_at(vars(spross_dm), funs(round(., 1)))
spross <- spross %>% mutate_at(vars(harvest_dm), funs(round(., 1)))
spross <- spross %>% mutate_at(vars(straw_dm), funs(round(., 1)))


c <- filter(spross, treatment == "5")
f <- filter(spross, treatment == "6")

spross <- bind_rows(c, f)


spross <- transform(spross, rainout.shelter = as.character(rainout.shelter))
spross$rainout.shelter[is.na(spross$rainout.shelter)] = "without"

spross <- transform(spross, Year = as.factor(Year), 
                treatment = as.character(treatment))

spross$treatment[spross$treatment == "5"] <- "Ch"
spross$treatment[spross$treatment == "6"] <- "Fe"

spross <- spross[-c(103, 104), ] 

ms_sp<- drop_na(spross, spross_dm)
ms_sp <- spross %>% group_by(Year, treatment, rainout.shelter, JDay) %>% 
  summarise(mean_sp = mean(spross_dm), sd_sp = sd(spross_dm))

ms_h <- drop_na(spross, harvest_dm)
ms_h <- spross %>% group_by(Year, treatment, rainout.shelter, JDay) %>% 
  summarise(mean_h =mean(harvest_dm), sd_h = sd(harvest_dm))

           
ms_st <- drop_na(spross, straw_dm)
ms_st <- spross %>% group_by(Year, treatment, rainout.shelter, JDay) %>% 
              summarise( mean_st=mean(straw_dm), sd_st = sd(straw_dm)) 


#line und point plot mit den unterschiedlichen harvests-----erstmal raus wg h sp unterschiede raps usw------
ms1<- filter(ms, Year != "2015")

ggplot(ms1, aes(x = JDay, y = mean_sp, colour = interaction(rainout.shelter, treatment),
               group = interaction(treatment, rainout.shelter))) + 
  geom_point() + geom_line() +
  facet_grid(cols = vars(Year)) +
  scale_colour_manual(values = c("brown4", "steelblue4", "brown2", "steelblue2"), 
                      name = "Treatment")+ 
                      labels = c("Ch Rain Shelter", "Ch Rainfed", "Fe Rain Shelter", "Fe Rainfed")) +
  labs(x = "", y = "Mean Grain Harvest [kg  " ~ha^-1 ~"]", title = "Dry Matter") +
  theme_bw() +
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 11), 
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 10), 
        strip.text.x = element_text(size = 10)) + 
  theme_bw()
#error weil 2016 nur an einem date gemessen wurde -> mähsde nix

#DM barplot mit grain harvest-------
ms1 <- ms_h %>%
  group_by(Year) %>%
  filter(JDay == max(JDay))
ms_max<- filter(ms1, Year != "2015")
ms_max$Year <- as.character(ms_max$Year)

ms_max$Year[ms_max$Year == "2014"] <- "2014 - Spring Barley"
ms_max$Year[ms_max$Year == "2016"] <- "2016 - Winter Barley"
ms_max$Year[ms_max$Year == "2017"] <- "2017 - Oats"

ms_max <- unite(ms_max, treatment, rainout.shelter, col = treatment, sep = "-")

names(ms_max)[2] <- "Treatment"

addline_format <- function(x,...){
  gsub("\\s","\n",x)
}



b <- ggplot(ms_max, aes(x = Treatment, y = mean_h, fill = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black") + 
  geom_errorbar(aes(ymin = mean_h-sd_h, ymax = mean_h+sd_h), width=.2,
                position=position_dodge(.9)) +
  scale_x_discrete(limits = c("Ch-with", "Fe-with","Ch-without", "Fe-without"))+
  scale_fill_manual(values = c("red4", "steelblue4", "red1", "steelblue1") 
                   ,labels = c("Ch Rainshelter", "Ch Rainfed", "Fe Rainshelter", "Fe Rainfed")
                  ) +
  facet_grid(cols = vars(Year)) +
  labs(x = "Treatment", 
       y = bquote("Mean Grain Harvest [kg*" ~ha^-1 ~"]")) + 
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 14),
        axis.title.x = element_blank(), 
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 13), 
        strip.text.x = element_text(size = 13),
        axis.text.x = element_blank(),
        legend.position = c(0.12, 0.85))
b

#barplot mit letztem harvest raps---------?h/st/sp?---------

ms2<- filter(ms1, Year == "2015")
ms2 <- unite(ms2, treatment, rainout.shelter, col = treatment, sep = "-")

ggplot(ms2, aes(x = treatment, y = mean_sp, fill = treatment)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black") + 
  geom_errorbar(aes(ymin = mean_sp-sd_sp, ymax = mean_sp+sd_sp), width=.2,
                position=position_dodge(.9)) +
  scale_fill_manual(values = c("red4", "steelblue4", "red1", "steelblue1")) +
  #facet_grid(cols = vars(Year)) +
  scale_x_discrete(breaks = unique(ms2$treatment),
             #falsch     # labels = addline_format(c("Ch Rainfed", "Fe Rainfed", 
 #                                            "Ch Rainshelter", "Fe Rainshelter"))) +
  labs(x = "Treatment", 
       y = bquote("Mean Shoot Dry Matter [kg*" ~ha^-1 ~"]")) + 
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 14),
        axis.title.x = element_blank(), 
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 13), 
        strip.text.x = element_text(size = 13), 
        legend.position = "none")

#barplot mit straw ---------
ms1 <- ms_st %>%
  group_by(Year) %>%
  filter(JDay == max(JDay))
ms_max<- filter(ms1, Year != "2015")
ms_max$Year <- as.character(ms_max$Year)

ms_max$Year[ms_max$Year == "2014"] <- "2014 - Spring Barley"
ms_max$Year[ms_max$Year == "2016"] <- "2016 - Winter Barley"
ms_max$Year[ms_max$Year == "2017"] <- "2017 - Oats"

ms_max <- unite(ms_max, treatment, rainout.shelter, col = treatment, sep = "-")

addline_format <- function(x,...){
  gsub("\\s","\n",x)
}

names(ms_max)[2] <- "Treatment"

ggplot(ms_max, aes(x = Treatment, y = mean_st, fill = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black") + 
  geom_errorbar(aes(ymin = mean_st-sd_st, ymax = mean_st+sd_st), width=.2,
                position=position_dodge(.9)) +
  scale_x_discrete(limits = c("Ch-with", "Fe-with","Ch-without", "Fe-without"))+
  scale_fill_manual(values = c("red4", "steelblue4", "red1", "steelblue1")
                    #,labels = c("Ch Rainshelter", "Ch Rainfed", "Fe Rainshelter", "Fe Rainfed")
                    ) +
  facet_grid(cols = vars(Year)) +
  labs(x = "Treatment", 
       y = bquote("Mean Straw Harvest Dry Matter [kg * " ~ha^-1 ~"]")) + 
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 14),
        axis.title.x = element_blank(), 
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 13), 
        strip.text.x = element_text(size = 13),
        axis.text.x = element_blank(),
        legend.position = c(0.12, 0.85))



