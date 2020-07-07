#changing data frame ######
library(tidyverse)
library(readxl)
library(gridExtra)
library(ggpubr)
library(lme4)
library(chillR)
RLD <- read_delim("RLD4.csv", ";", escape_double = FALSE, 
                  col_types = cols(`sampling date` = col_date(format = "%d.%m.%Y"),
                                   `RLD total (cm cm-3)` = col_number(), 
                                   `RLD bulk (cm cm-3)` = col_number(), 
                                   `RLD BP (cm cm-3)` = col_number(), 
                                   `RL total (m m-2 in 5 cm soil depth)` = col_number(), 
                                   `RL bulk (m m-2 in 5 cm soil depth)` = col_number(), 
                                   `RL BP (m m-2 in 5 cm soil depth)` = col_number()), 
                  locale = locale(decimal_mark = ","), 
                  trim_ws = TRUE)

RLD[17:33] <- NULL
names(RLD)[5]<-"plot"
names(RLD)[6]<-"field.rep"
names(RLD)[8] <- "rainout.shelter"
names(RLD)[10] <- "depth"
names(RLD)[11] <- "tot_cm3"
names(RLD)[12] <- "blk_cm3"
names(RLD)[13] <-"BP_cm3"
names(RLD)[14] <- "tot_m2"
names(RLD)[15] <- "blk_m2"
names(RLD)[16]  <- "BP_m2"
RLD[1] <- NULL
RLD[2] <- NULL
RLD[5] <- NULL
RLD[4] <- NULL
names(RLD)[1]  <- "sampling.date"
names(RLD)[5]  <- "crop"

Fe2 <- filter(RLD, treatment == "6")
Ch2 <- filter(RLD, treatment == "5")
RLD <- bind_rows(Fe2, Ch2)
RLD$treatment[RLD$treatment == "6"]<- "Fe"
RLD$treatment[RLD$treatment == "5"]<- "Ch"
RLD$crop[RLD$crop == "winter oilseed rape"]<- "S. Oilseed Rape"
RLD$crop[RLD$crop == "spring barley"]<- "Spring Barley"
RLD$crop[RLD$crop == "oats"]<- "Oats"


RLD <- RLD[complete.cases(RLD[ , 7:8]),]
RLD$rainout.shelter[is.na(RLD$rainout.shelter)] = "rainfed"
RLD$rainout.shelter[RLD$rainout.shelter == 'rainshelter'] <- 'rain shelter'

RLD <- separate(RLD, sampling.date, sep = "-", into =c("Year", "Month", "Day"))
RLD <- make_JDay(RLD)
RLD[2:3] <- NULL

RLD <- transform(RLD, treatment = as.factor(treatment),
                 plot = as.factor(plot),
                 rainout.shelter = as.factor(rainout.shelter))
#Backup--------
RLD0<- RLD
RLD<-RLD0

#JDay Solution-----------------------
  # make data
value <- rnorm(365, mean = 0, sd = 5)
jday <- 1:365 # represents your Julian days 1-365; assumes no leap years

  # make data frame and add julian day
d <- data.frame(jday = jday, value = value, stringsAsFactors = FALSE)
head(d)



#Total Rootlegth (m m-2) Troughout the Year----------------------------------------
  #summarize RLD of 5cm x m^2 to get total rootlength per m^2

RL_tot <- RLD %>% group_by(JDay, Year, treatment, rainout.shelter) %>% 
  summarise(tot_m2=sum(tot_m2), blk_m2=sum(blk_m2), BP_m2=sum(BP_m2))

#plot
ggplot(RL_tot, aes(x = as.Date(JDay, origin = as.Date("2013-01-01")), y = blk_m2, 
                colour = interaction(rainout.shelter, treatment),
               group = interaction(treatment, rainout.shelter))) + 
  geom_point() + geom_line() +
  facet_grid(cols = vars(Year)) +
  scale_colour_manual(values = c("brown4", "steelblue4", "brown2", "steelblue2"), name = "Treatment",
                      labels = c("Ch rain shelter", "Ch rainfed", "Fe rain shelter", "Fe rainfed"))+
  labs(x = "", y = "Rootlength [m  " ~m^2 ~"]", title = "Total Rootlegth Troughout the Year") +
  theme_bw() +
  scale_x_date(date_labels = "%b")+
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 11), 
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 10), 
        strip.text.x = element_text(size = 10))+ 
  theme_bw()
# Data for 2016 is missing and some for 2017 too?

#for max values comparing blk and BP for all years and all depth-----not nice jet--------------------------
RL_tot_max <- RL_tot %>%group_by(Year, treatment, rainout.shelter) %>%filter(JDay == max(JDay))
RL_tot_max <- gather(RL_tot_max, "blk_m2", "BP_m2", key = "pore_kind", value = "m2")
RL_tot_max <- unite(RL_tot_max, treatment, rainout.shelter, col = treatment, sep = "-")
#plot
ggplot(RL_tot_max, aes(x = treatment, y = m2, fill = pore_kind)) +
  geom_bar(stat = "identity") + 
  facet_grid(cols = vars(Year)) +
  theme_bw()


#for single values of harvesting date------------------------------------------------
RLD2 <- gather(RLD,  "tot_cm3","BP_cm3", key = "pore_kind", value = "cm3")
RLD2 <- RLD2 %>%group_by(pore_kind,crop, Year, treatment, rainout.shelter, depth) %>%filter(JDay == max(JDay))
RLD2 <- RLD2 %>% group_by(pore_kind, crop, Year, treatment, rainout.shelter, depth) %>% 
  summarise(cm3=mean(cm3))

ggplot(RLD2, aes(x = depth, y = cm3, colour = interaction(rainout.shelter, pore_kind),
                 group = interaction(pore_kind, rainout.shelter))) + 
   geom_line() +
  facet_grid(treatment ~ Year + crop) +
  scale_colour_manual(values = c( "olivedrab2", "green3","orange2", "orange4"), 
                      name = "Treatment",labels = c("BP Rain Shelter", "BP Rainfed",
                                                    "Total Rain Shelter", "Total Rainfed"))+
  labs(x = bquote("Soil Depth [cm]"), y= "Rootlength Density [m *" ~cm^-3 ~"]") +
  theme_bw() +
  scale_x_reverse()+
  coord_flip()+
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 13), 
        strip.text.x = element_text(size = 13),
        legend.position = "bottom") 
        
  theme_bw()


# percentages of BP use from total root emergance--------------------------------------------
RLDP <- RLD %>%group_by(Year, crop, treatment, rainout.shelter, depth) %>%filter(JDay == max(JDay))
RLDP <- RLDP %>% group_by(Year, crop, treatment, rainout.shelter, depth) %>% 
  summarise(tot_m2=mean(tot_m2), BP_m2=mean(BP_m2))
RLDP <- mutate (RLDP, percentage= BP_m2/tot_m2*100 )
RLDP$percentage[is.na(RLDP$percentage)] = "0"
RLDP$percentage <- as.numeric(RLDP$percentage)

ggplot(RLDP, aes(x = depth, y = percentage, colour = interaction(rainout.shelter, treatment),
                   group = interaction(rainout.shelter, treatment))) + 
                  geom_line() +
  facet_grid(cols= vars(Year , crop)) +
  scale_colour_manual(values = c("brown4", "steelblue4", "brown2", "steelblue2"), name = "Treatment",
  labels = c("Ch rain shelter", "Ch rainfed", "Fe rain shelter", "Fe rainfed"))+
  labs(x = "Soildepth [cm]", y = "Used BP of total Rootlength [%]") +
  scale_x_reverse()+
  coord_flip()+
  scale_y_continuous(breaks=c(0,50,100))+
  theme_bw()+
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 13),
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 13), 
        strip.text.x = element_text(size = 13),
        legend.position = "bottom")


# Total Rootlegth for different depth <30 / >30 throughout the year---------------------------------------------------
t30 <- filter(RLD, depth <=30)
s30 <- filter(RLD, depth >30)
  
RL_t <- t30 %>% group_by(JDay, Year, crop, treatment, rainout.shelter) %>% 
  summarise(tot_m2=sum(tot_m2), blk_m2=sum(blk_m2), BP_m2=sum(BP_m2))
RL_s <- s30 %>% group_by(JDay, Year, crop, treatment, rainout.shelter) %>% 
    summarise(tot_m2=sum(tot_m2), blk_m2=sum(blk_m2), BP_m2=sum(BP_m2))

RL_t$depth <- rep("<30cm Soildepth", nrow(RL_t))
RL_s$depth <- rep(">30cm Soildepth", nrow(RL_s))
RL_t_s<- bind_rows(RL_t, RL_s)

  
ggplot(RL_t_s, aes(x = as.Date(JDay, origin = as.Date("2013-01-01")), y = blk_m2, colour = interaction(rainout.shelter, treatment),
                   group = interaction(treatment, rainout.shelter))) + 
  geom_point() + geom_line() +
  facet_grid(depth ~ Year + crop)  +
  scale_colour_manual(values = c("brown4", "steelblue4", "brown2", "steelblue2"), name = "Treatment",
                      labels = c("Ch rain shelter", "Ch rainfed", "Fe rain shelter", "Fe rainfed")
                      )+
  labs(x="", y = "Rootlength [m  " ~m^2 ~"]") +
  scale_x_date(date_labels = "%b")+
  theme_bw()+
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 13), 
        strip.text.x = element_text(size = 13),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title=element_text(size=14))




  
  
  
  