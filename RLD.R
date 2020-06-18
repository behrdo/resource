library(tidyverse)
library(readxl)
library(gridExtra)
library(ggpubr)
library(lme4)
library(chillR)
#changing data frame
RLD <- read_delim("RLD3.csv", ";", escape_double = FALSE, 
                       col_types = cols(`sampling date` = col_date(format = "%d.%m.%Y"),
                                        `RLD total (cm cm-3)` = col_number(), 
                                                `RLD bulk (cm cm-3)` = col_number(), 
                                                  `RLD BP (cm cm-3)` = col_number(), 
                                                  `RLU total (cm /m^2 in soil depth)` = col_number(), 
                                                  `RLU bulk (cm /m^2 in soil depth)` = col_number(), 
                                                 `RLU BP (cm /m^2 in soil depth)` = col_number()), 
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

ww2 <- filter(RLD, treatment == "5")
rs2 <- filter(RLD, treatment == "6")
RLD <- bind_rows(ww2, rs2)

RLD <- RLD[complete.cases(RLD[ , 7:8]),]
RLD$rainout.shelter[is.na(RLD$rainout.shelter)] = "rainfed"
RLD$rainout.shelter[RLD$rainout.shelter == 'rainshelter'] <- 'rain shelter'

RLD <- transform(RLD, treatment = as.factor(treatment),
                 plot = as.factor(plot),
                 rainout.shelter = as.factor(rainout.shelter))

RLD <- separate(RLD, sampling.date, sep = "-", into =c("Year", "Month", "Day"))

RLD <- make_JDay(RLD)

#summarize RLD of 5cm x m^2 to get total rootlength per m^2
RL_tot <- RLD %>% group_by(JDay, Year, treatment, rainout.shelter) %>% 
  summarise(tot_m2=sum(tot_m2), blk_m2=sum(blk_m2), BP_m2=sum(BP_m2))


#plot
ggplot(RL_tot, aes(x = JDay, y = tot_m2, colour = interaction(rainout.shelter, treatment),
               group = interaction(treatment, rainout.shelter))) + 
  geom_point() + geom_line() +
  facet_grid(cols = vars(Year)) +
  scale_colour_manual(values = c("brown4", "steelblue4", "brown2", "steelblue2"), name = "Treatment",
                      labels = c("RS2 rain shelter", "RS2 rainfed", "WW2 rain shelter", "WW2 rainfed")) +
  labs(x = "Day Number", y = "Rootlength [m  " ~m^2 ~"]", title = "Total Rootlegnth") +
  theme_bw() +
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 11), 
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 10), 
        strip.text.x = element_text(size = 10)) + 
  theme_bw()
# Data for 2016 is missing and some for 2017 too?
ww2 <- filter(RLD, treatment == "5")


# for different depth <30 />30

#for single values


