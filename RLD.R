#changing data frame ######
library(tidyverse)
library(readxl)
library(gridExtra)
library(ggpubr)
library(lme4)
library(chillR)
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

Fe2 <- filter(RLD, treatment == "5")
Ch2 <- filter(RLD, treatment == "6")
RLD <- bind_rows(Fe2, Ch2)
RLD$treatment[RLD$treatment == "5"]<- "Fe"
RLD$treatment[RLD$treatment == "6"]<- "Ch"

RLD <- RLD[complete.cases(RLD[ , 7:8]),]
RLD$rainout.shelter[is.na(RLD$rainout.shelter)] = "rainfed"
RLD$rainout.shelter[RLD$rainout.shelter == 'rainshelter'] <- 'rain shelter'

RLD <- transform(RLD, treatment = as.factor(treatment),
                 plot = as.factor(plot),
                 rainout.shelter = as.factor(rainout.shelter))

RLD <- separate(RLD, sampling.date, sep = "-", into =c("Year", "Month", "Day"))
RLD <- make_JDay(RLD)
RLD[2:3] <- NULL

#summarize RLD of 5cm x m^2 to get total rootlength per m^2-------------------

RL_tot <- RLD %>% group_by(JDay, Year, treatment, rainout.shelter) %>% 
  summarise(tot_m2=sum(tot_m2), blk_m2=sum(blk_m2), BP_m2=sum(BP_m2))


#plot
ggplot(RL_tot, aes(x = JDay, y = blk_m2, colour = interaction(rainout.shelter, treatment),
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

#for max values compareing blk and BPfor all years and all depth-------------------------------

RL_tot_max <- RL_tot %>%group_by(Year, treatment, rainout.shelter) %>%filter(JDay == max(JDay))
RL_tot_max <- gather(RL_tot_max, "blk_m2", "BP_m2", key = "pore_kind", value = "m2")
RL_tot_max <- unite(RL_tot_max, treatment, rainout.shelter, col = treatment, sep = "-")
#plot
ggplot(RL_tot_max, aes(x = treatment, y = m2, fill = pore_kind)) +
  geom_bar(stat = "identity") + 
  facet_grid(cols = vars(Year)) +
  theme_bw()


#for single values of harvesting date------------------
RLD1 <- gather(RLD,  "tot_cm3","BP_cm3", key = "pore_kind", value = "cm3")
RLD1 <- RLD1 %>%group_by(pore_kind,Year, treatment, rainout.shelter, depth) %>%filter(JDay == max(JDay))
RLD1 <- RLD1 %>% group_by(pore_kind, Year, treatment, rainout.shelter, depth) %>% 
  summarise(cm3=mean(cm3))

#RLDY <- filter (RLD1, Year == 2017)
#cm/ M!!!!!!!!!!!!!

ggplot(RLD1, aes(x = depth, y = cm3, colour = interaction(rainout.shelter, pore_kind),
                 group = interaction(pore_kind, rainout.shelter))) + 
   geom_line() +
  facet_grid(cols = vars(Year), rows = vars(treatment)) +
  scale_colour_manual(values = c( "olivedrab2", "green3","orange2", "orange4"), 
                      name = "Treatment",labels = c("BP Rain Shelter", "BP Rainfed",
                                                    "Total Rain Shelter", "Total Rainfed"))+
  labs(x = bquote("Rootlength [m *" ~cm^-3 ~"]"), y = "Soil Depth [cm]", title = "Total Rootlegnth") +
  theme_bw() +
  scale_x_reverse()+
  coord_flip()+
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 11), 
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 10), 
        strip.text.x = element_text(size = 10),
        legend.position="bottom") + 
  theme_bw()

# percentages of BP use from total root emergance--------------------------------------------
RLDP <- RLD %>%group_by(Year, treatment, rainout.shelter, depth) %>%filter(JDay == max(JDay))
RLDP <- RLDP %>% group_by(Year, treatment, rainout.shelter, depth) %>% 
  summarise(tot_m2=mean(tot_m2), BP_m2=mean(BP_m2))
RLDP <- mutate (RLDP, percentage= BP_m2/tot_m2*100 )
 
#RLDY <- filter (RLDP, Year == 2017)

 ggplot(RLDP, aes(x = depth, y = percentage, colour = interaction(rainout.shelter, treatment),
                   group = interaction(rainout.shelter, treatment))) + 
    geom_line() +
      facet_grid(cols = vars(Year)) +
      #scale_colour_manual(values = c("orange2", "orange4", "olivedrab2", "green3"), name = "Treatment")+
      #labels = c("RS2 rain shelter", "RS2 rainfed", "WW2 rain shelter", "WW2 rainfed")) +
      #labs(x = "Day Number", y = "Rootlength [m  " ~m^2 ~"]", title = "Total Rootlegnth") +
      theme_bw() +
      scale_x_reverse()+
      coord_flip()+
      theme(axis.text = element_text(size = 10), 
            axis.title = element_text(size = 11), 
            plot.title = element_text(size = 15), 
            strip.text.y = element_text(size = 10), 
            strip.text.x = element_text(size = 10)) + 
      theme_bw()


# for different depth <30 / >30 at last sampling date--------------------------------------------------

  u30 <- filter(RLD, depth <=30)
  o30 <- filter(RLD, depth >=30)
  frames = c(u30, o30)
  RL30 <- pd.concat (frames, keys =c("<30",">30") )
  
  u30 <- filter(RLD, depth <=30)
  o30 <- filter(RLD, depth >=30)
  # add colomn that distinguishes <30 / >30
  
  u30 <- u30 %>% group_by(JDay, Year, treatment, rainout.shelter) %>% 
    summarise(tot_m2=sum(tot_m2), blk_m2=sum(blk_m2), BP_m2=sum(BP_m2))
  o30 <- o30 %>% group_by(JDay, Year, treatment, rainout.shelter) %>% 
    summarise(tot_m2=sum(tot_m2), blk_m2=sum(blk_m2), BP_m2=sum(BP_m2))
  
  
  RL30 <- gather(RL30, "blk_m2", "BP_m2", key = "pore_kind", value = "m2")
  
  ggplot(RL30, aes(x = depth, y = m2, colour = interaction(rainout.shelter, treatment),
                   group = interaction(treatment, rainout.shelter))) + 
    geom_point() + geom_line() +
    facet_grid(cols = vars(Year)) +
    scale_colour_manual(values = c("brown4", "steelblue4", "brown2", "steelblue2"), name = "Treatment")+
    #labels = c("RS2 rain shelter", "RS2 rainfed", "WW2 rain shelter", "WW2 rainfed")) +
    #labs(x = "Day Number", y = "Rootlength [m  " ~m^2 ~"]", title = "Total Rootlegnth") +
    theme_bw() +
    scale_x_reverse(breaks=c(1,2,3,4))+
    coord_flip()+
    theme(axis.text = element_text(size = 10), 
          axis.title = element_text(size = 11), 
          plot.title = element_text(size = 15), 
          strip.text.y = element_text(size = 10), 
          strip.text.x = element_text(size = 10)) + 
    theme_bw()
  
  
  
  