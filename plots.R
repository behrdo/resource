library(tidyverse)
library(readxl)
library(gridExtra)
library(ggpubr)
library(lme4)
library(chillR)

#biopores
biopore <- read_excel("data_Trial_C_2020_05_12.xlsx", 
                      sheet = "biopores")

#changing the dataframe into something i can work with :D
biopore[12:21] <- NULL
biopore[6] <- NULL

biopore <- transform(biopore, treatment = as.factor(treatment))

names(biopore)[8] <- "2.5mm"
names(biopore)[9] <- "5mm"

#creating a new dataframe with data that i can plot
f <- filter(biopore, treatment == 5)
s <- filter(biopore, treatment == 6)

df <- bind_rows(f, s)

df <- select(df, "treatment", "2.5mm", "5mm")

df <- gather(df, "2.5mm", "5mm", key = "depth", value = "pores")

#plotting
ggplot(df, aes(x = treatment, y = pores, fill = depth)) +
  geom_bar(stat = "identity") + 
  theme_bw()

ggplot(df, aes(x = treatment, y = pores, fill = depth)) +
  geom_bar(stat = "identity", position = position_dodge()) + 
  theme_bw()

ms <- df %>% group_by(treatment, depth) %>% summarise(mean = mean(pores), sd = sd(pores))

a <- ggplot(ms, aes(x = depth, y = mean, fill = treatment)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black") + 
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), width=.2,
                position=position_dodge(.9)) +
  scale_fill_manual(values = c("red3", "#0072B2")) +
  labs(fill = "Treatment",  x = "Depth", 
       y = bquote("Mean Biopores [" ~m^-2 ~ "]"),
       title = "A") + 
  theme_bw() +
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 11), 
        plot.title = element_text(size = 15, vjust = -10, hjust = 0.03), 
        strip.text.y = element_text(size = 10), 
        strip.text.x = element_text(size = 10), 
        legend.position = c(0.8, 0.85))
a

#dry matter
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

spross[19:39] <- NULL

spross <- separate(spross, date, sep = "-", into =c("Year", "Month", "Day"))

spross <- make_JDay(spross)

spross <- transform(spross, spross_dm = as.numeric(spross_dm))

spross <- spross %>% mutate_at(vars(spross_dm), funs(round(., 1)))

f <- filter(spross, treatment == "5")
s <- filter(spross, treatment == "6")

spross <- bind_rows(f, s)

###
#spross <- spross[-c(106), ]
###

spross <- transform(spross, rainout.shelter = as.character(rainout.shelter))
spross$rainout.shelter[is.na(spross$rainout.shelter)] = "without"

spross <- drop_na(spross, spross_dm)

ms <- spross %>% group_by(Year, treatment, rainout.shelter, JDay) %>% 
  summarise(mean = mean(spross_dm), sd = sd(spross_dm))

ms$treatment[ms$treatment == "5"] <- "WW2"
ms$treatment[ms$treatment == "6"] <- "RS2"

ms <- transform(ms, Year = as.factor(Year), 
                treatment = as.factor(treatment))

#line und point plot mit den unterschiedlichen harvests
ggplot(ms, aes(x = JDay, y = mean, colour = interaction(rainout.shelter, treatment),
               group = interaction(treatment, rainout.shelter))) + 
  geom_point() + geom_line() +
  facet_grid(cols = vars(Year)) +
  scale_colour_manual(values = c("red3", "#F0E442", "darkgreen", "#0072B2"), name = "Treatment", 
                      labels = c("WW2 Without", "RS2 Without", "WW2 With", "WW2 Without")) +
  labs(x = "Day Number", y = "Mean dry matter [kg  " ~ha^-1 ~"]", title = "Dry Matter") +
  theme_bw() +
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 11), 
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 10), 
        strip.text.x = element_text(size = 10)) + 
  theme_bw()
#error weil 2016 nur an einem date gemessen wurde -> mähsde nix

#DM barplot mit letztem harvest
ms_max <- ms %>%
  group_by(Year) %>%
  filter(JDay == max(JDay))

ms_max <- unite(ms_max, treatment, rainout.shelter, col = treatment, sep = "-")

addline_format <- function(x,...){
  gsub("\\s","\n",x)
}

b <- ggplot(ms_max, aes(x = treatment, y = mean, fill = treatment)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black") + 
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), width=.2,
                position=position_dodge(.9)) +
  scale_fill_manual(values = c("red3", "#F0E442", "darkgreen", "#0072B2")) +
  facet_grid(cols = vars(Year)) +
  scale_x_discrete(breaks = unique(ms_max$treatment),
                   labels = addline_format(c("WW2 Without", "RS2 Without", "WW2 With", "RS2 With"))) +
  labs(x = "Treatment", 
       y = bquote("Mean dry matter [kg  " ~ha^-1 ~"]"),
       title = "Dry matter at the last Harvest") + 
  theme_bw() +
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 11), 
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 10), 
        strip.text.x = element_text(size = 10), 
        legend.position = "none")
b















