library(tidyverse)
library(readxl)
library(gridExtra)
library(ggpubr)
library(lme4)

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





