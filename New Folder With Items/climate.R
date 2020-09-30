#climate
library(tidyverse)
library(readxl)
library(gridExtra)
library(ggpubr)
library(lme4)
library(chillR)

#first attempt with the mean data
alles <- read_excel("years_stat korrig.xlsx")

colnames(alles) = alles[1, ]
alles <- slice(alles, 2:n())

names(alles)[6] <- "temp_mean"
names(alles)[18] <- "wasserbilanz"

alles <- slice(alles, 7:11)

alles <- transform(alles, Jahr = as.numeric(Jahr), 
                   wasserbilanz = as.numeric(wasserbilanz), 
                   temp_mean = as.numeric(temp_mean))


ggplot(alles, aes(x = Jahr, y = wasserbilanz)) +
  geom_col(fill = "#0072B2", color = "black") +
  scale_fill_manual(values = c("#0072B2")) +
  labs(fill = "Treatment",  x = "Years", 
       y = bquote("Annual Climatic Water Balance [mm]"),
       title = "A") + 
  theme_bw() +
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 11), 
        plot.title = element_text(size = 15, vjust = -10, hjust = 0.03), 
        strip.text.y = element_text(size = 10), 
        strip.text.x = element_text(size = 10), 
        legend.position = c(0.8, 0.85))


#second one with daily data of all the years combined
#1.1 as boxlots
days_2013 <- read_excel("days_2013_calc korrig.xlsx")
days_2014 <- read_excel("days_2014_calc korrig.xlsx")
days_2015 <- read_excel("days_2015_calc korrig.xlsx")
days_2016 <- read_excel("days_2016_calc korrig.xlsx")
days_2017 <- read_excel("days_2017_calc korrig.xlsx")

days_2017 <- slice(days_2017, 41:n())
colnames(days_2017) = days_2017[1, ]
days_2017 <- slice(days_2017, 2:n())

days_2016 <- slice(days_2016, 41:n())
colnames(days_2016) = days_2016[1, ]
days_2016 <- slice(days_2016, 2:n())

days_2015 <- slice(days_2015, 41:n())
colnames(days_2015) = days_2015[1, ]
days_2015 <- slice(days_2015, 2:n())

days_2014 <- slice(days_2014, 41:n())
colnames(days_2014) = days_2014[1, ]
days_2014 <- slice(days_2014, 2:n())

days_2013 <- slice(days_2013, 41:n())
colnames(days_2013) = days_2013[1, ]
days_2013 <- slice(days_2013, 2:n())

names(days_2013)[20] <- "wasserbilanz"
names(days_2014)[20] <- "wasserbilanz"
names(days_2015)[20] <- "wasserbilanz"
names(days_2016)[20] <- "wasserbilanz"
names(days_2017)[20] <- "wasserbilanz"

days_2013[2:19] <- NULL
days_2013[3:22] <- NULL

days_2014[2:19] <- NULL
days_2014[3:22] <- NULL

days_2015[2:19] <- NULL
days_2015[3:22] <- NULL

days_2016[2:19] <- NULL
days_2016[3:22] <- NULL

days_2017[2:19] <- NULL
days_2017[3:22] <- NULL

days_2013$jahr <- rep(2013, nrow(days_2013))
days_2014$jahr <- rep(2014, nrow(days_2014))
days_2015$jahr <- rep(2015, nrow(days_2015))
days_2016$jahr <- rep(2016, nrow(days_2016))
days_2017$jahr <- rep(2017, nrow(days_2017))

climate <- bind_rows(days_2013, days_2014, days_2015, days_2016, days_2017)

climate <- transform(climate, jahr = as.factor(jahr), 
                   wasserbilanz = as.numeric(wasserbilanz))

climate %>% mutate_at(vars(wasserbilanz), funs(round(., 2)))

ggplot(climate, aes(x = jahr, y = wasserbilanz, fill = jahr)) +
  geom_boxplot()
#-> geringe varianz aber extrem viele ausreißer, macht keinen sinn so zu plotten

#2.2 as a bar chart
days_2013 <- days_2013[-c(39, 40, 41, 42), ]

climate <- bind_rows(days_2013, days_2014, days_2015, days_2016, days_2017)

climate <- transform(climate, jahr = as.factor(jahr), 
                     wasserbilanz = as.numeric(wasserbilanz))

climate %>% mutate_at(vars(wasserbilanz), funs(round(., 2)))


ms <- climate %>% group_by(jahr) %>% summarise(mean = mean(wasserbilanz), sd = sd(wasserbilanz))

ggplot(ms, aes(x = jahr, y = mean, fill = jahr)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black") + 
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), width=.2,
                position=position_dodge(.9)) +
  scale_fill_manual(values = c("gray0", "gray", "red3", "#0072B2", "#F0E442")) +
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
#macht so auch keinen sinn







