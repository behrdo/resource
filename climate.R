#climate
library(tidyverse)
library(readxl)
library(gridExtra)
library(ggpubr)
library(lme4)
library(chillR)
library(anytime)

#water balance ####
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

#soil water content ####
#creating a data frame for 2015 
wasser <- read_delim("soil water C.csv", ";", escape_double = FALSE, 
                    col_types = cols(VWC = col_number(), VWC_gilt = col_number()), trim_ws = TRUE)

wasser <- wasser[complete.cases(wasser[ , 10:12]),]

wasser <- transform(wasser, VWC = as.numeric(VWC), 
                    VWC_gilt = as.numeric(VWC_gilt), 
                    depth = as.character(depth))

wasser <- mutate(wasser, VWC = VWC/100, VWC_gilt = VWC_gilt/1000)

wasser <- separate(wasser, date, sep = "-", into = c("Year", "Month", "Day"))

wasser <- make_JDay(wasser)

wasser <- unite(wasser, Year, Month, Day, col = "Dates", sep = "-")

wasser[12] <- NULL
wasser[11] <- NULL
wasser[9] <- NULL
wasser[5] <- NULL
wasser[1] <- NULL

wasser$Year <- rep(2015, nrow(wasser))
wasser <- wasser[,c(1,8,6,7,3,4,5,2,9)]

names(wasser)[7] <- "trtcomp"

#creating a dataframe for 2016 ####
wa16  <- read_excel("2016_AVR_VWC.xlsx")

wa16 <- separate(wa16, Date, sep = "-", into =c("Year", "Month", "Day"))
wa16 <- make_JDay(wa16)

wa16 <- unite(wa16, Year, Month, Day, col = "Date", sep = "-")

#plot 16
p16a <- select(wa16, Date, JDay, "16-A-W15", "16-A-W45", "16-A-W75", "16-A-W105", "16-A-W135", "16-A-W165",
              "16-A-W195")

p16b <- select(wa16, Date, JDay, "16-B-W15", "16-B-W45", "16-B-W75", "16-B-W105", "16-B-W135", "16-B-W165",
              "16-B-W195")

p16a <- p16a[,c(1,3,4,5,6,7,8,9,2)]
p16b <- p16b[,c(1,3,4,5,6,7,8,9,2)]

p16a[3] <- NULL
p16b[4] <- NULL

names(p16a)[2] <- "15"
names(p16a)[3] <- "75"
names(p16a)[4] <- "105"
names(p16a)[5] <- "135"
names(p16a)[6] <- "165"
names(p16a)[7] <- "195"

p16a <- gather(p16a, "15", "75", "105", "135", "165", "195", key = "depth", value = "VWC")
p16a$treatment <- rep(6, nrow(p16a))
p16a$rainshelter <- rep("without", nrow(p16a))
p16a$trtcomp <- rep(1, nrow(p16a))

names(p16b)[2] <- "15"
names(p16b)[3] <- "45"
names(p16b)[4] <- "105"
names(p16b)[5] <- "135"
names(p16b)[6] <- "165"
names(p16b)[7] <- "195"

p16b <- gather(p16b, "15", "45", "105", "135", "165", "195", key = "depth", value = "VWC")
p16b$treatment <- rep(6, nrow(p16b))
p16b$rainshelter <- rep("with", nrow(p16b))
p16b$trtcomp <- rep(3, nrow(p16b))

p16a$VWC <- as.numeric(p16a$VWC)
p16b$VWC <- as.numeric(p16b$VWC)

p16a <- drop_na(p16a)
p16b <- drop_na(p16b)

p16 <- bind_rows(p16a, p16b)
p16$plot <- rep(16, nrow(p16))

#plot 17
p17a <- select(wa16, Date, JDay, "17-A-W15", "17-A-W45", "17-A-W75", "17-A-W105", "17-A-W135", "17-A-W165",
               "17-A-W195")

p17b <- select(wa16, Date, JDay, "17-B-W15", "17-B-W45", "17-B-W75", "17-B-W105", "17-B-W135", "17-B-W165",
               "17-B-W195")

p17a <- p17a[,c(1,3,4,5,6,7,8,9,2)]
p17b <- p17b[,c(1,3,4,5,6,7,8,9,2)]

p17b[5] <- NULL

names(p17a)[2] <- "15"
names(p17a)[3] <- "45"
names(p17a)[4] <- "75"
names(p17a)[5] <- "105"
names(p17a)[6] <- "135"
names(p17a)[7] <- "165"
names(p17a)[8] <- "195"

p17a <- gather(p17a, "15", "45", "75", "105", "135", "165", "195", key = "depth", value = "VWC")
p17a$treatment <- rep(5, nrow(p17a))
p17a$rainshelter <- rep("without", nrow(p17a))
p17a$trtcomp <- rep(2, nrow(p17a))

names(p17b)[2] <- "15"
names(p17b)[3] <- "45"
names(p17b)[4] <- "75"
names(p17b)[5] <- "135"
names(p17b)[6] <- "165"
names(p17b)[7] <- "195"

p17b <- gather(p17b, "15", "45", "75", "135", "165", "195", key = "depth", value = "VWC")
p17b$treatment <- rep(5, nrow(p17b))
p17b$rainshelter <- rep("with", nrow(p17b))
p17b$trtcomp <- rep(4, nrow(p17b))

p17a$VWC <- as.numeric(p17a$VWC)
p17b$VWC <- as.numeric(p17b$VWC)

p17a <- drop_na(p17a)
p17b <- drop_na(p17b)

p17 <- bind_rows(p17a, p17b)
p17$plot <- rep(17, nrow(p17))

#plot 19
p19a <- select(wa16, Date, JDay, "19-A-W15", "19-A-W45", "19-A-W75", "19-A-W105", "19-A-W135", "19-A-W165",
               "19-A-W195")

p19b <- select(wa16, Date, JDay, "19-B-W15", "19-B-W45", "19-B-W75", "19-B-W105", "19-B-W135", "19-B-W165",
               "19-B-W195")

p19a <- p19a[,c(1,3,4,5,6,7,8,9,2)]
p19b <- p19b[,c(1,3,4,5,6,7,8,9,2)]

p19a[6] <- NULL

names(p19a)[2] <- "15"
names(p19a)[3] <- "45"
names(p19a)[4] <- "75"
names(p19a)[5] <- "105"
names(p19a)[6] <- "165"
names(p19a)[7] <- "195"

p19a <- gather(p19a, "15", "45", "75", "105", "165", "195", key = "depth", value = "VWC")
p19a$treatment <- rep(5, nrow(p19a))
p19a$rainshelter <- rep("without", nrow(p19a))
p19a$trtcomp <- rep(2, nrow(p19a))

names(p19b)[2] <- "15"
names(p19b)[3] <- "45"
names(p19b)[4] <- "75"
names(p19b)[5] <- "105"
names(p19b)[6] <- "135"
names(p19b)[7] <- "165"
names(p19b)[8] <- "195"

p19b <- gather(p19b, "15", "45", "75", "105", "135", "165", "195", key = "depth", value = "VWC")
p19b$treatment <- rep(5, nrow(p19b))
p19b$rainshelter <- rep("with", nrow(p19b))
p19b$trtcomp <- rep(4, nrow(p19b))

p19a$VWC <- as.numeric(p19a$VWC)
p19b$VWC <- as.numeric(p19b$VWC)

p19a <- drop_na(p19a)
p19b <- drop_na(p19b)

p19 <- bind_rows(p19a, p19b)
p19$plot <- rep(19, nrow(p19))

#plot 21
p21a <- select(wa16, Date, JDay, "21-A-W15", "21-A-W45", "21-A-W75", "21-A-W105", "21-A-W135", "21-A-W165",
               "21-A-W195")

p21b <- select(wa16, Date, JDay, "21-B-W15", "21-B-W45", "21-B-W75", "21-B-W105", "21-B-W135", "21-B-W165",
               "21-B-W195")

p21a <- p21a[,c(1,3,4,5,6,7,8,9,2)]
p21b <- p21b[,c(1,3,4,5,6,7,8,9,2)]

p21a[5] <- NULL

names(p21a)[2] <- "15"
names(p21a)[3] <- "45"
names(p21a)[4] <- "75"
names(p21a)[5] <- "135"
names(p21a)[6] <- "165"
names(p21a)[7] <- "195"

p21a <- gather(p21a, "15", "45", "75", "135", "165", "195", key = "depth", value = "VWC")
p21a$treatment <- rep(6, nrow(p21a))
p21a$rainshelter <- rep("without", nrow(p21a))
p21a$trtcomp <- rep(1, nrow(p21a))

names(p21b)[2] <- "15"
names(p21b)[3] <- "45"
names(p21b)[4] <- "75"
names(p21b)[5] <- "105"
names(p21b)[6] <- "135"
names(p21b)[7] <- "165"
names(p21b)[8] <- "195"

p21b <- gather(p21b, "15", "45", "75", "105", "135", "165", "195", key = "depth", value = "VWC")
p21b$treatment <- rep(6, nrow(p21b))
p21b$rainshelter <- rep("with", nrow(p21b))
p21b$trtcomp <- rep(3, nrow(p21b))

p21a$VWC <- as.numeric(p21a$VWC)
p21b$VWC <- as.numeric(p21b$VWC)

p21a <- drop_na(p21a)
p21b <- drop_na(p21b)

p21 <- bind_rows(p21a, p21b)
p21$plot <- rep(21, nrow(p21))

#plot 7
p7a <- select(wa16, Date, JDay, "7-A-W15", "7-A-W45", "7-A-W75", "7-A-W105", "7-A-W135", "7-A-W165",
               "7-A-W195")

p7b <- select(wa16, Date, JDay, "7-B-W15", "7-B-W45", "7-B-W75", "7-B-W105", "7-B-W135", "7-B-W165",
               "7-B-W195")

p7a <- p7a[,c(1,3,4,5,6,7,8,9,2)]
p7b <- p7b[,c(1,3,4,5,6,7,8,9,2)]

p7a[4] <- NULL

names(p7a)[2] <- "15"
names(p7a)[3] <- "45"
names(p7a)[4] <- "105"
names(p7a)[5] <- "135"
names(p7a)[6] <- "165"
names(p7a)[7] <- "195"

p7a <- gather(p7a, "15", "45", "105", "135", "165", "195", key = "depth", value = "VWC")
p7a$treatment <- rep(5, nrow(p7a))
p7a$rainshelter <- rep("without", nrow(p7a))
p7a$trtcomp <- rep(2, nrow(p7a))

names(p7b)[2] <- "15"
names(p7b)[3] <- "45"
names(p7b)[4] <- "75"
names(p7b)[5] <- "105"
names(p7b)[6] <- "135"
names(p7b)[7] <- "165"
names(p7b)[8] <- "195"

p7b <- gather(p7b, "15", "45", "75", "105", "135", "165", "195", key = "depth", value = "VWC")
p7b$treatment <- rep(5, nrow(p7b))
p7b$rainshelter <- rep("with", nrow(p7b))
p7b$trtcomp <- rep(4, nrow(p7b))

p7a$VWC <- as.numeric(p7a$VWC)
p7b$VWC <- as.numeric(p7b$VWC)

p7a <- drop_na(p7a)
p7b <- drop_na(p7b)

p7 <- bind_rows(p7a, p7b)
p7$plot <- rep(7, nrow(p7))

#plot 8
p8a <- select(wa16, Date, JDay, "8-A-W15", "8-A-W45", "8-A-W75", "8-A-W105", "8-A-W135", "8-A-W165",
              "8-A-W195")

p8b <- select(wa16, Date, JDay, "8-B-W15", "8-B-W45", "8-B-W75", "8-B-W105", "8-B-W135", "8-B-W165",
              "8-B-W195")

p8a <- p8a[,c(1,3,4,5,6,7,8,9,2)]
p8b <- p8b[,c(1,3,4,5,6,7,8,9,2)]

p8b[6] <- NULL

names(p8a)[2] <- "15"
names(p8a)[3] <- "45"
names(p8a)[4] <- "75"
names(p8a)[5] <- "105"
names(p8a)[6] <- "135"
names(p8a)[7] <- "165"
names(p8a)[8] <- "195"

p8a <- gather(p8a, "15", "45", "75", "105", "135", "165", "195", key = "depth", value = "VWC")
p8a$treatment <- rep(6, nrow(p8a))
p8a$rainshelter <- rep("without", nrow(p8a))
p8a$trtcomp <- rep(1, nrow(p8a))

names(p8b)[2] <- "15"
names(p8b)[3] <- "45"
names(p8b)[4] <- "75"
names(p8b)[5] <- "105"
names(p8b)[6] <- "165"
names(p8b)[7] <- "195"

p8b <- gather(p8b, "15", "45", "75", "105", "165", "195", key = "depth", value = "VWC")
p8b$treatment <- rep(6, nrow(p8b))
p8b$rainshelter <- rep("with", nrow(p8b))
p8b$trtcomp <- rep(3, nrow(p8b))

p8a$VWC <- as.numeric(p8a$VWC)
p8b$VWC <- as.numeric(p8b$VWC)

p8a <- drop_na(p8a)
p8b <- drop_na(p8b)

p8 <- bind_rows(p8a, p8b)
p8$plot <- rep(8, nrow(p8))

#all together 2016
wasser16 <- bind_rows(p7, p8, p16, p17, p19, p21)

names(wasser16)[1] <- "Dates"
wasser16$Year <- rep(2016, nrow(wasser16))

#creating a dataframe for 2017 ####
wa17 <- read_excel("2017_AVR_VWC.xlsx")

wa17 <- separate(wa17, Dates, sep = "-", into =c("Year", "Month", "Day"))
wa17 <- make_JDay(wa17)

wa17 <- unite(wa17, Year, Month, Day, col = "Dates", sep = "-")

#plot 16
p16a <- select(wa17, Dates, JDay, "16-A-W15", "16-A-W45", "16-A-W75", "16-A-W105", "16-A-W135", "16-A-W165",
               "16-A-W195")

p16b <- select(wa17, Dates, JDay, "16-B-W15", "16-B-W45", "16-B-W75", "16-B-W105", "16-B-W135", "16-B-W165",
               "16-B-W195")

p16a <- p16a[,c(1,3,4,5,6,7,8,9,2)]
p16b <- p16b[,c(1,3,4,5,6,7,8,9,2)]

p16a[3] <- NULL
p16b[8] <- NULL
p16b[5] <- NULL
p16b[4] <- NULL

names(p16a)[2] <- "15"
names(p16a)[3] <- "75"
names(p16a)[4] <- "105"
names(p16a)[5] <- "135"
names(p16a)[6] <- "165"
names(p16a)[7] <- "195"

p16a <- gather(p16a, "15", "75", "105", "135", "165", "195", key = "depth", value = "VWC")
p16a$treatment <- rep(6, nrow(p16a))
p16a$rainshelter <- rep("without", nrow(p16a))
p16a$trtcomp <- rep(1, nrow(p16a))

names(p16b)[2] <- "15"
names(p16b)[3] <- "45"
names(p16b)[4] <- "135"
names(p16b)[5] <- "165"

p16b <- gather(p16b, "15", "45", "135", "165", key = "depth", value = "VWC")
p16b$treatment <- rep(6, nrow(p16b))
p16b$rainshelter <- rep("with", nrow(p16b))
p16b$trtcomp <- rep(3, nrow(p16b))

p16a$VWC <- as.numeric(p16a$VWC)
p16b$VWC <- as.numeric(p16b$VWC)

p16a <- drop_na(p16a)
p16b <- drop_na(p16b)

p16 <- bind_rows(p16a, p16b)
p16$plot <- rep(16, nrow(p16))

#plot 17
p17a <- select(wa17, Dates, JDay, "17-A-W15", "17-A-W45", "17-A-W75", "17-A-W105", "17-A-W135", "17-A-W165",
               "17-A-W195")

p17b <- select(wa17, Dates, JDay, "17-B-W15", "17-B-W45", "17-B-W75", "17-B-W105", "17-B-W135", "17-B-W165",
               "17-B-W195")

p17a <- p17a[,c(1,3,4,5,6,7,8,9,2)]
p17b <- p17b[,c(1,3,4,5,6,7,8,9,2)]

p17b[5] <- NULL

names(p17a)[2] <- "15"
names(p17a)[3] <- "45"
names(p17a)[4] <- "75"
names(p17a)[5] <- "105"
names(p17a)[6] <- "135"
names(p17a)[7] <- "165"
names(p17a)[8] <- "195"

p17a <- gather(p17a, "15", "45", "75", "105", "135", "165", "195", key = "depth", value = "VWC")
p17a$treatment <- rep(5, nrow(p17a))
p17a$rainshelter <- rep("without", nrow(p17a))
p17a$trtcomp <- rep(2, nrow(p17a))

names(p17b)[2] <- "15"
names(p17b)[3] <- "45"
names(p17b)[4] <- "75"
names(p17b)[5] <- "135"
names(p17b)[6] <- "165"
names(p17b)[7] <- "195"

p17b <- gather(p17b, "15", "45", "75", "135", "165", "195", key = "depth", value = "VWC")
p17b$treatment <- rep(5, nrow(p17b))
p17b$rainshelter <- rep("with", nrow(p17b))
p17b$trtcomp <- rep(4, nrow(p17b))

p17a$VWC <- as.numeric(p17a$VWC)
p17b$VWC <- as.numeric(p17b$VWC)

p17a <- drop_na(p17a)
p17b <- drop_na(p17b)

p17 <- bind_rows(p17a, p17b)
p17$plot <- rep(17, nrow(p17))

#plot 19
p19a <- select(wa17, Dates, JDay, "19-A-W15", "19-A-W45", "19-A-W75", "19-A-W105", "19-A-W135", "19-A-W165",
               "19-A-W195")

p19b <- select(wa17, Dates, JDay, "19-B-W15", "19-B-W45", "19-B-W75", "19-B-W105", "19-B-W135", "19-B-W165",
               "19-B-W195")

p19a <- p19a[,c(1,3,4,5,6,7,8,9,2)]
p19b <- p19b[,c(1,3,4,5,6,7,8,9,2)]

p19a[6] <- NULL

names(p19a)[2] <- "15"
names(p19a)[3] <- "45"
names(p19a)[4] <- "75"
names(p19a)[5] <- "105"
names(p19a)[6] <- "165"
names(p19a)[7] <- "195"

p19a <- gather(p19a, "15", "45", "75", "105", "165", "195", key = "depth", value = "VWC")
p19a$treatment <- rep(5, nrow(p19a))
p19a$rainshelter <- rep("without", nrow(p19a))
p19a$trtcomp <- rep(2, nrow(p19a))

names(p19b)[2] <- "15"
names(p19b)[3] <- "45"
names(p19b)[4] <- "75"
names(p19b)[5] <- "105"
names(p19b)[6] <- "135"
names(p19b)[7] <- "165"
names(p19b)[8] <- "195"

p19b <- gather(p19b, "15", "45", "75", "105", "135", "165", "195", key = "depth", value = "VWC")
p19b$treatment <- rep(5, nrow(p19b))
p19b$rainshelter <- rep("with", nrow(p19b))
p19b$trtcomp <- rep(4, nrow(p19b))

p19a$VWC <- as.numeric(p19a$VWC)
p19b$VWC <- as.numeric(p19b$VWC)

p19a <- drop_na(p19a)
p19b <- drop_na(p19b)

p19 <- bind_rows(p19a, p19b)
p19$plot <- rep(19, nrow(p19))

#plot 21
p21a <- select(wa17, Dates, JDay, "21-A-W15", "21-A-W45", "21-A-W75", "21-A-W105", "21-A-W135", "21-A-W165",
               "21-A-W195")

p21b <- select(wa17, Dates, JDay, "21-B-W15", "21-B-W45", "21-B-W75", "21-B-W105", "21-B-W135", "21-B-W165",
               "21-B-W195")

p21a <- p21a[,c(1,3,4,5,6,7,8,9,2)]
p21b <- p21b[,c(1,3,4,5,6,7,8,9,2)]

p21a[5] <- NULL

names(p21a)[2] <- "15"
names(p21a)[3] <- "45"
names(p21a)[4] <- "75"
names(p21a)[5] <- "135"
names(p21a)[6] <- "165"
names(p21a)[7] <- "195"

p21a <- gather(p21a, "15", "45", "75", "135", "165", "195", key = "depth", value = "VWC")
p21a$treatment <- rep(6, nrow(p21a))
p21a$rainshelter <- rep("without", nrow(p21a))
p21a$trtcomp <- rep(1, nrow(p21a))

names(p21b)[2] <- "15"
names(p21b)[3] <- "45"
names(p21b)[4] <- "75"
names(p21b)[5] <- "105"
names(p21b)[6] <- "135"
names(p21b)[7] <- "165"
names(p21b)[8] <- "195"

p21b <- gather(p21b, "15", "45", "75", "105", "135", "165", "195", key = "depth", value = "VWC")
p21b$treatment <- rep(6, nrow(p21b))
p21b$rainshelter <- rep("with", nrow(p21b))
p21b$trtcomp <- rep(3, nrow(p21b))

p21a$VWC <- as.numeric(p21a$VWC)
p21b$VWC <- as.numeric(p21b$VWC)

p21a <- drop_na(p21a)
p21b <- drop_na(p21b)

p21 <- bind_rows(p21a, p21b)
p21$plot <- rep(21, nrow(p21))

#plot 7
p7a <- select(wa17, Dates, JDay, "7-A-W15", "7-A-W45", "7-A-W75", "7-A-W105", "7-A-W135", "7-A-W165",
              "7-A-W195")

p7b <- select(wa17, Dates, JDay, "7-B-W15", "7-B-W45", "7-B-W75", "7-B-W105", "7-B-W135", "7-B-W165",
              "7-B-W195")

p7a <- p7a[,c(1,3,4,5,6,7,8,9,2)]
p7b <- p7b[,c(1,3,4,5,6,7,8,9,2)]

p7a[4] <- NULL

names(p7a)[2] <- "15"
names(p7a)[3] <- "45"
names(p7a)[4] <- "105"
names(p7a)[5] <- "135"
names(p7a)[6] <- "165"
names(p7a)[7] <- "195"

p7a <- gather(p7a, "15", "45", "105", "135", "165", "195", key = "depth", value = "VWC")
p7a$treatment <- rep(5, nrow(p7a))
p7a$rainshelter <- rep("without", nrow(p7a))
p7a$trtcomp <- rep(2, nrow(p7a))

names(p7b)[2] <- "15"
names(p7b)[3] <- "45"
names(p7b)[4] <- "75"
names(p7b)[5] <- "105"
names(p7b)[6] <- "135"
names(p7b)[7] <- "165"
names(p7b)[8] <- "195"

p7b <- gather(p7b, "15", "45", "75", "105", "135", "165", "195", key = "depth", value = "VWC")
p7b$treatment <- rep(5, nrow(p7b))
p7b$rainshelter <- rep("with", nrow(p7b))
p7b$trtcomp <- rep(4, nrow(p7b))

p7a$VWC <- as.numeric(p7a$VWC)
p7b$VWC <- as.numeric(p7b$VWC)

p7a <- drop_na(p7a)
p7b <- drop_na(p7b)

p7 <- bind_rows(p7a, p7b)
p7$plot <- rep(7, nrow(p7))

#plot 8
p8a <- select(wa17, Dates, JDay, "8-A-W15", "8-A-W45", "8-A-W75", "8-A-W105", "8-A-W135", "8-A-W165",
              "8-A-W195")

p8b <- select(wa17, Dates, JDay, "8-B-W15", "8-B-W45", "8-B-W75", "8-B-W105", "8-B-W135", "8-B-W165",
              "8-B-W195")

p8a <- p8a[,c(1,3,4,5,6,7,8,9,2)]
p8b <- p8b[,c(1,3,4,5,6,7,8,9,2)]

p8b[6] <- NULL

names(p8a)[2] <- "15"
names(p8a)[3] <- "45"
names(p8a)[4] <- "75"
names(p8a)[5] <- "105"
names(p8a)[6] <- "135"
names(p8a)[7] <- "165"
names(p8a)[8] <- "195"

p8a <- gather(p8a, "15", "45", "75", "105", "135", "165", "195", key = "depth", value = "VWC")
p8a$treatment <- rep(6, nrow(p8a))
p8a$rainshelter <- rep("without", nrow(p8a))
p8a$trtcomp <- rep(1, nrow(p8a))

names(p8b)[2] <- "15"
names(p8b)[3] <- "45"
names(p8b)[4] <- "75"
names(p8b)[5] <- "105"
names(p8b)[6] <- "165"
names(p8b)[7] <- "195"

p8b <- gather(p8b, "15", "45", "75", "105", "165", "195", key = "depth", value = "VWC")
p8b$treatment <- rep(6, nrow(p8b))
p8b$rainshelter <- rep("with", nrow(p8b))
p8b$trtcomp <- rep(3, nrow(p8b))

p8a$VWC <- as.numeric(p8a$VWC)
p8b$VWC <- as.numeric(p8b$VWC)

p8a <- drop_na(p8a)
p8b <- drop_na(p8b)

p8 <- bind_rows(p8a, p8b)
p8$plot <- rep(8, nrow(p8))

#all together 2017
wasser17 <- bind_rows(p7, p8, p16, p17, p19, p21)
wasser17$Year <- rep(2017, nrow(wasser17))

#plotting VWC ####
df <- bind_rows(wasser, wasser16, wasser17)

df <- mutate(df, VWC = VWC*100)

df <- df %>% group_by(JDay, Year, trtcomp, depth) %>%
  summarise(Anzahl_Parzellen = length(VWC), 
            mean_VWC= mean(VWC, na.rm=TRUE), 
            sd_VWC = sd(VWC, na.rm=TRUE))
#there are round about 400 values in 2017 that are >100%! <- decided to exclude them for now
df <- filter(df, mean_VWC < 100)
df <- na.omit(df)

df$trtcomp[df$trtcomp == "1"] <- "Fe Rainfed" #fe=6
df$trtcomp[df$trtcomp == "2"] <- "Ch Rainfed" #ch=5
df$trtcomp[df$trtcomp == "3"] <- "Fe Rain Shelter" #fe=6
df$trtcomp[df$trtcomp == "4"] <- "Ch Rain Shelter" #ch=5
df$Year[df$Year == "2015"] <- "2015 - Spring Oilseed Rape"
df$Year[df$Year == "2016"] <- "2016 - Winter Barley"
df$Year[df$Year == "2017"] <- "2017 - Oats"
df$depth[df$depth == "15"] <- "15cm"
df$depth[df$depth == "45"] <- "45cm"
df$depth[df$depth == "75"] <- "75cm"
df$depth[df$depth == "105"] <- "105cm"
df$depth[df$depth == "135"] <- "135cm"
df$depth[df$depth == "165"] <- "165cm"
df$depth[df$depth == "195"] <- "195cm"

df <- transform(df, depth = as.factor(depth))

df$depth_f = factor(df$depth, levels = c("15cm", "45cm", "75cm", "105cm", "135cm", 
                                         "165cm", "195cm"))

ggplot(df, aes(x = as.Date(JDay, origin = as.Date("2018-01-01")), y = mean_VWC, color = trtcomp)) + 
  geom_line() +
  facet_grid(cols = vars(Year), rows = vars(depth_f)) +
  scale_colour_manual(values = c("brown4", "steelblue4", "brown2", "steelblue2"), 
                      name = "Treatment") +
  scale_x_date(date_labels = "%b", date_breaks = "2 months") +
  labs(x = "Months", y = "Mean Volumetric Water Content [%]") +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 14),
        axis.title.x = element_blank(), 
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 13), 
        strip.text.x = element_text(size = 13),
        legend.position = "bottom", 
        legend.text = element_text(size = 13), 
        legend.title = element_text(size = 13))

#Monthly water balance ####
days_2013 <- read_excel("days_2013_calc korrig.xlsx", skip = 41)
days_2014 <- read_excel("days_2014_calc korrig.xlsx", skip = 41)
days_2015 <- read_excel("days_2015_calc korrig.xlsx", skip = 41)
days_2016 <- read_excel("days_2016_calc korrig.xlsx", skip = 41)
days_2017 <- read_excel("days_2017_calc korrig.xlsx", skip = 41)

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

days_2013 <- separate(days_2013, date, sep = "-", into =c("Year", "Month", "Day"))
days_2014 <- separate(days_2014, date, sep = "-", into =c("Year", "Month", "Day"))
days_2015 <- separate(days_2015, date, sep = "-", into =c("Year", "Month", "Day"))
days_2016 <- separate(days_2016, date, sep = "-", into =c("Year", "Month", "Day"))
days_2017 <- separate(days_2017, date, sep = "-", into =c("Year", "Month", "Day"))

days_2013$wasserbilanz <- as.numeric(days_2013$wasserbilanz)
days_2014$wasserbilanz <- as.numeric(days_2014$wasserbilanz)
days_2015$wasserbilanz <- as.numeric(days_2015$wasserbilanz)
days_2016$wasserbilanz <- as.numeric(days_2016$wasserbilanz)
days_2017$wasserbilanz <- as.numeric(days_2017$wasserbilanz)

climate <- bind_rows(days_2014, days_2015, days_2016, days_2017)

climate <- na.omit(climate)

ms <- climate %>% group_by(Year, Month) %>% summarise(sum_wasserbilanz = sum(wasserbilanz))

ms <- mutate_at(ms, vars(sum_wasserbilanz), funs(round(., 2)))

ms$Month[ms$Month == "01"] <- "Jan"
ms$Month[ms$Month == "02"] <- "Feb"
ms$Month[ms$Month == "03"] <- "Mar"
ms$Month[ms$Month == "04"] <- "Apr"
ms$Month[ms$Month == "05"] <- "May"
ms$Month[ms$Month == "06"] <- "Jun"
ms$Month[ms$Month == "07"] <- "Jul"
ms$Month[ms$Month == "08"] <- "Aug"
ms$Month[ms$Month == "09"] <- "Sep"
ms$Month[ms$Month == "10"] <- "Okt"
ms$Month[ms$Month == "11"] <- "Nov"
ms$Month[ms$Month == "12"] <- "Dez"

ggplot(ms, aes(x = Month, y = sum_wasserbilanz)) +
  geom_col(fill = "#0072B2", color = "black") +
  scale_fill_manual(values = c("#0072B2")) +
  labs(x = "Months", y = bquote("Monthly Climatic Water Balance [mm]")) + 
  facet_wrap(vars(Year)) +
  scale_x_discrete(labels = c("Jan", "Feb", "Mar", "Apr", "May", 
                            "Jun", "Jul", "Aug", "Sep", "Okt", "Nov", "Dez")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 14),
        axis.title.x = element_blank(), 
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 13), 
        strip.text.x = element_text(size = 13))

#stuff we probably wont need anymore ####
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







