library(tidyverse)
library(readxl)
library(gridExtra)
library(ggpubr)
library(lme4)
library(chillR)

#rohdaten soil
roh <- read.csv2("soil data C.csv")

r1 <- filter(roh, treatment == 5)
r2 <- filter(roh, treatment == 6)

roh <- bind_rows(r1, r2)

roh <- transform(roh, depth = as.factor(depth), 
                 treatment = as.factor(treatment), 
                 date = as.factor(date), 
                 plot = as.factor(plot), 
                 fieldrep = as.factor(fieldrep), 
                 depth_class = as.factor(depth_class))



































