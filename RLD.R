RLD <- read_delim("RLD3.csv", ";", escape_double = FALSE, 
                       col_types = cols(`RLD total (cm cm-3)` = col_number(), 
                                                `RLD bulk (cm cm-3)` = col_number(), 
                                                  `RLD BP (cm cm-3)` = col_number(), 
                                                  `RLU total (cm /m^2 in soil depth)` = col_number(), 
                                                  `RLU bulk (cm /m^2 in soil depth)` = col_number(), 
                                                 `RLU BP (cm /m^2 in soil depth)` = col_number()), 
                     locale = locale(decimal_mark = ","), 
                       trim_ws = TRUE)
#wenn ich das nicht so Kompliziert wie obern mache - dann aber probleme mit numeric
#RLD <-read_delim("RLD3.csv", ";", escape_double = FALSE, 
 #                   trim_ws = TRUE)
#colnames(RLD)=RLD[3,]

#RLD <- slice(RLD, 5:n())


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
RLD$rainout.shelter[is.na(RLD$rainout.shelter)] = "without"

RLD <- transform(RLD, treatment = as.factor(treatment),
                 plot = as.factor(plot),
                 rainout.shelter = as.factor(rainout.shelter))


#ms <- spross %>% group_by(Year, treatment, rainout.shelter, JDay) %>% 
#summarise(mean = mean(spross_dm), sd = sd(spross_dm))



