library(tidyverse)
library(readxl)
library(gridExtra)
library(ggpubr)
library(lme4)
library(chillR)

Rohdaten_shoot <- read.csv2("precrop data C.csv")

#dry matter ####
#creating df and calculating mean, sd and such
q <- filter(Rohdaten_shoot, treatment == 5)
w <- filter(Rohdaten_shoot, treatment == 6)

qw <- bind_rows(q, w)

qw <- transform(qw, treatment = as.factor(treatment))

summary(q$DM_mean) #treatment ch
summary(w$DM_mean) #treatment fe
sd(q$DM_mean) #treatment ch
sd(w$DM_mean) #treatment fe

#checking for normality
shapiro.test(q$DM_mean)
shapiro.test(w$DM_mean)

#t-test
t.test(q$DM_mean, w$DM_mean)
#p=0.16 

#one way anova
model1 <- lm(DM_mean ~ treatment, data = qw)
model2 <- lm(DM_mean ~ 1, data = qw)

par(mfrow = c(2, 2))
plot(model1)
plot(model2)
par(mfrow = c(1, 1))
#shapiro test suggests that the data is not normal distributed, 
#based on the results of the normalqq-plot i would say that it is.
#The low sample size makes it probably hard to tell though.

anova(model1, model2)
#model1 is better than model2 -> treatment has an impact on DM_mean,
#which however is not significant (p = 0.1604) -> similar p value as in the t-test

#biopores ####
#1. changing the dataframe into something i can work with :D
biopore <- read_excel("data_Trial_C_2020_05_12.xlsx", 
                      sheet = "biopores")

biopore[12:21] <- NULL
biopore[6] <- NULL

biopore <- transform(biopore, treatment = as.factor(treatment))

names(biopore)[8] <- "2.5mm"
names(biopore)[9] <- "5mm"

f <- filter(biopore, treatment == 5)
s <- filter(biopore, treatment == 6)

summary(f$gesamt) #treatment ch
summary(s$gesamt) #treatment fe
sd(f$gesamt) #treatment ch
sd(s$gesamt) #treatment fe

summary(f$`2.5mm`) #treatment ch
summary(s$`2.5mm`) #treatment fe
sd(f$`2.5mm`) #treatment ch
sd(s$`2.5mm`) #treatment fe

summary(f$`5mm`) #treatment ch
summary(s$`5mm`) #treatment fe
sd(f$`5mm`) #treatment ch
sd(s$`5mm`) #treatment fe

df <- bind_rows(f, s)

shapiro.test(df$gesamt)
#-> no nd

model1 <- lm(gesamt ~ treatment, data = df)
par(mfrow = c(2, 2))
plot(model1)
par(mfrow = c(1, 1))
#-> this again looks nd to me (sample sizes are probably just too low for a shapiro.test)

#t-test gesamt
q <- filter(df, treatment == 5)
w <- filter(df, treatment == 6)

t.test(q$gesamt, w$gesamt)
#p=0.54 

#one-way anova
model2 <- lm(gesamt ~ 1, data = df)

anova(model1, model2)
#model1 is slightly better than model2 -> treatment has an impact on DM_mean,
#which however is not significant (p = 0.5398) -> similar p value as in the t-test


#two way anova, with fieldrep as a random factor
model1 <- lmer(gesamt ~ treatment + (1|field.repetition), data = df)
summary(model1)
#the variance dependent on fieldrep is much lower than the residual variance, the
#variance dependent on fieldrep appears not to be significantly different from zero

confint(model1, parm = "sd_(Intercept)|field.repetition", level=0.95, 
        method = "profile", oldNames = F)
#Conclusion: The confidence interval for the standard deviation does include zero; 
#-> accept the null hypothesis that the variance is zero.
#a significant effect of fieldrep on the DM_mean can not be shown

model2 <- lmer(gesamt ~ (1|field.repetition), data = df)
summary(model1)

anova(model1, model2)
#p-value: 0.3812 -> the treatment has no significant effect on the Biopores gesamt

#precrop nutrients ####
Rohdaten_shoot <- read.csv2("precrop data C.csv")

nutr1 <- filter(Rohdaten_shoot, treatment == 5)
nutr2 <- filter(Rohdaten_shoot, treatment == 6)

summary(nutr1$N_mean) #treatment ch
summary(nutr2$N_mean) #treatment fe
sd(nutr1$N_mean) #treatment ch
sd(nutr2$N_mean) #treatment fe

summary(nutr1$P_mean) #treatment ch
summary(nutr2$P_mean) #treatment fe
sd(nutr1$P_mean) #treatment ch
sd(nutr2$P_mean) #treatment fe

summary(nutr1$K_mean) #treatment ch
summary(nutr2$K_mean) #treatment fe
sd(nutr1$K_mean) #treatment ch
sd(nutr2$K_mean) #treatment fe


nutr <- bind_rows(nutr1, nutr2)

nutr$treatm[nutr$treatm == "Ch2"] <- "Ch"
nutr$treatm[nutr$treatm == "Fe2"] <- "Fe"

nutr[11] <- NULL
nutr[7] <- NULL

#one way anova - N
model1 <- lm(N_mean ~ treatment, data = nutr)
summary(model1)

model2 <- lm(N_mean ~ 1, data = nutr)
summary(model2)

anova(model1, model2)
#p-value: 0.3963 -> the treatment has no significant effect
#but are they even nd? ->
par(mfrow = c(2, 2))
plot(model1)
plot(model2)
par(mfrow = c(1, 1))
#jup i would say so

#two way anova, introducing fieldrep as a random factor
model1 <- lmer(N_mean ~ treatment + (1|fieldrep), data = nutr)
summary(model1)
#the variance dependent on fieldrep is much higher than the residual variance

model2 <- lmer(N_mean ~ (1|fieldrep), data = nutr)
summary(model1)

anova(model1, model2)
#p-value: 0.04344 -> the N content of the two precrops is significantly different

#one way anova - P
model1 <- lm(P_mean ~ treatment, data = nutr)
summary(model1)

model2 <- lm(P_mean ~ 1, data = nutr)
summary(model2)

anova(model1, model2)
#p-value: 0.02593 -> the P content of the two precrops is significantly different
#but are they even nd? ->
par(mfrow = c(2, 2))
plot(model1)
plot(model2)
par(mfrow = c(1, 1))
#jup i would say so

#one way anova - K
model1 <- lm(K_mean ~ treatment, data = nutr)
summary(model1)

model2 <- lm(K_mean ~ 1, data = nutr)
summary(model2)

anova(model1, model2)
#p-value: 0.02621 -> the K content of the two precrops is significantly different
#but are they even nd? ->
par(mfrow = c(2, 2))
plot(model1)
plot(model2)
par(mfrow = c(1, 1))
#jup i would say so

#dry matter  shoot at last harvest - two way anova ####
#1. preparing data frame
spross <- read_excel("data Trial C_2020_06_10.xlsx", 
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

spross <- slice(spross, 1:360)

names(spross)[9] <- "date"
names(spross)[10] <- "spross_dm"

spross[19:39] <- NULL

spross <- separate(spross, date, sep = "-", into =c("Year", "Month", "Day"))

spross <- make_JDay(spross)

spross <- transform(spross, spross_dm = as.numeric(spross_dm))

spross <- spross %>% mutate_at(vars(spross_dm), funs(round(., 1)))
#this error just suggests to use another function then funs, it works for me though

f <- filter(spross, treatment == "5")
s <- filter(spross, treatment == "6")

spross <- bind_rows(f, s)

spross <- transform(spross, rainout.shelter = as.factor(rainout.shelter), 
                    field.rep = as.factor(field.rep), 
                    plot = as.factor(plot), 
                    Rep = as.factor(Rep))

spross$rainout.shelter[is.na(spross$rainout.shelter)] = "without"

spross <- drop_na(spross, spross_dm)

spross[13:20] <- NULL
spross[6] <- NULL
spross[1] <- NULL

spross <- spross %>%  group_by(Year) %>%  filter(JDay == max(JDay))


#creating dataframes for each year/plant-species
s_barley <- filter(spross, crop == "spring barley")#keine rainout.shelter
s_osr <- filter(spross, crop == "spring oilseed rape")
w_barley <- filter(spross, crop == "winter barley")
oats <- filter(spross, crop == "oats")

s_osr[5] <- NULL

#2014/spring barley - shoot ####
#mean and stuff
f <- filter(s_barley, treatment == "5")
s <- filter(s_barley, treatment == "6")

t.test(spross_dm ~ treatment, data = s_barley, var.equal=TRUE)

summary(f$spross_dm) #treatment ch
summary(s$spross_dm) #treatment fe
sd(f$spross_dm) #treatment ch
sd(s$spross_dm) #treatment fe

#one way anova
model1 <- lm(spross_dm ~ treatment, data = s_barley)
summary(model1)

model2 <- lm(spross_dm ~ 1, data = s_barley)
summary(model2)

anova(model1, model2)
#p-value: 0.05026 -> the treatment has a significant effect?? at least close :D
#but are they even nd? ->
par(mfrow = c(2, 2))
plot(model1)
plot(model2)
par(mfrow = c(1, 1))
#jup i would say so

#miriams code -> the csv file is not included in the folder. results are the same though, did this just to be sure
Rohdaten_shoot <- read_delim("Trial C shoot.csv", 
                                   ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                                         grouping_mark = ""), trim_ws = TRUE)
Daten_ttest <- Rohdaten_shoot %>%
  group_by(fieldrep,treatment)%>%
  summarise (mean_DM_ttest=mean(grain_tha))

Daten_ttest_Var <- Daten_ttest[(Daten_ttest$treatment=="5"|
                                  Daten_ttest$treatment=="6"),]

Daten_ttest_Var$treatment <- as.factor (Daten_ttest_Var$treatment)
Daten_ttest_Var$fieldrep <- as.factor (Daten_ttest_Var$fieldrep)

t.test(mean_DM_ttest~treatment, data = Daten_ttest_Var, var.equal=TRUE)

#2015/spring oil seed rape - shoot ####
#mean and stuff
f <- filter(s_osr, treatment == "5")
f_with <- filter(f, rainout.shelter == "with")
f_without <- filter(f, rainout.shelter == "without")
s <- filter(s_osr, treatment == "6")
s_with <- filter(s, rainout.shelter == "with")
s_without <- filter(s, rainout.shelter == "without")

#with shelter
summary(f_with$spross_dm) #treatment ch
summary(s_with$spross_dm) #treatment fe
sd(f_with$spross_dm) #treatment ch
sd(s_with$spross_dm) #treatment fe

#without shelter
summary(f_without$spross_dm) #treatment ch
summary(s_without$spross_dm) #treatment fe
sd(f_without$spross_dm) #treatment ch
sd(s_without$spross_dm) #treatment fe

#two way anova, with treatment and rainout shelter as main effects
model1 <- lm(spross_dm ~ treatment*rainout.shelter, data = s_osr)
summary(model1)

model2 <- lm(spross_dm ~ treatment, data = s_osr)
summary(model1)

anova(model1, model2)
#p-value: 0.6509 -> no significant effect

#introducing field.rep as a random factor
model1 <- lmer(spross_dm ~ treatment*rainout.shelter + (1|field.rep), data = s_osr)
summary(model1)

model2 <- lmer(spross_dm ~ treatment + (1|field.rep) , data = s_osr)
summary(model1)

anova(model1, model2)
#p-value: 0.21 -> no significant effect

model1 <- lmer(spross_dm ~ treatment*rainout.shelter + (1|field.rep), data = s_osr)
summary(model1)

model2 <- lmer(spross_dm ~ rainout.shelter + (1|field.rep) , data = s_osr)
summary(model1)

anova(model1, model2)
#p-value: 0.2073 -> no significant effect

model1 <- lmer(spross_dm ~ treatment + (1|field.rep), data = s_osr)
summary(model1)

model2 <- lmer(spross_dm ~ (1|field.rep) , data = s_osr)
summary(model1)

anova(model1, model2)
#p-value: 0.3882 -> no significant effect

model1 <- lmer(spross_dm ~ rainout.shelter + (1|field.rep), data = s_osr)
summary(model1)

model2 <- lmer(spross_dm ~ (1|field.rep) , data = s_osr)
summary(model1)

anova(model1, model2)
#p-value: 0.3963 -> no significant effect
#conclusions: no significant effect of either the fixed or the random effects on the DM

#2016/winter barley - shoot ####
#mean and stuff
f <- filter(w_barley, treatment == "5")
f_with <- filter(f, rainout.shelter == "with")
f_without <- filter(f, rainout.shelter == "without")
s <- filter(w_barley, treatment == "6")
s_with <- filter(s, rainout.shelter == "with")
s_without <- filter(s, rainout.shelter == "without")

#with shelter
summary(f_with$spross_dm) #treatment ch
summary(s_with$spross_dm) #treatment fe
sd(f_with$spross_dm) #treatment ch
sd(s_with$spross_dm) #treatment fe

#without shelter
summary(f_without$spross_dm) #treatment ch
summary(s_without$spross_dm) #treatment fe
sd(f_without$spross_dm) #treatment ch
sd(s_without$spross_dm) #treatment fe

#two way anova, with treatment and rainout shelter as main effects
model1 <- lm(spross_dm ~ treatment*rainout.shelter, data = w_barley)
summary(model1)

model2 <- lm(spross_dm ~ treatment, data = w_barley)
summary(model1)

anova(model1, model2)
#p-value: 1.119e-06 -> significant effect of rainout.shelter
model1 <- lm(spross_dm ~ treatment*rainout.shelter, data = w_barley)
summary(model1)

model2 <- lm(spross_dm ~ rainout.shelter, data = w_barley)
summary(model1)

anova(model1, model2)
#p-value: 0.7249 -> not significant effect of treatment
#but are they even nd? ->
par(mfrow = c(2, 2))
plot(model1)
plot(model2)
par(mfrow = c(1, 1))
#jup i would say so

#2017/oats - shoot ####
#mean and stuff
f <- filter(oats, treatment == "5")
f_with <- filter(f, rainout.shelter == "with")
f_without <- filter(f, rainout.shelter == "without")
s <- filter(oats, treatment == "6")
s_with <- filter(s, rainout.shelter == "with")
s_without <- filter(s, rainout.shelter == "without")

#with shelter
summary(f_with$spross_dm) #treatment ch
summary(s_with$spross_dm) #treatment fe
sd(f_with$spross_dm) #treatment ch
sd(s_with$spross_dm) #treatment fe

#without shelter
summary(f_without$spross_dm) #treatment ch
summary(s_without$spross_dm) #treatment fe
sd(f_without$spross_dm) #treatment ch
sd(s_without$spross_dm) #treatment fe


#two-way anova with treatment and rainout shelter as fixed effects
model1 <- lm(spross_dm ~ treatment*rainout.shelter, data = oats)
summary(model1)

model2 <- lm(spross_dm ~ treatment, data = oats)
summary(model1)

anova(model1, model2)
#p-value: 0.7249 -> no significant effect 

model1 <- lm(spross_dm ~ treatment*rainout.shelter, data = oats)
summary(model1)

model2 <- lm(spross_dm ~ rainout.shelter, data = oats)
summary(model1)

anova(model1, model2)
#p-value: 0.5976 -> no significant effect 

#two-way anova introducing field.rep as random effect
model1 <- lmer(spross_dm ~ treatment*rainout.shelter + (1|field.rep), data = oats)
summary(model1)

model2 <- lmer(spross_dm ~ rainout.shelter + (1|field.rep) , data = oats)
summary(model1)

anova(model1, model2)
#p-value: 0.4782 -> no significant effect 

model1 <- lmer(spross_dm ~ treatment*rainout.shelter + (1|field.rep), data = oats)
summary(model1)

model2 <- lmer(spross_dm ~ treatment + (1|field.rep) , data = oats)
summary(model1)

anova(model1, model2)
#p-value: 0.4215 -> no significant effect 

model1 <- lmer(spross_dm ~ rainout.shelter + (1|field.rep), data = oats)
summary(model1)

model2 <- lmer(spross_dm ~ (1|field.rep) , data = oats)
summary(model1)

anova(model1, model2)
#p-value: 0.4814 -> no significant effect

model1 <- lmer(spross_dm ~ treatment + (1|field.rep), data = oats)
summary(model1)

model2 <- lmer(spross_dm ~ (1|field.rep) , data = oats)
summary(model1)

anova(model1, model2)
#p-value: 0.6219 -> no significant effect


#two-way anova introducing Rep as random effect
model1 <- lmer(spross_dm ~ treatment*rainout.shelter + (1|Rep), data = oats)
summary(model1)

model2 <- lmer(spross_dm ~ rainout.shelter + (1|Rep) , data = oats)
summary(model1)

anova(model1, model2)
#p-value: 0.5123 -> no significant effect 

model1 <- lmer(spross_dm ~ treatment*rainout.shelter + (1|Rep), data = oats)
summary(model1)

model2 <- lmer(spross_dm ~ treatment + (1|Rep) , data = oats)
summary(model1)

anova(model1, model2)
#p-value: 0.4566 -> no significant effect 
#makes no sence to continue further, effect of Rep is lower than the effect of field.rep

#dry matter at last harvest grain ####
spross <- read_excel("data Trial C_2020_06_10.xlsx", 
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

spross <- slice(spross, 1:360)

names(spross)[9] <- "date"
names(spross)[19] <- "grain_dm"

spross[20:27] <- NULL
spross[21:31] <-NULL

spross <- separate(spross, date, sep = "-", into =c("Year", "Month", "Day"))

spross <- make_JDay(spross)

spross <- transform(spross, grain_dm = as.numeric(grain_dm))

spross <- spross %>% mutate_at(vars(grain_dm), funs(round(., 1)))
#this error just suggests to use another function then funs, it works for me though

f <- filter(spross, treatment == "5")
s <- filter(spross, treatment == "6")

spross <- bind_rows(f, s)

spross <- transform(spross, rainout.shelter = as.factor(rainout.shelter), 
                    field.rep = as.factor(field.rep), 
                    plot = as.factor(plot), 
                    Rep = as.factor(Rep))

spross$rainout.shelter[is.na(spross$rainout.shelter)] = "without"

spross <- drop_na(spross, grain_dm)

spross[13:20] <- NULL
spross[6] <- NULL
spross[1] <- NULL
spross[12] <- NULL
spross[10] <- NULL

spross <- spross %>%  group_by(Year) %>%  filter(JDay == max(JDay))

#creating dataframes for each year/plant-species
s_barley <- filter(spross, crop == "spring barley")#keine rainout.shelter
w_barley <- filter(spross, crop == "winter barley")
oats <- filter(spross, crop == "oats")

#grain yield 2014-sb ####
#mean and stuff
f <- filter(s_barley, treatment == "5")
s <- filter(s_barley, treatment == "6")

summary(f$grain_dm) #treatment ch
summary(s$grain_dm) #treatment fe
sd(f$grain_dm) #treatment ch
sd(s$grain_dm) #treatment fe

#one way anova
model1 <- lm(grain_dm ~ treatment, data = s_barley)
summary(model1)

model2 <- lm(grain_dm ~ 1, data = s_barley)
summary(model2)

anova(model1, model2)
#p-value: 0.04631 -> the treatment has a significant effect
#but are they even nd? ->
par(mfrow = c(2, 2))
plot(model1)
plot(model2)
par(mfrow = c(1, 1))
#jup i would say so

#t.test as suggested by miriam
t.test(grain_dm~treatment, data = s_barley, var.equal=TRUE)
#the result is only significant if we assume that the variances are equal
#p-value = 0.04631 | without this assumtion: p-value = 0.05248

#2016/winter barley - grain ####
#mean and stuff
f <- filter(w_barley, treatment == "5")
f_with <- filter(f, rainout.shelter == "with")
f_without <- filter(f, rainout.shelter == "without")
s <- filter(w_barley, treatment == "6")
s_with <- filter(s, rainout.shelter == "with")
s_without <- filter(s, rainout.shelter == "without")

#with shelter
summary(f_with$grain_dm) #treatment ch
summary(s_with$grain_dm) #treatment fe
sd(f_with$grain_dm) #treatment ch
sd(s_with$grain_dm) #treatment fe

#without shelter
summary(f_without$grain_dm) #treatment ch
summary(s_without$grain_dm) #treatment fe
sd(f_without$grain_dm) #treatment ch
sd(s_without$grain_dm) #treatment fe

#two way anova, with treatment and rainout shelter as main effects
model1 <- lm(grain_dm ~ treatment*rainout.shelter, data = w_barley)
summary(model1)

model2 <- lm(grain_dm ~ treatment, data = w_barley)
summary(model1)

anova(model1, model2)
#p-value: 0.2592 -> no significant effect of rainout.shelter
model1 <- lm(grain_dm ~ treatment*rainout.shelter, data = w_barley)
summary(model1)

model2 <- lm(grain_dm ~ rainout.shelter, data = w_barley)
summary(model1)

anova(model1, model2)
#p-value: 0.6686 -> not significant effect of treatment
#but are they even nd? ->
par(mfrow = c(2, 2))
plot(model1)
plot(model2)
par(mfrow = c(1, 1))
#jup i would say so

#2017/oats - grain ####
#mean and stuff
f <- filter(oats, treatment == "5")
f_with <- filter(f, rainout.shelter == "with")
f_without <- filter(f, rainout.shelter == "without")
s <- filter(oats, treatment == "6")
s_with <- filter(s, rainout.shelter == "with")
s_without <- filter(s, rainout.shelter == "without")

#with shelter
summary(f_with$grain_dm) #treatment ch
summary(s_with$grain_dm) #treatment fe
sd(f_with$grain_dm) #treatment ch
sd(s_with$grain_dm) #treatment fe

#without shelter
summary(f_without$grain_dm) #treatment ch
summary(s_without$grain_dm) #treatment fe
sd(f_without$grain_dm) #treatment ch
sd(s_without$grain_dm) #treatment fe


#two-way anova with treatment and rainout shelter as fixed effects
model1 <- lm(grain_dm ~ treatment*rainout.shelter, data = oats)
summary(model1)

model2 <- lm(grain_dm ~ treatment, data = oats)
summary(model1)

anova(model1, model2)
#p-value: 0.6953 -> no significant effect 

model1 <- lm(grain_dm ~ treatment*rainout.shelter, data = oats)
summary(model1)

model2 <- lm(grain_dm ~ rainout.shelter, data = oats)
summary(model1)

anova(model1, model2)
#p-value: 0.6624 -> no significant effect 

#two-way anova introducing field.rep as random effect
model1 <- lmer(grain_dm ~ treatment*rainout.shelter + (1|field.rep), data = oats)
summary(model1)

model2 <- lmer(grain_dm ~ rainout.shelter + (1|field.rep) , data = oats)
summary(model1)

anova(model1, model2)
#p-value: 0.5706 -> no significant effect 

model1 <- lmer(grain_dm ~ treatment*rainout.shelter + (1|field.rep), data = oats)
summary(model1)

model2 <- lmer(grain_dm ~ treatment + (1|field.rep) , data = oats)
summary(model1)

anova(model1, model2)
#p-value: 0.6094 -> no significant effect 

model1 <- lmer(grain_dm ~ rainout.shelter + (1|field.rep), data = oats)
summary(model1)

model2 <- lmer(grain_dm ~ (1|field.rep) , data = oats)
summary(model1)

anova(model1, model2)
#p-value: 0.8194 -> no significant effect

model1 <- lmer(grain_dm ~ treatment + (1|field.rep), data = oats)
summary(model1)

model2 <- lmer(grain_dm ~ (1|field.rep) , data = oats)
summary(model1)

anova(model1, model2)
#p-value: 0.6683 -> no significant effect

#two-way anova introducing Rep as random effect
model1 <- lmer(grain_dm ~ treatment*rainout.shelter + (1|Rep), data = oats)
summary(model1)

model2 <- lmer(grain_dm ~ rainout.shelter + (1|Rep) , data = oats)
summary(model1)

anova(model1, model2)
#p-value: 0.6246 -> no significant effect 

model1 <- lmer(grain_dm ~ treatment*rainout.shelter + (1|Rep), data = oats)
summary(model1)

model2 <- lmer(grain_dm ~ treatment + (1|Rep) , data = oats)
summary(model1)

anova(model1, model2)
#p-value: 0.6602 -> no significant effect 
#makes no sence to continue further, effect of Rep is lower than the effect of field.rep

#statistics for root data ####
#to get this code running you unfortunately have to run the RLD.R file first
#copying the RLD code into this file would have made this one way to big and confusing
#1. max and min of used biopres of total root length ####
rldp_2014 <- filter(RLDP, Year == 2014)
rldp_2015 <- filter(RLDP, Year == 2015)
rldp_2017 <- filter(RLDP, Year == 2017)

summarise(rldp_2014, max(percentage))
summarise(rldp_2014, min(percentage))

summarise(rldp_2015, max(percentage))
summarise(rldp_2015, min(percentage))

summarise(rldp_2017, max(percentage))
summarise(rldp_2017, min(percentage))

#2. max of RLD
rld2_2014 <- filter(RLD2, Year == 2014)
rld2_2015 <- filter(RLD2, Year == 2015)
rld2_2017 <- filter(RLD2, Year == 2017)

rld2_2015bp <- filter(rld2_2015, pore_kind == "BP_cm3")
rld2_2017bp <- filter(rld2_2017, pore_kind == "BP_cm3")

summarise(rld2_2014, max(cm3))
summarise(rld2_2015, max(cm3))
summarise(rld2_2017, max(cm3))








