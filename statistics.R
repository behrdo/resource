library(tidyverse)
library(readxl)
library(gridExtra)
library(ggpubr)
library(lme4)

Rohdaten_shoot <- read.csv2("precrop data C.csv")

#statistical analysis for dry matter
#checking for normality
q <- filter(Rohdaten_shoot, treatment == 5)
w <- filter(Rohdaten_shoot, treatment == 6)

qw <- bind_rows(q, w)

ms1 <- transform(ms1, treatment = as.factor(treatment))

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


###
#two way anova, introducing fieldrep as a random factor (not sure if this makes too much sense
#since the sample size of fieldrep is only 2)
model1 <- lmer(DM_mean ~ treatment + (1|fieldrep), data = qw)
summary(model1)
#the variance dependent on fieldrep is much higher than the residual variance, the
#variance dependent on fieldrep appears to be significantly different from zero

confint(model1, parm = "sd_(Intercept)|fieldrep", level=0.95, 
        method = "profile", oldNames = F)
#Conclusion: The confidence interval for the standard deviation does not include zero; 
#-> reject the null hypothesis that the variance is zero.
#a significant effect of fieldrep on the DM_mean can be shown 

model2 <- lmer(DM_mean ~ (1|fieldrep), data = qw)
summary(model1)

anova(model1, model2)
#model1 is the better model
#p-value: 0.01052 -> after removing the effect of fieldrep on DM_mean, it can be shown, that
#treatments have a significant effect on DM_mean
###

#statistical analysis for biopores
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


#two way anova, again with fieldrep as a random factor
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










