library(tidyverse)
library(readxl)
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

df2 <- select(df, "treatment", "2.5mm", "5mm")

df2 <- gather(df2, "2.5mm", "5mm", key = "depth", value = "pores")

#plotting
ggplot(df2, aes(x = treatment, y = pores, fill = depth)) +
  geom_bar(stat = "identity") + 
  theme_bw()

ggplot(df2, aes(x = treatment, y = pores, fill = depth)) +
  geom_bar(stat = "identity", position = position_dodge()) + 
  theme_bw()

ms <- df2 %>% group_by(treatment, depth) %>% summarise(avg = mean(pores), sd = sd(pores))

ggplot(ms, aes(x = treatment, y = avg, fill = depth)) +
  geom_bar(stat = "identity", position = position_dodge()) + 
  geom_errorbar(aes(ymin = avg-sd, ymax = avg+sd), width=.2,
                position=position_dodge(.9)) +
  theme_bw()
#-> not finished but i guess this should be enough to see where i want to go with it. Is this ok?

#dry matter
Rohdaten_shoot <- read.csv2("precrop data C.csv")

ms1 <- Rohdaten_shoot %>% group_by(treatment) %>% summarise(avg = mean(DM_mean), sd = sd(DM_mean))

ms2 <- filter(ms1, treatment == 5)
ms3 <- filter(ms1, treatment == 6)

ms1 <- bind_rows(ms2, ms3)

ms1 <- transform(ms1, treatment = as.factor(treatment))

ggplot(ms1, aes(x = treatment, y = avg, fill = treatment)) +
  geom_bar(stat = "identity", position = position_dodge()) + 
  geom_errorbar(aes(ymin = avg-sd, ymax = avg+sd), width=.2,
                position=position_dodge(.9)) +
  theme_bw()
#we could also visualize this as a table i think

#statistical analysis
#checking for normality
q <- filter(Rohdaten_shoot, treatment == 5)
w <- filter(Rohdaten_shoot, treatment == 6)

qw <- bind_rows(q, w)

ms1 <- transform(ms1, treatment = as.factor(treatment))

shapiro.test(q$DM_mean)
shapiro.test(w$DM_mean)

#one factorial anova
model1 <- lm(DM_mean ~ treatment, data = qw)
model2 <- lm(DM_mean ~ 1, data = qw)

par(mfrow = c(2, 2))
plot(model1)
plot(model2)
par(mfrow = c(1, 1))
#shapiro test suggests that the data is not normal distributed, 
#based on the results of the normalqq-plot i would say that it is though.

anova(model1, model2)
#model1 is better than model2 -> treatment has an impact on DM_mean,
#which however is not significant

#t-test
t.test(q$DM_mean, w$DM_mean)
#p=0.16 -> similar results as in the one factorial anova

#two factorial anova, introducing fieldrep as a random factor (not sure if this makes too much sense
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
####










