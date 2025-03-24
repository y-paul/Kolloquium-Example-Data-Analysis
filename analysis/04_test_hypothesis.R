# We are going to test assumptions and hypothesis now
# We want to test 3 hypothesis:

# Hyp 1: There is a significant, positive correlation between
# bdi at baseline and our new hopelessness questionaire

# Hyp 2: We want to test if there is a significant difference in age between both groups

# Hyp 3: We want to test our main design - a 2x3 mixed anova with the three BDI Scores
# at three points of measurement (within) and the two groups (between)

# There will be some small examples for one-way ANOVAs, Regression and comparisons of
# correlations

rm(list=ls())
library(tidyverse)
library(pwr)

data <- read.csv("data/processed/full_data_wide.csv")






# ============================ #
# Hyp. 1: There is a significant, positive correlation between
# bdi at baseline and our new hopelessness questionaire
# ============================ #


# Assumptions: Normality, Interval Scaling (automatically assumed) 

ggplot(data, aes(sample=bdi_t0)) + stat_qq() + stat_qq_line()

ggplot(data, aes(sample=hope)) + stat_qq() + stat_qq_line()


shapiro.test(data$bdi_t0)
shapiro.test(data$hope)


ggplot(data %>% filter(group=="p"), aes(sample=bdi_t0)) + stat_qq() + stat_qq_line()

ggplot(data %>% filter(group=="p"), aes(sample=hope)) + stat_qq() + stat_qq_line()


ggplot(data %>% filter(group=="c"), aes(sample=bdi_t0)) + stat_qq() + stat_qq_line()

ggplot(data %>% filter(group=="c"), aes(sample=hope)) + stat_qq() + stat_qq_line()
hist(data$bdi_t0)
hist(data$hope)
shapiro.test(data$bdi_t0[data$group=="p"])
shapiro.test(data$hope[data$group=="p"])
shapiro.test(data$bdi_t0[data$group=="c"])
shapiro.test(data$hope[data$group=="c"])


# outlier?

model <- lm(bdi_t0 ~ hope, data=data)
plot(model,4) # Cooks distance > 1 relevant oder 4/n, in diesem fall 0.047

# do the tests
cor.test(data$bdi_t0[data$group=="p"],
         data$hope[data$group=="p"])
plot(data$bdi_t0[data$group=="p"],
         data$hope[data$group=="p"])


cor.test(data$bdi_t0[data$group=="c"],
         data$hope[data$group=="c"], method="spearman")
plot(data$bdi_t0[data$group=="c"],
         jitter(data$hope[data$group=="c"], 0.4))




# ============================ #
# Hyp 2: We want to test if there is a significant difference in age between both groups
# ============================ #

age_c <- (data %>% filter(group=="c"))$age
age_p <- (data %>% filter(group=="p"))$age

qqnorm(age_c)
qqline(age_c)

qqnorm(age_p)
qqline(age_p)

shapiro.test(age_c)
shapiro.test(age_p)

# outlier?
boxplot(age_c)
boxplot(age_p)

# we dont need to test variances because we are using Welsh t-Test
# paired=T would give test for dependent samples
t.test(age_c, age_p, alternative="two.sided")


# lets also see the post hoc power

# first calc cohens d (see https://www.datanovia.com/en/lessons/t-test-effect-size-using-cohens-d-measure/)

cd <- (mean(age_c) - mean(age_p)) / (sqrt( (var(age_c) + var(age_p))/2 ))

pwr.t.test(n = (length(age_c) + length(age_p))/2,
           sig.level=.05, 
           d = cd,
           type="two.sample",
           alternative="two.sided")
# --> Power is really low here because effect size is very small




# ============================ #
# Hyp 3: We want to test our main design - a 2x3 mixed anova with the three BDI Scores
# at three points of measurement (within) and the two groups (between)
# Really usefull: https://www.datanovia.com/en/lessons/mixed-anova-in-r/
# ============================ #



library(rstatix)
library(ggpubr)
# lets first do a df where there is just what we need

anova_data <- data %>% select(vp_code, group, bdi_t0, post_bdi, follow_up_bdi) %>% 
  rename(bdi1 = bdi_t0, bdi2 = post_bdi, bdi3 = follow_up_bdi)

# now we also need that as long format

anova_data_long <- anova_data %>% pivot_longer(cols=c(bdi1, bdi2, bdi3)) %>% 
  rename(time = name, bdi = value)


# normality

anova_data_long %>% group_by(time, group) %>% 
  shapiro_test(bdi)
# --> This is actually significant, lets look at that graphically

ggplot(anova_data_long, aes(sample=bdi)) +
  stat_qq() + 
  stat_qq_line() +
  facet_wrap(group~time)
# --> This looks alright, we are going to trust this here




# Homogenity of Variance

anova_data_long %>% 
  group_by(time) %>% 
  levene_test(bdi ~ group)
# --> Not met here - we could use a different method, (robust ANOVA), but we still stay with
# this example


# Homogenity of covariance

box_m(anova_data_long[, "bdi", drop=F], anova_data_long$group)
#--> Violated, but usually ignored, especially if group differences are equal
# could do a separate analysis for both groups



# Sphericity will be calculated automatically by the test and corrections applied



# calculate anova:
res.aov <- anova_test(
  data=anova_data_long, dv=bdi, wid=vp_code,
  between=group, within= time,
  effect.size = "ges",
  detailed=T)

res.aov
get_anova_table(res.aov)



ggplot(anova_data_long, aes(y=bdi, x=time, fill=group)) +
  geom_boxplot(position=position_dodge(), notch=T) + theme_bw()














# ============================ #
# ============================ #
# Misc Analysis
# ============================ #
# ============================ #





# ============================ #
# Regression
# ============================ #

library(car)

# do GAD7, HOPE and age predict BDI at baseline?
model <- lm(bdi_t0 ~ gad7 + hope + age, data=data)

# easy assumption tests:
# 1: Linear relationship, horizontal line no clear patterns
# 2: QQ Plot
# 3: Homogenity of Variance, Horizontal line with roughly equal spreading is the goal
# 4: Outlier
plot(model)

# additional Cooks index
plot(model, 4)

# Variance Inflation Factor, should be < 1, test colliniearity
vif(model) 


# results
summary(model)





# ============================ #
# compare two correlations
# ============================ #

library(cocor)

# within group
cocor(~bdi_t0 + gad7 | bdi_t0 + hope, data=data)

# between groups
patients <- data %>% filter(group=="p") 
controls <- data %>% filter(group=="c")
cocor(~bdi_t0 + gad7 | bdi_t0 + gad7, data=list(patients, controls))







# ============================ #
# ANCOVA:
# ============================ #


# what happens when we inlcude Hope as a covariate?
anova_data <- data %>% select(vp_code, group, bdi_t0, post_bdi, follow_up_bdi, hope) %>% 
  rename(bdi1 = bdi_t0, bdi2 = post_bdi, bdi3 = follow_up_bdi)

# now we also need that as long format

anova_data_long <- anova_data %>% pivot_longer(cols=c(bdi1, bdi2, bdi3)) %>% 
  rename(time = name, bdi = value)

# calculate anova:
res.aov <- anova_test(
  data=anova_data_long, dv=bdi, wid=vp_code,
  between=group, within= time,
  covariate = hope,
  effect.size = "ges",
  detailed=T)

res.aov
get_anova_table(res.aov)







# ============================ #
# just anova for time
# ============================ #


res.aov <- anova_test(data=anova_data_long %>% filter(group=="p"), dv=bdi, wid=vp_code, 
                      within=time)
get_anova_table(res.aov)
