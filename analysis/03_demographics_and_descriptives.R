# So lets do the descriptive analysis.
# What do we want to do?
# - Demographic tables for all variables
# - small look into missing data
# - look at some item statistics for the BDI-II and the HOPE questionnaire
# - plotting we will do later


rm(list=ls())

library(tidyverse)
library(vtable)
library(psych)

data <- read.csv("data/processed/full_data_wide.csv")


# lets first get a general table going with some info
sumtable(data[,c("group", "age", "sex")],
         add.median=T)



# okay, lets do some more stuff, but split between the groups, including some simple
# tests for us
sumtable(data[,c("group", "age", "sex", "gad7",
                 "hope", "bdi_t0", "post_bdi", "follow_up_bdi")],
         group="group", group.test=T)

# --> Okay nice!


# confidence intervals around bdi_t0? For each group?
t.test(data$bdi_t0[data$group=="c"])[["conf.int"]]
t.test(data$bdi_t0[data$group=="p"])[["conf.int"]]





# correlation matrix?


round(cor(data %>% select(bdi_t0, post_bdi, follow_up_bdi, gad7, hope)),2)






# lets do some item statistics!

alpha(data %>% select(bdi1:bdi21),
      check.keys=F,
      )
# --> We only need to do this when we do questionnaire analysis...






# okay next step: lets look at the missing data


missing_data <- read.csv("data/processed/missing_data_wide.csv")

sumtable(missing_data %>% select(group, age, sex,
                        gad7, hope, bdi_t0,
                        post_bdi, follow_up_bdi))


# lets do a loose comparison between both on some variables

full_data <- rbind(
  missing_data,
  data
)
full_data$source <- rep(c("missing", "full"),
                        times=c(14, 86))


sumtable(full_data %>% select(
  group, age, sex, bdi_t0, source
), group="source", group.test=T)


# okay no systematic differences!
# lets get to hypothesis testing!


