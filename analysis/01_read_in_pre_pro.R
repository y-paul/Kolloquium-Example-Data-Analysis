library(tidyverse)


#======================================#
#=== read in data t0 ==================#
#======================================#



data_t0 <- read.csv("data/t0_data.csv")

# Overview
summary(data_t0)




# What needs to be done?
# -> We need to calculate the total BDI and Hopelessness Score
# Everything else seems to be fine



bdi_items <- c(
  "bdi1",
  "bdi2",
  "bdi3",
  "bdi4",
  "bdi5",
  "bdi6",
  "bdi7",
  "bdi8",
  "bdi9",
  "bdi10",
  "bdi11",
  "bdi12",
  "bdi13",
  "bdi14",
  "bdi15",
  "bdi16",
  "bdi17",
  "bdi18",
  "bdi19",
  "bdi20",
  "bdi21"
)

# shorter alternative that might only work depending on how the variables
# are named:
#bdi_items <- colnames(data_t0 %>% select(bdi1:bdi21))



# add to dataset
bdi_t0 <- rowSums(data_t0[,bdi_items])
data_t0$bdi_t0 <- bdi_t0



# lets do the hopelessness questionnaire
# first: Recode item 2 and 3
data_t0$hope2n <- 4 - data_t0$hope2
data_t0$hope3n <- 4 - data_t0$hope3

data_t0$hope <- data_t0$hope1+
  data_t0$hope2n +
  data_t0$hope3n +
  data_t0$hope4



# Lets clean up: 
#data_t0 <- data_t0 %>% select(vp_code, group, age, sex, gad7, hope, bdi_t0)
# I am going to keep the BDI items here to do item analysis later, but
# for the research question you could just drop them via the line commented out
rm(bdi_items, bdi_t0)

#--> T0 ready to go!





#======================================#
#=== read in data follow up ===========#
#======================================#

data_fu <- read.csv("data/follow_up_data.csv")

summary(data_fu)

# what needs to be done?
# -> all questionnaire scores already calculated
# -> but missing data in here - how to deal with that?
# -> Here we want to do listwise deletion and just remove them, but we want to keep
# the set of the missing data to maybe look into missing characteristics
# --> But first we need to join both datasets

full_df <- left_join(data_t0, data_fu, by=c("vp_code"="vp_code"))


# Handle the missing:
# so looking at the data you  can see that some miss "post", even more miss
# "post" and "follow_up"

# is either post, follow up, or both missing?
is_missing <- is.na(full_df$post_bdi) | is.na(full_df$follow_up_bdi)

# get all missing data into one DF

data_missing <- full_df[is_missing, ]
data_clean <- full_df[!(is_missing),]


# sanity check:

dim(data_missing)[1] + dim(data_clean)[1]
# --> Check





# okay all necessary preprocessing done, lets save the processed files for the
# analysis



# create directory if it does not exist
dir.create("data/processed", showWarnings = FALSE)


write.csv(data_clean, "data/processed/full_data_wide.csv")
write.csv(data_missing, "data/processed/missing_data_wide.csv")







