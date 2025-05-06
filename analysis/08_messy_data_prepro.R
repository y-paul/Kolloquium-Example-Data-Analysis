rm(list=ls())
library(tidyverse)

# this is a dataset based on a real limesurvey output
# VP codes are randomly generated (so not real),
# datetimes are randomly generated (so not real)
# all questionnaire scores have been randomized, so that there is no
# actual reasonable data for a real analysis and fully anonymous
# age and sex have also been randomized
# dataset only for demo purposes

data  <- read.csv("data/messy_example/results.csv", na.strings="NA")


# okay this is messy, lets inspect:
# Questionnaires:
#   - demographics
#   - PHQ (one variable is not 1-3 but A1-AX coded, that we have to fix)
#   - BDI (where the variable names have been named after symptoms, not BDI1, BDI2 etc.)
#   - UIS, PSWQ, GAD7 and ERQ Questionnaires
#   - for the sake of brevity I will only do PHQ, BDI and UIS, but everything we do can be
#     applied to the other vars

# okay lets look at our missing stuff (here we need some knowledge about limesurvey)
# 1. All participants who completed the  questionnaire (pressed "Abschicken" at the very end)
#    will have a date in the variable "submitdate"
# --> We know these have completed the questionnaire for sure!
# all the others have not. There are some participants who only have the PHQ,
# but than have one answer with A1, and then only NAs. These participants have answered
# the suicidality item of the phq > 1, so get a text displayed and then close the browser window
# because these do not have a submitdate, they are also covered by 1.
# there could be some participants which have stopped at some point in the survey. These do not have a submitdate.
# 2. In theory there could be some participants, which completed the whole questionnaire, but
# on the very last page, instead of "abschicken" they just closed the browser window. In that
# case they will have a full dataset, but now submitdate. On visual inspection this does not seem to be
# the case in our dataset, but lets check later anyway.
# 3. There might be some test runs of us experimenters in here. The only way to discern these from real
# vps is by either manually checking the VP-Code, or by knowing the correct dates - sorry, no automated way here
# so lets do this step by step:




# Step one: The easy stuff

# first lets get all data with submitdates because we know these are for real
# note that missing values are actually empty strings ("") here, not NAs
complete_data <- data[data$submitdate!="", ]

incomp_data <- data[data$submitdate=="",]


## --> Okay, lets scan the incomplete data for the other cases

# Step two: Are there any participants who have no submitdate, but complete questionnaires?
# hint: order of columns in limesurvey is always order of presentation (if not randomized),
# therefore lets see if the last real item (PSWQ1.PSWQ16.) is answered anywhere
# (btw, PSWQ is spelled wrong in the dataset...)

# are there any values where PSWQ16 is not na in here?
any(!is.na(incomp_data$PSQW1.PSWQ16.))
# --> no, so this is not the case, we do not have to worry (haha) about it
# if there would be some:
# rbind(complete_data, incomp_data[!is.na(incomp_data$PSQW1.PSWQ16.),])
# --> that would join the complete dataset with these cases




# step3: are there people with no submite-date which stopped at some point in the survey?

# this calculates the number of not NAs in each row (=each participant)
incomp_data$n_nas <- rowSums(!is.na(incomp_data))
# a lot here are ~22, thats because these are the datestamps and such, so this is the number
# of not-NAs you get when you open and start the questionnaire, but do not fill out any questions
# some are 38, these are the ones who completed the PHQ but then got sorted out.
hist(incomp_data$n_nas)
# --> if there would have been people here with values > 38, we might want to include them anyway
# (here not the case). We could have an hard threshhold for example:
# n_nas <- incomp_data$n_nas
# incomp_data$n_nas <- NULL
# rbind(complete_data, incomp_data[n_nas>55],)
# or just add them manually after visual single case inspection:
# incomp_data$n_nas <- NULL
# rbind(complete_data, incomp_data[vpcode=="XXXXXX"],)


# step 4: Eliminate test runs

# if there are some obvious ones, I can just delete manually, because for example
# I know my personal VP code or because the vp code is "test123"

complete_data <- complete_data[complete_data$vpcode!="1199MLNK6875",]

# sometimes I want to include a date range, which is a bit more complicated
# please note that this only works with the specified format! If you dates are different
# then you need to adapt that

# first convert to not just a string, but a DateTime Object
dates <- as.POSIXct(complete_data$submitdate, format = "%Y-%m-%d %H:%M:%S")
cutoff <- as.POSIXct("2085-05-03 12:00:00")
complete_data <- complete_data[dates>=cutoff,]
# --> includes all submitdates made after cutoff (maybe the day we activated the study
# on SONA)




# --> okay now complete_data is actually complete. Lets get to fixing the vars


# first of: lets fix the phq item with the wrong coding (nr. 2)

complete_data$PHQ2 <- recode(complete_data$PHQ2,
                             A1 = 0,
                             A2 = 1,
                             A3 = 2,
                             A4 = 3)


# okay the bdis are really named awfully
# here students used the symptom titles instead of numbers

# selects fist bdi item (Traurigkeit) and last one (sexuellesInteresse)
# and recodes them to bdi + number
# please take care, that these are actually in the right order and 
# are actually sequential with nothing in between
complete_data <- complete_data %>% rename_with(
  .fn = ~paste0("bdi", seq_along(.)),
  .cols = Traurigkeit:sexuellesInteresse
)

# bdi items are sometimes coded 2a, 2b ( for example too much or not enough sleep),
# for the analysis this is just =2, so lets change it

#--> `` are backticks and necessary, cause 2 = ... is not a valid r command because it starts with a number
complete_data <- complete_data %>% 
  mutate(across(bdi1:bdi21, ~recode(.x,
                                    `1` = 1,
                                    `2` = 2,
                                    `3` = 3,
                                    `1a` = 1,
                                    `1b` = 1,
                                    `2a` = 2,
                                    `2b` = 2,
                                    `3a` = 3,
                                    `3b` = 3,
                                    `0` = 0)))
# recodes all bdi variabls according to recode




# so lets also do the UIS scale

complete_data <- complete_data %>% rename_with(
  .fn = ~paste0("UIS", seq_along(.)),
  .cols = UIS01.UIS18001.:UIS01.UIS18018.
)



# nice, lets calculate the sum scores


complete_data <- complete_data %>% 
  mutate(bdi_sum = rowSums(across(bdi1:bdi21)),
         phq_sum = rowSums(across(PHQ01:PHQ9)),
         uis_sum = rowSums(across(UIS1:UIS18)))


# when you code stuff like that, always good check one or two participants by hand


complete_data %>% select(PHQ01:PHQ9)

# first row is 1 + 0 +1+1+2+1+1+0+0 = 7
# second row is 1+1+0+2+2+1+2+0+0 = 9
# lets check
complete_data$phq_sum[1:2]
# correct!



# okay lets get a clean dataset. We can either delete everything we do not need, or choose what to keep
# lets choose what to keep here, because its much less then what we want to loose
# if we decide to need more vars, we can always just add here and rerun the script

data <- complete_data %>% 
  select(id, submitdate, vpcode,
         Alter,
         Geschlecht,
         Schulabschluss,
         Berufabschluss,
         Studium,
         Studienfach,
         uis_sum,
         phq_sum,
         bdi_sum)

# okay on inspection: Lets not use empty strings, but real NAs for missing studienfach angaben

data$Studienfach[data$Studienfach==""] <- NA



# lets save
write.csv(data, "data/messy_example/clean.csv")

# voila
