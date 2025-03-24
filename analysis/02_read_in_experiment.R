# This will not be discussed in the syllabus
# read this to understand how to read in individual participant files

rm(list=ls())
library(tidyverse)



# get all the experiment files listed in a specific folder
# pattern gets us all files with a .csv ending ($ is end of name)

all_files <- list.files("data/experiment", pattern="csv$")




# lets read one in to have a look (paste0 joins two strings):

test_file <- read.csv(paste0("data/experiment/", all_files[1]))
View(test_file)


# okay this looks messy - this is in long format, there is missing data in here.
# the first 4 and the last 2 rows are just NA and can be dropped. 
# How do we want to preprocess this? Well we have two approaches, and we are going to do
# both. First we will just calculate indices that are interesting to us, second
# we will create a long_format dataset with all datasets combined
# the first one is interesting for group differences and correlations,
# the second one for a more in depth look into the experiment









#====================================#
#== Approach 1 ======================#
#== Calculating Indices==============#
#====================================#

# okay what are we interested in? 
# We might want:
# - Medians of reaction times for all stimuli (happy sad)
# - Number of correct answers/number of false answers for each stimulus type
# - number of missed trials
# --> so lets go!


# We now define a function. A function is reusable bit of code (ask ChatGPT for explanation)
# this is particularly usefull here, because we plan on repeating the same thing
# (calculating individual indices) about ~100 times, and we do not want to do that manually


# define a function called calc_indices that takes a df from a participant as input
calc_indices <- function(participant_df) {

  # first drop the NA rows (double check in your data if this is the same in every file!)
  # this removes the first four and last 2 rows
  df <- participant_df[5:(length(participant_df$X)-2),]
  
  # get participant code
  vp_code <- df$code[1]
  
  # could also do something like for a more automated approach:
  # participant_df[!(is.na(participant_df$stim)),]
  
  # recode responses
  # see below for comment on treating NAs
  df <- df %>% mutate(response=recode(response,
                                      correct=T,
                                      false=F,
                                      .missing=F))
  
  
  
  # here we calculate median reaction time, relative correct and false
  # and number of NAs
  # note that here we handle NAs by just coding them as false answers, however it could be crucial
  # to have a closer look on how many these are and if you want to treat them differently
  sum_df <- df %>% group_by(stim) %>% 
    summarise(rt_m = median(rt, na.rm=T),
              rel_correct = mean(response),
              rel_false = mean(!response),
              number_na = sum(is.na(rt)))
  
  
  # no we prep a single row data frame that can later be used to rbind together
  
  return_df <- data.frame(
    code = vp_code,
    rt_happy=sum_df$rt_m[1],
    rt_sad=sum_df$rt_m[2],
    correct_happy=sum_df$rel_correct[1],
    correct_sad= sum_df$rel_correct[2],
    false_happy= sum_df$rel_false[1],
    false_sad= sum_df$rel_false[2],
    n_na_happy = sum_df$number_na[1],
    n_na_sad = sum_df$number_na[2]
  )
  
  # return this
  return(return_df)
}



# now lets do it for all data files
# first lets prep an empty df to append to

# Zero observations but all variables names included
experiment_df <- calc_indices(test_file)[NULL,]



# lets loop through every file we have

# for every file in all_files, do the following
for (p_file in all_files) {
  
  # for reading in data
  full_filename = paste0("data/experiment/", p_file)
  print("Working on:")
  print(full_filename)
  print("")
  print("")
  
  # We use tryCatch here, because often there are incomplete data files
  # in the folder, with experiments that just got started and stopped quickly
  # so reading these will lead to errors. When tryCatch runs, it tries to execute
  # the code in the first {}-brackets, if there is an error, it will just print the error
  # and continue with the next - ask ChatGPT for clarification. If you loop returns weird stuff
  # try maybe remove tryCatch to better catch the error
  tryCatch(
    {
      # 1: read data file
      pp_dat <- read.csv(full_filename)
      
      # 2: Apply our function, get one line df as return
      processed <- calc_indices(pp_dat)
      
      # 3: bind together with all previous ones
      experiment_df <- rbind(experiment_df, processed)
    }, error = function(mssg) {print(mssg)}
  )

}


summary(experiment_df)
# --> worked nicely! No lets combine this with questionnaires and we are good to go

q_data <- read.csv("data/processed/full_data_wide.csv")
experiment_df <- left_join(experiment_df, q_data, by=c(code="vp_code"))

# there will be a bit of participants with NA in questionnaires - these are the ones
# we excluded here, because the had missing in post and follow-up - you could intergrate these from
# missing_data_wide.csv or do something else - here I will not be handling them seperatly


write.csv(experiment_df, "data/processed/experiment_wide.csv")






#====================================#
#== Approach 2 ======================#
#== Long format        ==============#
#====================================#

# so lets bring all participants into a full long format for further analysis
# of the experiment


# read first file and remove all unneccessary NA rows, again check if
# the same for all files
exp_long <- read.csv(paste0("data/experiment/", all_files[1]))
exp_long <- exp_long[5:(length(exp_long$X)-2),]


# now lets loop through all files again

for (file in all_files[2:length(all_files)]) {
  
  
  
  filename = paste0("data/experiment/", file)
  print("Now Working on:")
  print(filename)
  print("")
  
  tryCatch({
    dat <- read.csv(filename)
    dat <- dat[5:(length(dat$response)-2),]
    exp_long = rbind(exp_long, dat)
  },
  error = function(mssg) {
    print(mssg)
  })

}

summary(exp_long)


# now lets join this with the questionnaire data
exp_long <- left_join(exp_long, q_data, by=c(code="vp_code"))

# please note the data structure here: while the experiment data is in long format,
# the questionnaires are wide, meaning the exact same values are repeated for every long point
# in this df - that means mean(exp_long$bdi_overall) does NOT produce the
# expected result. Sometimes this format can however be handy. 

# lets save:

write.csv(exp_long, "data/processed/experiment_long.csv")





