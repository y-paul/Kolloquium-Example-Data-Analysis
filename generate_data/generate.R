


# This is not meant to be understood - just as a reproducable version to generate a
# simple data structure for teaching with between/within differences and 
# a correlational structure. 

library(truncnorm) # for drawing normal variables with natural 0s
library(tidyverse) # data manipulation
library(rtdists) # for generating credible reaction time data


# Variables Generated:
# - group_vector = patients (p) or controls (c)?
# - bdi1:bdi21 = answers an all bdi items - with a generated group difference
# - gad7 = GAD7 answers correlated with the overall bdi score
# - hope1:hope4 = a short imaginary questionnaire with 4 items for hopelessness
# item 3 and 4 have to be recoded
# - age and sex, not related to our vars
# - post and follow up bdis, which are lower and subsequently slightly higher than
# the starting bdi scores
# - A reaction time experiment with correct/false answers, rts and happy/sad pictures
# vp codes for matching stored in per PP files under data/experiment, correlated with BDI

# outfiles:
# - t0_data.csv: All variables but follow-up and RT experiment variables, no missing data
# - follow_up.csv: Follow up BDIs + vp codes with missing data included
# - experiment/X_exp_faces.csv: Reaction time experiments with RT, correct/false,
# stimulus type and VP-code for each participant seperately 



# groups
group_vector <- rep(c("p", "c"), each=50)


# means and SDs for BDI item scores
mean_patient_item <- 1
mean_control_item <- 0.5
sd_patient_item <- 0.75
sd_control_item <- 0.25

# generate for both groups looped
bdi_items <- list()

for (i in 1:50) {
  bdi_items[[length(bdi_items)+1]] <- round(rtruncnorm(21, 0, 4, mean_patient_item, sd_patient_item))
}

for (i in 1:50) {
  bdi_items[[length(bdi_items)+1]] <- round(rtruncnorm(21, 0, 4, mean_control_item, sd_control_item))
}

# make into DF, clean up, calc overall score for generating correlated vars
x <- as.data.frame(t(data.frame(bdi_items)))
rownames(x) <- NULL
colnames(x) <- paste0("bdi", 1:21)
x$group <- group_vector
x$bdi_overall <- rowSums(x[,1:21])



# generate demographics
x$age <- rtruncnorm(100, 18, 60, mean=30, sd=10)
x$sex <- sample(c("m", "f", "d"),size=100, replace=T, prob=c(0.45,0.45,0.1))


# generate GAD7 correlated
gad7 <- x$bdi_overall + rnorm(100, 0, sd=1)
gad7 <- scale(gad7)
gad7 <- round((gad7 + 5) * 2.5)
x$gad7 <- gad7





# generate short hopelessness questionnaire uncorrelated
mean_patient_item <- 1.5
mean_control_item <- 0.5
sd_patient_item <- 0.75
sd_control_item <- 0.25
hope_inventory <- list()

for (i in 1:50) {
  hope_inventory[[length(hope_inventory)+1]] <- round(rtruncnorm(4, 0, 4, mean_patient_item, sd_patient_item))
}


for (i in 1:50) {
  hope_inventory[[length(hope_inventory)+1]] <- round(rtruncnorm(4, 0, 4, mean_control_item, sd_control_item))
}
y <- as.data.frame(t(data.frame(hope_inventory)))
rownames(y) <- NULL
colnames(y) <- paste0("hope", 1:4)

# recode
y$hope2 <- 4 - y$hope2
y$hope3 <- 4 - y$hope3


# bring together
x <- cbind(x, y)






# generate some pseudo-realistic VP codes
vp_code <- c() 

for (i in 1:100) {
  
  p1 <- str_flatten(sample(LETTERS,size= 2, replace=T))
  p2 <- str_flatten(sample(1:9,size= 4, replace=T))
  p3 <- str_flatten(sample(LETTERS,size= 2, replace=T))
  
  vp_code[length(vp_code)+1] <- paste0(p1, p2, p3)
}

# actually unique?
length(unique(vp_code))

x$vp_code <- vp_code





# generate offsets for post and follow-up
x$post_bdi <- x$bdi_overall
x$post_bdi[x$group=="p"] <- round(x$post_bdi[x$group=="p"] - rtruncnorm(50, 0, Inf, 15, 2.5))
x$post_bdi[x$group=="c"] <- round(x$post_bdi[x$group=="c"] - rtruncnorm(50, 0, Inf, 2, 0.5))
x$post_bdi <- ifelse(x$post_bdi<0, 0, x$post_bdi)

x$follow_up_bdi <- x$post_bdi
x$follow_up_bdi[x$group=="p"] <- round(x$follow_up_bdi[x$group=="p"] + rtruncnorm(50, 0, Inf, 2, 2.5))
x$follow_up_bdi[x$group=="c"] <- round(x$follow_up_bdi[x$group=="c"] + rtruncnorm(50, 0, Inf, 2, 0.5))
x$follow_up_bdi <- ifelse(x$follow_up_bdi<0, 0, x$follow_up_bdi)






# generate reaction time data
library(rtdists)

# drift rates as a function of BDI scores, makes them dependent
x$drift_rates <- x$bdi_overall
x$drift_rates <- x$drift_rates / max(x$drift_rates)
x$drift_rates <- 1 - x$drift_rates


all_rts <- list()

# for every participant
for (i in x$drift_rates) {
  v <- i
  
  # use a diffusion model to generate scores for picture types seperatly 
  rt_sad <- rdiffusion(n=30, a=1.5, v=v, t0=0.015)
  rt_happy <- rdiffusion(n=30, a=1.5, v=1, t0=0.015)
  
  
  # bring together and recode
  rt_df <- rbind(rt_sad, rt_happy)
  rt_df$response <- recode(rt_df$response,lower="false", upper="correct")
  rt_df$stim <- rep(c("sad", "happy"), each=30)
  
  
  # introduce NAs for reactions to long
  NAs <- rt_df$rt > 1.5
  rt_df$rt[NAs] <- NA
  rt_df$response[NAs] <- NA
  rt_df$trial_num <- 1:60
  rt_df <- rt_df[sample(1:60, 60, replace=F),]
  
  na_df <- data.frame(rt=c(NA, NA), response=c(NA, NA), stim=c(NA, NA), trial_num=c(NA, NA))
  
  rt_df <- rbind(na_df, na_df, rt_df, na_df)
  
  all_rts[[length(all_rts)+1]] <- rt_df
}







# clean up and make into separate files
x1 <- x %>% select(vp_code, group, age, sex,gad7, hope1:hope4, bdi1:bdi21)
x2 <- x %>% select(vp_code, post_bdi, follow_up_bdi)


# simulate some dropout and missing data
dropt1 <- sample(1:100, size=4)
dropt2 <- sample(1:100, size=10)

x2$post_bdi[dropt1] <- NA
x2$follow_up_bdi[dropt1] <- NA
x2$follow_up_bdi[dropt2] <- NA


# oopsie
x1$age <- round(x1$age)



# write everything to disk
write.csv(x1, "data/t0_data.csv")
write.csv(x2, "data/follow_up_data.csv")

for (i in 1:100) {
  vp_code_to_write <- vp_code[i]
  
  rt_data <- all_rts[[i]]
  rt_data$code <- vp_code_to_write
  
  filename <- paste0("data/experiment/", as.character(i), "_exp_faces.csv")
  
  write.csv(rt_data, filename)
}




