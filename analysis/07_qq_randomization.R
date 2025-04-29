library(tidyverse)
set.seed(699) # keep this so the data below is the same always for demo

data <- read.csv("data/processed/full_data_wide.csv")


# is bdi0 normally distributed within the patient group?

data <- data %>% filter(group=="p")

data %>% ggplot(aes(sample=bdi_t0)) +
  stat_qq() + stat_qq_line()

# --> okay, but are these deviations okay or are they not? We do not really have a reference
# how much deviation is okay?
# Consider the following, where we know (because we simulate ourself) that the data is actually normally distributed:


dat_demo <- data.frame(x = rnorm(20, 0, 5)) 

dat_demo %>% 
  ggplot(aes(sample=x)) + 
  stat_qq() + stat_qq_line()
# --> This looks a little wonky (you might have to try multiple times), but the data
# is normally distributed!
# consider exactly the same distribution, but with more samples

dat_demo <- data.frame(x = rnorm(2000, 0, 5)) 

dat_demo %>% 
  ggplot(aes(sample=x)) + 
  stat_qq() + stat_qq_line()
# this looks almost perfect
# so sample size influences how strong the deviation is we allow in qq plots



# Idea: Let's plot our real qq-plot, and then simulate data drawn from a 
# normal distribution with exactly the same sample size, mean and sd as the real data
# we do that not once, but ~10 times
# then if we do a qqplot for these simulated datasets, we get a feeling for the deviation in qqplots
# that is okay given the sample size and can interpret our real qqplot accordingly



# get values
bdi <- data$bdi_t0
n_bdi <- length(bdi)
m_bdi <- mean(data$bdi_t0)
sd_bdi <- sd(data$bdi_t0)

# prep a df: source is for differntiating between the real and the simulated data set
sim_data <- data.frame(bdi = data$bdi_t0,
                       source = rep("real", times=length(bdi)),
                       type=rep("real", times=length(bdi)))


# do 10 times:
for (i in 1:10) {
  # create label to differntiate simulation runs
  label <- paste0("sim", as.character(i))
  
  # simulate
  sim <- data.frame(bdi= rnorm(n_bdi, m_bdi, sd_bdi),
                    source=rep(label, times=n_bdi),
                    type=rep("sim", times=n_bdi))
  
  # join with rest
  sim_data <- rbind(sim_data, sim)
}



sim_data %>% ggplot(aes(sample=bdi, color=type)) + 
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~source) +
  theme_bw() +
  scale_color_manual(values=c("firebrick", "black"))

# --> We see, that the deviations in our data are (apart from being vaguely different
# because of scaling) within the range of deviations expected given the sample size
