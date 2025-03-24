library(tidyverse)
library()

data_long <- read.csv("data/processed/experiment_long.csv")
data_wide <- read.csv("data/processed/experiment_wide.csv")

# okay lets analyze the experiment
# I will do some interesting analysis here, there are multiple possible ones
# this is just a sample on what you could look at






# Nr. 1: Do people get faster in the course of the experiment?
# this is often interesting to see for the internal validity of the experiment
# --> Leveraging the trial_num variable


data_long %>% group_by(trial_num) %>% 
  summarize(m = median(rt, na.rm=T), sem=sd(rt, na.rm=T)/sqrt(length(rt))) %>% 
  mutate(upper = m + sem, lower = m - sem) %>% 
  ggplot(aes(x=trial_num, y= m, ymin= lower, ymax = upper)) +
  geom_smooth(se=F)+
  geom_point(color="black", fill="white") + 
  geom_linerange() +
  theme_classic()

# --> No trend here, if people would be getting better, we would see a clear trend downwards
# (faster, more practice), if people would be getting tired, we would see a trend upwards (slower)


# often enough the trends are only visible when we "bin" the trials nums, (meaning grouping these into a number
# of time slots), so lets try that here aswell

data_long %>% arrange(trial_num) %>% 
  mutate(bin = cut(trial_num, 6, labels=paste0("bin", 1:6))) %>% 
  group_by(bin) %>% 
  summarize(m = median(rt, na.rm=T), sem=sd(rt, na.rm=T)/sqrt(length(rt))) %>% 
  mutate(upper = m + sem, lower = m - sem) %>% 
  ggplot(aes(x=bin, y= m, ymin= lower, ymax = upper, group=1)) +
  geom_line() +
  geom_point(color="black", fill="white", size=5, shape=4, stroke=1.2) + 
  geom_linerange() +
  theme_classic()

# interesting! Here we see people get a bit faster over time




# Nr. 2: Are time and errors connected to each other?

data_long %>% group_by(code) %>% 
  mutate(response = recode(response, correct=T, false=F)) %>% 
  summarize(m = median(rt, na.rm=T), n_errors = sum(!response, na.rm=T)) %>% 
  ggplot(aes(x = n_errors, y = m)) +
  geom_smooth(method="lm", se=F) + 
  geom_point(position = position_jitter(width=0.5), alpha=.6) + 
  theme_classic()
# sort of --> Could look into that per group etc., but lets keep it for now





# Nr. 3: How are RTs distributed over the whole sample?
# this is usually really interesting to look at

data_long %>% ggplot(aes(x=rt)) + 
  geom_histogram(bins=sqrt(6000), color="black", fill="white") +
  theme_classic()
# --> Thats a classic RT shape right there! Lets see if that is different for groups and for conditions


data_long %>% filter(!(is.na(group)), group=="p", stim=="happy") %>% 
  ggplot(aes(x=rt)) + geom_histogram(bins=sqrt(1500), color="darkblue", fill="darkblue", alpha=.5) +
  geom_histogram(data= data_long %>% filter(!(is.na(group)), group=="p", stim=="sad"),
                 aes(x=rt), fill="darkred", color="darkred", alpha=.5,bins=sqrt(1500)) + theme_classic() +
  labs(title="Patient Happy + Sad")

data_long %>% filter(!(is.na(group)), group=="c", stim=="happy") %>% 
  ggplot(aes(x=rt)) + geom_histogram(bins=sqrt(1500), color="darkblue", fill="darkblue", alpha=.5) +
  geom_histogram(data= data_long %>% filter(!(is.na(group)), group=="c", stim=="sad"),
                 aes(x=rt), fill="darkred", color="darkred", alpha=.5,bins=sqrt(1500)) + theme_classic() +
  labs(title="Control Happy + Sad")





# Nr. 4: Lets get to the main point: Does RT, Number of Errors and number of NAs 
# differ between groups and conditions?


# RT for whole sample
data_long %>% filter(!is.na(group)) %>%  ggplot(aes(x=stim, y = rt, fill = group)) +
  geom_boxplot(notch=T) +
  theme_classic()
# this is way too much noise for inference in this case


# now on participant level

data_long %>% filter(!is.na(group)) %>% group_by(stim, group) %>% 
  summarize(m = median(rt, na.rm=T), sd = sd(rt, na.rm=T)/sqrt(length(rt))) %>% 
  mutate(upper = m + sd, lower = m - sd) %>% 
  ggplot(aes(x=stim, color=group, y=m, ymin = lower, ymax= upper)) + 
  geom_point(position=position_dodge(width=.5), size=2) + 
  geom_linerange(position=position_dodge(width=.5))
# --> Difference of condition but nothing between groups




# Number of errors:

data_wide %>% filter(!is.na(group)) %>%  pivot_longer(cols = c(false_happy, false_sad)) %>% 
  ggplot(aes(x= name, y= value, fill = group)) +
  geom_boxplot(notch=T) + 
  theme_classic()

# --> interesting, here we have a group effect! Is that significant?

t.test(data_wide$false_sad[data_wide$group=="p"],
       data_wide$false_sad[data_wide$group=="c"])
# --> Yes!




# Now lets do number of nas

data_wide %>% filter(!is.na(group)) %>% 
  pivot_longer(cols=c(n_na_happy, n_na_sad) ) %>%
  ggplot(aes(x= name, y = value, color=group)) +
  geom_point(position=position_jitter(width=.1), alpha=.5) +
  theme_classic()
# hmm it is kind of difficult to see, lets overlay means there 

data_plot <- data_wide %>% filter(!is.na(group)) %>% 
  pivot_longer(cols=c(n_na_happy, n_na_sad) ) %>% group_by(name, group) %>% 
  summarize(m = mean(value))



data_wide %>% filter(!is.na(group)) %>% 
  pivot_longer(cols=c(n_na_happy, n_na_sad) ) %>%
  ggplot(aes(x= name, y = value, color=group)) +
  geom_point(position=position_jitter(width=.1), alpha=.5)+ 
  geom_point(position=position_dodge(width=.3), data=data_plot, aes(x=name, color=group, y = m), shape=17, size=3) +
  theme_classic()
# --> Nah, nothing interesting







# Nr. 5: does reaction time to sad faces correlated with bdi at baseline?



with(data_wide, cor.test(bdi_t0, rt_sad))
with(data_wide %>% filter(group=="c"), cor.test(bdi_t0, rt_sad))
with(data_wide %>% filter(group=="p"), cor.test(bdi_t0, rt_sad))


ggplot(data = data_wide %>% filter(!is.na(group)), aes(x = bdi_t0, y = rt_sad, color=group)) + 
  geom_smooth(method="lm", se=F, color="grey") + 
  geom_point(alpha=.5) + 
  theme_classic()
# --> Nah, it does not





