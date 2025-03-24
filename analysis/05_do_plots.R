
# what do we want to plot?
# lets first plot our main hypothesis!
# To remind you:
# Hyp 1: There is a significant, positive correlation between
# bdi at baseline and our new hopelessness questionaire

# Hyp 2: We want to test if there is a significant difference in age between both groups

# Hyp 3: We want to test our main design - a 2x3 mixed anova with the three BDI Scores
# at three points of measurement (within) and the two groups (between)



# we will plot some of them very extensivly, others less optimized




rm(list=ls())
library(tidyverse)


# read the data

data <- read.csv("data/processed/full_data_wide.csv")





#========================================#
#==== Hyp 1: ============================#
#========================================#

# Hyp 1: There is a significant, positive correlation between
# bdi at baseline and our new hopelessness questionaire





# Basic plot 1
data %>% ggplot(aes(x=bdi_t0, y=hope)) +
  geom_smooth(se=F, method="lm", color="grey") + 
  geom_point()
# --> This is fine, but interesting would be the plot for each group seperatly as we clearly
# see group differences in BDI


# basic plot 2
data %>% ggplot(aes(x=bdi_t0, y=hope)) +
  geom_smooth(se=F, method="lm", color="grey") + 
  geom_point() +
  facet_wrap(~group)


# okay - this looks better. For publication ready plots, what do we need to change?
# 1. Font Size must be larger
# 2. grey background is bad
# 3. panels at the top kind of ugly
# 4. Axis should have better labels
# 5. facet groups on top should have full words, not just letters
# 6. Points and lines a little bit larger
# 7. Scatter points should be somewhat rectangular
# 8. a little bit more complicated: There are overlapping points here, which hide the amount
# of data: Lets use a jitter
# 9. Lets not use the same X-Axis for both plots
# --> Let's get to work!




data %>% 
  mutate(group=dplyr::recode(group, c="Control", p="Patient")) %>%  #5. for the panels at the top - must be done this way
  ggplot(aes(x=bdi_t0, y=hope))  +
  geom_smooth(se=F, method="lm", color="grey", size=2) + 
  geom_point(position = position_jitter(width=0.5, height=0.5), size=2, alpha=.8) + # 6. and 8. 
  facet_wrap(~group, scales = "free_x") + # 9. freed up the x-axis
  theme_bw() + # 1. Removes the grey background, alternatively theme_mininal()
  labs(x="BDI Baseline [Sum]", y="Hopelessness Questionnaire [Sum]") + #4 Different Axis labels
  theme(text=element_text(size=14)) + #text size increased
  theme(strip.background = element_rect(fill="white")) + # 3. Background to white
  theme(aspect.ratio = 1) #7. Rectangular Plots
  
# --> Perfect!






#========================================#
#==== Hyp 2: ============================#
#========================================#
# Hyp 2: We want to test if there is a significant difference in age between both groups


# --> A lot of different possibilities, let's do a boxplot here!



# basic plot
data %>% 
  ggplot(aes(x=group, y = age)) +
  geom_boxplot()


# key, let's make this publication ready


data %>% 
  mutate(group = dplyr::recode(group, c="Control", p="Patient")) %>% # recode these
  ggplot(aes(x=group, y = age, fill=group)) + # lets add some color here aswell
  geom_boxplot(notch=T) + # lets add a notch
  geom_point(position=position_jitter(width=0.25, height=0), alpha=.3) + #these points are not necessary, but I like them
  theme_bw() +
  theme(text=element_text(size=14)) + # increased text size
  labs(x="Group", y="Age") +  # different lab labels
  guides(fill="none")+ # lets drop the legend, in this case we do not need it 
  scale_fill_manual(values=c("firebrick", "skyblue")) # lets change up colors here







#========================================#
#==== Hyp 3: ============================#
#========================================#
# Hyp 3: We want to test our main design - a 2x3 mixed anova with the three BDI Scores
# at three points of measurement (within) and the two groups (between)
# --> Also a lot of possibilities, lets do a classic anova point-plot here!


# this time we need to process the data a bit more


plot_data <- data %>% select(vp_code, group, bdi_t0, post_bdi, follow_up_bdi) %>% 
  pivot_longer(cols=c("bdi_t0", "post_bdi", "follow_up_bdi")) %>% 
  rename(time=name, bdi = value) %>% 
  mutate(time = dplyr::recode(time, bdi_t0 = "Baseline", post_bdi="Post", follow_up_bdi="Follow Up"),
         group = dplyr::recode(group, c = "Control", p = "Patient")) %>% 
  group_by(group, time) %>% 
  summarize(m = mean(bdi), sem = sd(bdi)/sqrt(length(bdi))) %>% 
  mutate(upper = m + sem, lower = m - sem)


plot_data %>% 
  ggplot(aes(x = time, y= m, ymin = lower, ymax = upper, color = group)) +
  geom_line(aes(group=group), position=position_dodge(width=0.5)) +
  geom_point(position=position_dodge(width=0.5)) +
  geom_errorbar(position="dodge", width=0.5)

# looks nice!
# note that position dodge is not mandatory but helps in this case
# if lines overlap less, you might want to not use that
# --> Lets make it look really good


plot_data %>% 
  ggplot(aes(x = time, y= m, ymin = lower, ymax = upper, color = group, group=group)) +
  geom_line(aes(group=group), position=position_dodge(width=0.5), size=1, alpha=.5) +
  geom_errorbar(position="dodge", width=0.5, size=1) +
  geom_point(position=position_dodge(width=0.5), size=3) +
  theme_classic() + 
  labs(x = "Time", y = "BDI Score [Sum]", color="Group") +
  theme(text=element_text(size=12)) +
  scale_color_manual(values=c("darkgreen", "darkblue"))


















# How do we optimally save a plot? 

# notice I am assigning the plot to a variable
plot1 <- plot_data %>% 
  ggplot(aes(x = time, y= m, ymin = lower, ymax = upper, color = group, group=group)) +
  geom_line(aes(group=group), position=position_dodge(width=0.5), size=1, alpha=.5) +
  geom_errorbar(position="dodge", width=0.5, size=1) +
  geom_point(position=position_dodge(width=0.5), size=3) +
  theme_classic() + 
  labs(x = "Time", y = "BDI Score [Sum]", color="Group") +
  theme(text=element_text(size=12)) +
  scale_color_manual(values=c("darkgreen", "darkblue"))



# use this to save plots precisely with 300 DPI and with precise measurements
# only change filename, plot, width and height, do not change dpi and do not
# resize plot in Word - then it will always stay with the optimal aspect ratio!
ggsave("plot1.png", plot=plot1, units="cm", dpi=300,
       width=13,
       height=10)



















#========================================#
#==== Additional types of plots =========#
#========================================#





# histogram for normality

ggplot(data=data, aes(x = age)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins=sqrt(length(data$group)), color="black", fill="white") +
  stat_function(fun=dnorm, args=list(mean= mean(data$age), sd=sd(data$age)), color="red") +
  theme_bw()




# violin plots

data %>% 
  mutate(group = dplyr::recode(group, c="Control", p="Patient")) %>% # recode these
  ggplot(aes(x=group, y = age, fill=group)) + # lets add some color here aswell
  geom_violin() + 
  geom_point(position=position_jitter(width=0.25, height=0), alpha=.3) + #these points are not necessary, but I like them
  theme_bw() +
  theme(text=element_text(size=14)) + # increased text size
  labs(x="Group", y="Age") +  # different lab labels
  guides(fill="none")+ # lets drop the legend, in this case we do not need it 
  scale_fill_manual(values=c("firebrick", "skyblue")) # lets change up colors here



# barplots

plot_data %>% 
  ggplot(aes(x = time, y= m, ymin = lower, ymax = upper, fill = group)) +
  geom_col(position="dodge", width=0.5, color="black") +
  geom_errorbar(position=position_dodge(width=0.5), width=0.3, size=1, color="black") +
  theme_classic() + 
  labs(x = "Time", y = "BDI Score [Sum]", color="Group") +
  theme(text=element_text(size=12)) 


# more creative scatterplots

ggplot(data=data, aes(x=bdi_t0, y=hope, shape=group)) +
  geom_smooth(method="lm", se=F) + 
  geom_point(position=position_jitter(width=0.5, height=0.5),
             size=2, alpha=.5) +
  theme_bw()

# or

ggplot(data=data, aes(x=bdi_t0, y=hope)) +
  geom_smooth(method="lm", se=F) + 
  geom_point(aes(shape=group),
             position=position_jitter(width=0.5, height=0.5),
             size=2, alpha=.5) +
  theme_bw()


ggplot(data=data, aes(x= age, y= hope + bdi_t0 + gad7)) + 
  geom_point() + geom_rug()




# add text to a plot


ggplot(data=data, aes(x=bdi_t0, y=hope, shape=group)) +
  geom_smooth(method="lm", se=F) + 
  geom_point(position=position_jitter(width=0.5, height=0.5),
             size=2, alpha=.5) +
  theme_bw() +
  annotate("text", x = 20, y = 1, label="test Hallo")




# put many different plots into one panel
# a lot of possibilities: https://patchwork.data-imaginist.com/articles/patchwork.html
library(patchwork)


p1 <- ggplot(data = data, aes(x=bdi_t0, gad7)) + 
  geom_point() + theme_bw()
p2 <- ggplot(data = data, aes(x=post_bdi, gad7)) + 
  geom_point() + theme_bw()
p3 <- ggplot(data = data, aes(x=follow_up_bdi, gad7)) + 
  geom_point() + theme_bw()
p4 <- ggplot(data = data, aes(x=age, gad7)) + 
  geom_point() + theme_bw()
p5 <- ggplot(data = data, aes(x=hope, gad7)) + 
  geom_point() + theme_bw()



p1 + p2 + p3 + p4 + p5



p1 + labs(tag = "A") + p2 + p3 + p4 + p5 + plot_layout(nrow=3)

p1 + p2 + p3 + p4 + p5 + plot_layout(widths=c(1,1,2))


p1 + labs(tag = "A") +
  p2 + labs(tag = "B") +
  p3 + labs(tag = "C") +
  p4 + labs(tag = "D") +
  p5 + labs(tag = "E")


