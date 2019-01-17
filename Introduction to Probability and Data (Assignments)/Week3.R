library(statsr)
library(dplyr)
library(ggplot2)

load(url("https://stat.duke.edu/~mc301/data/hot_hand.RData"))
names(kobe_basket)

#1. Fill in the blank: A streak length of 1 means one ___ followed by one miss.
kobe_streak <- calc_streak(kobe_basket$shot)
ggplot(data = kobe_streak, aes(x = length)) +
  geom_histogram(binwidth = 1)

#3. Which of the following is false about the distribution of Kobe’s streak lengths from the 2009 NBA finals.
kobe_streak %>%
  summarise(shortest = min(length), IQR = IQR(length), longest = max(length))

#4. If you were to run the simulation of the independent shooter a second time, how would you expect its streak distribution to compare to the distribution from the exercise above?
## Simulating the Independent Shooter
shot_outcomes <- c("H", "M")
sim_basket <- sample(shot_outcomes, size = 76, replace = TRUE)
table(sim_basket)
