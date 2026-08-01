############
# Initial data read and explorationf for egg oodor glmm project
# M.kinneen 01/08/2026
############
library(ggplot2)
library(here)
library(dplyr)
library(tidyr)
library(readxl)



#Read and check data
dat <- read_excel(here("data","Egg Odor Experiment 2015-2020-2025.xlsx"), sheet = "Results - simplified")
str(dat)
head(dat)


#Pull sculping and 3 -spine
long_dat <- dat|>
  select(!c("9-spine","charr","rainbow trout"))|>
  pivot_longer(cols = c("sculpin","3-spine"),names_to = "species")


#Start by just looking at means across groups
ggplot(long_dat, aes(x = treatment, y = log(value)))+
  geom_boxplot()+
  facet_grid(species ~ year, scales = "free")

#Check the dispersion
long_dat|>
  group_by(species,year)|>
  summarise(
    mean = mean(value),
    var = var(value)
  )

#Check for zeros
ggplot(long_dat, aes(x = value, fill = species))+
  geom_histogram()+
  facet_wrap(~treatment)+
  theme_classic()


##store the long data
write.csv(x = long_dat,here("data","sculpin_stickleback_long.csv"))
