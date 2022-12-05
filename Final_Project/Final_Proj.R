library(tidyverse)
library(patchwork)
library(janitor)
library(easystats)
library(modelr)
library(broom)
library(ggplot2)

# Load in the 100m splits data set
data100m <- read_csv(file = "Olympic100mSplits.csv")

# Tidy the data100m  data set
df100m <- 
  pivot_longer(data100m, 2:8, names_to = "runners", values_to = "times")

colnames(df100m)[1] = "splits"

df100m$splits<-gsub("m","",as.character(df100m$splits))

# Load in the 400m splits data set
data400m <- read_csv(file = "Olympic400mSplits.csv")

# Tidy the data400m data set
df400m <- 
  pivot_longer(data400m, 2:9, names_to = "runners", values_to = "times")

colnames(df400m)[1] = "splits"

# begin some plotting
p1 <- df100m %>% 
  filter(!grepl("RT", splits)) %>% 
  filter(!grepl("TOTAL", splits)) %>% 
  filter(!grepl("Start-10", splits)) %>% 
  ggplot(mapping = aes(x = splits, y = times)) +
  geom_point() +
  facet_wrap(~runners) +
  theme(axis.text.x = element_text(angle = 90))

p1

ggsave(file = "100m_splits.png")

# plot for 400m
p2 <- df400m %>% 
  filter(!grepl("Differential", splits)) %>% 
  filter(!grepl("RT", splits)) %>% 
  filter(!grepl("0-100", splits)) %>% 
  filter(!grepl("TOTAL", splits)) %>%
  filter(!grepl("1st", splits)) %>% 
  filter(!grepl("2nd", splits)) %>% 
  ggplot(mapping = aes(x = splits, y = times)) +
  geom_point() +
  facet_wrap(~runners) +
  theme(axis.text.x = element_text(angle = 90))
  
p2

ggsave(file = "400m_splits.png")


df100m %>% 
  filter(!grepl("RT", splits)) %>% 
  filter(!grepl("TOTAL", splits)) %>% 
  filter(!grepl("Start-10", splits)) %>% 
  ggplot(mapping = aes(x = splits, y = times)) +
  geom_smooth() +
  geom_point() +
  facet_wrap(~runners) +
  theme(axis.text.x = element_text(angle = 90))












