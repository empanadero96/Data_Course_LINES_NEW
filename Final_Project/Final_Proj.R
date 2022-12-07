library(tidyverse)
library(patchwork)
library(janitor)
library(easystats)
library(modelr)
library(broom)
library(ggplot2)

# Load in the 100m splits data set
data100m <- read_csv(file = "Olympic100mSplits.csv")

# Tidy the data100m  data set, call it "df100m"
df100m <- 
  pivot_longer(data100m, 2:8, names_to = "runners", values_to = "times")

colnames(df100m)[1] = "splits"

df100m$splits<-gsub("m","",as.character(df100m$splits))

# Flip the data100m data set, call it "flip100m"
names100<-c("RT", "Start-10m", "10-20m", "20-30m", "30-40m", "40-50m", "50-60m", "60-70m", "70-80m", "80-90m", "90-100m", "TOTAL")

flip100m <- setNames(data.frame(t(data100m[,-1])), data100m[,1])

colnames(flip100m) <- names100

# Load in the 400m splits data set
data400m <- read_csv(file = "Olympic400mSplits.csv")

# Tidy the data400m data set, call it "df400m"
df400m <- 
  pivot_longer(data400m, 2:9, names_to = "runners", values_to = "times")

colnames(df400m)[1] = "splits"

#Flip the data400m data set, call it "flip400m"
names400 <- c("Start-100m", "100-150m", "150-200m", "200-250m", "250-300m", "300-350m", "350-400m", "1st200m", "2nd200m", "Differential", "TOTAL")

flip400m <- setNames(data.frame(t(data400m[,-1])), data400m[,1])

colnames(flip400m) <- names400

#Plot df100m
p1 <- df100m %>% 
  filter(!grepl("RT", splits)) %>% 
  filter(!grepl("TOTAL", splits)) %>% 
  filter(!grepl("Start-10", splits)) %>% 
  ggplot(mapping = aes(x = splits, y = times)) +
  geom_point() +
  facet_wrap(~runners) +
  theme(axis.text.x = element_text(angle = 90))

p1

#Save this 100m plot
ggsave(file = "100m_splits.png")

# plot df400m
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

#Save this 400m plot
ggsave(file = "400m_splits.png")


#Find correlation for 100m
cor.test(flip100m$TOTAL, flip100m$RT)
cor.test(flip100m$TOTAL, flip100m$`Start-10m`)
cor.test(flip100m$TOTAL, flip100m$`10-20m`)
cor.test(flip100m$TOTAL, flip100m$`20-30m`)
cor.test(flip100m$TOTAL, flip100m$`30-40m`)
cor.test(flip100m$TOTAL, flip100m$`40-50m`)
cor.test(flip100m$TOTAL, flip100m$`50-60m`)
cor.test(flip100m$TOTAL, flip100m$`60-70m`)
cor.test(flip100m$TOTAL, flip100m$`70-80m`)
cor.test(flip100m$TOTAL, flip100m$`80-90m`)
cor.test(flip100m$TOTAL, flip100m$`90-100m`)

#See that the 20-30m and 60-70m are significant; P-value less that 0.05

#Find correlation for 400m
cor.test(flip400m$TOTAL, flip400m$`Start-100m`)
cor.test(flip400m$TOTAL, flip400m$`100-150m`)
cor.test(flip400m$TOTAL, flip400m$`150-200m`)
cor.test(flip400m$TOTAL, flip400m$`200-250m`)
cor.test(flip400m$TOTAL, flip400m$`250-300m`)
cor.test(flip400m$TOTAL, flip400m$`300-350m`)
cor.test(flip400m$TOTAL, flip400m$`350-400m`)
cor.test(flip400m$TOTAL, flip400m$`1st200m`)
cor.test(flip400m$TOTAL, flip400m$`2nd200m`)
cor.test(flip400m$TOTAL, flip400m$`Differential`)

#See that 200-250m, 300-300m, 2nd200m, and Differential are significant; P-value less than 0.05



