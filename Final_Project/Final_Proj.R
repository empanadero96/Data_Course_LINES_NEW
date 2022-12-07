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

#Save this plot (100m_splits)
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

#Save this plot (400m_splits)
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

# Do some plotting with our new findings in the 100 meter dash
# Plot the 20-30m splits compared to total

plot2030m <- df100m %>% 
  filter(splits == "TOTAL" | splits == "20-30") %>% 
  pivot_wider(names_from = splits, values_from = times)

colnames(plot2030m)[2] <- "split"

p3 <- ggplot(plot2030m, aes(x = TOTAL, y = split, col = runners)) + 
  geom_point() +
  labs(x = "TOTAL", y = "20-30m split")
  
p3

# Save this plot (20-30m_splits)
ggsave(file = "20-30m_splits.png")

# Plot the 60-70m splits compared to total
plot6070m <- df100m %>% 
  filter(splits == "TOTAL" | splits == "60-70") %>% 
  pivot_wider(names_from = splits, values_from = times)

colnames(plot6070m)[2] <- "split"

p4 <- ggplot(plot6070m, aes(x = TOTAL, y = split, col = runners)) + 
  geom_point() +
  labs(x = "TOTAL", y = "60-70m split")

p4

# Save this plot (60-70m_splits)
ggsave(file = "60-70m_splits.png")

#Compare the 20-30m split to the 60-70m split
comp100msplits <- df100m %>% 
  filter(splits == "60-70" | splits == "20-30") %>%
  pivot_wider(names_from = splits, values_from = times)

colnames(comp100msplits)[c(2:3)]<-c("early", "late")

p5 <- ggplot(comp100msplits, aes(x = early, y=,late, color = runners))+ 
  geom_point() +
  labs(x = "20-30m split", y = "60-70m split")

p5

#Save this plot (comp100m_splits)
ggsave(file = "comp100m_splits.png")

# Do some plotting with our new findings in the 400 meter dash

#200-250m, 300-350m, 2nd200m, and Differential

#Plot the 200-250m data
plot200250m <- df400m %>% 
  filter(splits == "TOTAL" | splits == "200-250") %>% 
  pivot_wider(names_from = splits, values_from = times)

colnames(plot200250m)[2] <- "split"

p6 <- ggplot(plot200250m, aes(x = TOTAL, y = split, col = runners)) + 
  geom_point()+
  labs(y = "200-250m split")

#Save this plot(200-250m_splits)
ggsave(file = "200-250m_splits.png")

#Plot the 300-350m data
plot300350m <- df400m %>% 
  filter(splits == "TOTAL" | splits == "300-350") %>% 
  pivot_wider(names_from = splits, values_from = times)

colnames(plot300350m)[2] <- "split"

p7 <- ggplot(plot300350m, aes(x = TOTAL, y = split, col = runners)) + 
  geom_point()+
  labs(y = "300-350m split")

p7

#Whoa! p7 is by FAR the the split with the greatest correlation
#Definitely use this

#Save this plot(300-350m_splits)
ggsave(file = "300-350m_splits.png")

#Plot the data for the 2nd 200m
plot2nd200m <- df400m %>% 
  filter(splits == "TOTAL" | splits == "2nd_200") %>% 
  pivot_wider(names_from = splits, values_from = times)

colnames(plot2nd200m)[2] <- "split"

p8 <- ggplot(plot2nd200m, aes(x = TOTAL, y = split, col = runners)) + 
  geom_point()+
  labs(y = "2nd200m split")

p8

#Save this plot(2nd200m_split)
ggsave(file = "2nd200m_split.png")

#Plot the data for the Differential
plot400mdiffer <- df400m %>% 
  filter(splits == "TOTAL" | splits == "Differential") %>% 
  pivot_wider(names_from = splits, values_from = times)

colnames(plot400differ)[2] <- "split"

p9 <- ggplot(plot2nd200m, aes(x = TOTAL, y = split, col = runners)) + 
  geom_point()+
  labs(y = "1st and 2nd 200m difference")

p9

#Save this plot(1st_2nd_diff)
ggsave(file = "1st_2nd_diff.png")




