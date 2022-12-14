library(tidyverse)

# This is a redo of Exam 1

#Task 1, read the cleaned_covid_data.csv file into an R data frame. 
read_csv(file ="cleaned_covid_data.csv")

data <- read_csv(file ="cleaned_covid_data.csv")

#Task 2, subset the data set to just show states that begin with "A" and save this as an object called A_states

A_states <- subset(data %>% 
                     filter(grepl("A", Province_State)))

#Task 3, create a plot _of that subset_ showing Deaths over time, with a separate facet for each state.

p1 <- A_states %>% 
  ggplot(mapping=aes(x = Last_Update,y = Deaths)) +
  geom_point() +
  geom_smooth(se=F) +
  facet_wrap(~Province_State,
             scales = "free")
p1

#Task 4, Find the "peak" of Case_Fatality_Ratio for each state and save this as a new data frame object called state_max_fatality_rate

state_max_fatality_rate <- data %>% 
  group_by(Province_State) %>%
  slice(which.max(Case_Fatality_Ratio)) %>% 
  select(-Last_Update, -Confirmed, -Deaths, -Recovered, -Active) %>% 
  arrange(desc(Case_Fatality_Ratio))

names(state_max_fatality_rate)[names(state_max_fatality_rate) == "Case_Fatality_Ratio"] <- "Maximum_Fatality_Ratio"

# Task 5, use that new data frame from task IV to create another plot
p2 <- state_max_fatality_rate %>% 
  ggplot(mapping = aes(x = reorder(Province_State, -Maximum_Fatality_Ratio),
                       y = Maximum_Fatality_Ratio)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Province_State")

p2

# Task 6, using the FULL data set, plot cumulative deaths for the entire US over time

Total_Deaths = data %>% 
  group_by(Last_Update) %>% 
  summarize(sum(Deaths))

names(Total_Deaths)[names(Total_Deaths) == "sum(Deaths)"] <- "Total"

Total_Deaths %>% 
  ggplot(mapping = aes(x = Last_Update, y = Total)) +
  geom_point() +
  labs(y = "TOTAL_DEATHS")
        
  