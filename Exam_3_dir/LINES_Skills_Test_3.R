library(tidyverse)
library(patchwork)
library(janitor)
library(easystats)
library(modelr)
library(broom)

# Task 1: Load the csv files

money <- read_csv(file = "FacultySalaries_1995.csv")

oil <- read_csv(file = "Juniper_Oils.csv")

# Tidy the data
# Let's use pivot_longer to make each row contain only one observation

money1 <-
  pivot_longer(money, 5:17, names_to = "Observation", values_to = "Total")

# Re-create the graph shown in "fig1.png"

money1 %>% filter(grepl('Salary', Observation)) %>%
  filter(!grepl('AvgProfSalaryAll', Observation)) %>%
  filter(!grepl('VIIB', Tier)) %>%
  ggplot(mapping = aes(y = Total,fill = Observation))+
  geom_boxplot()+facet_grid(~Tier)+theme_minimal()+
  scale_fill_discrete(name = "Rank", labels = c("Assist", "Assoc", "Full")) +
  labs(y = "Salary",x = "Rank") +theme(axis.text.x = element_text(angle = 45))

# This looks great, let's call this graph p1

p1 <- money1 %>% filter(grepl('Salary', Observation)) %>%
  filter(!grepl('AvgProfSalaryAll', Observation)) %>%
  filter(!grepl('VIIB', Tier)) %>%
  ggplot(mapping = aes(y = Total,fill = Observation))+
  geom_boxplot()+facet_grid(~Tier)+theme_minimal()+
  scale_fill_discrete(name = "Rank", labels = c("Assist", "Assoc", "Full")) +
  labs(y = "Salary",x = "Rank") +theme(axis.text.x = element_text(angle = 45))

# Task 2, Build an ANOVA model and display the summary output in your report.
# let's modify the money data set to build an ANOVA model

money <- pivot_longer(money,contains("Salary"),
                      names_to = "Rank_Salary",
                      values_to = "Salary")
# Add an aov

aov(money,formula = money$Salary ~
      money$Tier+money$Rank_Salary+money$State)

# Task 3, tidy the Juniper_Oils data set

oil <- pivot_longer(oil, 11:33, names_to = "ChemicalID")

# Task 4, plotting

oil %>% 
  ggplot(mapping = aes(x = YearsSinceBurn,y = value)) +
  geom_smooth(method = "loess") +
  facet_wrap(~ChemicalID, scales = "free") +
  theme_minimal() +
  labs(y = "Concentration")

# This is perfect, let's call this p2

p2 <- oil %>% 
  ggplot(mapping = aes(x = YearsSinceBurn,y = value)) +
  geom_smooth(method = "loess") +
  facet_wrap(~ChemicalID, scales = "free") +
  theme_minimal() +
  labs(y = "Concentration")

# Task 5, Use a generalized linear model to find which chemicals show concentrations 
# that are significantly (significant, as in P < 0.05) affected by “Years Since Burn”.

mod1 <- glm(formula=oil$Yield_percent~
              oil$YearsSinceBurn)

tidy(mod1) %>%
  filter(p.value< 0.05)

