library(tidyverse)
library(patchwork)
library(janitor)
library(easystats)
library(modelr)


# Task 1
# Read in the unicef data 
dat <- read_csv(file = "unicef-u5mr.csv")

# do some exploring
unique(dat$Continent)
unique(dat$CountryName)
unique(dat$Region)

# Task 2
# Get it into tidy format
names(dat) = gsub(pattern = "U5MR.", replacement = "", x = names(dat))

dat <- dat %>% pivot_longer(cols = -c(CountryName,Region,Continent),
                            names_to = "year",
                            values_to = "Under_5_mortality_rate")

# make the year continuous

dat$year <- as.numeric(dat$year)

# Task 3
# Plot each country’s U5MR over time
dat %>% 
  group_by(year) %>% 
  ggplot(mapping = aes(x = year,y = Under_5_mortality_rate,fill = CountryName)) +
  geom_line() +
  facet_wrap(~Continent)+
  scale_x_discrete(breaks = c(1960,1980,2000))+
  theme(legend.position = "none")

# Task 4, saving the plot as LINES_Plot_1.png
LINES_Plot_1.png <- 
  dat %>% 
  group_by(year) %>% 
  ggplot(mapping = aes(x = year,y = Under_5_mortality_rate,fill = CountryName)) +
  geom_line() +
  facet_wrap(~Continent)+
  scale_x_discrete(breaks = c(1960,1980,2000)) +
  theme(legend.position = "none")

LINES_Plot_1.png

# Task 5
# Create another plot that shows the mean U5MR for all the countries within a given continent at each year
dat %>% group_by(Continent) %>%
  mutate(Under_5_mortality_rate_n= as.numeric(Under_5_mortality_rate)) %>%
  filter(!is.na(Under_5_mortality_rate_n)) %>%
  ggplot(mapping = aes(x = year,y = Under_5_mortality_rate,color = Continent)) +
  geom_line() +
  scale_x_discrete(breaks = c(1960,1980,2000))+
  theme(legend.position="none")

# Task 6
# Save that plot as LASTNAME_Plot_2.png

LINES_Plot_2.png <- 
  dat %>% 
  group_by(year) %>% 
  ggplot(mapping = aes(x = year,y = Under_5_mortality_rate,color = Continent)) +
  geom_smooth()+
  geom_line() +
  scale_x_discrete(breaks = c(1960,1980,2000))

# Task 7
# Create three models of U5MR
mod1 <- glm(data = dat,
            formula = dat$Under_5_mortality_rate~dat$year)

mod2 <- glm(data = dat,
            formula = dat$Under_5_mortality_rate~dat$year+dat$Continent)

mod3 <- glm(data = dat,
            formula = dat$Under_5_mortality_rate~dat$year*dat$Continent)


# Task 8
# Compare the three models with respect to their performance
#from what I see here, model 3 has the best r2, with AIC and BIC being lower and RMSE and Sigma
#also being lower

compare_models(mod1,mod2,mod3)

# Task 9
# Plot the 3 models’ predictions
dat %>% gather_predictions(mod1,mod2,mod3,type="response") %>%
  ggplot(aes(x = year, y = pred, color=Continent))+geom_line()+
  geom_smooth(method = "lm",se=FALSE)+
  facet_wrap(~model)+
 scale_x_discrete(breaks = c(1960,1980,2000))

# Do not delete this
# Can not fix Task5
dat %>% group_by(Continent) %>%
  mutate(Under_5_mortality_rate_n= as.numeric(Under_5_mortality_rate)) %>%
  filter(!is.na(Under_5_mortality_rate_n)) %>%
  mean(Under_5_mortality_rate) %>%
  ggplot(mapping = aes(x = year,y = Under_5_mortality_rate,fill = CountryName)) +
  geom_point() +
  scale_x_discrete(breaks = c(1960,1980,2000))+
  theme(legend.position="none")


