library(tidyverse)
library(modelr)
library(GGally)
library(easystats)
library(broom)

# load the file

read_csv(file = "GradSchool_Admissions.csv")

# save it as a dataframe

df <- read_csv(file = "GradSchool_Admissions.csv")

glimpse(df)

# see some general correlations between the columns
df %>% 
  select(admit,gre,gpa,rank) %>% 
  ggpairs()

# change admission from 1/0 to TRUE/FALSE
df <- df %>% mutate(admit=case_when(admit=="1"~TRUE,
                                    TRUE~FALSE))

# observe how the GRE scores and the gpa affect admission

df %>% 
  ggplot(aes(x = gre,y = gpa)) +
  facet_wrap(~admit) +
  geom_point()

# very interesting results, some students have very high GRE scores and GPA but were not admitted
# let's analyze the rank of their undergraduate studies

df %>% 
  ggplot(aes(x = gre,fill = admit)) +
  facet_grid(admit~rank) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90))

# very good plot to start, showing acceptance by undergrad rank and GRE score
# let's call this p1
p1 <- df %>% 
  ggplot(aes(x = gre,fill = admit)) +
  facet_grid(admit~rank) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90))

# let's plot the exact same data but with GPA instead of GRE scores
# let's call this p2
p2 <- df %>% 
  ggplot(aes(x = gpa,fill = admit)) +
  geom_density(alpha=0.5) +
  facet_grid(admit~rank) +
  theme(axis.text.x = element_text(angle = 90))

# plot everything we've learned into one plot, showing gre, gpa, rank, and admission
# let's call this p3
p3 <- 
  df %>% 
  ggplot(mapping = aes(x = gre, y=gpa, color=rank, shape=admit))+
  geom_point()+geom_smooth(aes(sd(FALSE)))

# make a model of the rank of the undergraduate institution factoring in gre and gpa
mod1 <- glm(data = df,formula = rank ~ gre + gpa)
summary(mod1)

# clean it up and perform the report function
tidy(mod1) %>% filter(p.value<0.05)
report(mod1)

# create a second model, this time showing admissions by gre and gpa
mod2 <- glm(data = df,formula = admit ~ gre + gpa)
summary(mod2)

# mod2 is very practical, showing significant correlation between the gre + gpa for admission
# tidy mod2, use the report function
mod2 %>% tidy() %>% filter(p.value<0.05)
report(mod2)

# compare performance, as learned in week 10
compare_performance(mod1,mod2)

# R2 is higher in mod2
# create mod3 using stepAIC
fullmod <- glm(data = df,formula = admit ~ gre + gpa + rank)
step <- MASS::stepAIC(fullmod, trace=0)
mod3 <- glm(data = df,formula = step$formula)

# compare all 3 models
comp <- compare_performance(mod1,mod2,mod3)

# make a prediction function
gpa_check <- df %>%
  gather_predictions(mod1,mod2,mod3) %>%
  ggplot(aes(x = gpa, y = pred, color=model))+
  geom_segment(aes(x = 0,y = 0,xend=4, yend=4),linetype=2,
               color="black",alpha=.5)+
  geom_smooth(method = "lm",se=FALSE)+
  facet_wrap(~admit)
