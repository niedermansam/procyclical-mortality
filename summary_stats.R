# Summary Statistics and Regressions ###########
library(tidyverse)
library(stargazer)

# Summary Statistics #############


# Traffic Mortality
df %>% 
  as.data.frame() %>% 
  select(traffic_deaths,traffic_crude_rate,traffic_age_adjusted_rate) %>% 
  stargazer(summary = T, type ="text")


# All Cause Mortality
df %>% 
  as.data.frame() %>%
  select(deaths, crude_rate, age_adjusted_rate) %>% 
  stargazer(summary = T)


# Traffic and All-Cause Mortality
df %>% 
  as.data.frame() %>%
  select(traffic_deaths,traffic_crude_rate,traffic_age_adjusted_rate,
         deaths, crude_rate, age_adjusted_rate) %>% 
  stargazer(summary = T, digits = 1, digits.extra = 2)


# Employment and Population
df %>% as.data.frame() %>% 
  select(population, unemp_rate, unemployed, employed, labor_force) %>% 
  stargazer(summary = T, digits = 1, digits.extra = 2)


# Long Commute Proportion
df %>% as.data.frame() %>%
  select(long_commute_prop, long_car_commute_prop, long_car_commute_alone_prop) %>%
  stargazer(summary = T, digits = 1, digits.extra = 2)

names(df)
df[4:ncol(df)] %>% as.data.frame() %>% stargazer(summary = T)
# Regressions #################

# Panel Regressions
reg1 <- plm(Traffic_Crude_Rate~long_commute_prop, data = panel, model = "within")
reg1 %>% summary()

reg2 <- plm(log(deaths)~long_car_commute_alone_prop, data = panel, model = "within")
reg2 %>% summary()

reg3 <- plm(log(traffic_crude_rate)~long_car_commute_alone_prop, data = panel, model = "within")
reg3 %>% summary() 

reg3 <- plm(log(deaths)~long_car_commute_alone_prop, data = panel, model = "within")
reg3 %>% summary() 


# Naive Models (No FE)
reg4 <- lm(crude_rate~long_commute_prop, data = panel %>% as.data.frame())
reg4 %>% summary() 

reg5 <- lm(Crude_Rate~long_commute_prop, data = panel %>% as.data.frame())
reg5 %>% summary() 