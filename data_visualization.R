# Thesis Visualizations ######

library(ggplot2)
library(tidyverse)
library(reshape2)

# Indiviual Years using reshape2 ########

df %>% 
  select(year, 
         state, 
         traffic_crude_rate, 
         long_car_commute_alone_prop) %>%
  filter(state == "Alabama") %>% select(-state) %>%
  melt(id='year') %>% 
  ggplot(aes(x=year,y=value,color=variable)) + geom_line()


df %>% 
  select(year, 
         state, 
         traffic_crude_rate, 
         long_car_commute_alone_prop) %>%
  filter(state == "New York") %>% select(-state) %>%
  melt(id='year') %>% 
  ggplot(aes(x=year,y=value,color=variable)) + geom_line()



df %>% 
  select(year, 
         state, 
         traffic_crude_rate, 
         long_car_commute_alone_prop) %>%
  filter(state == "Mississippi") %>% 
  select(-state) %>%
  melt(id='year') %>% 
  ggplot(aes(x=year,y=value,color=variable)) + geom_line() 

# Facet Graphs by State ##################
df$year %<>% as.integer()
df %>% 
  select(year, 
         state, 
         traffic_crude_rate, 
         long_car_commute_prop) %>%
  filter(state != "District of Columbia") %>%
  ggplot(aes(x=year-2000)) + 
  geom_line(aes(y=traffic_crude_rate, color = "Traffic Deaths per 100,000"), size = 1) +
  geom_line(aes(y=long_car_commute_prop, color = "% Commute > 30 min."), size = 1) +
  facet_wrap(state ~ ., ncol = 10)  + 
  scale_color_manual(values=c("#9999CC","#CC6666")) +
  theme(panel.grid.minor = element_blank()) + 
  labs(x="", y = "", color = "Variable", title = "Traffic Deaths and Long Car Commutes")



df %>% 
  select(year, 
         state, 
         crude_rate, 
         long_car_commute_alone_prop) %>%
  filter(state != "District of Columbia") %>%
  ggplot(aes(x=year)) + 
  geom_line(aes(y=long_car_commute_alone_prop, color = "Long Commute")) +
  geom_line(aes(y=crude_rate/100, color = "Deaths")) +
  facet_wrap(state ~ ., ncol = 10)  + 
  scale_color_manual(values=c("#CC6666","#9999CC")) +
  theme(panel.grid.minor = element_blank()) + 
  labs(x="", y = "", color = "Variable", title = "Traffic Deaths and Long Commutes")

