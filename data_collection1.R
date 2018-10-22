library(tidycensus)
library(tidyverse)
library(magrittr)

setwd("G:/procyclical-mortality")

cas <- tidycensus::load_variables(2013,'acs3')

cas %>% filter(str_detect(concept, "EMPLOYMENT STATUS$"))



setwd("C:/users/sam/Desktop/Recession-Mortality/data/county_mortality_2011-2016")

files <- list.files() %>% str_remove(",") %>% str_replace_all(" ",'_')

mort <- read.delim(list.files()[6]) %>% as.tibble()

for(i in 1:length(files)){
  foo <- read.delim(list.files()[i])
  new <- list.files()[i] %>% str_remove(",") %>% str_replace_all(" ",'_') %>% str_replace('txt$','csv') 
  print(new)
  write_csv(foo, new)
}


mort %>% select(County,Year,Crude.Rate) %>% as.data.frame()

mort %>% mutate(Crude.Rate = as.double(as.character(Crude.Rate))) %>% group_by(County) %>% 
  summarise(range = max(Crude.Rate, na.rm = T)- min(Crude.Rate, na.rm = T)) %>% summary()

mort %>% group_by(County) %>% mutate(Crude.Rate = as.double(Crude.Rate)) %>% summary()

mort %>% filter(County == 'Abbeville County, SC')

gh("/niedermansam/procyclical-mortality", type = "public")

library(gh)

bar <- blscrapeR::bls_api('LAUCN300300000000003','2000','2018')

bar$year %>% summary

"https://download.bls.gov/pub/time.series/la/la.data.64.County"

dir.create("State_Unemployment")

county_unemp <- read_delim("https://download.bls.gov/pub/time.series/la/la.data.64.County", delim = "\t")

county_unemp %<>% select(-footnote_codes) %>% map(str_trim) %>% as.tibble()

county_unemp %<>% mutate(
  fips = series_id %>% str_extract("LAUCN.....") %>% str_remove("LAUCN"),
  series = series_id %>% str_extract("..$"),
  date = paste0(year,'-',period %>% str_remove("M"))
)

county_unemp %<>% select(date,fips,series,value)
county_unemp %<>% mutate(year = date %>% str_extract("^...."))

county_unemp %>% filter(series == '03', year %>% as.integer() > 1999)



library(rvest)

# BLS Unemployment Data #########

county_unemp$date

temp <- state_unemp %>% filter(series_id %>% str_extract("^..........") %>% str_extract("...$") %>% as.integer() == 0)

state_unemp <- temp

state_unemp %>% select(fips, year, date, value, series)

states_grep <- blscrapeR::state_fips[1:51,]
states_grep <- states_grep$state %>% str_replace("District of Columbia","DC") %>% str_replace(" ","") %>% str_flatten(collapse = "|")
county_unemp

bls_html <- read_html("https://download.bls.gov/pub/time.series/la/")
bls_states <- bls_html %>% html_nodes("br+ a")%>% html_attr("href") %>% str_subset(states_grep)
bls_states <- paste0("https://download.bls.gov",bls_states)

bls_states
state_unemp <- read_delim(bls_states[1], "\t")

for(i in 2:length(bls_states)) {
  print(i)
  state_unemp %<>% bind_rows(read_delim(bls_states[i], "\t")) 
  
}

state_unemp %<>% map(str_trim) %>% as.tibble()
state_unemp %<>% mutate(
  fips = series_id %>% str_extract("^.......") %>% str_remove("^....."),
  series = series_id %>% str_extract("..$"),
  date = paste0(year,'-',period %>% str_remove("M"))
)

state_unemp$fips %>% unique() %>% as.double() %>% sort()

state_unemp$year


load("G:/r-env/thesis.RData")

thesis$unemp$state_month <- state_unemp

save(thesis,file = "G:/r-env/thesis.RData")
# ACS Data #############

library(tidycensus)

var <- load_variables("2016",'acs5')
var$concept %>% str_subset("TRAVEL TIME") %>% unique()

labs <- var[var$concept %>% str_detect("^TRAVEL TIME TO WORK$"),]


labs
?get_acs


# State Level Travel Times ############

travel_time <- 
  get_acs("state", table="B08303", output = "wide", survey = "acs1")
travel_time$year <- 2016

labs1 <- var %>% filter(concept == "MEANS OF TRANSPORTATION TO WORK BY TRAVEL TIME TO WORK")
labs1

for(yr in 2012:2015){
  temp <- get_acs("state", table="B08303", output = "wide", survey = "acs1", year = yr)
  temp$year <- yr
  travel_time %<>% bind_rows(temp)
}

labs$tidyLabel <- labs$label %>% str_remove_all("^.*!!") %>% str_replace_all(" ","_") %>% str_replace_all("$","_") %>% str_remove_all("_minutes") %>% str_replace("^","Minutes_")
labs$name[1]
labs$tidyLabel[1]

for(i in 1:nrow(labs)){
  names(travel_time) %<>% str_replace_all(labs$name[i],labs$tidyLabel[i])
}

dir.create("./Travel_Time")
write_csv(travel_time, "./Travel_Time/state_travel-time.csv")



travel_time
names(travel_time) %>% str_replace_all(labs$name[2],labs$tidyLabel[2])
labs %>% tail()
travel_time
labs
travel_time <- travel_time1


write.csv(travel_time, "State_TravelTime.csv")


travel_time1 <- 
  get_acs("state", table="B08134", output = "wide", survey = "acs1")
travel_time1$year <- 2016
travel_time1

labs1 <- var %>% filter(concept == "MEANS OF TRANSPORTATION TO WORK BY TRAVEL TIME TO WORK")
labs1$label

for(yr in 2012:2015){
  temp <- get_acs("state", table="B08134", output = "wide", survey = "acs1", year = yr)
  temp$year <- yr
  travel_time1 %<>% bind_rows(temp)
}

labs1$tidyLabel <- labs1$label %>% str_remove_all("Estimate!!Total!!|,|\\(|\\)|-") %>% str_replace_all(" |!!","_") %>% str_replace_all("$","_")
labs1$name[1]
labs1$tidyLabel

for(i in 1:nrow(labs1)){
  names(travel_time1) %<>% str_replace_all(labs1$name[i],labs1$tidyLabel[i])
}

names(travel_time1) %<>% str_replace_all("-","_")
names(travel_time1)[5:22] %<>% str_replace_all("^","Total_")
travel_time1

write.csv(travel_time1, "State_TravelTime_meansOfTransport.csv")


# County Travel Times ###########

travel_time <- 
  get_acs("county", table="B08303", output = "wide", survey = "acs1")
travel_time$year <- 2016

for(yr in 2012:2015){
  temp <- get_acs("county", table="B08303", output = "wide", survey = "acs1", year = yr)
  temp$year <- yr
  travel_time %<>% bind_rows(temp)
}

labs$tidyLabel <- labs$label %>% str_remove_all("^.*!!") %>% str_replace_all(" ","_") %>% str_replace_all("$","_") %>% str_remove_all("_minutes") %>% str_replace("^","Minutes_")
labs$name[1]
labs$tidyLabel[1]

for(i in 1:nrow(labs)){
  names(travel_time) %<>% str_replace_all(labs$name[i],labs$tidyLabel[i])
}

travel_time
names(travel_time) %>% str_replace_all(labs$name[2],labs$tidyLabel[2])
labs %>% tail()
travel_time
labs
travel_time <- travel_time1
travel_time1 <- travel_time

travel_time1$year <- ifelse(is.na(travel_time1$year), 2016, travel_time1$year)

names(travel_time1)
temp <- travel_time1 %>% transmute(fips = GEOID,
                                   state = NAME,
                                   year = year,
                                   long_commute_prop = (Total_30_to_34_minutes_E + Total_35_to_44_minutes_E + Total_45_to_59_minutes_E + Total_60_or_more_minutes_E) / Estimate_Total_E,
                                   long_car_commute_prop = (Car_truck_or_van_30_to_34_minutes_E + Car_truck_or_van_35_to_44_minutes_E + Car_truck_or_van_45_to_59_minutes_E + Car_truck_or_van_60_or_more_minutes_E) / Estimate_Total_E,
                                   long_car_commute_alone_prop = (Car_truck_or_van_Drove_alone_30_to_34_minutes_E + Car_truck_or_van_Drove_alone_35_to_44_minutes_E + Car_truck_or_van_Drove_alone_45_to_59_minutes_E + Car_truck_or_van_Drove_alone_60_or_more_minutes_E) / Estimate_Total_E
                                   
)

# Make a little graph
temp %>% 
  ggplot(aes(x = year, y = long_commute_prop, group = state)) + geom_line()

df <- panel %>% as.tibble()

df
library(reshape2)

df$year %<>% as.character() %>% as.integer()

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

df$state %<>% as.character()


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


temp %>% filter(state=="Montana")
write.csv(travel_time, "County_TravelTime.csv")

# Travel Time for Metro/Micro Core Areas

travel_time2 <- 
  get_acs("county", table="B08134", output = "wide", survey = "acs1")
travel_time2$year <- 2016
travel_time2 %>% filter(!is.na(B08134_001E))

for(yr in 2012:2015){
  temp <- get_acs("county", table="B08134", output = "wide", survey = "acs1", year = yr)
  temp$year <- yr
  travel_time2 %<>% bind_rows(temp)
}

write_csv(travel_time2,"./Travel_Time/counties")

state_mort <- read_delim('G:/procyclical-mortality/traffic_mortality/traffic_state_year.txt',"\t")

names(state_mort) %<>% str_replace_all(" ","_")
names(state_mort)[3] <- "fips"
names(state_mort)[4] <- "year"
temp

panel <- left_join(state_mort, temp, by=c('fips','year'))

library(plm)
panel %<>% filter(!is.na(year))
panel %>% select(fips,year) %>% duplicated()

panel %<>% mutate(long_car_commute_alone_prop = long_car_commute_alone_prop * 100,
                  long_car_commute_prop = long_car_commute_prop * 100,
                  long_commute_prop = long_commute_prop * 100) 

panel %>% select(long_commute_prop, long_car_commute_prop, long_car_commute_alone_prop)
panel %<>% as.tibble()
panel %<>% select(-Notes, -Year_Code, -state)



names(panel)[6:9] %<>% str_replace("^","Traffic_")

all <- read_delim('G:/procyclical-mortality/all-cause_state_year.txt','\t')

all %<>% filter(!is.na(Year))
names(all) %<>% str_replace_all(" ","_")

all %<>% select(-Notes, -Year_Code)
names(all)[2] <- "fips"
names(all) %<>% str_to_lower()
names(panel)[4] %<>% str_replace("^","Traffic_")
panel
all

panel$year %<>% as.character() %>% as.integer()

panel <- left_join(panel, all, by=c('fips','year'))


panel %<>% pdata.frame(index = c('fips','year'))
reg1 <- plm(Traffic_Crude_Rate~long_commute_prop, data = panel, model = "within")
reg1 %>% summary()

reg2 <- plm(log(deaths)~long_car_commute_alone_prop, data = panel)
reg2 %>% summary()

reg3 <- plm(log(traffic_crude_rate)~long_car_commute_alone_prop, data = panel)
reg3 %>% summary() 

reg3 <- plm(log(deaths)~long_car_commute_alone_prop, data = panel)
reg3 %>% summary() 


panel %>% as.tibble() %>% select(traffic_crude_rate) %>% summary()

names(panel) %<>% str_to_lower()
reg4 <- lm(crude_rate~long_commute_prop, data = panel %>% as.data.frame())
reg4 %>% summary() 

reg5 <- lm(Crude_Rate~long_commute_prop, data = panel %>% as.data.frame())
reg5 %>% summary() 

write_csv(panel,"PANEL.csv")

# Numbe of Vehicles ##########

cas %>% filter(str_detect(concept, "TRAVEL TIME")) %>% select(label) %>% unique()
cas %>% filter(str_detect(concept, "VEHICLES")) %>% select(label) %>% unique()
cas %>% filter(str_detect(concept, "AGGREGATE NUMBER OF VEHICLES ")) %>% select(name, label) %>% unique()
cas %>% filter(str_detect(concept, "VEHICLES")) %>% select(concept) %>% unique()

# State Unemployment Rates ########

temp <- state_unemp %>% filter(year %>% as.integer() >= 2012)
temp %<>% filter(series == "03")
temp$value %<>% as.double()

unemp <- temp %>%  filter(series == "03") %>%
  group_by(year,fips) %>% 
  summarize(unemp_rate = mean(value %>% as.double()))

unemp_num <- temp %>% 
  filter(series == "04") %>% 
  group_by(year,fips) %>% 
  summarize(unemployed = mean(value %>% as.double()))

emp <- temp %>% 
  filter(series == "05") %>% 
  group_by(year,fips) %>% 
  summarize(employed = mean(value %>% as.double()))

lf <- temp %>% 
  filter(series == "06") %>% 
  group_by(year,fips) %>% 
  summarize(labor_force = mean(value %>% as.double()))

df
lf %>% filter(fips == "01")
df$fips %<>% as.character()
df$year %<>% as.character()
lf

unemp
df %<>% select(-unemp_rate)

df <- left_join(df, unemp)

df %<>% left_join(unemp_num)

df %<>% left_join(emp)

df %<>% left_join(lf)
df %>% filter(fips =="01") %>% select(unemp_rate,unemployed, employed, labor_force)

write_csv(df, "commutes_mortality_unemp_panel.csv")
