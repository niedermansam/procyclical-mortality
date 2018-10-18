library(tidycensus)

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

county_unemp

library(rvest)

# BLS Unemployment Data #########

bls_html <- read_html("https://download.bls.gov/pub/time.series/la/")
bls_states <- bls_html %>% html_nodes("br+ a")%>% html_attr("href") %>% str_subset(states_grep)
bls_states <- paste0("https://download.bls.gov",bls_states)


write.csv(bls_states,"BLS_StateUnemp_URLS.csv")

states_grep <- blscrapeR::state_fips[1:51,]
states_grep <- states_grep$state %>% str_replace("District of Columbia","DC") %>% str_replace(" ","") %>% str_flatten(collapse = "|")
county_unemp

# ACS Data #############3

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

travel_time
names(travel_time) %>% str_replace_all(labs$name[2],labs$tidyLabel[2])
labs %>% tail()
travel_time
labs
travel_time <- travel_time1


write.csv(travel_time, "State_TravelTime.csv")

# County Travel Times ############33

travel_time <- 
  get_acs("county", table="B08303", output = "wide", survey = "acs1")
travel_time$year <- 2016

rm(county_unemp)

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

travel_time

write.csv(travel_time, "County_TravelTime.csv")

# Numbe of Vehicles ##########3

cas %>% filter(str_detect(concept, "TRAVEL TIME")) %>% select(label) %>% unique()
cas %>% filter(str_detect(concept, "VEHICLES")) %>% select(label) %>% unique()
cas %>% filter(str_detect(concept, "AGGREGATE NUMBER OF VEHICLES ")) %>% select(name, label) %>% unique()
cas %>% filter(str_detect(concept, "VEHICLES")) %>% select(concept) %>% unique()


