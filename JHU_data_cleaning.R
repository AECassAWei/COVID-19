library(tidyverse)
library(dplyr)

# load JHU death data
covid.death.raw <- read_csv("./csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")
glimpse(covid.death.raw)

# Exclude these states
# American Samoa, Diamond Princess, Grand Princess, Guam, Northern Mariana Islands, Puerto Rico, Virgin Islands

covid.death <- covid.death.raw %>% 
  select(-c(UID, iso2, iso3, code3, FIPS, Admin2, Country_Region, Lat, Long_, Combined_Key)) %>%
  rename(state = Province_State, population = Population) %>% 
  group_by(state) %>% # Specify group indicator
  summarise(across(everything(), sum)) %>% # Specify column
  pivot_longer(-c(state, population),
               names_to = "date",
               values_to = "cumulative.death.cases") %>% 
  filter(state %in% c(state.name, "District of Columbia")) %>% 
  group_by(state) %>% 
  mutate(#state = toupper(state),
         death.cases = cumulative.death.cases - lag(cumulative.death.cases),
         date = as.Date(date, format = "%m/%d/%y"), 
         month = match(months(date), month.name),
         death.cases = replace_na(death.cases, 0))
  
covid.deaths <- covid.death[c(1, 2, 3, 6, 4, 5)]


# load JHU confirmed data
covid.confirmed.raw <- read_csv("./csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
glimpse(covid.confirmed.raw)

# Exclude these states
# American Samoa, Diamond Princess, Grand Princess, Guam, Northern Mariana Islands, Puerto Rico, Virgin Islands

covid.confirmed <- covid.confirmed.raw %>% 
  select(-c(UID, iso2, iso3, code3, FIPS, Admin2, Country_Region, Lat, Long_, Combined_Key)) %>%
  rename(state = Province_State) %>% 
  group_by(state) %>% # Specify group indicator
  summarise(across(everything(), sum)) %>% # Specify column
  pivot_longer(-c(state),
               names_to = "date",
               values_to = "cumulative.confirmed.cases") %>% 
  filter(state %in% c(state.name, "District of Columbia")) %>% 
  group_by(state) %>% 
  mutate(#state = toupper(state),
         confirmed.cases = cumulative.confirmed.cases - lag(cumulative.confirmed.cases),
         date = as.Date(date, format = "%m/%d/%y"), 
         confirmed.cases = replace_na(confirmed.cases, 0))

covid <- covid.deaths %>% inner_join(covid.confirmed, by = c("state","date"))
write.csv(covid, "E:/UCSD/2020-2021 Senior/PSYC 201 Project/JHU Databse/covid time series (death & confirmed) version 1.csv")

### Plot
midwest.states <- c("North Dakota","South Dakota","Nebraska","Kansas","Indiana","Missouri","Iowa","Ohio","Wisconsin","Michigan","Minnesota","Illinois")
covid.state.midwest <- filter(covid.time.series, state %in% Midwest.states)
northeast.states <- c("Pennsylvania","New Hampshire","Maine","Connecticut","New Jersey","Rhode Island","New York","Vermont","Massachusetts")
covid.state.northeast <- filter(covid.time.series, state %in% Northeast.states)
south.states <- c("West Virginia","Oklahoma","Kentucky","Alabama","Arkansas","Tennessee","Louisiana","Mississippi","South Carolina","Texas","Georgia","North Carolina","Florida","Virginia","Delaware","Maryland","District of Columbia")
covid.state.south <- filter(covid.time.series, state %in% South.states)
west.states <- c("Wyoming","Idaho","Montana","Utah","Alaska","Arizona","Nevada","Colorado","New Mexico","Oregon","Washington","California","Hawaii")
covid.state.west <- filter(covid.time.series, state %in% West.states)

covid.state.midwest %>%
  ggplot(aes(x = date, y = cumulative.death.cases, color = state)) + 
  geom_line()

covid.state.northeast %>%
  ggplot(aes(x = date, y = cumulative.death.cases, color = state)) + 
  geom_line()

covid.state.south %>%
  ggplot(aes(x = date, y = cumulative.death.cases, color = state)) + 
  geom_line()

covid.state.west %>%
  ggplot(aes(x = date, y = cumulative.death.cases, color = state)) + 
  geom_line()


covid.state.midwest %>%  
  group_by(month) %>% 
  summarize(death.rate = mean(death.cases / population),
            state = state) %>% 
  ggplot(aes(x = month, y = death.rate, color = state)) + 
  geom_line()

covid.state.northeast %>%  
  group_by(month) %>% 
  summarize(death.rate = mean(death.cases / population)) %>% 
  ggplot(aes(x = month, y = death.rate)) + 
  geom_line()

covid.state.south %>%  
  group_by(month) %>% 
  summarize(death.rate = mean(death.cases / population)) %>% 
  ggplot(aes(x = month, y = death.rate)) + 
  geom_line()

covid.state.west %>%  
  group_by(month) %>% 
  summarize(death.rate = mean(death.cases / population)) %>% 
  ggplot(aes(x = month, y = death.rate)) + 
  geom_line()


covid.state.midwest %>% 
  ggplot(aes(x = date, y = cumulative.confirmed.cases)) + 
  geom_line()

covid.state.northeast %>% 
  ggplot(aes(x = date, y = cumulative.confirmed.cases)) + 
  geom_line()

covid.state.south %>% 
  ggplot(aes(x = date, y = cumulative.confirmed.cases)) + 
  geom_line()

covid.state.west %>%  
  ggplot(aes(x = date, y = cumulative.confirmed.cases)) + 
  geom_line()


covid.state.midwest %>% 
  ggplot(aes(x = date, y = confirmed.cases)) + 
  geom_line()

covid.state.northeast %>% 
  ggplot(aes(x = date, y = confirmed.cases)) + 
  geom_line()

covid.state.south %>% 
  ggplot(aes(x = date, y = confirmed.cases)) + 
  geom_line()

covid.state.west %>% 
  ggplot(aes(x = date, y = confirmed.cases)) + 
  geom_line()




# Loop through dates
dates <- format(seq(from = as.Date("2020/04/13"), to = as.Date("2020/11/03"), by = "day"), "%m-%d-%Y")

date = "04-12-2020"
reports.raw <- read_csv(paste("./csse_covid_19_data/csse_covid_19_daily_reports_us/", date, ".csv", sep = ""))
report <- reports.raw %>%
  select(-c(Country_Region, Last_Update, Lat, Long_, Confirmed, Deaths, FIPS, UID, ISO3)) %>% 
  rename(state = Province_State,
         recovered.cases = Recovered,
         active.cases = Active,
         incident.rate = Incident_Rate,
         people.tested = People_Tested,
         people.hospitalized = People_Hospitalized,
         mortality.rate = Mortality_Rate,
         testing.rate = Testing_Rate,
         hospitalization.rate = Hospitalization_Rate) %>% 
  filter(state %in% c(state.name, "District of Columbia")) %>% 
  mutate(# state = toupper(state),
         date = as.Date(date, format = "%m-%d-%y"),
         month = match(months(date), month.name))

for (date in dates){
  reports.raw <- read_csv(paste("./csse_covid_19_data/csse_covid_19_daily_reports_us/", date, ".csv", sep = ""))
  rpt <- reports.raw %>%
    select(-c(Country_Region, Last_Update, Lat, Long_, Confirmed, Deaths, FIPS, UID, ISO3)) %>% 
    rename(state = Province_State,
           recovered.cases = Recovered,
           active.cases = Active,
           incident.rate = Incident_Rate,
           people.tested = People_Tested,
           people.hospitalized = People_Hospitalized,
           mortality.rate = Mortality_Rate,
           testing.rate = Testing_Rate,
           hospitalization.rate = Hospitalization_Rate) %>% 
    filter(state %in% c(state.name, "District of Columbia")) %>% 
    mutate(# state = toupper(state),
           date = as.Date(date, format = "%m-%d-%y"),
           month = match(months(date), month.name))
  
  report <- rbind(report, rpt)
}

reports <- report[c(1, 10, 11, 2, 3, 4, 5, 6, 7, 8, 9)][order(report$state, report$date),]

covid.time.series <- covid %>% full_join(reports, by = c("state","date"))
covid.time.series <- reports
View(covid.time.series)

write.csv(covid.time.series, "E:/UCSD/2020-2021 Senior/PSYC 201 Project/JHU Databse/covid time series (other variables) version 2.csv")

covid.time.series %>% 
  filter(state == "ALABAMA") %>% 
  ggplot(aes(x = date, y = recovered.cases)) + 
  geom_line()

covid.time.series %>% 
  filter(state == "ALABAMA") %>% 
  ggplot(aes(x = date, y = active.cases)) + 
  geom_line()

covid.time.series %>% 
  filter(state == "ALABAMA") %>% 
  ggplot(aes(x = date, y = incident.rate)) + 
  geom_line()

covid.time.series %>% 
  filter(state == "ALABAMA") %>% 
  ggplot(aes(x = date, y = people.tested)) + 
  geom_line()

covid.time.series %>% 
  filter(state == "ALABAMA") %>% 
  ggplot(aes(x = date, y = people.hospitalized)) + 
  geom_line()

covid.time.series %>% 
  filter(state == "ALABAMA") %>% 
  ggplot(aes(x = date, y = mortality.rate)) + 
  geom_line()

covid.time.series %>% 
  filter(state == "ALABAMA") %>% 
  ggplot(aes(x = date, y = testing.rate)) + 
  geom_line()

covid.time.series %>% 
  filter(state == "ALABAMA") %>% 
  ggplot(aes(x = date, y = hospitalization.rate)) + 
  geom_line()
