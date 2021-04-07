## Import libraries
library(tidyverse)
library(data.table)
library(lmtest)
library(lubridate)
# Load data
setwd("~/W203/Assignments_MJS/Lab 2/data")
#table <- read_csv("CUSP_NYTimes.csv")

# COVID policies
table1 = read_csv("CUSP_NYTimes.csv")
#unique(table1$state)

# NYT COVID cases
table2 = read_csv("NYT_Covid_Cases.csv")
#unique(table2$state)

table2 = table2 %>% rename(State.FIPS.Code = fips)

table3 = inner_join(table2,table1,c("State.FIPS.Code"))

table3_winterOnly = table3 %>% 
  filter(`date`>="2020-12-01",`date`<"2021-03-01") %>% 
  mutate(earliest_cases = ifelse(`date`==min(date),cases.x,0)) %>% 
  mutate(latest_cases = ifelse(`date`== max(date),cases.x,0)) %>% 
  group_by(state.x, Stay.at.home.shelter.in.place, End.stay.at.home.shelter.in.place,
           Population.density.per.square.mile, Population.2018,
           Percent.living.under.the.federal.poverty.line..2018.,
           Public.face.mask.mandate, End.face.mask.mandate, Business.face.mask.mandate,
           Face.mask.mandate.enforced.by.fines, 
           Face.mask.mandate.enforced.by.criminal.charge.citation,
           State.of.emergency,
           Religious.Gatherings.Exempt.Without.Clear.Social.Distance.Mandate.,
           Quarantine.mandate.for.all.travelers,
           Quarantine.mandate.for.some.travelers,
           Quarantine.mandate.ended) %>% 
  summarise(sum(earliest_cases), sum(latest_cases)) %>% 
  mutate(inc_cases = `sum(latest_cases)`-`sum(earliest_cases)`) %>% 
  mutate(stayAtHomeSincePandemic = as.double(difftime(lubridate::ymd(Stay.at.home.shelter.in.place),
                                                      lubridate::ymd("2020-03-11"), units = "days"))) %>% 
  mutate(stayAtHome1 = as.double(difftime(lubridate::ymd("2021-02-28"),
                                          lubridate::ymd(Stay.at.home.shelter.in.place), units = "days"))) %>% 
  mutate(stayAtHome2 = as.double(difftime(lubridate::ymd(End.stay.at.home.shelter.in.place),
                                          lubridate::ymd(Stay.at.home.shelter.in.place), units = "days"))) %>% 
  mutate(stayAtHomeLength = 
           ifelse(is.na(End.stay.at.home.shelter.in.place), stayAtHome1,stayAtHome2)) %>% 
  mutate(inc_cases_per100k = inc_cases/Population.2018*100000) %>% 
  mutate(businessFMLength = 
           ifelse(is.na(End.face.mask.mandate),
                  as.double(difftime(lubridate::ymd("2021-02-28"),
                                     lubridate::ymd(Business.face.mask.mandate), units = "days")),
                  as.double(difftime(lubridate::ymd(End.face.mask.mandate),
                                          lubridate::ymd(Business.face.mask.mandate), units = "days")))) %>% 
  mutate(stateOfEmergencySpeed = as.double(difftime(lubridate::ymd(State.of.emergency),
                                                    lubridate::ymd("2020-03-11"), units = "days"))) %>% 
  mutate(quarantineStartDate = fifelse(is.na(Quarantine.mandate.for.all.travelers),
                                       Quarantine.mandate.for.some.travelers,
                                       Quarantine.mandate.for.all.travelers
                                       )) %>% 
  mutate(quarantineTravelersLength = 
           ifelse(is.na(Quarantine.mandate.ended),
                  as.double(difftime(lubridate::ymd("2021-02-28"),
                                     lubridate::ymd(quarantineStartDate), units = "days")),
                  as.double(difftime(lubridate::ymd(Quarantine.mandate.ended),
                                     lubridate::ymd(quarantineStartDate), units = "days"))))

table3_winterOnly %>% select (Stay.at.home.shelter.in.place,End.stay.at.home.shelter.in.place,stayAtHomeLength)

hist(table3_winterOnly$inc_cases_per100k)
hist(log(table3_winterOnly$Population.density.per.square.mile))
hist(table3_winterOnly$stayAtHomeSincePandemic*table3_winterOnly$stayAtHomeLength)

model1 = lm(formula = inc_cases_per100k ~ log(Population.density.per.square.mile) + 
              stayAtHomeSincePandemic * stayAtHomeLength, 
           data = table3_winterOnly)
summary(model1)

hist(table3_winterOnly$Percent.living.under.the.federal.poverty.line..2018.)
hist(table3_winterOnly$businessFMLength)
hist(table3_winterOnly$stateOfEmergencySpeed)

model2 = lm(formula = inc_cases_per100k ~ log(Population.density.per.square.mile) + 
              stayAtHomeSincePandemic * stayAtHomeLength + 
              Percent.living.under.the.federal.poverty.line..2018.+
              stateOfEmergencySpeed +
              (Religious.Gatherings.Exempt.Without.Clear.Social.Distance.Mandate. == "FALSE"), 
            data = table3_winterOnly)
summary(model2)

plot(model2, which =1 )
model2$coefficients
bptest(model2)
plot(model2, which = 2)

hist(table3_winterOnly$quarantineTravelersLength)

model3 = lm(formula = inc_cases_per100k ~ log(Population.density.per.square.mile) + 
              stayAtHomeSincePandemic * stayAtHomeLength + 
              Percent.living.under.the.federal.poverty.line..2018.+
              stateOfEmergencySpeed +
              (Religious.Gatherings.Exempt.Without.Clear.Social.Distance.Mandate. == "FALSE")+
              (Face.mask.mandate.enforced.by.fines == "TRUE") +
              (Face.mask.mandate.enforced.by.criminal.charge.citation == "TRUE") +
              businessFMLength +
              quarantineTravelersLength, 
            data = table3_winterOnly)
summary(model3)


plot(model3, which =1 )
model3$coefficients
bptest(model3)
plot(model3, which = 2)


table4 = table3 %>% select(date, state.x, state.y,incremental.cases,State.of.emergency
                  , Stay.at.home.shelter.in.place
                  , End.stay.at.home.shelter.in.place
                  , Public.face.mask.mandate
                  , End.face.mask.mandate
                  , Face.mask.mandate.enforced.by.fines
                  , Face.mask.mandate.enforced.by.criminal.charge.citation)

table3 %>% filter(date>="12/1/2020") %>% 
  filter(date<="4/1/2021")



model1 = lm(incemental.cases ~ (date >= stay.at.home.shelter.in.place && date <= End.stay.at.home.shelter.in.place)
            ,data = table4)

head(table3)