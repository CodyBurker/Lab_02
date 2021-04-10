## Import libraries
library(tidyverse)
library(data.table)
library(lmtest)
library(lubridate)
# Load data
#### CHANGE ME ############################
# setwd("~/W203/Assignments_MJS/Lab 2/data")
setwd("/home/rstudio/MIDS 203/Lab_02_SSH/src/data")
###########################################
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
  #filter(`date`>="2020-03-01",`date`<"2020-06-01") %>% 
  filter(`date`<"2020-06-01") %>% 
  mutate(earliest_cases = ifelse(`date`==min(date),cases.x,0)) %>% 
  mutate(latest_cases = ifelse(`date`== max(date),cases.x,0)) %>% 
  group_by(state.x, 
           Stay.at.home.shelter.in.place, 
           End.stay.at.home.shelter.in.place,
           Stay.at.home.order.issued.but.did.not.specifically.restrict.movement.of.the.general.public,
           Began.to.reopen.businesses.statewide,
           Population.density.per.square.mile, 
           Population.2018,
           Percent.living.under.the.federal.poverty.line..2018.,
           Public.face.mask.mandate, End.face.mask.mandate, Business.face.mask.mandate,
           Face.mask.mandate.enforced.by.fines, 
           Face.mask.mandate.enforced.by.criminal.charge.citation,
           State.of.emergency,
           Religious.Gatherings.Exempt.Without.Clear.Social.Distance.Mandate.,
           Quarantine.mandate.for.all.travelers,
           Quarantine.mandate.for.some.travelers,
           Quarantine.mandate.ended,
           Closed.restaurants,
           Reopened.restaurants,
           Closed.gyms,
           Reopened.gyms,
           Closed.K.12.public.schools,
           Closed.bars,
           Reopened.bars) %>% 
  summarise(sum(earliest_cases), sum(latest_cases)) %>% 
  #mutate(stayAtHomeStartDate = fifelse(is.na(Stay.at.home.order.issued.but.did.not.specifically.restrict.movement.of.the.general.public),
  #                                     Stay.at.home.shelter.in.place,Stay.at.home.order.issued.but.did.not.specifically.restrict.movement.of.the.general.public)) %>% 
  mutate(stayAtHomeStartDate = fifelse(is.na(Stay.at.home.shelter.in.place),
                                       Stay.at.home.order.issued.but.did.not.specifically.restrict.movement.of.the.general.public,
                                       Stay.at.home.shelter.in.place )) %>% 
  mutate(inc_cases = `sum(latest_cases)`-`sum(earliest_cases)`) %>% 
  mutate(stayAtHomeSincePandemic = as.double(difftime(lubridate::ymd(stayAtHomeStartDate),
                                                      lubridate::ymd("2020-03-11"), units = "days"))) %>% 
  mutate(stayAtHome1 = as.double(difftime(lubridate::ymd(Began.to.reopen.businesses.statewide),
                                          lubridate::ymd(stayAtHomeStartDate), units = "days"))) %>% 
  mutate(stayAtHome2 = as.double(difftime(lubridate::ymd(End.stay.at.home.shelter.in.place),
                                          lubridate::ymd(stayAtHomeStartDate), units = "days"))) %>% 
  mutate(stayAtHome3 = as.double(difftime(lubridate::ymd("2020-05-31"),
                                          lubridate::ymd(stayAtHomeStartDate), units = "days"))) %>% 
  mutate(stayAtHomeLength = ifelse(is.na(End.stay.at.home.shelter.in.place), 
                                   min(stayAtHome1,stayAtHome3),min(stayAtHome2,stayAtHome3))) %>% 
  mutate(stayAtHomeToNow = as.double(difftime(lubridate::ymd("2020-04-30"),
                                                      lubridate::ymd(stayAtHomeStartDate), units = "days"))) %>% 
  mutate(inc_cases_per100k = inc_cases/Population.2018*100000) %>% 
   
  mutate(stateOfEmergencySpeed = as.double(difftime(lubridate::ymd(State.of.emergency),
                                                    lubridate::ymd("2020-03-11"), units = "days"))) %>% 
  mutate(restaurantsLength1 = as.double(difftime(lubridate::ymd(Reopened.restaurants),
                                                lubridate::ymd(Closed.restaurants), units = "days"))) %>% 
  mutate(restaurantsLength2 = as.double(difftime(lubridate::ymd("2020-05-31"),
                                                 lubridate::ymd(Closed.restaurants), units = "days"))) %>% 
  mutate(restaurantsLength = fifelse(is.na(restaurantsLength1),
                                     restaurantsLength2,min(restaurantsLength1,restaurantsLength2))) %>%
  mutate(gymsLength1 = as.double(difftime(lubridate::ymd(Reopened.gyms),
                                                 lubridate::ymd(Closed.gyms), units = "days"))) %>% 
  mutate(gymsLength2 = as.double(difftime(lubridate::ymd("2020-05-31"),
                                                 lubridate::ymd(Closed.gyms), units = "days"))) %>% 
  mutate(gymsLength = fifelse(is.na(gymsLength1),
                              gymsLength2,min(gymsLength1,gymsLength2))) %>% 
  mutate(businessFMLength1 = as.double(difftime(lubridate::ymd(End.face.mask.mandate),
                                          lubridate::ymd(Business.face.mask.mandate), units = "days"))) %>% 
  mutate(businessFMLength2 = as.double(difftime(lubridate::ymd("2020-05-31"),
                                          lubridate::ymd(Business.face.mask.mandate), units = "days"))) %>% 
  mutate(businessFMLength = fifelse(is.na(businessFMLength1),
                                    businessFMLength2,min(businessFMLength1,businessFMLength2))) %>%
  mutate(businessFMLength = ifelse(businessFMLength<0,0,businessFMLength)) %>% 
  mutate(quarantineStartDate = fifelse(is.na(Quarantine.mandate.for.all.travelers),
                                       Quarantine.mandate.for.some.travelers,
                                       Quarantine.mandate.for.all.travelers)) %>% 
  mutate(quarantineTravelersLength1 = as.double(difftime(lubridate::ymd(Quarantine.mandate.ended),
                                                lubridate::ymd(quarantineStartDate), units = "days"))) %>% 
  mutate(quarantineTravelersLength2 = as.double(difftime(lubridate::ymd("2020-05-31"),
                                                lubridate::ymd(quarantineStartDate), units = "days"))) %>% 
  mutate(quarantineTravelersLength = fifelse(is.na(quarantineTravelersLength1),
                                             quarantineTravelersLength2,min(quarantineTravelersLength1,quarantineTravelersLength2))) %>% 
  mutate(quarantineTravelersLength = ifelse(quarantineTravelersLength<0,0,quarantineTravelersLength)) %>% 
  mutate(barsLength1 = as.double(difftime(lubridate::ymd(Reopened.bars),
                                                lubridate::ymd(Closed.bars), units = "days"))) %>% 
  mutate(barsLength2 = as.double(difftime(lubridate::ymd("2020-05-31"),
                                                lubridate::ymd(Closed.bars), units = "days"))) %>% 
  mutate(barsLength = fifelse(is.na(barsLength1),
                              barsLength2,min(barsLength1,barsLength2))) %>% 
  mutate(schoolLength = as.double(difftime(lubridate::ymd("2020-05-31"),
                                          lubridate::ymd(Closed.K.12.public.schools), units = "days")))


table3_winterOnly = table3_winterOnly %>% 
  mutate(Population_density_log = log(Population.density.per.square.mile)) %>% 
  mutate(inc_cases_log = log(inc_cases_per100k))
table3_winterOnly[is.na(table3_winterOnly$stayAtHomeLength),"stayAtHomeLength"]<-0
table3_winterOnly[is.na(table3_winterOnly$stayAtHomeSincePandemic),"stayAtHomeSincePandemic"]<-60
table3_winterOnly[is.na(table3_winterOnly$restaurantsLength),"restaurantsLength"]<-0
table3_winterOnly[is.na(table3_winterOnly$stayAtHomeToNow),"stayAtHomeToNow"]<-0
table3_winterOnly[is.na(table3_winterOnly$gymsLength),"gymsLength"]<-0
table3_winterOnly[is.na(table3_winterOnly$businessFMLength),"businessFMLength"]<-0
table3_winterOnly[is.na(table3_winterOnly$quarantineTravelersLength),"quarantineTravelersLength"]<-0
table3_winterOnly[is.na(table3_winterOnly$barsLength),"barsLength"]<-0
table3_winterOnly[is.na(table3_winterOnly$schoolLength),"schoolLength"]<-0


hist(table3_winterOnly$inc_cases_log)
hist(log(table3_winterOnly$Population.density.per.square.mile))
hist(table3_winterOnly$stayAtHomeSincePandemic*table3_winterOnly$stayAtHomeLength)


model1 = lm(formula = inc_cases_log ~ Population_density_log, 
           data = table3_winterOnly)
summary(model1)

plot(model1, which =1 )
model1$coefficients
bptest(model1)
plot(model1, which = 2)

hist(table3_winterOnly$Percent.living.under.the.federal.poverty.line..2018.)
hist(table3_winterOnly$businessFMLength)
hist(table3_winterOnly$stateOfEmergencySpeed)
hist(table3_winterOnly$restaurantsLength)
hist(table3_winterOnly$quarantineTravelersLength)

model2 = lm(formula = inc_cases_log ~ Population_density_log + 
              businessFMLength + 
              quarantineTravelersLength +
              stayAtHomeLength,
            data = table3_winterOnly)
summary(model2)

anova(model1,model2)

plot(model2, which =1 )
model2$coefficients
bptest(model2)
plot(model2, which = 2)

hist(table3_winterOnly$quarantineTravelersLength)

model3 = lm(formula = inc_cases_log ~ Population_density_log + 
              businessFMLength + 
              quarantineTravelersLength +
              stayAtHomeLength +
              restaurantsLength +
              gymsLength +
              barsLength,
            data = table3_winterOnly)
summary(model3)

anova(model2,model3)

plot(model3, which =1 )
model3$coefficients
bptest(model3)
plot(model3, which = 2)


