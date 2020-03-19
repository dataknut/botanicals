# DESC: Load the weather data and do various things ----

# Libraries ----
library(here) # where are we?
library(data.table) # data frames with superpowers
library(ggplot2) # plots
library(lubridate) # date & time stuff
library(skimr) # descriptive & summary stats

# Parameters ----
dPath <- paste0(here::here(), "/data/")
dFile <- paste0(dPath, "ClimateData_1976_2019.csv")

# get data ----
weatherDT <- data.table::fread(dFile) # load data

head(weatherDT) # what have we got?

# do some pre-processing ----
weatherDT[, date := lubridate::dmy(Date)] # make nice date
weatherDT[, year := lubridate::year(date)] # make nice date
weatherDT[, month := lubridate::month(date,label = TRUE, abbr = TRUE)] # make nice date

# just for fun
plotDT <- weatherDT[, .(meanPrcpt = mean(prcpt)), 
                    keyby = .(year, month)]

ggplot2::ggplot(plotDT, aes(x = month, y = meanPrcpt, colour = year)) +
  geom_point() 

# Challenge 1:
# Count number of spans (per year?) where 15 or more days  
# when the daily rainfall < 0.2. This is the UK definition of drought.
skimr::skim(weatherDT) # check data
setkey(weatherDT,date) # ensures ordered by date
weatherDT[, droughtThresh := ifelse(prcpt < 0.2, 1, 0)] # to make life easier
weatherDT[, droughtPeriod := ifelse(droughtThresh == 1 & 
                                      shift(droughtThresh == 0), # first of 1 -> n days with low
                                   "Start", 
                                   NA)] 
weatherDT[, droughtPeriod := ifelse(droughtThresh == 0 & 
                                      shift(droughtThresh == 1), # last of 1 -> n days with low
                                    "End", 
                                    droughtPeriod)] 

lowPrcptPeriodsDT <- weatherDT[droughtPeriod == "Start" | 
                                 droughtPeriod == "End"]

lowPrcptPeriodsDT[, periodCount := ifelse(droughtPeriod == "End",
                                          date - shift(date), # n days between start & end (they are ordered)
                                          NA) # undefined between end & start (we don't care about periods between droughts)
                  ] 

lowPrcptPeriodsDT[, drought := ifelse(periodCount > 14, "Drought", "Not drought")]

table(lowPrcptPeriodsDT$year, lowPrcptPeriodsDT$drought)

plotDT <- lowPrcptPeriodsDT[, .(nDroughts = .N), # data.table magic
                            keyby = .(drought, month, year)] # add up number of droughts/no droughts in these periods of low precip

ggplot2::ggplot(plotDT[drought == "Drought"], 
                aes(x = month, fill = nDroughts, y = year)) +
  geom_tile() +
  labs(caption = "Note months with no droughts not shown")

# counts per year & month
setkey(plotDT, year, month)
plotDT[drought == "Drought"]

# counts per year
# none in 1976...
lowPrcptPeriodsDT[drought == "Drought", .(nDroughts = .N), # data.table magic
                  keyby = .(year)] # add up number of droughts/no droughts in these periods of low precip

# 1995: https://en.wikipedia.org/wiki/1995_Great_Britain_and_Ireland_heat_wave

# 2003: https://en.wikipedia.org/wiki/2003_European_heat_wave#United_Kingdom
