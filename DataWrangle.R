rm(list = ls())
setwd("~/Desktop/NYU Academic/Statistics & Econometrics 1/Project/Data")
getwd()
library(tidyverse)
library(dplyr)

### Data wrangling from source World Bank data (extraneous data omitted manually in Excel already).
# Get country codes list in correct form.
countries <- read.delim("CountryCodes.txt", header = FALSE)
codes <- substring(countries[,1], 1, 3)

# Income Data
gni <- read.csv("WORLDGNI_Data_WB.csv")
# Extract only desired countries, and restrict observations. 
y <- select(filter(gni, Country.Code %in% codes),-c(2:36))
y <- as_tibble(select(y,-c(24:26)))
names(y) <- sub("^X", "", names(y))
# CO2 Data
co2 <- read.csv("WORLDCO2_Data_WB.csv")
e <- select(filter(co2, Country.Code %in% codes),-c(2:36))
as_tibble(e)
names(e) <- sub("^X", "", names(e))
# Natural Resource Exports
ore <- read.csv("WORLDORE_Data_WB.csv")
o <- select(filter(ore, Country.Code %in% codes), -c(2:36))
o <- as_tibble(select(o,-c(24:26)))
names(o) <- sub("^X", "", names(o))
fuel <- read.csv("WORLDFUEL_Data_WB.csv")
f <- select(filter(fuel, Country.Code %in% codes), -c(2:36))
f <- as_tibble(select(f,-c(24:26)))
names(f) <- sub("^X", "", names(f))
# Land Area
land <- read.csv("LND.TOTL.csv")
l <- select(filter(land, Country.Code %in% codes), -c(2:36))
l <- as_tibble(select(l,-c(24:25)))
names(l) <- sub("^X", "", names(l))
# Population Density
pop <- read.csv("POP.DNST.csv")
p <- select(filter(pop, Country.Code %in% codes), -c(2:36))
p <- as_tibble(select(p,-c(24:29)))
names(p) <- sub("^X", "", names(p))

rm(ore,fuel,co2,gni,land,pop)

# Make data tidy for panel form
yr <- as.character(1995:2016)
y <- y %>% pivot_longer(all_of(yr), names_to = "year", values_to = "income")
e <- e %>% pivot_longer(all_of(yr), names_to = "year", values_to = "co2")
o <- o %>% pivot_longer(all_of(yr), names_to = "year", values_to = "ore")
f <- f %>% pivot_longer(all_of(yr), names_to = "year", values_to = "fuel")
l <- l %>% pivot_longer(all_of(yr), names_to = "year", values_to = "land")
p <- p %>% pivot_longer(all_of(yr), names_to = "year", values_to = "popdens")

# Create single data frame with all variables in columns
data <- left_join(y,e)
data <- left_join(data,o)
data <- left_join(data,f)
data <- left_join(data,l)
data <- left_join(data,p)
colnames(data)[1] <- "country"

rm(y,e,o,f,l,p)

write_csv(x=data, "data1.csv")
