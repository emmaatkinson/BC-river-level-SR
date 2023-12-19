# Date: 05-Jul-2018
# Last updated: 05-Jul-2018
# Name: Emma Atkinson
# Description: Looking at updated NuSEDS data for DFO Area 12 
#              Compiling data by species for further preparation
# Notes: Data from Pieter Van Will (DFO Area 12 manager)

rm(list=ls())

library(gplots)
library(gdata)
library(repmis)
library(scales)
library(dplyr)
library(stringr)

# Set working directory #
dir.data <- "C:/Users/Emma Atkinson/Desktop/Research/SoS/Analysis/Data-raw"
dir.out <- "C:/Users/Emma Atkinson/Desktop/Research/SoS/Analysis/Data-generated"

setwd(dir.data)

# Read in NuSEDS data #
all.data <- read.csv(file = "area_12_nuseds_escapement_2017.csv", header=T)
new.data <- read.csv(file="NUSEDS_20180416.csv", header=T)
new.data <- filter(new.data, AREA==12)
write.csv(new.data, file="NUSEDS_AREA12_20180416.csv")
  
# Parse by rivers # 
all.data <- all.data %>% 
  select(Analysis.Year, Area, Waterbody.Name, Species, Population, Estimate.Classification, Estimate.Method, Max.Estimate, SEN.Presence.Adult, NUSED1.Enumeration.Method.2)

new.data <- new.data %>% 
  select(ANALYSIS_YR, AREA, WATERBODY, SPECIES, POPULATION, ESTIMATE_CLASSIFICATION, ESTIMATE_METHOD, MAX_ESTIMATE)

# Parse by species #
spp <- c("Pink", "Chinook","Coho","Chum","Sockeye")

setwd(dir.out)

for (species in spp){ # Writing species data to separate files #
  temp <- filter(all.data, Species==species)
  write.csv(temp, file=paste(species,"_nuseds_2017.csv", sep=""))
}

temp2 <- filter(all.data, Species=="Chum")
write.csv(temp, file="Pink_nuseds_2017.csv")

temp <- filter(new.data, SPECIES=="Chum")
write.csv(temp, file="Pink_nuseds_2017.csv")

test <- read.csv(file="Pink_nuseds_2017.csv", header=T)
unique(test$Waterbody.Name)

nm <- c("site_id","year","day","month","location","fish_num","species","length","height")
d[nm] <- lapply(nm, function(x) spacelice[[x]][match(d$fish_id, spacelice$fish_id)])

