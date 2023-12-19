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

# Read in NuSEDS, Pieter, and Steph's data to compare #
pieter <- read.csv(file = "area_12_nuseds_escapement_2017.csv", header=T) # This is the data from Pieter Van Will (updated with 2017 data)
nuseds <- read.csv(file="NUSEDS_20180416.csv", header=T) # This is the most recently available NuSEDS data (downloaded 4-Aug-2018)
steph <- read.csv("nuSEDS_PINK.csv")

# Checking number of rivers from each dataset from the most recent common year # 
length(unique(steph[steph$Yr==2010 & steph$Area==12,]$River))
length(unique(nuseds[nuseds$ANALYSIS_YR==2010 & nuseds$AREA==12,]$WATERBODY))
length(unique(pieter[pieter$Analysis.Year==2010 & pieter$Area==12,]$Waterbody.Name))

nuseds <- filter(nuseds, AREA==12) # filter nuseds for area 12

# Pull out the columns you want # 
pieter <- pieter %>% 
  select(Analysis.Year, Area, Waterbody.Name, Species, Population, Population.Id, Estimate.Classification, Estimate.Method, Max.Estimate, Stream.Id, Run.Type)

nuseds <- nuseds %>% 
  select(ANALYSIS_YR, AREA, WATERBODY, SPECIES, POPULATION, POP_ID, ESTIMATE_CLASSIFICATION, ESTIMATE_METHOD, MAX_ESTIMATE, GFE_ID, RUN_TYPE)

# Pull 2017 data from Pieter Van Will's NuSEDS data 
# This is because the data he provided encompass fewer 
# rivers than the online NuSEDS data (80 vs. 90)
# so will add new 2017 data to NuSEDS from DFO website
pieter17 <- filter(pieter, Analysis.Year==2017)
names(pieter17) <- c("ANALYSIS_YR", "AREA", "WATERBODY", "SPECIES", "POPULATION", "POP_ID", "ESTIMATE_CLASSIFICATION", "ESTIMATE_METHOD", "MAX_ESTIMATE", "GFE_ID", "RUN_TYPE")

data <- rbind(nuseds, pieter17)

setwd(dir.out)
write.csv(data, file="updated_nuseds_2017.csv")

# Organizing catch data - pulling data from Steph up until 2017
catch<-read.delim("catch comparison.txt", header=TRUE, na.string="")
write.csv(catch, file="catch_1980-2010.csv")

