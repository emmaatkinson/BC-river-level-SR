# Date created: 20-Mar-2023
# Last updated: 19-Apr-2023
# Author: Emma Atkinson
# Description: Central Coast river-level SR data compilation
# Description: Central Coast river-level SR data compilation
# Notes: Code to compile river-level SR data for all species, updated for sub-contract work for Kitasoo-Xai'xais Stewardship Authority
#        Requires: stream-level escapement & conservation unit-level age table from PSF Salmon Watersheds Program.
#        Output: stream-level stock-recruitment (S-R) data for all species (where sufficient data were available)
#        *Note that output S-R data relies on any assumptions made in the compilation of the stream-level escapement 
#         and age-at-return data compilation. 
#
#        This script checks for discrepancies in CU indices between the escapement and age/exploitation datasets
#        It also filters out data we won't be using in the compilation 


# --- Prepping environment --- #
# options(warn=2)

rm(list=ls())
graphics.off()

# Installing packages #
# install.packages("dpylr")
# install.packages("reshape2")
# install.packages("stringr")

# Loading packages #
library(dplyr)
library(tidyr)
library(reshape2)
library(stringr)
library(here)

setwd(here("data","spawner-recruit","raw","EA-river-level-SR-2023-update"))

# --- Inputs --- #

# Read in data from NCC database #
esc <- read.csv("escape_NCC_2023-03-21_CLEAN.csv", header=TRUE, stringsAsFactors = FALSE)
agebyCU <- read.csv("agebyCU_infilled_2023-03-21.csv", header=TRUE, stringsAsFactors = FALSE)

species <- unique(esc$SPP)
species2 <- c(unique(agebyCU$SpeciesId), "SX")


# Filter out Chinook and sockeye data because it's a mess and we're not using it for this project #
# (Update: it's not actually a total mess, there are just a lot of residual CUs in NuSEDS that aren't in the PSF data)
esc = esc[which(esc$SPP %in% c("CM","CO","PKE","PKO")),]
agebyCU = agebyCU[which(agebyCU$SpeciesId %in% c("CM","CO","PKe","PKo")),]

# --- STEP 1: Create decoder table between escapement and age data frames --- #
# Check whether/which CUs are in the escapement data but not in the age data and vice versa

cu_esc <- as.data.frame(matrix(nrow=length(unique(esc$CU_findex)), ncol=5))
names(cu_esc) <- c("SPP","CU_findex","CU_name","CU_index","Area")
cu_esc$CU_findex = unique(esc$CU_findex)

for (i in unique(cu_esc$CU_findex)){
  cu_esc[cu_esc$CU_findex==i,1] = esc[esc$CU_findex==i,]$SPP[1]
  cu_esc[cu_esc$CU_findex==i,3] = esc[esc$CU_findex==i,]$CU_name[1]
  cu_esc[cu_esc$CU_findex==i,4] = esc[esc$CU_findex==i,]$CU_index[1]
  cu_esc[cu_esc$CU_findex==i,5] = esc[esc$CU_findex==i,]$Area[1]
}

cu_age <- as.data.frame(matrix(nrow=length(unique(agebyCU$CU)), ncol=3))
names(cu_age) <- c("SPP","CU_index","CU_name")
cu_age$CU_index = unique(agebyCU$CU)

for (i in unique(cu_age$CU_index)){
  cu_age[cu_age$CU_index==i,1] = agebyCU[agebyCU$CU==i,]$SpeciesId[1]
  cu_age[cu_age$CU_index==i,3] = agebyCU[agebyCU$CU==i,]$CU_Name[1]
 }

# Re-format CU data to match escapement data and add column to escape dataframe to index CUs with age table #
cu_age$CU_index_2 <- cu_age$CU_index
cu_age$CU_index_2 <- gsub("CN","CK",cu_age$CU_index_2)
cu_age$CU_index_2 <- gsub("PKe","PKE",cu_age$CU_index_2)
cu_age$CU_index_2 <- gsub("PKo","PKO",cu_age$CU_index_2)
cu_age$CU_index_2 <- gsub("SX_L","SEL",cu_age$CU_index_2)
cu_age$CU_index_2 <- gsub("SX_R","SER-",cu_age$CU_index_2)
cu_age$CU_index_2 <- gsub("_","-",cu_age$CU_index_2)

# check for differences
# CUs that are in the age data but not esc
setdiff(unique(cu_age$CU_index_2),unique(cu_esc$CU_findex))
# CUs that are in the esc data but not age
setdiff(unique(cu_esc$CU_findex),unique(cu_age$CU_index_2))

# Update bigger dataframe
agebyCU$CU_index_2 <- NA

for (cu in unique(agebyCU$CU)){
  agebyCU[agebyCU$CU==cu,]$CU_index_2 = cu_age[cu_age$CU_index==cu,]$CU_index_2[1]
}

# --- STEP 2: Filter out non-focal areas and CUs based on input from Eric Hertz at PSF --- #

#### AGE DATA ####
# We are not going to filter the age data to exclude Areas 1-5 because it is collated at
# a CU level and there are a few CUs that include systems in both Area 5 and Area 6.
# Instead, we'll rely on the filtering of the escapement data to ensure we don't include
# data from excluded areas. (Per conversation with E. Hertz)

# Filter age data for Bella Coola CUs
agebyCU_2 = agebyCU[(which(!(agebyCU$CU_index_2 %in% c("CM-17","CM-16","CO-22")))),]

#### ESCAPEMENT DATA ####
# We are not using any data from Areas 1-5 due to lack of clarity on methods used by LGL for run reconstructions #
esc_2 = esc[which(esc$Area %in% c("6","7","8","9","10")),]

# We are not using any data for the Bella Coola chum/coho CUs because of enhancement (chum) 
# and lack of monitoring (coho).
esc_3 = esc_2[which(!(esc_2$CU_findex %in% c("CM-17","CM-16","CO-22"))),]

# Now that we have done all this filtering, let's check back on how many remaining discrepancies
# exist between CU lists in each dataframe
cu_age = cu_age[cu_age$CU_index_2 %in% unique(agebyCU_2$CU_index_2),]
cu_esc = cu_esc[cu_esc$CU_findex %in% unique(esc_3$CU_findex),]

# There is still one CU that shows up in escapement data but not age data
# (Wannock chum - this is a CU for which there are no recent data, not to worry about)
setdiff(unique(esc_3$CU_findex),unique(agebyCU_2$CU_index_2))


# Write filtered/cleaned up data to file
write.csv(agebyCU_2, paste("agebyCU_NCC_",Sys.Date(),"_CLEANED_FILTERED.csv",sep=""), row.names = FALSE)
write.csv(esc_3, paste("escape_NCC_",Sys.Date(),"_CLEANED_FILTERED.csv",sep=""), row.names = FALSE)

