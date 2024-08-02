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

# --- Inputs --- #

# Read in data from NCC database #

setwd(here("code","2023-NCC-SR-assembly","generated-data"))
esc<- read.csv("escape_NCC_2024-07-04_CLEAN.csv", header=TRUE, stringsAsFactors = FALSE)
rivers <- read.csv("conservation_unit_system_sites_cleaned_20240419.csv")


dat = read.csv("NCC_streams_river-level_SR_data_2024-07-19.csv", header=TRUE, stringsAsFactors = FALSE)
dat1= read.csv("NCC_streams_river-level_SR_data_2023-12-19.csv")

dat1[which(dat1$GFE_ID==dat$GFE_ID[1000]),]$Spawners
dat[which(dat$GFE_ID==dat$GFE_ID[1000]),]$Returns


S#adding CU info to data
nrows<-nrow(esc)

esc$CU_findex<-rep(0,nrows)
esc$CU_name<-rep("A",nrows)
esc$CU_index<-rep("A",nrows)

ind_id_list<-unique(esc$IndexId)
ind_length<-length(ind_id_list)

for (i in 1:ind_length){
  
  #Current index_id, this will be used to reference between the rivers dataset (has CU information)
  #and the esc dataset (has catch and more info but does not have CU). This for loop uses the index_id
  #to add CU info to the esc dataset
  current_ind<-ind_id_list[i]
  
  #described above
  current_CU<-unique(rivers[which(rivers$IndexId==current_ind),]$FULL_CU_IN)
  current_CU_name<-unique(rivers[which(rivers$IndexId==current_ind),]$CU_NAME)
  current_CU_ind<-unique(rivers[which(rivers$IndexId==current_ind),]$CU_INDEX)
  
  #list which rows have the current index_id and count number of rows
  rows_current<-which(esc$IndexId==current_ind)
  nrows_current<-length(rows_current)

  #add CU info to selected rows 
  esc[rows_current,]$CU_findex<-rep(current_CU,nrows_current)
  esc[rows_current,]$CU_name<-rep(current_CU_name,nrows_current)
  esc[rows_current,]$CU_index<-rep(current_CU_ind,nrows_current)
}


esc[which(is.na(esc$TOTAL_RETURN_TO_RIVER)),]$TOTAL_RETURN_TO_RIVER <- rep(0,sum(is.na(esc$TOTAL_RETURN_TO_RIVER)))
esc[which(is.na(esc$NATURAL_SPAWNERS_TOTAL)),]$NATURAL_SPAWNERS_TOTAL <- rep(0,sum(is.na(esc$NATURAL_SPAWNERS_TOTAL)))

#?
esc$CU_findex<-gsub("PKe","PKE",esc$CU_findex)
esc$CU_findex<-gsub("PKo","PKO",esc$CU_findex)

#? 
#unique(trtc$CU)[which(!unique(trtc$CU)%in%unique(dat$CU))]
#unique(dat$CU)[which(!unique(dat$CU)%in%unique(trtc$CU))]

# Filter out Chinook and sockeye data because it's a mess and we're not using it for this project #
# (Update: it's not actually a total mess, there are just a lot of residual CUs in NuSEDS that aren't in the PSF data)
esc = esc[which(esc$species_acronym_ncc %in% c("CM","CO","PKE","PKO")),]
agebyCU = agebyCU[which(agebyCU$SpeciesId %in% c("CM","CO","PKe","PKo")),]

# --- STEP 1: Create decoder table between escapement and age data frames --- #
# Check whether/which CUs are in the escapement data but not in the age data and vice versa

cu_esc <- as.data.frame(matrix(nrow=length(unique(esc$CU_findex)), ncol=5))
names(cu_esc) <- c("SPP","CU_findex","CU_name","CU_index","Area")
cu_esc$CU_findex = unique(esc$CU_findex)

for (i in unique(cu_esc$CU_findex)){
  cu_esc[cu_esc$CU_findex==i,1] = esc[esc$CU_findex==i,]$species_acronym_ncc[1]
  cu_esc[cu_esc$CU_findex==i,3] = esc[esc$CU_findex==i,]$CU_name[1]
  cu_esc[cu_esc$CU_findex==i,4] = esc[esc$CU_findex==i,]$CU_index[1]
  cu_esc[cu_esc$CU_findex==i,5] = esc[esc$CU_findex==i,]$AREA[1]
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
cu_age
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
esc_2 = esc[which(esc$AREA %in% c("6","7","8","9","10")),]

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

