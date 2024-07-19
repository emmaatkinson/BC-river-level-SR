# Date created: 20-Mar-2023
# Last updated: 19-Apr-2023
# Author: Emma Atkinson
# Description: Central Coast river-level SR data compilation
# Notes: Code to compile river-level SR data for all species, updated for sub-contract work for Kitasoo-Xai'xais Stewardship Authority
#        Requires: stream-level escapement & conservation unit-level age table from PSF Salmon Watersheds Program.
#        Output: stream-level stock-recruitment (S-R) data for all species (where sufficient data were available)
#        *Note that output S-R data relies on any assumptions made in the compilation of the stream-level escapement 
#         and age-at-return data compilation. 
#
#         This script cleans up the river-level escapement data and checks for duplicate populations/systems

# --- Prepping environment --- #

rm(list=ls())
graphics.off()

# Installing packages #
# install.packages("dpylr")
# install.packages("reshape2")
# install.packages("stringr")

# Loading packages #
library(dplyr)
library(reshape2)
library(stringr)
library(here)


# --- Inputs --- #

# Read in data from NCC database 
setwd(here("code","2023-NCC-SR-assembly","raw-data"))
escape_old <- read.csv("escape_NCC_2023-03-21.csv", header=TRUE, stringsAsFactors = FALSE)

setwd(here("code","2024-NCC-SR-assembly","raw-data"))
escape_new <- read.csv("all_areas_nuseds_cleaned_20240419.csv", header=TRUE, stringsAsFactors = FALSE)

# subsetting full NuSEDS database update (cleaned by PSF) to just the river-level populations included in the NCC 
# using the GFE_IDs (stream specific) from the old NCC dataset.
NCC_pops = unique(escape_old$GFE_ID) #Take NCC streams
escape = escape_new[which(escape_new$GFE_ID %in% NCC_pops),] #filter NuSEDS to these NCC streams

#read in age data
agebyCU <- read.csv("agebyCU_infilled_2023-03-21.csv", header=TRUE, stringsAsFactors = FALSE)


# PUT IT IN A FUNCTION
clean_SR_dat <- function(escape, agebyCU) {
  
      # --- PART 1: Re-formatting and checking data --- 
      species <- unique(escape$species_acronym_ncc)
      
      # Re-format statistical area so that can cross-reference between data frames 
      escape[which(escape$StatArea == "3"),]$StatArea = "03"
      escape[which(escape$StatArea == "4"),]$StatArea = "04"
      escape[which(escape$StatArea == "2E"),]$StatArea = "02E"
      escape[which(escape$StatArea == "02W"),]$StatArea = "002W"
      
      # Check 1: are there any population duplicates (i.e.,  multiple year entries for the same population_id)? #
      is.dupe<-matrix(nrow =length(unique(escape$POP_ID)),ncol=2)
      
      for (i in 1:length(unique(escape$POP_ID))){
        
        temp_pop<-unique(escape$POP_ID)[i]
        is.dupe[i,1]<-temp_pop
        
        e <- escape[which(escape$POP_ID==temp_pop),]
        years<-e$Year
        
        is.dupe[i,2]<-sum(duplicated(years))
      }
      
      if (sum(is.dupe[,2]) == 0) { message("No duplicate years (POP_ID)")
      } else if (sum(is.dupe[,2]) != 0) {
        message("Duplicate streams detected, need to investigate (POP_ID)")
      } 
      
      # Check 2: are there any stream duplicates (i.e., within a species, multiple entries for a given stream ID)? #
      is.dupe2<-matrix(c(0,0,0),nrow =1,ncol=3)
    
      for (i in 1:length(unique(escape$GFE_ID))){
        
        temp_gfe<-unique(escape$GFE_ID)[i]
        e <- escape[which(escape$GFE_ID==temp_gfe),]
        
        for(j in unique(e$species_acronym_ncc)){
          
        temp_sp<-j
        
        e1 <- e[which(e$species_acronym_ncc==temp_sp),]
        years<-e1$Year
        dupe_sum<-sum(duplicated(years))
        
        is.dupe2<-rbind(is.dupe2,c(temp_gfe,temp_sp,dupe_sum))
      }
      }
      if (sum(as.numeric(is.dupe2[2:nrow(is.dupe2),3])) == 0) { message("No duplicate years (GFE_id)")
      } else if (sum(as.numeric(is.dupe2[2:nrow(is.dupe2),3])) != 0) {
        message("Duplicate streams detected, need to investigate! (GFE_id)")
        print(is.dupe2[which(is.dupe2[,3]!= "0"),])
      } 
      
      #remove problem populations
      esc<-escape[!(escape$POP_ID == 51772),]
    
      #return(is.dupe2)
      return(esc)
}

# Write to file for next step
write.csv(clean_SR_dat(escape, agebyCU), paste("escape_NCC_",Sys.Date(),"_CLEAN.csv",sep=""), row.names = FALSE)
