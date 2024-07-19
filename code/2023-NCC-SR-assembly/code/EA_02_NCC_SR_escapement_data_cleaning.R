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

setwd(here("data","spawner-recruit","raw","EA-river-level-SR-2023-update"))

# --- Inputs --- #

# Read in data from NCC database #
escape <- read.csv("escape_NCC_2023-03-21.csv", header=TRUE, stringsAsFactors = FALSE)
agebyCU <- read.csv("agebyCU_infilled_2023-03-21.csv", header=TRUE, stringsAsFactors = FALSE)

# PUT IT IN A FUNCTION
clean_SR_dat <- function(escape, agebyCU) {

      
      # --- PART 1: Re-formatting and checking data --- #
      
      species <- unique(escape$SPP)
      #species2 <- c(unique(agebySA$SpeciesId), "SX")
      
      # Re-format statistical area so that can cross-reference between data frames #
      escape$StatArea <- paste("0",escape$StatArea,sep="")
      escape[substr(escape$StatArea,1,2) == "03",]$StatArea = "03"
      escape[substr(escape$StatArea,1,2) == "04",]$StatArea = "04"
      escape[substr(escape$StatArea,1,3) == "010",]$StatArea = "10"
      
      
      # Check 1: are there any population duplicates (i.e., population IDs with multiple entries)? #
      check <- data.frame(matrix(nrow=1, ncol=ncol(escape)))
      names(check) <- names(escape)
      
      for (i in unique(escape$POP_ID)){
        e <- escape[which(escape$POP_ID==i),]
        if (nrow(e) > 1 & !(e$SPP[1] %in% c("PKE","PKO"))){
          check <- rbind(check, e) 
        }
      }
      
      check <- check[-1,]
      if (nrow(check) == 0) { message("No duplicate populations (POP_ID)")
      } else if (nrow(check) != 0) {
          stop("Duplicate populations detected, need to investigate!")
        } 
      
      # Check 2: are there any stream duplicates (i.e., within a species, multiple entries for a given stream ID)? #
      check <- data.frame(matrix(nrow=1, ncol=ncol(escape)))
      names(check) <- names(escape)
      
      for (j in species){
        temp <- escape[escape$SPP==j,]
        for (i in unique(temp$GFE_ID)){
          e <- temp[which(temp$GFE_ID==i),]
          if (nrow(e) > 1){
            check <- rbind(check, e) 
          }
        }
      }
      
      check <- check[-1,]
      
      # Filter out known duplicate systems #
      esc <- escape[(escape$POP_ID == 51772),]
      message(paste("Duplicate river-level systems removed, GFE_ID:", check$GFE_ID[1]))
      
      check = check[-(which(check$GFE_ID==968)),]
      
      if (nrow(check) == 0) { message("No duplicate streams (GFE_ID)")
      } else if (nrow(check) != 0) {
        stop("Duplicate streams detected, need to investigate!")
      } 
      
      return(esc)
}

# Write to file for next step
write.csv(clean_SR_dat(escape, agebyCU), paste("escape_NCC_",Sys.Date(),"_CLEAN.csv",sep=""), row.names = FALSE)
