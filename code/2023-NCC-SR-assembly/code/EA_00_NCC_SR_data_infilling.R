# Date created: 16-Mar-2023
# Last updated: 19-Apr-2023
# Author: Emma Atkinson
# Description: Central Coast river-level SR data compilation
# Notes: Code to compile river-level SR data for pink, chum, and coho, updated for sub-contract work for Kitasoo-Xai'xais Stewardship Authority
#        Requires: stream-level escapement & conservation unit-level age table from PSF Salmon Watersheds Program (SWP).
#        Output: stream-level stock-recruitment (S-R) data for all species (where sufficient data were available)
#        *Note that output S-R data relies on any assumptions made in the compilation of the stream-level escapement 
#         and age-at-return data compilation. 
#         
#         This script cleans up & infills data gaps in the version received from the PSF SWP.


# --- Prepping environment --- #

rm(list=ls())
graphics.off()

library(here)
setwd(here("NCC-SR-assembly","EA-river-level-SR-2023-update"))

# Installing packages #
# install.packages("tidyverse")
# install.packages("reshape2")
# install.packages("stringr")
# install.packages("readxl")

# Loading packages #
library(tidyverse)
library(reshape2)
library(stringr)
library(readxl)

# --- Reading in big salmon data file, as received from PSF --- #

# Getting names of individual datasheets #
sheets = excel_sheets("All-OUTPUT--nonlegacy-mode_20220222.xlsx")[1:7]

# Reading in and separating datasheets (note: NAs will produce warnings - that's fine) #
x = mapply(function(X) read_xlsx("All-OUTPUT--nonlegacy-mode_20220222.xlsx", guess_max = 7000, sheet=X), sheets) 
# Changing them to dataframes
xx = lapply(x, as.data.frame) 
# Giving them their names back
names(xx) <- sheets 

# Pulling out the sheets we want to use #
agebyCU_raw = xx[[1]] # age table, needs infilling from TRTC table that Eric Hertz updated
TRTCbyCU = xx[[6]] # TRTC table, this is the sheet that was updated by Eric Hertz
escape_NCC_old = xx[[5]] # stream-level escapement data, Eric Hertz was unsure that this had been updated so I used a separate version he sent directly

# Set up version of age table to work with #
agebyCU = agebyCU_raw

# Pulling in the NuSEDS sheet that Eric sent (Mar-2023) #
# Note: ideally, we would actually just pull the 5th sheet from the XLSX file:
#       "OUTPUT NCC Streams Escapement". So for future I have included that code too.

#escape = xx[[5]]
escape_NCC_new = read.csv("NuSEDS_escapement_data_collated_20221101.csv", stringsAsFactors = FALSE)

# subsetting full NuSEDS database update (received from Eric Hertz) to just the river-level populations included in the NCC
NCC_pops = unique(escape_NCC_old$GFE_ID)
escape = escape_NCC_new[which(escape_NCC_new$GFE_ID %in% NCC_pops),]

# --- Infilling 'age' table with info from TRTC table --- #
# E.H. only updated the TRTC table in the latest NCC compilation so there was some infilling required
# to produce a version of the 'age table' that could be used for the river-level compilation

# adding rows for 2019-2021 #

for (i in unique(agebyCU$CU)) {
  
  if (max(agebyCU[agebyCU$CU==i,"BroodYear"])==2019) { # if last available year in age table is 2019
    
    n = nrow(agebyCU)
    agebyCU[c(n+1, n+2),c("SpeciesId","CU","CU_Name","Age2","Age3","Age4","Age5","Age6","Age7")] = agebyCU[which(agebyCU$CU==i & agebyCU$BroodYear==2019),c("SpeciesId","CU","CU_Name","Age2","Age3","Age4","Age5","Age6","Age7")]  
    agebyCU[c(n+1, n+2),"BroodYear"] = c(2020,2021) # add rows for 2020 and 2021
    
    # pull 'Total Escapement' (TE) from TRTC table   
    if (agebyCU[agebyCU$CU==i,]$SpeciesId[1] != "PKo") { agebyCU[c(n+1),"Escape"] = TRTCbyCU[which(TRTCbyCU$CU==i & TRTCbyCU$Year==2020),"TE"] }
    if (agebyCU[agebyCU$CU==i,]$SpeciesId[1] != "PKe") { agebyCU[c(n+2),"Escape"] = TRTCbyCU[which(TRTCbyCU$CU==i & TRTCbyCU$Year==2021),"TE"] }
    # pull 'Total Exploitation Rate' (Total ER) from TRTC table
    if (agebyCU[agebyCU$CU==i,]$SpeciesId[1] != "PKo") { agebyCU[c(n+1),"Total ER"] = TRTCbyCU[which(TRTCbyCU$CU==i & TRTCbyCU$Year==2020),"Total ER"] }
    if (agebyCU[agebyCU$CU==i,]$SpeciesId[1] != "PKe") { agebyCU[c(n+2),"Total ER"] = TRTCbyCU[which(TRTCbyCU$CU==i & TRTCbyCU$Year==2021),"Total ER"] }
    
  }
  
  else if (max(agebyCU[agebyCU$CU==i,"BroodYear"])==2018) { # if last available year in age table is 2018 (e.g., for even year pink)
    
    n = nrow(agebyCU)
    agebyCU[c(n+1, n+2, n+3),c("SpeciesId","CU","CU_Name","Age2","Age3","Age4","Age5","Age6","Age7")] = agebyCU[which(agebyCU$CU==i & agebyCU$BroodYear==2018),c("SpeciesId","CU","CU_Name","Age2","Age3","Age4","Age5","Age6","Age7")]  
    agebyCU[c(n+1, n+2, n+3),"BroodYear"] = c(2019,2020,2021) # add rows for 2019-2021
    
    # pull 'Total Escapement' (TE) from TRTC table
    if (agebyCU[agebyCU$CU==i,]$SpeciesId[1] != "PKe") { agebyCU[c(n+1),"Escape"] = TRTCbyCU[which(TRTCbyCU$CU==i & TRTCbyCU$Year==2019),"TE"] }
    if (agebyCU[agebyCU$CU==i,]$SpeciesId[1] != "PKo") { agebyCU[c(n+2),"Escape"] = TRTCbyCU[which(TRTCbyCU$CU==i & TRTCbyCU$Year==2020),"TE"] }
    if (agebyCU[agebyCU$CU==i,]$SpeciesId[1] != "PKe") { agebyCU[c(n+3),"Escape"] = TRTCbyCU[which(TRTCbyCU$CU==i & TRTCbyCU$Year==2021),"TE"] }
    # pull 'Total Exploitation Rate' (Total ER) from TRTC table
    if (agebyCU[agebyCU$CU==i,]$SpeciesId[1] != "PKe") { agebyCU[c(n+1),"Total ER"] = TRTCbyCU[which(TRTCbyCU$CU==i & TRTCbyCU$Year==2019),"Total ER"] }
    if (agebyCU[agebyCU$CU==i,]$SpeciesId[1] != "PKo") { agebyCU[c(n+2),"Total ER"] = TRTCbyCU[which(TRTCbyCU$CU==i & TRTCbyCU$Year==2020),"Total ER"] }
    if (agebyCU[agebyCU$CU==i,]$SpeciesId[1] != "PKe") { agebyCU[c(n+3),"Total ER"] = TRTCbyCU[which(TRTCbyCU$CU==i & TRTCbyCU$Year==2021),"Total ER"] }
    
  }
  
}

# Clean up extra rows for PKe and PKo CUs because you don't have time to write nice code 
agebyCU = agebyCU[-(which(agebyCU$SpeciesId == "PKe" & agebyCU$BroodYear %in% c(2019, 2021))),] # take out odd years for even-year pop'ns 
agebyCU = agebyCU[-(which(agebyCU$SpeciesId == "PKo" & agebyCU$BroodYear %in% c(2020))),] # take out even year for odd-year pop'ns

# infilling age-specific return estimates #
# not necessary for river-level compilation but in order to generate CU diagnostic plots, need to update the age-specific return
# estimates in the age table using ERs from TRTC table and age-distributions from last available years
# age distributions are always assumed to be constant through time, so for infilling 2019-2021 we assume the same age distribution as for last
# available year.
for (i in 1:nrow(agebyCU)){
  
  cu <- agebyCU$CU[i]
  y <- agebyCU$BroodYear[i]
  ymax <- max(agebyCU[agebyCU$CU==cu,]$BroodYear)
  spp <- agebyCU$SpeciesId[i]
 
  if (spp %in% c("PKe", "PKo")) { # if pink, just need to calculate age-2 returns
    if ((ymax-y) >= 2) { agebyCU[i,"TR2"] = agebyCU[which(agebyCU$CU==cu & agebyCU$BroodYear==y+2),"Age2"]*TRTCbyCU[which(TRTCbyCU$CU==cu & TRTCbyCU$Year==y+2),"Total Run"] }
    
    if (FALSE %in% is.na(agebyCU[i,c("TR2")])) { # if there is an estimate, then infill a 'Total' run estimate
      agebyCU[i,"Total"] = sum(agebyCU[i,c("TR2")], na.rm=TRUE) 
    } else { 
      agebyCU[i,"Total"] = NA # if no data, leave blank
    }
  } else {
  # if not pink, calculate age-specific returns for as many years are available 
    if ((ymax-y) >= 2) { agebyCU[i,"TR2"] = agebyCU[which(agebyCU$CU==cu & agebyCU$BroodYear==y+2),"Age2"]*TRTCbyCU[which(TRTCbyCU$CU==cu & TRTCbyCU$Year==y+2),"Total Run"] }
    if ((ymax-y) >= 3) { agebyCU[i,"TR3"] = agebyCU[which(agebyCU$CU==cu & agebyCU$BroodYear==y+3),"Age3"]*TRTCbyCU[which(TRTCbyCU$CU==cu & TRTCbyCU$Year==y+3),"Total Run"] }
    if ((ymax-y) >= 4) { agebyCU[i,"TR4"] = agebyCU[which(agebyCU$CU==cu & agebyCU$BroodYear==y+4),"Age4"]*TRTCbyCU[which(TRTCbyCU$CU==cu & TRTCbyCU$Year==y+4),"Total Run"] }
    if ((ymax-y) >= 5) { agebyCU[i,"TR5"] = agebyCU[which(agebyCU$CU==cu & agebyCU$BroodYear==y+5),"Age5"]*TRTCbyCU[which(TRTCbyCU$CU==cu & TRTCbyCU$Year==y+5),"Total Run"] }
    if ((ymax-y) >= 6) { agebyCU[i,"TR6"] = agebyCU[which(agebyCU$CU==cu & agebyCU$BroodYear==y+6),"Age6"]*TRTCbyCU[which(TRTCbyCU$CU==cu & TRTCbyCU$Year==y+6),"Total Run"] }
    if ((ymax-y) >= 7) { agebyCU[i,"TR7"] = agebyCU[which(agebyCU$CU==cu & agebyCU$BroodYear==y+7),"Age7"]*TRTCbyCU[which(TRTCbyCU$CU==cu & TRTCbyCU$Year==y+7),"Total Run"] }
  
    if (FALSE %in% is.na(agebyCU[i,c("TR2","TR3","TR4","TR5","TR6","TR7")])) { # 
      agebyCU[i,"Total"] = sum(agebyCU[i,c("TR2","TR3","TR4","TR5","TR6","TR7")], na.rm=TRUE) 
      } else { 
       agebyCU[i,"Total"] = NA 
      }
  }
 
}

# write infilled data to files
write.csv(agebyCU, paste("agebyCU_infilled_", Sys.Date(), ".csv", sep=""), row.names = FALSE)
write.csv(TRTCbyCU, paste("TRTCbyCU_", Sys.Date(), ".csv", sep=""), row.names = FALSE)
write.csv(escape, paste("escape_NCC_", Sys.Date(), ".csv", sep=""), row.names = FALSE)
