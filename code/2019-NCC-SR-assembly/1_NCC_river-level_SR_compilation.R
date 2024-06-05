# Date created: 01-Dec-2019
# Last updated: 17-Dec-2019
# Author: Emma Atkinson
# Description: Central Coast river-level SR data compilation
# Notes: Code to compile river-level SR data for all species 
#        Requires: stream-level escapement, Conservation Unit-level age table, and Statistical Area-level age table.
#        Output: stream-level stock-recruitment (S-R) data for all species (where sufficient data were available)
#         *Note that output S-R data relies on any assumptions made in the compilation of the stream-level escapement 
#          and age-at-return data compilation. 

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

dir.data <- "C:/Users/ematkinson/Google Drive/Research/PSF/Southern BC data compilation - Emma/River-level SR compilation"

# --- Inputs --- #

# Read in data from NCC database #
setwd(dir.data)

escape <- read.csv("NCC_streams_escapement.csv", header=TRUE, stringsAsFactors = FALSE)
agebySA <- read.csv("NCC_age_table_by_area.csv", header=TRUE, stringsAsFactors = FALSE)
agebyCU <- read.csv("NCC_age_table_by_CU.csv", header=TRUE, stringsAsFactors = FALSE)

# --- PART 1: Re-formatting and checking data --- #

species <- unique(escape$SPP)
species2 <- c(unique(agebySA$SpeciesId), "SX")

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
nrow(check)

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
nrow(check)

# Filter out duplicate systems #
esc <- escape[!(escape$POP_ID == 51772),]

# --- PART 2: Generate stream-level estimates for returns and recruits (indexed by brood year) --- #

Ymax <- 2017 # set this to most recent year with escapement data - will need to change as data is updated
ny <- length(1950:Ymax) # Set time spans and years to use for indexing 
Ymax_recruits <- max(agebyCU$BroodYear, na.rm=TRUE) # set this to most recent year in age table

# Set up destination dataframe for outputs #
Z <- as.data.frame((matrix(nrow=1, ncol=13)))
names(Z) <- c("GFE_ID","BroodYear","River","Species","Indicator","xLONG","yLAT","StatArea","CU","CU_2","Spawners","Returns","Recruits")

# Big loop starts here - will calculate returns and recruits for each species/CU/brood year #  
for (k in 1:length(species)){ # for each species

    spp1 = species[k] # species code for escapement dataframe
    spp2 = species2[k] # species code for age dataframe
    
    escape <- esc[which(esc$SPP==spp1),] # subset for species k
    escape <- escape[,c(1:59, 82:(82+ny-1))] # subset for selected range of years (should be robust if you change 'ny')
    
    # Subset age data for species k
    ageSA <- agebySA[which(agebySA$SpeciesId==spp2),]
    ageCU <- agebyCU[which(agebyCU$SpeciesId==spp2),]
    
    # Re-format CU data to match escapement data and add column to escape dataframe to index CUs with age table #
    ageCU$CU <- gsub("_","-",ageCU$CU)
    escape$CU_index_2 <- paste(spp2, escape$CU_index, sep="-") # this added CU index is to facilitate cross-referencing between age tables and escapement data
    
    # --- Set up species destination dataframe --- #
    d <- data.frame(matrix(nrow=1, ncol=11))
    names(d) <- c("River", "GFE_ID","Species", "BroodYear", 
                  "Indicator","xLONG", "yLAT", 
                  "StatArea", "CU","CU_2","Spawners")
    
    # --- Populating with data --- #
    # Add river data #
    
    for (j in unique(escape$GFE_ID)){ # for each stream
      
      dat <- filter(escape, GFE_ID == j)
      temp <- data.frame(matrix(nrow=ny, ncol=ncol(d)))
      names(temp) <- names(d)
      temp[,c(1:3,5:10)] <- rep(c(dat$SYS_NM, 
                                 dat$GFE_ID, 
                                 dat$SPP, 
                                 dat$IsIndicator, 
                                 dat$yLAT, 
                                 dat$xLONG, 
                                 dat$StatArea,
                                 dat$CU_findex,
                                 dat$CU_index_2), each=ny)
      temp$BroodYear <- c(1950:Ymax)
      
      d <- rbind(d, temp)
      
    }
    
    d <- d[-1,]
    
    # --- Add spawner data --- #
    # Lengthen NuSEDS data #
    spawn <- reshape(escape, 
                     direction="long", 
                     varying=list(names(escape[60:(60+ny-1)])), 
                     v.names="Spawners",
                     idvar=c("GFE_ID"),
                     timevar="BroodYear",
                     times=paste("X",1950:Ymax,sep=""))
    
    # Edit year name #
    spawn$BroodYear <- gsub("X", "", spawn$BroodYear)
    
    # Edit row names #
    rownames(spawn) <- 1:length(spawn$ID)
    spawners <- spawn[,c("GFE_ID","BroodYear","Spawners")]
    
    # Add spawner data to destination data frame #
    data <- merge(d, spawners, by=c("GFE_ID","BroodYear"))
    
    # Format dataframe #
    data <- data[,-c(11)]
    names(data)[11] <- "Spawners"
    
    # Manipulate ER and age data #
    ER_SA <- ageSA[,c(2,3,12)]
    age_SA <- ageSA[,c(2,3,13:18)]
    
    ER_CU <- ageCU[,c(2,4,13)]
    age_CU <- ageCU[,c(2,4,14:19)]
    
    # Calcuate returns #
    i <- NA
    y <- NA
    
    Returns <- as.numeric(rep(NA, nrow(data))) # set up vector for returns estimates
    
    for (i in 1:nrow(data)){ # This takes a minute 
    #i <- 80 # set for testing loop  
      
      area <- data$StatArea[i]
      cu <- data$CU_2[i]
      y <- data$BroodYear[i]
      
      if (!(y %in% 1954:Ymax_recruits)) { } # if year i is not within specified time range, do nothing
      
      # If there was non-zero exploitation, calculate returns as the sum of catch and escapement: Returns = Spawners/(1-Exploitation rate)
      # Draw from CU-level data, if available
      else if (length(ER_CU[ER_CU$CU==cu & ER_CU$BroodYear==y,]$Total.ER) > 0 &
               !is.na(length(ER_CU[ER_CU$CU==cu & ER_CU$BroodYear==y,]$Total.ER))) {
        Returns[i] = data$Spawners[i]/(1-ER_CU[ER_CU$CU==cu & ER_CU$BroodYear==y,]$Total.ER)    }
      
      # If CU-level data not available, check for SA-level data 
      else if (length(ER_SA[ER_SA$StatArea==area & ER_SA$BroodYear==y,]$Total.ER) > 0 &
               !is.na(length(ER_SA[ER_SA$StatArea==area & ER_SA$BroodYear==y,]$Total.ER))) {
        Returns[i] = data$Spawners[i]/(1-ER_SA[ER_SA$StatArea==area & ER_SA$BroodYear==y,]$Total.ER)    }
      
      }
    
    z <- cbind(data, Returns)
    
    # Calculate recruits - this is the part that changes depending on species (because of diff. age structures) # 
    Recruits <- as.numeric(rep(NA, nrow(z)))
    
    # Setting up number of years to sum returns from. Pinks never change, so can set regardless of which CU/SA # 
    if (spp1 %in% c("PKE","PKO")){ first<-2; last<-2; n=1; age_range="YES_DAT" } # first = first returning age class; last = last returning age class; n = total number of age classes
    
    for (i in 1:nrow(z)){ # This takes a minute 
      
      area <- z$StatArea[i]
      cu <- z$CU_2[i]
      y <- z$BroodYear[i]
      
      if (y %in% 1954:Ymax_recruits) {
       
      # First, if not pink, need to set age range to sum returns from and calculate recruits #
      # Setting CU by CU because there is variation in the age-at-return distributions, even within species #
      if (spp1 %in% c("CO","CM","SEL","SER","CK")){
        
          aar_check_CU <- age_CU[age_CU$CU==cu & age_CU$BroodYear==y,3:8]
          aar_check_SA <- age_SA[age_SA$StatArea==area & age_SA$BroodYear==y,3:8]
          
          if (nrow(aar_check_CU) != 0) { age_range <- which(!is.na(aar_check_CU) & aar_check_CU > 0)+1 
          } else if (nrow(aar_check_SA) != 0) { age_range <- which(!is.na(aar_check_SA) & aar_check_SA > 0)+1 
          } else { age_range <- "NO_DAT"}
          
          if (age_range[1] != "NO_DAT"){ first<-min(age_range, na.rm=TRUE); last<-max(age_range, na.rm=TRUE); n<-last-first+1 }
          
          }
      
      if (age_range[1] != "NO_DAT"){
          ret <- z$Returns[c((i+first):(i+last))] # vector of returns (spawners + catch) for a given brood year
            
          if (length(ret[is.na(ret)]) < 1 & !(spp1 %in% c("PKE","PKO"))) { # only proceed if there are return estimates for all age classes and skip pinks for now
          
          SA <- length(age_SA[age_SA$StatArea==area & age_SA$BroodYear %in% c((y+first):(y+last)),1]) # how many years do we have age data for?
          CU <- length(age_CU[age_CU$CU==cu & age_CU$BroodYear %in% c((y+first):(y+last)),1])
          
          # only compile age distribution if there are age proportions for all age classes #
          if (SA >= n) { aar_SA <- age_SA[age_SA$StatArea==area & age_SA$BroodYear %in% c((y+first):(y+last)),(first+1):(last+1)] } else { aar_SA <- NA }
          if (CU >= n) { aar_CU <- age_CU[age_CU$CU==cu & age_CU$BroodYear %in% c((y+first):(y+last)),(first+1):(last+1)] } else {aar_CU <- NA }
          
          if (!("FALSE" %in% !is.na(aar_CU))) { # if there are CU-level age data, use these
            
            returns_at_age = ret[1:n]
            proportion_of_age = rep(NA, n)
            for (g in 1:n) { proportion_of_age[g] <- aar_CU[g,g] }
            recruits_by_year = returns_at_age*proportion_of_age
            Recruits[i] = sum(recruits_by_year)  
            
          }
          
          else if (!("FALSE" %in% !is.na(aar_SA))) { # if no CU-level age data, check for SA-level age data
            
            returns_at_age = ret[1:n] # assemble vector of returns for n age classes
            proportion_of_age = rep(NA, n) 
            for (g in 1:n) { proportion_of_age[g] <- aar_SA[g,g] } # assemble age proportions for n age classes
            recruits_by_year = returns_at_age*proportion_of_age # calculate recruits at each age class
            Recruits[i] = sum(recruits_by_year) # sum recruits across age classes for overall recruitment estimate
            
            }
          
          }
          if (spp1 %in% c("PKE","PKO")) { # if pinks, then don't need age data 
            Recruits[i] = ret
          }
        }
      }
    }
    
    zz <- cbind(z, Recruits) 
    Z <- rbind(Z, zz)

}

# Remove empty top row #
Z <- Z[-1,]

# Write to file - phewf! #
write.csv(Z, "NCC_streams_SR_data.csv", row.names = FALSE)




