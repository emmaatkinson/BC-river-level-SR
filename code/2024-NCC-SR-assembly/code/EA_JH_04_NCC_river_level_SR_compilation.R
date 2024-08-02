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
#         This is the main SR compilation script. For each river-level population, pulls spawner data from
#         escapement dataset and pulls exploitation/age data for appropriate CU to generate recruitment estimate


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
setwd(here("code","2024-NCC-SR-assembly","raw-data"))

# Read in data from NCC database #
esc <- read.csv("escape_NCC_2024-07-11_CLEANED_FILTERED.csv", header=TRUE, stringsAsFactors = FALSE)
agebyCU <- read.csv("agebyCU_NCC_2024-07-11_CLEANED_FILTERED.csv", header=TRUE, stringsAsFactors = FALSE)

species <- sort(unique(esc$species_acronym_ncc))
species2 <- sort(unique(agebyCU$SpeciesId))


esc$Returns
# --- PART 2: Generate stream-level estimates for returns and recruits (indexed by brood year) --- #

Ymax <- 2021 # set this to most recent year with escapement data - will need to change as data is updated
  
# SET UP IN A FUNCTION
compile_riverSR = function(esc, agebyCU, Ymax) {
  
        ny <- length(1920:Ymax) # Set time spans and years to use for indexing 
        
      Ymax_recruits <- max(agebyCU$BroodYear, na.rm=TRUE) # set this to most recent year in age table

        # Big loop starts here - will calculate returns and recruits for each species/CU/brood year #  
        for (k in 1:length(species)){ # for each species
        # k=1 #set k to test loop
          
            spp1 = species[k] # species code for escapement dataframe
            spp2 = species2[k] # species code for age dataframe
            
            escape <- esc[which(esc$species_acronym_ncc==spp1),] # subset for species k
            ageCU <- agebyCU[which(agebyCU$SpeciesId==spp2),]
            
            # EA NOTE (2023-03-21): The column specs for subsetting this dataframe are a bit annoying. The version of the escapement spreadsheet
            # I ended up using for this compilation had different dimensions from the one included in the big multi-sheet file that is usually 
            # spit out by the NCC PSF/LGL run reconstruction. So I have adjusted the dimensions in the following line of code to suit the spreadsheet
            # Eric sent me but have also left the line of code that would usually work for the output of the run reconstruction. 
            escape <- escape[which(escape$Year<=1920+ny),] # subset for selected range of years (should be robust if you change 'ny')
            #escape <- escape[,c(1:59, 82:(82+ny-1))] # subset for selected range of years (should be robust if you change 'ny')
            #old_names<-names(esc[,c(1:45)])
            # Subset age data for species k
          
            data<-escape
            
            # Manipulate ER and age data #
            ER_CU <- ageCU[,c(20,4,13)]

            names(ER_CU)[1] = "CU"
         
            age_CU <- ageCU[,c(20,4,14:19)]
            names(age_CU)[1] = "CU"
           
            # Calcuate returns #
            i <- NA
            y <- NA
            
            Returns <- as.numeric(rep(NA, nrow(data))) # set up vector for returns estimates
            Total_ER <- as.numeric(rep(NA, nrow(data))) # set up vector to store ER estimates
            
            for (i in 1:nrow(data)){ # This takes a minute 
            #i <-340 # set for testing loop  
              
              cu <- data$CU_findex[i]
              y <- data$Year[i]
              
              if (!(y %in% 1954:Ymax_recruits)) {}  # if year i is not within specified time range, do nothing
              
              # If there was non-zero exploitation, calculate returns as the sum of catch and escapement: Returns = Spawners/(1-Exploitation rate)
              # Draw from CU-level data
              else if (length(ER_CU[ER_CU$CU==cu & ER_CU$BroodYear==y,]$Total.ER) > 0 &
                       !is.na(length(ER_CU[ER_CU$CU==cu & ER_CU$BroodYear==y,]$Total.ER))) {
                 
                Returns[i] = data$MAX_ESTIMATE[i]/(1-ER_CU[ER_CU$CU==cu & ER_CU$BroodYear==y,]$Total.ER)
                Total_ER[i] = ER_CU[ER_CU$CU==cu & ER_CU$BroodYear==y,]$Total.ER
                
                }
            }
            
            z <- cbind(data, Returns, Total_ER)
            
            # Calculate recruits - this is the part that changes depending on species (because of diff. age structures) # 
            Recruits <- as.numeric(rep(NA, nrow(z)))
            
            age_specific_recruits <- as.data.frame(matrix(nrow=nrow(z), ncol=6))
            names(age_specific_recruits) <- c("Recruits_Age2","Recruits_Age3","Recruits_Age4","Recruits_Age5","Recruits_Age6","Recruits_Age7")
            
            returns_age_proportions <- as.data.frame(matrix(nrow=nrow(z), ncol=6))
            names(returns_age_proportions) <- c("Returns_Proportion_Age_2","Returns_Proportion_Age_3","Returns_Proportion_Age_4","Returns_Proportion_Age_5","Returns_Proportion_Age_6","Returns_Proportion_Age_7")
           
            # Setting up number of years to sum returns from. Pinks never change, so can set regardless of which CU/SA # 
            if (spp1 %in% c("PKE","PKO")){ first<-2; last<-2; n=1; age_range="YES_DAT" } # first = first returning age class; last = last returning age class; n = total number of age classes
            
            for (i in 1:nrow(z)){ # This takes a minute 
              
              cu <- z$CU_findex[i]
              y <- z$Year[i]
              
              if (y %in% 1954:Ymax_recruits) {
               
              # First, if not pink, need to set age range to sum returns from and calculate recruits #
              # Setting CU by CU because there is variation in the age-at-return distributions, even within species #
              if (spp1 %in% c("CO","CM")){
                
                  aar_check_CU <- age_CU[age_CU$CU==cu & age_CU$BroodYear==y,3:8]
                 
                  if (nrow(aar_check_CU) != 0) { 
                    
                    # first, if dealing with chum, set age-6 proportion to zero (this reflects decision to not consider age-6
                    # chum returns because they make up such a small, <1%, proportion of returns)
                    if (spp1 == "CM") { aar_check_CU$Age6 = 0 } 
                    # now calculate age range
                    age_range <- which(!is.na(aar_check_CU) & aar_check_CU > 0)+1
                      
                      }
                  else { age_range <- "NO_DAT"}
                  
                  if (age_range[1] != "NO_DAT"){ first<-min(age_range, na.rm=TRUE); last<-max(age_range, na.rm=TRUE); n<-last-first+1 }
                  
                  }
              
              if (age_range[1] != "NO_DAT"){
                  ret <- z$Returns[c((i+first):(i+last))] # vector of returns (spawners + catch) for a given brood year
                    
                  if (length(ret[is.na(ret)]) < 1 & !(spp1 %in% c("PKE","PKO"))) { # only proceed if there are return estimates for all age classes and skip pinks for now
                 
                  CU <- length(age_CU[age_CU$CU==cu & age_CU$BroodYear %in% c((y+first):(y+last)),1])
                  
                  # only compile age distribution if there are age proportions for all age classes #
                  if (CU >= n) { aar_CU <- age_CU[age_CU$CU==cu & age_CU$BroodYear %in% c((y+first):(y+last)),(first+1):(last+1)] } else {aar_CU <- NA }
                  
                  if (!("FALSE" %in% !is.na(aar_CU))) { # if there are CU-level age data, use these
                    
                    returns_at_age = ret[1:n]
                    proportion_of_age = rep(NA, n)
                    for (g in 1:n) { proportion_of_age[g] <- aar_CU[g,g] }
                    recruits_by_year = returns_at_age*proportion_of_age
                    
                    # store assembled estimates
                    Recruits[i] = sum(recruits_by_year)
                    age_specific_recruits[i,(first-1):(last-1)] = recruits_by_year
                    returns_age_proportions[i,(first-1):(last-1)] = proportion_of_age
                    
                    }
                  
                  }
                  if (spp1 %in% c("PKE","PKO")) { # if pinks, then don't need age data 
                    Recruits[i] = ret
                    age_specific_recruits[i,1] = ret 
                    returns_age_proportions[i,1] = 1
                  }
                }
              }
            }
            zz <- cbind(z, Recruits, age_specific_recruits, returns_age_proportions)
            
            if(k==1){#if this is the first species, "zz" (species specific dataset) is whole dataset
              Z<-zz
            }
            
            if(k!=1){ #if its not the first species, add zz to total dataset
            Z <- rbind(Z, zz)
            }
        }
        return(Z)
        
}

# Write to file - phewf! #
setwd(here("code","2024-NCC-SR-assembly","generated data"))
write.csv(compile_riverSR(esc, agebyCU, Ymax), paste("NCC_streams_river-level_SR_data_", Sys.Date(),".csv", sep=""), row.names = FALSE)

