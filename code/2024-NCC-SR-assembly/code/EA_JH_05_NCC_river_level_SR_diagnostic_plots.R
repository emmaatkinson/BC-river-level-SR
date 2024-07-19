# Date created: 16-Mar-2023
# Last updated: 19-Apr-2023
# Author: Emma Atkinson
# Description: Central Coast river-level SR data compilation
# Notes: Code to compile river-level SR data for all species, updated for sub-contract work for Kitasoo-Xai'xais Stewardship Authority
#        Requires: stream-level escapement & conservation unit-level age table from PSF Salmon Watersheds Program.
#        Output: stream-level stock-recruitment (S-R) data for all species (where sufficient data were available)
#        *Note that output S-R data relies on any assumptions made in the compilation of the stream-level escapement 
#         and age-at-return data compilation. 
#
#         This script generates river-level diagnostic plots visualising spawner-recruitment time series and 
#         age distribution of recruitment cohorts

# --- Prepping environment --- #

rm(list=ls())
graphics.off()

library(here)
setwd(here("code","2024-NCC-SR-assembly","generated data"))

# Installing packages #
# install.packages("dpylr")
# install.packages("reshape2")
# install.packages("stringr")
# install.packages("readxl")
# install.packages("scales")

# Loading packages #
library(dplyr)
library(reshape2)
library(stringr)
library(readxl)
library(scales)
library(wesanderson)


col1 <- wes_palette("Zissou1")[1]
col2 <- wes_palette("Zissou1")[4]
col3 <- wes_palette("Zissou1")[5]

# --- Read in data --- #
setwd(here("code","2024-NCC-SR-assembly","generated data"))
dat = read.csv("NCC_streams_river-level_SR_data_2024-07-19.csv", header=TRUE, stringsAsFactors = FALSE)

# make a unique population ID for plotting
dat$pop = paste(dat$species_acronym_ncc, dat$GFE_ID, sep="-")

# set directory for where to save plots
setwd(here("code","2024-NCC-SR-assembly","JH_NCC_update_figures"))

# set test GFE_ID
#g = "CM-1010"

for (g in unique(dat$pop)){

    d = dat[dat$pop==g,]
    d = d[order(d$Year),]
    spp = d$species_acronym_ncc[1]

  if (FALSE %in% is.na(d$Spawners)){ # don't plot if there aren't any data
    
    pdf(paste(Sys.Date(), g, "_stream_level_diagnostics.pdf", sep="_"), width=11, height=5, pointsize=12)
    par(mfrow=c(1,2),mar=c(4,4,1,2), oma=c(0,0,4,0))
    
    
      # set ymax
      if (FALSE %in% is.na(d$Recruits)) {
        ymax = 1.5*max(d$Recruits, na.rm=TRUE)
      } else {
        ymax = 1.5*max(d$Spawners, na.rm=TRUE)
      }
    
      
      if (spp %in% c("CM","CO")){
        plot(d$Year, d$Spawners, type="l", lwd=2, col=alpha(col1, 0.75), bty="n", xlab="Brood year", ylab="Numbers of fish", ylim=c(0,ymax))
        points(d$Year, d$Spawners, pch=21, col=col1, bg=NA, cex=0.9)
        lines(d$Year, d$Recruits, type="l", lwd=3, col=alpha(col2, 0.75))
        points(d$Year, d$Recruits, pch=21, col=col2, bg=NA, cex=0.9)
        legend("topright", c("Spawners","Recruits"),
               col=c(col1,col2), lwd=c(2,2), pch=c(21,21),
               bty="n", cex=0.8)
      }
      
      if (spp == "PKE"){
        
        d = d[d$Year %% 2 == 0,]
        
        plot(d$Year, d$Spawners, type="l", lwd=2, col=alpha(col1, 0.75), bty="n", xlab="Brood year", ylab="Numbers of fish", ylim=c(0,ymax))
        points(d$Year, d$Spawners, pch=21, col=col1, bg=NA, cex=0.9)
        lines(d$Year, d$Recruits, type="l", lwd=3, col=alpha(col2, 0.75))
        points(d$Year, d$Recruits, pch=21, col=col2, bg=NA, cex=0.9)
        legend("topright", c("Spawners","Recruits"),
               col=c(col1,col2), lwd=c(2,2), pch=c(21,21),
               bty="n", cex=0.8)
        
      }
    
    if (spp == "PKO"){
      
      d = d[d$BroodYear %% 2 != 0,]
      
      plot(d$Year, d$Spawners, type="l", lwd=2, col=alpha(col1, 0.75), bty="n", xlab="Brood year", ylab="Numbers of fish", ylim=c(0,ymax))
      points(d$Year, d$Spawners, pch=21, col=col1, bg=NA, cex=0.9)
      lines(d$Year, d$Recruits, type="l", lwd=3, col=alpha(col2, 0.75))
      points(d$Year, d$Recruits, pch=21, col=col2, bg=NA, cex=0.9)
      legend("topright", c("Spawners","Recruits"),
             col=c(col1,col2), lwd=c(2,2), pch=c(21,21),
             bty="n", cex=0.8)
      
    }
        
        if (spp == "CM"){
              plot(d$Year, d$Recruits_Age3/d$Recruits, type="l", lwd=2, col=alpha(col1, 0.75), bty="n", xlab="Brood year", ylab="Proportion of recruits", ylim=c(0,1))
              
              lines(d$Year, d$Returns_Proportion_Age_3,type="l", lty=2, col=alpha(col1, 0.75))
              lines(d$Year, d$Returns_Proportion_Age_4,type="l", lty=2, col=alpha(col2, 0.75))
              lines(d$Year, d$Returns_Proportion_Age_5,type="l", lty=2, col=alpha(col3, 0.75))
              
              lines(d$Year, d$Recruits_Age4/d$Recruits, type="l", lwd=2, col=alpha(col2, 0.75))
              lines(d$Year, d$Recruits_Age5/d$Recruits, type="l", lwd=2, col=alpha(col3, 0.75))
              
              legend("topright", c("Age 3","Age 4","Age 5"),
                     col=c(col1,col2,col3), lwd=c(2,2,2),
                     bty="n", cex=0.8)
        }
        
        if (spp == "CO"){
          plot(d$Year, d$Recruits_Age3/d$Recruits, type="l", lwd=2, col=alpha(col1, 0.75), bty="n", xlab="Brood year", ylab="Proportion of recruits", ylim=c(0,1))
          lines(d$Year, d$Returns_Proportion_Age_3,type="l", lty=2, col=alpha(col1, 0.75))
          lines(d$Year, d$Returns_Proportion_Age_4,type="l", lty=2, col=alpha(col2, 0.75))
          lines(d$Year, d$Recruits_Age4/d$Recruits, type="l", lwd=2, col=alpha(col2, 0.75))
          legend("topright", c("Age 3","Age 4"),
                 col=c(col1,col2), lwd=c(2,2),
                 bty="n", cex=0.8)
        }
        
        if (spp %in% c("PKE","PKO")){
          plot(1,1, axes=FALSE, xlab="",ylab="",col="white") 
        }
  
      mtext(paste("Species:" , spp , "System:", d$WATERBODY[1],"CU:", d$CU_name[1], sep=" "), side=3, outer=TRUE, cex=1.2, lwd=2, line=2)
      dev.off()
    }
}
