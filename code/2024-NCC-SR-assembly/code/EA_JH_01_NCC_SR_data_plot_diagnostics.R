# Date created: 16-Mar-2023
# Last updated: 16-Mar-2023
# Author: Emma Atkinson
# Description: Central Coast river-level SR data compilation
# Notes: Code to compile river-level SR data for all species, updated for sub-contract work for Kitasoo-Xai'xais Stewardship Authority
#        Requires: stream-level escapement & conservation unit-level age table from PSF Salmon Watersheds Program.
#        Output: stream-level stock-recruitment (S-R) data for all species (where sufficient data were available)
#        *Note that output S-R data relies on any assumptions made in the compilation of the stream-level escapement 
#         and age-at-return data compilation. 

# --- Prepping environment --- #

rm(list=ls())
graphics.off()

library(here)
setwd(here("code","2024-NCC-SR-assembly","raw-data"))

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
trtc_full = read.csv("all_areas_nuseds_cleaned_20240419.csv", header=TRUE, stringsAsFactors = FALSE)
rivers=read.csv("conservation_unit_system_sites_cleaned_20240419.csv")

species_list<-c("CM","CO","PKE","PKO")
trtc = trtc_full[which(trtc_full$species_acronym_ncc %in% species_list),]
trtc = trtc[order(trtc$Year),]

setwd("/Users/jacobhoutman/Documents/Git Hub/BC-river-level-SR/code/2023-NCC-SR-assembly/raw-data")
dat_full = read.csv("agebyCU_infilled_2023-03-21.csv", header=TRUE, stringsAsFactors = FALSE)

species_list<-c("CM","CO","PKe","PKo")

dat = dat_full[which(dat_full$SpeciesId %in% species_list),]
dat$CU=gsub("_","-",dat$CU)

#the catch data is not organized by CU, We need to add a CU column to the NuSEDS catch data 
# so that we can ~

nrows<-nrow(trtc)
trtc$CU<-rep(0,nrows)
trtc$CU_name<-rep("A",nrows)
trtc[trtc$CU==cu,]$TOTAL_RETURN_TO_RIVER

#Error
unique(trtc$CU)
unique(dat$CU)
unique(dat$CU)[!unique(dat$CU)%in%unique(trtc$CU)]
unique(trtc$SPECIES)




for (i in 1:nrows){
  
  ind_id<-trtc$IndexId[i]
  
  current_CU<-unique(rivers[which(rivers$IndexId==ind_id),]$FULL_CU_IN)
  current_CU_name<-unique(rivers[which(rivers$IndexId==ind_id),]$CU_NAME)
  
  trtc$CU[i]<-current_CU
  trtc$CU_name[i]<-  current_CU_name
  if(i%%23735==0){print(rep("-",i/23735))}
  
}

trtc[which(is.na(trtc$TOTAL_RETURN_TO_RIVER)),]$TOTAL_RETURN_TO_RIVER <- rep(0,sum(is.na(trtc$TOTAL_RETURN_TO_RIVER)))
trtc[which(is.na(trtc$NATURAL_SPAWNERS_TOTAL)),]$NATURAL_SPAWNERS_TOTAL <- rep(0,sum(is.na(trtc$NATURAL_SPAWNERS_TOTAL)))

dat$CU<-gsub("PKe","PKE",dat$CU)
dat$CU<-gsub("PKo","PKO",dat$CU)

unique(trtc$CU)[which(!unique(trtc$CU)%in%unique(dat$CU))]
unique(dat$CU)[which(!unique(dat$CU)%in%unique(trtc$CU))]



# set directory for where to save plots
setwd(here("code","2024-NCC-SR-assembly","JH_NCC_update_figures"))

unique(dat$CU)[52]
for (cu in unique(dat$CU)){

# set example cu
cu = cu
cu_name = dat[dat$CU==cu,]$CU_Name
Ymax = max(dat[dat$CU==cu,]$BroodYear)
spp = dat[dat$CU==cu,]$SpeciesId[1]

#windows()
pdf(paste(Sys.Date(), cu, cu_name, "diagnostics.pdf", sep="_"), width=9, height=8, pointsize=12)
#pdf("test.pdf", width=9, height=8, pointsize=12)

par(mfrow=c(2,2),mar=c(4,4,1,2), oma=c(0,0,4,0))
trtc$Removals

# PLOT 1: total run through time
if(max(trtc[trtc$CU==cu,]$TOTAL_RETURN_TO_RIVER)>0){
plot(trtc[trtc$CU==cu,]$Year, trtc[trtc$CU==cu,]$TOTAL_RETURN_TO_RIVER, type="l", lwd=2, col=alpha(col1,0.75), bty="n", xlab="Brood year", ylab="Total run size", ylim=c(0,1.2*max(trtc[trtc$CU==cu,]$TOTAL_RETURN_TO_RIVER, na.rm=TRUE)))
points(trtc[trtc$CU==cu,]$Year, trtc[trtc$CU==cu,]$TOTAL_RETURN_TO_RIVER, pch=21, col=col1, bg=NA, cex=0.9)

lines(trtc[trtc$CU==cu,]$Year, trtc[trtc$CU==cu,]$NATURAL_SPAWNERS_TOTAL, lwd=2, col=alpha(col2, 0.75))
points(trtc[trtc$CU==cu,]$Year, trtc[trtc$CU==cu,]$NATURAL_SPAWNERS_TOTAL, pch=21, col=col2, bg=NA, cex=0.9)

lines(trtc[trtc$CU==cu,]$Year, trtc[trtc$CU==cu,]$TOTAL_RETURN_TO_RIVER-trtc[trtc$CU==cu,]$NATURAL_SPAWNERS_TOTAL, lwd=2, col=alpha(col3,0.75))
points(trtc[trtc$CU==cu,]$Year, trtc[trtc$CU==cu,]$TOTAL_RETURN_TO_RIVER-trtc[trtc$CU==cu,]$NATURAL_SPAWNERS_TOTAL, pch=21, col=col3, bg=NA, cex=0.9)

legend("topright", c("Total run size", "Total escapement", "Total harvest"),
       col=c(col1, col2, col3),
       lwd=c(2,2,2,2),
       bty="n",
       pch=c(21,21,21), cex=0.8)
}
# PLOT 2: age structure through time
if (spp=="CM"){
  
    plot(dat[dat$CU==cu,]$BroodYear, dat[dat$CU==cu,]$Age3, type="l", ylim=c(0,1), bty="n", lwd=2, xlab="Brood year", ylab="Proportion returning")
    lines(dat[dat$CU==cu,]$BroodYear, dat[dat$CU==cu,]$Age4, lty=2, lwd=2)
    lines(dat[dat$CU==cu,]$BroodYear, dat[dat$CU==cu,]$Age5, lty=3, lwd=2)
    lines(dat[dat$CU==cu,]$BroodYear, dat[dat$CU==cu,]$Age6, lty=4, lwd=2)
    legend("topright", c("Age 3","Age 4","Age 5","Age 6"),
                   lty=c(1,2,3,4), lwd=c(2,2,2,2),
                   bty="n", cex=0.8)
} 

if (spp=="CO"){
  
  plot(dat[dat$CU==cu,]$BroodYear, dat[dat$CU==cu,]$Age3, type="l", ylim=c(0,1), bty="n", lwd=2, xlab="Brood year", ylab="Proportion returning")
  lines(dat[dat$CU==cu,]$BroodYear, dat[dat$CU==cu,]$Age4, lty=2, lwd=2)
  lines(dat[dat$CU==cu,]$BroodYear, dat[dat$CU==cu,]$Age5, lty=3, lwd=2)
  legend("topright", c("Age 3","Age 4","Age 5"),
         lty=c(1,2,3), lwd=c(2,2,2),
         bty="n", cex=0.8)
} 

if (spp %in% c("PKe", "PKo")){
 plot(1,1, axes=FALSE, xlab="",ylab="",col="white") 
}

# PLOT 3: ER through time
plot(dat[dat$CU==cu,]$BroodYear, dat[dat$CU==cu,]$Total.ER, ylim=c(0,1), type="l", lwd=2, xlab="Brood year", ylab="Exploitation rate", bty="n")
points(dat[dat$CU==cu,]$BroodYear, dat[dat$CU==cu,]$Total.ER, pch=21, col="black", bg="grey", cex=0.9)

# PLOT 4: expansion factors through time
#ef_ymax = 1.2*max(max(trtc[trtc$CU==cu,]$ExpFactor1, na.rm=TRUE), max(trtc[trtc$CU==cu,]$ExpFactor2, na.rm=TRUE), max(trtc[trtc$CU==cu,]$ExpFactor3, na.rm=TRUE))

#plot(trtc[trtc$CU==cu,]$Year, trtc[trtc$CU==cu,]$ExpFactor1, col=col1, type="l", lwd=2, bty="n", xlab="Brood year", ylab="Expansion factor", ylim=c(0.95,ef_ymax))
#points(trtc[trtc$CU==cu,]$Year, trtc[trtc$CU==cu,]$ExpFactor1, pch=21, col=col1, bg=NA, cex=0.9)

#lines(trtc[trtc$CU==cu,]$Year, trtc[trtc$CU==cu,]$ExpFactor2, col=col2, type="l", lwd=2)
#points(trtc[trtc$CU==cu,]$Year, trtc[trtc$CU==cu,]$ExpFactor2, pch=21, col=col2, bg=NA, cex=0.9)

#lines(trtc[trtc$CU==cu,]$Year, trtc[trtc$CU==cu,]$ExpFactor3, col=col3, type="l", lwd=2)
#points(trtc[trtc$CU==cu,]$Year, trtc[trtc$CU==cu,]$ExpFactor3, pch=21, col=col3, bg=NA, cex=0.9)

#legend("topright", c("EF 1 (Unmonitored indicator streams)", "EF 2 (Unmonitored non-indicator streams)", "EF 3 (Observer efficency & never-monitored systems)"),
   #    col=c(col1, col2, col3),
   #    lwd=c(2,2,2,2),
    #   bty="n",
    #   pch=c(21,21,21), cex=0.8)

mtext(paste("Species:" , spp , "          CU Name:", dat[dat$CU==cu,]$CU_Name, sep=" "), side=3, outer=TRUE, cex=1.2, lwd=2, line=2)


dev.off()

}
warnings()

