# Date: 11-Jul-2018
# Last updated: 08-Sept-2018
# Name: Emma Atkinson
# Description: Sockeye salmon stock-recruit data compilation.
# Notes: Data compilation (sockeye) adapted from Peacock et al. 2013
#        for use preparing data for State of the Salmon analyses
#        for Salmon Coast Field Station. Data updated through 2017 
#        from Pieter Van Will (DFO Area 12 manager).

# ### Install packages ###
# install.packages("dplyr")
# install.packages("scales")

### Loading packages ###
library(dplyr)
library(scales)

### INPUT ###
rm(list=ls())
graphics.off()

dir.code <- "C:/Users/Emma Atkinson/Desktop/Research/SoS/Analysis"
dir.gen <- "C:/Users/Emma Atkinson/Desktop/Research/SoS/Analysis/Data-generated"
dir.raw <- "C:/Users/Emma Atkinson/Desktop/Research/SoS/Analysis/Data-raw"
dir.plot <- "C:/Users/Emma Atkinson/Desktop/Research/SoS/Analysis/Plots/Sockeye"

setwd(dir.gen)

### Prepping data for use with Steph's code ###
nuseds <- read.csv("updated_nuseds_2017.csv")
data <- filter(nuseds, SPECIES=="Sockeye")


# --- Filtering rivers --- #
sockeye <- data %>% 
  select(ANALYSIS_YR, AREA, WATERBODY, MAX_ESTIMATE, GFE_ID) 

sockeye <- sockeye[,c(1,2,3,4,5)]
names(sockeye) <- c("Yr","Area","River", "Spawners","GFE_ID")

### Filtering sockeye data ###

# Some rivers have more than one site (if they have more than one chum run) 
# so first thing to do is assign a unique pop'n ID to each RIVER, not each run. 
# Also add GFE ID from nuSEDS database so there is a common index to work with data from multiple species together #
sockeye$streamID <- NA
sockeye$popID <- NA

sockeye$River <- factor(sockeye$River, levels=levels(nuseds$WATERBODY)) # Change levels of river column to reflect those of nuseds dataframe

for (i in 1:length(unique(sockeye$River))){
  sockeye$streamID[sockeye$River==unique(sockeye$River)[i]] <- unique(nuseds$GFE_ID[nuseds$WATERBODY==unique(sockeye$River)[i]])
  sockeye$popID[sockeye$River==unique(sockeye$River)[i]] <- i
}

# How many years does that data currently span? # 
range(sockeye$Yr) # Actually start in 1929, but will stick with 1954 for consistency

# Because data are very sparse before 1954, we won't include 1950-54 in total # of years
# Alternative - include all years #
ny.spawn <- length(unique(sockeye$Yr[sockeye$Yr>=1954&sockeye$Yr<=2017])) 

# Given criteria for population to be included in analysis, which ones to keep? #
# Min.N.spawn <- round((1/2)*ny.spawn) # Given total no. of years with data, set minimum req'd no. of S-R pairs to 1/2. 
Min.N.spawn <- 15

# Now filter for escapement criteria #
Z.lengths<-c(); Z.max.spawn <- c(); L<-unique(sockeye$popID);
for(i in 1:length(L)){
  z1<-subset(sockeye, popID==L[i]);
  z2<-which(!is.na(z1$Spawners));
  #Z.lengths[i]<-length(z1$Spawners)-length(z2);
  Z.lengths[i]<-length(z2);
  if (Z.lengths[i] > 0){ Z.max.spawn[i] <- max(z1$Spawners, na.rm=TRUE) }
}

R.LongEnough<-which(Z.lengths>=Min.N.spawn)
R.BigEnough<- which(Z.max.spawn>=100)
R.overall <- R.LongEnough[R.LongEnough %in% R.BigEnough]

R.River<-c(); R.Area<-c();
for(i in 1:length(R.overall)){
  z1<-subset(sockeye, popID==R.overall[i])
  R.Area<-c(R.Area,z1$Area[1])
  R.River<-c(R.River,z1$popID[1])
}

# Which rivers met criteria? #
sockeye.rivers <- unique(sockeye$River[sockeye$popID%in%R.River])
as.character(sockeye.rivers)
runIDs <- as.character(sockeye.rivers)

# Clean up data for years where there are multiple entries (when multiple entries, sum, because usually this means there were two runs) #
for (run in runIDs){
  years <- unique(sockeye[sockeye$River==run,]$Yr)
  for (i in years){
    l <- length(sockeye[sockeye$River==run & sockeye$Yr==i,]$Yr)
    if (l > 1){
      r <- which(sockeye$River==run & sockeye$Yr==i)
      sockeye[r[1],]$Spawners = sum(sockeye[r,]$Spawners, na.rm=T)
      sockeye = sockeye[-c(r[2:length(r)]),]
    }
    
  }
}

### PLOTTING ###

plot.prod = function(run){
  data = sockeye[sockeye$River==run,]	
  data = data[!(is.na(data$Spawners)),]
  
  x.min = 1950
  spawn.data = data
  
  trans = identity#sqrt# log #transformation to apply to data
  inv.trans = identity#function(x){x^2}
  scaling = 1000
  plot.scaling = 1#0.5
  
  y.max = max(spawn.data$Spawners, na.rm = TRUE) #max(spawn.data$MAX_ESTIMATE, na.rm = TRUE)#
  y.max = trans(y.max)
  y.max = 1.2*max(y.max,10000)/scaling
  
  plot(spawn.data$Yr ,spawn.data$Spawners-100000000, 
       xlim=c(x.min,2020), 
       ylim=c(0,y.max),
       xlab='Spawn year', ylab='Spawners (thousands)', 
       #main='spawning adults', 
       axes=FALSE)
  axis(1, at=seq(1905,2020,5), labels=paste('\'',c('05',as.character(seq(10,95,5)),'00','05','10','15','20'),sep=''))#, las=2, cex.axis=0.8)
  axis(2, las=2); #box()
  to.plot = rep(NA,(max(spawn.data$Yr)-min(spawn.data$Yr)+1))
  to.plot[spawn.data$Yr-min(spawn.data$Yr)+1] = spawn.data$Spawners
  to.plot.years = seq(min(spawn.data$Yr),max(spawn.data$Yr))
  
  
  lines(to.plot.years, to.plot/scaling, lwd=2*plot.scaling)
  points(to.plot.years, to.plot/scaling, pch=21, col="black", bg="grey", cex=0.6)
  
  text(1950, y.max, 
       run, 
       adj=c(0,1), cex=0.9)
  
  for(i in unique(spawn.data$Yr)) {
    if(!(i-1)%in%spawn.data$Yr & !(i+1)%in%spawn.data$Yr) {
      yr = which(sort(unique(spawn.data$Yr))==i)
      points(i, spawn.data$Spawners[yr]/scaling, pch=21,  col="black", bg="grey", cex=0.6)
    }
  }
  box()
  
}

setwd(dir.plot)

for(i in as.character(runIDs)) {
  #windows(9,3.5)
  #print(i)
  pdf(paste(i,'_sockeye_Feb-2019.pdf',sep=''),5,5)
  plot.prod(i)
  dev.off()
}

########## STEP 2: Classifying population status  ##########

# --- Filter which populations are data deficient with respect to status assessment --- #
# --- Can try out a few different cut-off marks: For now using 2006 as a cut-off    --- #

# Source functions for filling in status table #
setwd(dir.code)
source("10_status_assessment_functions.R")

# Set up dataframe for data information #
dat <- sockeye

names(dat)[3] <- 'Population'
names(dat)[1] <- 'Brood_Y'
names(dat)[4] <- 'Spawners'
dat$Population <- as.character(dat$Population)

dat <- filter(dat, Population %in% runIDs)

length <- length(unique(dat$Population))

status.table <- data.frame(Population = c(unique(dat$Population)),
                           year2000 = rep(NA, length),
                           year2001 = rep(NA, length),
                           year2002 = rep(NA, length),
                           year2003 = rep(NA, length),
                           year2004 = rep(NA, length),
                           year2005 = rep(NA, length),
                           year2006 = rep(NA, length),
                           year2007 = rep(NA, length),
                           year2008 = rep(NA, length),
                           year2009 = rep(NA, length),
                           year2010 = rep(NA, length),
                           year2011 = rep(NA, length),
                           year2012 = rep(NA, length),
                           year2013 = rep(NA, length),
                           year2014 = rep(NA, length),
                           year2015 = rep(NA, length),
                           year2016 = rep(NA, length),
                           year2017 = rep(NA, length),
                           S_25 = rep(NA, length),
                           S_75 = rep(NA, length),
                           S_avg_year = rep(NA, length),
                           S_avg = rep(NA, length),
                           historic_avg = rep(NA, length),
                           status_spawn = rep(NA, length))

# Fill in data status table #
# pop <- unique(dat$Population)[1]
# i <- 2

for (pop in unique(dat$Population)){
  statusdat <- filter(dat, Population==pop)
  for (year in unique(statusdat[statusdat$Brood_Y >= 2000 & !is.na(statusdat$Spawners),]$Brood_Y)) {
    status.table[status.table$Population==pop, year-1998] = statusdat[statusdat$Brood_Y==year & !is.na(statusdat$Spawners),]$Spawners
  }
}

# Calculate spawner abundance based benchmarks (percentile approach) #
for (pop in unique(dat$Population)){
  data <- filter(dat, Population==pop)
  status.table[status.table$Population==pop,]$S_25 = quantile(data$Spawners, 0.25, na.rm=TRUE)
  status.table[status.table$Population==pop,]$S_75 = quantile(data$Spawners, 0.75, na.rm=TRUE)
}

# Calculate geometric average spawner abundance historically and for most recent generation #
for (pop in unique(dat$Population)){
  status.table[status.table$Population==pop,]$S_avg = sockeye_Savg(pop)[3]
  status.table[status.table$Population==pop,]$S_avg_year = sockeye_Savg(pop)[1]
  status.table[status.table$Population==pop,]$historic_avg = sockeye_Savg(pop)[2]
}

# Assess status based on two metrics/three benchmarks #
spawn.assess <- as.character(status.table[status.table$S_avg_year >= 2007,]$Population)

# Abundance status (percentile/SR) #
for (pop in unique(dat$Population)) {
  if (!(pop %in% spawn.assess)) { status.table[status.table$Population==pop,]$status_spawn = 4 }
  
  else if (pop %in% spawn.assess) {
    # Percentile benchmarks #
    s.est <- status.table[status.table$Population==pop,]$S_avg
    s.25 <- status.table[status.table$Population==pop,]$S_25
    s.75 <- status.table[status.table$Population==pop,]$S_75
    if (s.est < s.25) { status.table[status.table$Population==pop,]$status_spawn = 1 }
    if (s.est >= s.25 & s.est < s.75) {status.table[status.table$Population==pop,]$status_spawn = 2 }
    if (s.est >= s.75) {status.table[status.table$Population==pop,]$status_spawn = 3 }
    
  }
}

# Write status table to file #
setwd(dir.plot)
write.csv(status.table, file = "1_sockeye_status_table_Feb-2019.csv", row.names=F)










