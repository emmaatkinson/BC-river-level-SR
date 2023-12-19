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
data <- read.csv("updated_nuseds_2017.csv")
data <- filter(data, SPECIES=="Sockeye")
data$WATERBODY <- as.character(data$WATERBODY)

z <- data %>% 
  select(ANALYSIS_YR, AREA, WATERBODY, MAX_ESTIMATE) 

z <- z[,c(2,3,1,4)]
names(z) <- c("Area", "River", "Yr", "Escapement")

Yr<-rep(1950:2017, length(unique(z$River)))
River<-rep(unique(z$River), each=length(1950:2017))
Escapement<-numeric(length(Yr)); Area<-numeric(length(Yr)); L<-numeric(length(Yr))
for(i in 1:length(Yr)){
  L[i]<-length(z[z$Yr==Yr[i]&z$River==River[i],1])
  # If there is no escapement value, fill in NA
  if(L[i]==0) Escapement[i]<-NA 
  # If there is a single, unique value, use it!
  if(L[i]==1) Escapement[i]<-z$Escapement[z$Yr==Yr[i]&z$River==River[i]]
  # If there are more than one value, then take the sum (usually one is NA)
  if(L[i]==2) Escapement[i]<-sum(z$Escapement[z$Yr==Yr[i]&z$River==River[i]], na.rm=TRUE)
  Area[i]<-z$Area[z$River==River[i]][1]
}

z<-data.frame(Area, River=as.numeric(River), RiverName=River, Yr, Escapement)
z <- z[order(z$River),] # Ordering rivers alphabetically so that indexing is consistent

cat("Total number of rivers in nuSEDS data: ", length(unique(z$River)))

#-------------------------------------------------------------------------------
# 2) Calculate adjusted escapement using Pmax (Appendix A)
#-------------------------------------------------------------------------------
ny.spawn <- length(unique(data$Yr[coho$Yr>=1954&coho$Yr<=2017])) 

zz<-data.frame(AdjustedEsc=numeric(length(unique(Area))*length(1950:2017)))
zz$Area<-rep(unique(z$Area),each=length(1950:2017))
zz$Yr<-rep(c(1950:2017), length(unique(z$Area)))

a <- 1 # Setting area to Area 12 in loop

#for(a in 1:length(unique(z$Area))){ # For each area
z1<-subset(z, Area==unique(z$Area)[a]) #subset data

#choose index streams as those that have at least 20 years data
ind<-as.numeric(tapply(is.na(z1$Escapement), z1$River, sum)) 
index<-unique(z1$River)[which(ind<23)] # EA: Changed criteria to max 23 missing years for now

test <- filter(z1, River%in%index)
runIDs <- as.character(unique(test$RiverName)) # Plot spawners for these years

### PLOTTING ###

plot.prod = function(run){
  data = z[z$RiverName==run,]	
  data = data[!(is.na(data$Escapement)),]
  
  x.min = 1950#min(spawn.data$ANALYSIS_YR)#spawners
#river.equiv = data.frame(NUSEDS = use.rivers, SR = use.SR.rivers)
spawn.data = data

trans = identity#sqrt# log #transformation to apply to data
inv.trans = identity#function(x){x^2}
scaling = 1000
plot.scaling = 1#0.5

y.max = max(spawn.data$Escapement, na.rm = TRUE) #max(spawn.data$MAX_ESTIMATE, na.rm = TRUE)#
y.max = trans(y.max)
y.max = 1.1*max(y.max,10000)/scaling

plot(spawn.data$Yr ,spawn.data$Escapement-100000000, 
     xlim=c(x.min,2020), 
     ylim=c(0,y.max),
     xlab='Spawn year', ylab='Spawners (thousands)', 
     #main='spawning adults', 
     axes=FALSE)
axis(1, at=seq(1905,2020,5), labels=paste('\'',c('05',as.character(seq(10,95,5)),'00','05','10','15','20'),sep=''))#, las=2, cex.axis=0.8)
axis(2); #box()
to.plot = rep(NA,(max(spawn.data$Yr)-min(spawn.data$Yr)+1))
to.plot[spawn.data$Yr-min(spawn.data$Yr)+1] = spawn.data$Escapement
to.plot.years = seq(min(spawn.data$Yr),max(spawn.data$Yr))


lines(to.plot.years, to.plot/scaling, lwd=2*plot.scaling)
points(to.plot.years, to.plot/scaling, pch=21, col="black", bg="grey", cex=0.6)

text(1950, y.max, 
     run, 
     adj=c(0,1), cex=0.9)

for(i in unique(spawn.data$Yr)) {
  if(!(i-1)%in%spawn.data$Yr & !(i+1)%in%spawn.data$Yr) {
    yr = which(sort(unique(spawn.data$Yr))==i)
    points(i, spawn.data$Escapement[yr]/scaling, pch=21,  col="black", bg="grey", cex=0.6)
  }
}
box()

}

setwd(dir.plot)

for(i in as.character(runIDs)) {
  #windows(9,3.5)
  #print(i)
  pdf(paste(i,'_sockeye.pdf',sep=''),5,4)
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
dat <- z1

names(dat)[3] <- 'Population'
names(dat)[4] <- 'Brood_Y'
names(dat)[5] <- 'Spawners'
dat$Population <- as.character(dat$Population)

dat <- filter(dat, River %in% index)

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
  for (i in seq(1,18,1)){
    year = 1999+i
      status.table[status.table$Population==pop,i+1] = statusdat[statusdat$Brood_Y==year,]$Spawners
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

# Write status table to file #
setwd(dir.plot)
write.csv(status.table, file = "1_sockeye_status_table.csv")










