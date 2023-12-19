# Date: 11-Jul-2018
# Last updated: 23-Oct-2018
# Name: Emma Atkinson
# Description: Coho salmon stock-recruit data compilation.
# Notes: Data compilation (coho) adapted from Peacock et al. 2013
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

dir.gen <- "C:/Users/Emma Atkinson/Desktop/Research/SoS/Analysis/Data-generated"
dir.raw <- "C:/Users/Emma Atkinson/Desktop/Research/SoS/Analysis/Data-raw"
dir.plot <- "C:/Users/Emma Atkinson/Desktop/Research/SoS/Analysis/Plots"

setwd(dir.gen)

### Prepping data for use with Steph's code ###
data <- read.csv("updated_nuseds_2017.csv")
data <- filter(data, SPECIES=="Coho")

z <- data %>% 
  select(ANALYSIS_YR, AREA, WATERBODY, MAX_ESTIMATE) 

z <- z[,c(2,3,1,4)]
names(z) <- c("Area", "River", "Yr", "Escapement")

################################################################################
## DATA COMPILATION
################################################################################

################################################################################
## A. Calculation of exploitation rates using Pmax technique
################################################################################

#-------------------------------------------------------------------------------
# 1) Read in data
#-------------------------------------------------------------------------------

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

zz<-data.frame(AdjustedEsc=numeric(length(unique(Area))*length(1950:2017)))
zz$Area<-rep(unique(z$Area),each=length(1950:2017))
zz$Yr<-rep(c(1950:2017), length(unique(z$Area)))

a <- 1 # Setting area to Area 12 

#for(a in 1:length(unique(z$Area))){ # For each area, in case you are looping through multiple areas
	z1<-subset(z, Area==unique(z$Area)[a]) #subset data
	
	# choose index streams as those that have at least 20 years data
	ind<-as.numeric(tapply(is.na(z1$Escapement), z1$River, sum)) 
	index<-unique(z1$River)[which(ind<26)] # EA: Changed criteria to max 26 missing years (~1/3 of total years = 23 + 3 years b/c no rivers have data for 1950-52)
	
	test <- filter(z1, River%in%index)
	unique(test$RiverName) # Printing index river names to make sure there aren't any weird ones
	
	# The observed sum is the sum of escapement for index streams each year
	obs.sum<-as.numeric(tapply(z1$Escapement[is.element(z1$River, index)], z1$Yr[is.element(z1$River, index)], sum, na.rm=TRUE))
	
	# The decade mean escapement for each stream 
	decade.mean.esc<-matrix(nrow=length(unique(z1$River)), ncol=8)
	decade.mean.esc[,1]<-as.numeric(tapply(z1$Escapement[z1$Yr<1960], z1$River[z1$Yr<1960], mean, na.rm=TRUE))
	decade.mean.esc[,2]<-as.numeric(tapply(z1$Escapement[z1$Yr>=1960&z1$Yr<1970], z1$River[z1$Yr>=1960&z1$Yr<1970], mean, na.rm=TRUE))
	decade.mean.esc[,3]<-as.numeric(tapply(z1$Escapement[z1$Yr>=1970&z1$Yr<1980], z1$River[z1$Yr>=1970&z1$Yr<1980], mean, na.rm=TRUE))
	decade.mean.esc[,4]<-as.numeric(tapply(z1$Escapement[z1$Yr>=1980&z1$Yr<1990], z1$River[z1$Yr>=1980&z1$Yr<1990], mean, na.rm=TRUE))
	decade.mean.esc[,5]<-as.numeric(tapply(z1$Escapement[z1$Yr>=1990&z1$Yr<2000], z1$River[z1$Yr>=1990&z1$Yr<2000], mean, na.rm=TRUE))
	decade.mean.esc[,6]<-as.numeric(tapply(z1$Escapement[z1$Yr>=2000&z1$Yr<2010], z1$River[z1$Yr>=2000&z1$Yr<2010], mean, na.rm=TRUE))
	decade.mean.esc[,7]<-as.numeric(tapply(z1$Escapement[z1$Yr>=2010], z1$River[z1$Yr>=2010], mean, na.rm=TRUE))
	decade.mean.esc[,8]<-as.numeric(tapply(z1$Escapement, z1$River, mean, na.rm=TRUE))
	
	dimnames(decade.mean.esc)<-list(unique(z1$River), c("1950s", "1960s", "1970s", "1980s", "1990s", "2000s","2010s", "all"))
	
	# Calculate the sum of escapement for each decade, from index streams only
	if(length(index)==1) decade.sum<-decade.mean.esc[which(is.element(unique(z1$River),index)),]
	if(length(index)>1) decade.sum<-apply(decade.mean.esc[which(is.element(unique(z1$River),index)),], 2, sum, na.rm=TRUE)
	
	#Porportion of decade.sum that each stream contributed, for each decade
	prop.obs.total<-matrix(nrow=length(index), ncol=7)
	for(i in 1:7){prop.obs.total[,i]<-decade.mean.esc[which(is.element(unique(z1$River),index)),i]/decade.sum[i]}
	apply(prop.obs.total, 2, sum, na.rm=TRUE) 
	
	#Calculate annual escapement contribution from each index stream
	esc.contribution<-matrix(nrow=length(index), ncol=length(1950:2017))
	for(i in 1:length(index)){
		for(y in 1:length(1950:2017)){
			if(sum(z1$Escapement[which(z1$River==index[i]&z1$Yr==c(1950:2017)[y])], na.rm=TRUE)==0){esc.contribution[i,y]<-0}else{
				if(y<=10) esc.contribution[i,y]<-prop.obs.total[i,1]
				if(y>10&y<=20) esc.contribution[i,y]<-prop.obs.total[i,2]
				if(y>20&y<=30) esc.contribution[i,y]<-prop.obs.total[i,3]
				if(y>30&y<=40) esc.contribution[i,y]<-prop.obs.total[i,4]
				if(y>40&y<=50) esc.contribution[i,y]<-prop.obs.total[i,5]
				if(y>50&y<=60) esc.contribution[i,y]<-prop.obs.total[i,6] 
				if(y>60) esc.contribution[i,y]<-prop.obs.total[i,7]
				}
			}
		}
	
	# Determine expansion factor: how much of annual escapement for index streams is NA (i.e., infilling of index streams)
	expansion<-apply(esc.contribution, 2, sum)
	
	# Adjusted total escapement of the area for the year
	adj.sum<-obs.sum/expansion
	
	plot(1950:2017, obs.sum, "l", ylab="Escapement", xlab="Year", bty="n", main=paste("Area ", unique(z$Area)[a]), xlim=c(1950,2020), ylim=c(0,510000))
	lines(1950:2017, adj.sum, col=2)
	
	# Analysis of 1980s baseline period for index streams contribution
	if(length(index)>1) proportion<-as.numeric(apply(decade.mean.esc[which(is.element(unique(z1$River),index)),], 2, sum, na.rm=TRUE)/apply(decade.mean.esc, 2, sum, na.rm=TRUE)) else proportion<-decade.mean.esc[which(is.element(unique(z1$River),index)),]/apply(decade.mean.esc, 2, sum, na.rm=TRUE)
	
	proportion<-c(rep(proportion[1:7], each=10), proportion[7])
	proportion <- proportion[-c(68, 69, 70)] # Last decade only goes to 2017 so removing extra three years
	
	# Observed sum of all streams (not just index) in area (i.e., infilling of non-index streams)
	obs.sum.all<-adj.sum/proportion
	lines(1950:2017, obs.sum.all, col=3)
	
	# Brian S. multiplies the observed sum by a 1.5 expansion factor, presumably for the streams that are not monitored at all.  It would be good to see how sensitive the results are to this.
	adj.sum.all<-obs.sum.all*1.5
	lines(1950:2017, adj.sum.all, col=4)

	
	zz$AdjustedEsc[zz$Area==unique(zz$Area)[a]]<-adj.sum.all # Adjusted escapement is the total adjusted escapement per year for all of Area 12 (adjusting for streams that aren't monitored or have missing years, etc)
	
	#} #end area

#-------------------------------------------------------------------------------
# 3) Calculate exploitation rates - note: major edits to SP code by EA
#-------------------------------------------------------------------------------
setwd(dir.raw)

# Reading in coho Area 12 comm'l catch data obtained from P. VanWill in June 2018
x<-read.csv("Coho_commercial_catch_1983-2017.csv") 
a <- 12

### Calculating harvest rate estimates using raw catch data and adjusted escapement from above ###

zz$Catch<-numeric(dim(zz)[1])
#for(a in unique(zz$Area)){
	for(j in 1983:2017){ # Filling in catch data for 1983-2017
		if(length(x$Catch[x$Area==a&x$Year==j])>0){
			zz$Catch[zz$Area==a&zz$Yr==j]<-x$Catch[x$Area==a&x$Year==j] 
			}else{
			zz$Catch[zz$Area==a&zz$Yr==j]<-NA
			}
		}
#	}
zz$Exploitation<-zz$Catch/(zz$AdjustedEsc+zz$Catch) # Harvest rate = Catch/(Catch + Adjusted Escapement)

#-------------------------------------------------------------------------------
# 4) Substitute DFO estimates for Area 12 (includes estimates of fish caught 
#    that were returning to more southern rivers (e.g., the Fraser River))
#-------------------------------------------------------------------------------

# EA: Here I assume that all catch in Area 12 were Area 12 coho and not southern coho. 
# In part, that is because there were no adjusted exploitation rates available, and
# in part that is because the catch for 2000-17 is very low and so do not hugely influence
# the escapement estimates. 

# Plot exploitation rate estimates #
setwd(dir.plot)

png(filename="coho_exploitation rates_1983-17.png", width=2000, height= 2000, res=300, units="px", pointsize=12)
plot(1,1, "n", ylim=c(0,1), xlim=c(1980,2020), bty="n", ylab="harvest rate", xlab="return year",main="Area 12 coho exploitation rates", las=1)
lines(zz[zz$Yr>=1983,]$Yr, zz[zz$Yr>=1983,]$Exploitation, col=alpha("black", 0.5), lwd=1.5)
legend(2000, 1, col=alpha("black", 0.5), lty=1, "Area 12 calculated exploitation", bg="white", cex=0.7)

dev.off()

# Plot catch estimates #
setwd(dir.plot)
png(filename="coho_area_12_catch.png", width=2000, height=2000, res=300, units="px", pointsize=12)
plot(1,1, "n", ylim=c(0,50000), xlim=c(1980,2020), bty="n", yaxt="n",ylab="catch (thousands of fish)", xlab="return year", main="Area 12 coho catch", las=1)
lines(zz[!is.na(zz$Exploitation)&zz$Yr>=1983,]$Yr, zz[!is.na(zz$Exploitation)&zz$Yr>=1983,]$Exploitation*zz[!is.na(zz$Exploitation)&zz$Yr>=1983,]$AdjustedEsc, col=alpha("black", 0.5), lwd=1.5)
axis(2, at=seq(0,50000,10000), lab=seq(0,50,10), las=1)
legend(2000, 49500, col=alpha("black", 0.5), lty=1, "Area 12 calculated catch", bg="white", cex=0.7)

dev.off()

#-------------------------------------------------------------------------------
# 1) Read in data
#-------------------------------------------------------------------------------

# Read in previous coho S-R pairs (from Brendan Connors' 2010 paper)
# Because catch data were not received for pre-2000, we will use these S-R data
# for 1975-1999 period and will check overlapping years (2000-2009) to make
# sure estimates are simlar. These data are currently indexed by return year 
# so will need to re-index by brood year in final dataset. 

setwd(dir.raw)
coho <- read.csv("coho.csv")
coho <- filter(coho, Area==12)

setwd(dir.gen)
data <- read.csv("updated_nuseds_2017.csv")
data <- filter(data, SPECIES=="Coho")

z <- data %>% 
  select(ANALYSIS_YR, AREA, WATERBODY, MAX_ESTIMATE) 

z <- z[,c(2,3,1,4)]
names(z) <- c("Area", "River", "Yr", "Escapement")

x<-zz # Exploitation data #

# Match river names in both data frames #
# First create references for river names from each dataframe #
fullrivers <- data.frame(riv=sort(unique(z$River)))
fullrivers$riv <- as.character(paste(fullrivers$riv))
partrivers <- data.frame(riv2=sort(unique(coho$River)))
partrivers$riv2 <- as.character(paste(partrivers$riv2))

temp <- sapply(partrivers$riv2, agrep, fullrivers$riv) # Some of the river names are mis-spelt so have to use 'agrep' for "fuzzy" matching 
temp <- unlist(temp) # 'agrep' produces a list, so unlist it first
temp[c(4,5,6,24)] # because of fuzzy matching, some matches are wrong so need to check 
temp <- temp[-c(4,5,6,24)] # removing wrong matches

# Now create dataframe to index different versions of river names
riverindex <- data.frame(short=partrivers$riv2,
                         number=rep(NA,length(partrivers$riv2)),
                         full=rep(NA,length(partrivers$riv2))) 

for (i in 1:length(temp)){ riverindex$number[i]=temp[[i]] }
for (i in unique(riverindex$number)){ riverindex$full[riverindex$number==i] = fullrivers$riv[i] }

# Add full river names to coho dataframe #
coho$FullRiver <- NA

for (river in unique(coho$River)) { coho$FullRiver[coho$River==river] = riverindex$full[riverindex$short==river] }

#-------------------------------------------------------------------------------
# 2) Set up the overall S-R database 1954-2017, all Rivers
#-------------------------------------------------------------------------------

# a) Enter a row of NA for each missing year
L<-levels(z$River); 
yr1<-c(); area1<-c(); river1<-c(); 

for(i in 1:length(L)){
	river<-L[i]; zz<-subset(z,River==river); area<-zz$Area[1];
	for(t in 1950:2017){yr1<-c(yr1,t); area1<-c(area1,area); river1<-c(river1,river)};
	}
	
# b) Create dataframe
Z<-data.frame(Yr=yr1, Area=area1, River=river1); Z$Escapement<-NA; Z$Returns <- NA; Z$Returns_NEW <- NA; Z$Returns_FINAL <- NA; Z$Recruits <- NA; Z$Recruits_NEW <- NA; Z$Recruits_FINAL <- NA; Z$Expl<-NA; Z$Expl_BC <- NA; Z$Catch_BC <- NA

# c) Add exploitation rates to dataframe
for(a in unique(Z$Area)){
	for(y in 1983:2017){
		Z$Expl[which(Z$Yr==y & Z$Area==a)]<-x$Exploitation[which(x$Yr==y & x$Area==a)]
	}
}

# d) Add the nuSEDs escapement values (takes a minute)
for(i in 1:length(Z$Yr)){
  E<-z$Escapement[which(z$Yr==Z$Yr[i] & z$River==Z$River[i])]; 
  if(length(E)==1) Z$Escapement[i]<-E;
  if(length(E)>1) E<-E[which(E>0)]; 
  if(length(E)!=0) Z$Escapement[i]<-max(E); # Sometimes there is more than one estimate greater than 0 so then I pick the max (rarely more of a difference than a couple hundred)
}

# e) Replace 0s with NAs
Z$Escapement[which(Z$Escapement==0)]<-NA;

# f) Returns estimates Returns = N/(1-u)
#    For 1975-1999, add returns from Brendan's data where Returns for 
#    year t will be the Recruits from year t in Brendan's data.
for (river in unique(coho$FullRiver)){
  years <- unique(coho$Return_YR[coho$FullRiver==river])
  for(i in years){
    Z$Returns[Z$River==river & Z$Yr==i] = coho$Recruits[coho$FullRiver==river & coho$Return_YR==i]
    Z$Recruits[Z$River==river & Z$Yr==i-3] = coho$Recruits[coho$FullRiver==river & coho$Return_YR==i]
    } 
}

for (river in unique(Z$River)) {
  for (i in 1983:2017){
    Z$Returns_NEW[Z$River==river & Z$Yr==i] = Z$Escapement[Z$River==river & Z$Yr==i]/(1-Z$Expl[Z$River==river & Z$Yr==i])
    Z$Recruits_NEW[Z$River==river & Z$Yr==i-3] = Z$Returns_NEW[Z$River==river & Z$Yr==i]
  }
}

# g) Back-calculate exploitation rates from Brendan's S-R estimates
for (river in unique(Z$River)) {
  for (i in 1975:2006){
    Z$Expl_BC[Z$River==river & Z$Yr==i] = (Z$Returns[Z$River==river & Z$Yr==i]-Z$Escapement[Z$River==river & Z$Yr==i])/(Z$Returns[Z$River==river & Z$Yr==i])
    Z$Catch_BC[Z$River==river & Z$Yr==i] = Z$Expl_BC[Z$River==river & Z$Yr==i]*Z$Escapement[Z$River==river & Z$Yr==i]
  }
}

# Assembling back-calculated exploitation rates from Brendan's S-R pairs #
brendan_expl <- data.frame(Yr=rep(c(1975:2006), each=length(unique(coho$FullRiver))),
                           River=rep(unique(coho$FullRiver),length(c(1975:2006))),
                           Expl_BC=rep(NA, length(c(1975:2006))*length(unique(coho$FullRiver))),
                           Catch_BC=rep(NA, length(c(1975:2006))*length(unique(coho$FullRiver))))

for (river in unique(brendan_expl$River)) {
  for (i in 1975:2006){
    brendan_expl$Expl_BC[brendan_expl$River==river & brendan_expl$Yr==i] = Z$Expl_BC[Z$River==river & Z$Yr==i]
    brendan_expl$Catch_BC[brendan_expl$River==river & brendan_expl$Yr==i] = Z$Catch_BC[Z$River==river & Z$Yr==i] 
  }
}

# Summarizing back-calculated catch on an annual basis 
annual_catch <- data.frame(Yr=c(1975:2006),
                           Catch_BC=rep(NA, length(1975:2006)),
                           Expl_BC=rep(NA, length(1975:2006)))
for (i in 1975:2006){
  annual_catch[annual_catch$Yr==i,]$Catch_BC <- sum(brendan_expl[brendan_expl$Yr==i,]$Catch_BC, na.rm=TRUE)
  annual_catch[annual_catch$Yr==i,]$Expl_BC <- mean(brendan_expl[brendan_expl$Yr==i,]$Expl_BC, na.rm=TRUE)
}
  
# Plotting catch and exploitation rates for mine and Brendan's data
plot(x[x$Yr>1982,]$Yr, x[x$Yr>1982,]$Catch, pch=20, col=alpha("black", 0.6))
points(annual_catch$Yr, annual_catch$Catch_BC, pch=2, col=alpha("blue", 0.6))
legend("topright", col=c(alpha("black", 0.6), alpha("blue", 0.6)), pch=c(20,20), c("Actual catch in area 12 from Pieter", "Back-calculated catch based on Brendan's CWT data"))

# Plotting catch and exploitation rates for mine and Brendan's data
plot(x[x$Yr>1982,]$Yr, x[x$Yr>1982,]$Exploitation, pch=20, col=alpha("black", 0.6), ylim=c(0,1.2))
points(brendan_expl$Yr, brendan_expl$Expl_BC, pch=2, col=alpha("blue", 0.6))
points(annual_catch$Yr, annual_catch$Expl_BC, pch=2, col="red")
legend("topright", col=c(alpha("black", 0.6), alpha("blue", 0.6)), pch=c(20,2), c("Calculated exploitation from Pmax", "Back-calculated exploitation from Brendan's CWT data"))

# Plot Emma vs. Brendan
plot(log(x[x$Yr%in%1983:2006,]$Exploitation), log(annual_catch[annual_catch$Yr%in%1983:2006,]$Expl_BC))
abline(0,1)

plot(coho[coho$FullRiver=="KWALATE RIVER",]$Spawners, coho[coho$FullRiver=="KWALATE RIVER",]$Recruits, pch=20, col=alpha("black", 0.8), ylim=c(0,6000), xlim=c(0,6000))
points(Z[Z$River=="KAKWEIKEN RIVER",]$Escapement, Z[Z$River=="KAKWEIKEN RIVER",]$Recruits_NEW, pch=20, col=alpha("red", 0.4), ylim=c(0,6000), xlim=c(0,6000))

# For over-lapping years, new estimates are consistently slightly lower than Brendan's #
# On average, what is the difference? #
diff.dat <- Z[!is.na(Z$Returns) & !is.na(Z$Returns_NEW),]
diff.dat <- filter(diff.dat, Yr > 1999)

diff <- NA

for (i in 1:length(diff.dat$Yr)) {
    diff[i] <- diff.dat$Returns_NEW[i]/diff.dat$Returns[i]
}

mean.diff <- mean(diff) # On average, new estimate is about 96% of previous estimate 

plot(diff$year, diff$diff, ylim=c(0,3), pch=20)

# Picking final recruits/returns data  - if new estimates available, I used these divided by correction factor from above #
# Option 1 - Use Brendan's data for 1975-1999 (leaving gaps) and Emma's for 2000-2017.
for (river in unique(Z$River)) {
  for (i in 1950:1999) {
      Z$Returns_FINAL[Z$River==river & Z$Yr==i] = Z$Returns[Z$River==river & Z$Yr==i]
      Z$Recruits_FINAL[Z$River==river & Z$Yr==i-3] = Z$Returns[Z$River==river & Z$Yr==i]
    }
}

for (river in unique(Z$River)) {
  for (i in 2000:2017) {
    Z$Returns_FINAL[Z$River==river & Z$Yr==i] = Z$Returns_NEW[Z$River==river & Z$Yr==i]/mean.diff
    Z$Recruits_FINAL[Z$River==river & Z$Yr==i-3] = Z$Returns_NEW[Z$River==river & Z$Yr==i]/mean.diff
  }
}

# # Option 2 - use Brendan's data for 1975-1999 and Emma's for 2000-2017 but fill in the gaps pre-2000 with Emma's data
# for (river in unique(Z$River)) {
#   for (i in 1950:1999) {
#     Z$Returns_FINAL[Z$River==river & Z$Yr==i] = Z$Returns[Z$River==river & Z$Yr==i]
#     Z$Recruits_FINAL[Z$River==river & Z$Yr==i-3] = Z$Returns[Z$River==river & Z$Yr==i]
#   }
# }
# 
# for (river in unique(Z$River)) {
#   for (i in 2000:2017) {
#     Z$Returns_FINAL[Z$River==river & Z$Yr==i] = Z$Returns_NEW[Z$River==river & Z$Yr==i]#/mean.diff
#     Z$Recruits_FINAL[Z$River==river & Z$Yr==i-3] = Z$Returns_NEW[Z$River==river & Z$Yr==i]#/mean.diff
#   }
# }
# 
# # Filling in with Emma's data for missing data pre-2000 #
# for (river in unique(Z$River)) {
#   for (i in 1975:1999) {
#     if (is.na(Z$Returns[Z$River==river & Z$Yr==i])==TRUE){
#       Z$Returns_FINAL[Z$River==river & Z$Yr==i] = Z$Returns_NEW[Z$River==river & Z$Yr==i]#/mean.diff
#       Z$Recruits_FINAL[Z$River==river & Z$Yr==i-3] = Z$Returns_NEW[Z$River==river & Z$Yr==i]#/mean.diff
#     }
#   }
# }


# g) Spawner estimates to pair with recruitment (S(t-2) corresponds to R(t))
# EA: Code below has been commented out so as to index by brood year (spawners) instead of return year.
# Now, S(t) corresponds to R(t-2)

# Z$S<-NA; Z$S[3:length(Z$S)]<-Z$Escapement[1:(length(Z$S)-2)];
# Z$S[which(Z$Yr==1960 | Z$Yr==1961)]<-NA;

# h) Calculate survival as log(R/S)
Z$Survival <- c(NA)
Z$Survival <- log(Z$Recruits_FINAL/Z$Escapement)

## EA:  Following code commented out for unfiltered version. ##

#-------------------------------------------------------------------------------
# 3) Remove ambiguous or confounding farm exposure, enhancement, etc
#-------------------------------------------------------------------------------

# # Area 12
# Z12<-subset(Z,Area==12)
# N12<-unique(Z12$River)
# N12
# N.BA<-c("AHNUHATI RIVER", "AHTA RIVER", "GLENDALE CREEK", "KAKWEIKEN RIVER", "KINGCOME RIVER", "LULL CREEK", "VINER SOUND CREEK", "WAKEMAN RIVER");
# 
# BA<-c(); for(i in 1:length(N.BA)) BA<-rbind(BA,subset(Z,River==N.BA[i]))
# 
# # Area 7
# Z7<-subset(Z,Area==7)
# N7<-unique(Z7$River)
# N7
# N.7<-c("PINE RIVER", "NEEKAS CREEK", "TANKEEAH RIVER", "KWAKUSDIS RIVER", "BULLOCK CHANNEL CREEKS", "QUARTCHA CREEK", "LEE CREEK", "ROSCOE CREEK", "CLATSE CREEK", "WALKER LAKE CREEK", "GOAT BUSHU CREEK", "DEER PASS LAGOON CREEKS", "KUNSOOT RIVER", "KADJUSDIS RIVER", "MCLOUGHLIN CREEK", "COOPER INLET CREEKS");
# 
# A7<-c(); for(i in 1:length(N.7)) A7<-rbind(A7,subset(Z,River==N.7[i]))
# 
# ZZ<-subset(Z,Area!=12 & Area!=7); ZZ<-rbind(A7,ZZ,BA);

ZZ <- Z; # EA: re-naming unfiltered dataset for the following code. 

#-------------------------------------------------------------------------------
# 4) Only keep populations with a minimum of 20 spawner-recruit pairs
#-------------------------------------------------------------------------------

# Min.N<-20; Z.lengths<-c(); L<-unique(ZZ$Popn); 
# for(i in 1:length(L)){
# 	z1<-subset(ZZ,Popn==L[i]);	
# 	z2<-which(is.na(z1$Survival)); 
# 	Z.lengths[i]<-length(z1$S)-length(z2);
# 	}
# 
# R.LongEnough<-which(Z.lengths>=20)
# R.River<-c(); R.Area<-c();
# for(i in 1:length(R.LongEnough)){
# 	z1<-subset(ZZ,Popn==R.LongEnough[i])
# 	R.Area<-c(R.Area,z1$Area[1])
# 	R.River<-c(R.River,z1$Popn[1])
# 	}

#Only keep those entries with estimates of survival
# Z1<-subset(ZZ, is.na(ZZ$Survival)==FALSE)

# cat("Final dataset: \n Total number of populations (even/odd): ", length(unique(Z1$Population)), "\n Total number of S-R pairs: ", dim(Z1)[1], "\n Total number of rivers: ", length(unique(Z1$River)))
cat("Final dataset: \n Total number of S-R pairs: ", dim(ZZ)[1], "\n Total number of rivers: ", length(unique(ZZ$River)))

## EA-AB: I've left the following code here in case you want to use louse data for your stuff. ##

################################################################################
## C. Inclusion of louse covariate data
################################################################################

# # Wild lice estimates
# W<-data.frame(return.year=c(2001:2010)+1, mean=c(12.1739692,6.228714679,0.692532577,6.226036908,2.657006199,0.906920939,0.869532124,0.390081339,0.202908529,0.627256331))
# 
# Z1$WildLice<-rep(0,dim(Z1)[1])
# for(i in 1:dim(W)[1]){
# 	Z1$WildLice[Z1$Area==12&Z1$Yr==W$return.year[i]]<-rep(W$mean[i], length(which(Z1$Area==12&Z1$Yr==W$return.year[i])))
# }

################################################################################
## D. Write final database to file
################################################################################
setwd(dir.gen)

### EDIT

data.final= ZZ[,c(1:4,7,10,11,14)]
names(data.final)= c("Yr", "Area", "River", "Spawners", "Returns", "Recruits","Expl", "Survival")
write.csv(data.final, "Coho_stock_recruit_unfiltered_2018_FINAL.csv") 

data.brendan= ZZ[,c(1:5,8,12,13)]
names(data.brendan)= c("Yr", "Area", "River", "Spawners", "Returns", "Recruits","Expl", "Catch")
write.csv(data.brendan, "Coho_stock_recruit_unfiltered_2018_BRENDAN.csv") 

data.emma= ZZ[,c(1:4,6,9,11)]
names(data.emma)= c("Yr", "Area", "River", "Spawners", "Returns", "Recruits","Expl")
write.csv(data.emma, "Coho_stock_recruit_unfiltered_2018_EMMA.csv") 

