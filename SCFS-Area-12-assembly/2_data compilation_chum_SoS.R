# Date: 12-Jul-2018
# Last updated: 12-Jul-2018
# Name: Emma Atkinson
# Description: Chum salmon stock-recruit data compilation.
# Notes: Data compilation (chum) adapted from Peacock et al. 2014
     
### Loading packages ###
library(dplyr)
library(scales)
library(plotrix)

### INPUT ###
rm(list=ls())
graphics.off()

dir.raw <- "C:/Users/Emma Atkinson/Desktop/Research/SoS/Analysis/Data-raw"
dir.gen <- "C:/Users/Emma Atkinson/Desktop/Research/SoS/Analysis/Data-generated"

setwd(dir.raw)

### Prepping data for use with Steph's code ###
data <- read.csv("updated_nuseds_2017.csv")
data <- filter(data, SPECIES=="Chum")

# Transposing chum data in order to compile S-R pairs with Steph's code #
z <- data[,-c(1,8,9)] # removing estimate classification/method columns and extra colum at the beginning 
require(reshape2)
z <- dcast(z, AREA + WATERBODY + SPECIES + POPULATION + POP_ID + GFE_ID ~ ANALYSIS_YR, value.var="MAX_ESTIMATE")
z <- z[order(z$WATERBODY),] # Ordering dataframe alphabetically for consistent indexing 

####################################################################################
# DATA COMPILATION

####################################################################################
## PART 1: Harvest rate calculation
####################################################################################

# a) Read in catch and escapement
#z<-read.delim("Chum_escapement_SP.txt", header=TRUE, na.string="") # EA: commented out Steph's data for now
z<-z[,c(1:6, 28:95)] # Select years 1950-2010

catch<-read.csv("chum_catch_area-12_1980-2017.csv", header=TRUE) # includes catch estimates from multiple sources
catch.mean<-apply(catch[,3:5], 1, mean, na.rm=TRUE) # Brian Spilsted did not provide catch for area 11, so that had to be taken from elsewhere.
catch.mean[32:38] <- catch$Pieter2018[32:38] # adding catch estimates for which there was only one estimate

brendanharvest <- read.csv("brendan_chum_harvest_2014.csv", header=TRUE)  # read in harvest data from Brendan Connors (spans 1954-2014)
names(brendanharvest) <- c("area", "year", "harvest.rate", "source")

# b) Set up vectors to store new data:
year<-c(1950:2017)
area<-rep(12, length(unique(year)))
total.esc<-numeric(length(area))
harvest.rate<-numeric(length(area)) # this vector to be used for storing calculated harvest rates

# c) Calculate total escapement and harvest rates
a <- 12 # By-passing loop to just compile for area 12
# for(a in 7:12){ #for each management area 
	 z1<-subset(z, area==a)
	
	# Select index populations as those that have at least 17/23 years (1980-2002) data
	# EA: modified the above criterion to be 45/67 years (1950-2017) but still works out to ~2/3. Modified because obtained additional harvest estimates from Pieter Van Will through Brendan Connors
	# This window was selected so harvest rates were consistent with previous estimates from B. Spilsted at Fisheries and Oceans Canada.
	index<-which((68-as.numeric(apply(is.na(z1[,7:74]), 1, sum)))>=45)
	z1$WATERBODY[index] # Print names of index rivers to check
	
	# The observed sum is the sum of escapement for index streams each year
	obs.sum<-as.numeric(apply(z1[index,7:74], 2, sum, na.rm=TRUE))
	
	# The decade mean escapement for each stream is the mean of 1980-1989, 1990-1999, 2000-2010 and 1980-2010
	# EA: update this for extra decades
	decade.mean.esc<-matrix(nrow=dim(z1)[1], ncol=8)
	decade.mean.esc[,1]<-as.numeric(apply(z1[,7:16], 1, mean, na.rm=TRUE)) # 1950s
	decade.mean.esc[,2]<-as.numeric(apply(z1[,17:26], 1, mean, na.rm=TRUE)) # 1960s
	decade.mean.esc[,3]<-as.numeric(apply(z1[,27:36], 1, mean, na.rm=TRUE)) # 1970s
	decade.mean.esc[,4]<-as.numeric(apply(z1[,37:46], 1, mean, na.rm=TRUE)) # 1980s
	decade.mean.esc[,5]<-as.numeric(apply(z1[,47:56], 1, mean, na.rm=TRUE)) # 1990s
	decade.mean.esc[,6]<-as.numeric(apply(z1[,57:66], 1, mean, na.rm=TRUE)) # 2000s
	decade.mean.esc[,7]<-as.numeric(apply(z1[,67:74], 1, mean, na.rm=TRUE)) # 2010s
	decade.mean.esc[,8]<-as.numeric(apply(z1[,7:74], 1, mean, na.rm=TRUE)) # All years
	
	# Calculate the sum of escapement for each decade, from index streams only
	decade.sum<-apply(decade.mean.esc[index,], 2, sum, na.rm=TRUE)
	
	# Porportion of decade.sum that each stream contributed, for each decade
	prop.obs.total<-matrix(nrow=length(index), ncol=7)
	for(i in 1:7){prop.obs.total[,i]<-decade.mean.esc[index,i]/decade.sum[i]}
	apply(prop.obs.total, 2, sum) 
	
	# Calculate annual escapement contribution from each index stream
	esc.contribution<-matrix(nrow=length(index), ncol=68) # need to change ncol argument to however many years you have
	for(i in 1:length(index)){
		for(y in 1:68){
			if(sum(z1[index[i],y+6], na.rm=TRUE)==0){esc.contribution[i,y]<-0}else{
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
	
	plot(1950:2017, obs.sum, "l", ylab="Escapement", xlab="Year", bty="n", main=paste("Area ", a), xlim=c(1950,2020), ylim=c(0,800000))
	lines(1950:2017, adj.sum, col=2)
	
	# Analysis of 1980s baseline period for index streams contribution **EA: need to check what Steph means by baseline period
	proportion<-apply(decade.mean.esc[index,], 2, sum, na.rm=TRUE)/apply(decade.mean.esc, 2, sum, na.rm=TRUE)
	proportion<-c(rep(proportion[1:6], each=10), rep(proportion[7], 8))
	
	# Observed sum of all streams (not just index) in area (i.e., infilling of non-index streams)
	obs.sum.all<-adj.sum/proportion
	lines(1950:2017, obs.sum.all, col=3)
	
	# Observed sums are multiplied by a 1.5 expansion factor, to account for the streams that are not monitored at all.  
	adj.sum.all<-obs.sum.all*1.5
	lines(1950:2017, adj.sum.all, col=4)

	# Calculate harvest rate
	adj.sum.all.1980 <- adj.sum.all[31:68] # just pulling data for 1980-2017
	harvest.rate[31:68]<-catch.mean[catch$area==a]/apply(cbind(catch.mean[catch$area==a], adj.sum.all.1980), 1, sum, na.rm=TRUE) # filling in harvest rates using catch data
	#harvest.rate[1:30] <- brendanharvest[brendanharvest$area==12,]$harvest.rate[1:30] # adding harvest rates for 1954-1979 from Brendan Connors (originally from Pieter VanWill). These were given as-is, not calculated. Need to check because they seem to be generally lower than other estimates
	total.esc[area==a]<-adj.sum.all	
	#} #end area

harvest.rate[harvest.rate=="NaN"]<-0 #When catch is zero, harvest rates are zero

# d) Adjust area 12 because majority of catch in this area are fish returning to more southern rivers through Johnstone Strait. (Pieter vanWill, pers. comm.)
# EA: May need to dig into this because have catch data for area 12 from Pieter Van Will. 
#     Is there a simpler way to do this for area 12? 

# Plot *unadjusted* harvest rates
plot(1,1, "n", ylim=c(0,1), xlim=c(1950,2020), bty="n", ylab="harvest rate", xlab="return year", las=1)
#for(a in 7:11){
  a <- 12
  #lines(1950:2017, harvest.rate[area==a], col=grey(0.6))#}
  lines(1980:2017, harvest.rate[31:68], col=grey(0.6))#} # these are calculated harvest rates using catch data
	lines(1954:2014, brendanharvest[brendanharvest$area==12,]$harvest.rate[1:61], col="blue") # these are harvest rates from Pieter (via Brendan)
	#lines(1980:2009, catch$PieterVW[1:30], col="red")#} # these are harvest rates from Pieter (via Steph)
	
	#lines(1950:2017, harvest.rate[area==12])
	# But what proportion of catch is actually catch for that area?
	diff<-catch$PieterVW[catch$area==12]/harvest.rate[31:68] # checking data from Pieter via Steph (1980-2017)
	diff2 <- brendanharvest[brendanharvest$area==12,]$harvest.rate[27:61]/harvest.rate[31:65] # checking data from Pieter via Brendan (1980-2014)
	mean.estimate.steph <- mean(diff, na.rm=TRUE) 
	mean.estimate.brendan <- mean(diff2, na.rm=TRUE)
	se.estimate.brendan <- std.error(diff2,na.rm=TRUE)
	# EA: need to check with Steph/Andrew whether this (below) is legit - mean of means?
	mean.diff <- mean(c(mean.estimate.steph, mean.estimate.brendan)) # mean average harvest rate estimate of steph and brendan's data
	
	#On average, the harvest rate from Pieter vanWill is only 15% calculated harvest rates
	# EA: looking at harvest rate data from Pieter VanWill via Brendan, harvest rate is on average 7% of the calculated harvest rates
	
	#lines(1980:2010, catch$PieterVW[catch$area==12], col=2)
	#lines(1980:2017, harvest.rate[31:68]*mean(diff, na.rm=TRUE), col="red", lty=2) # plotting estimates adjusted by 15%
	lines(1980:2017, harvest.rate[31:68]*mean(diff2, na.rm=TRUE), col="blue", lty=2) # plotting estimates adjusted by 7%
	legend(1950, 1, col=c(grey(0.6), "blue", "blue"), lty=c(1,1,2,1,2), c("Area 12 unadjusted ER 1980-2017", "DFO reconstructed ER 1954-2014","Area 12 adjusted 1980-2017 (7.6% of unadjusted)"), bg="white", cex=0.8)
	#legend(1995, 1, col=c(grey(0.6), 1, 2, 2), lty=c(1,1,1,2), c("Areas 7-11", "Area 12 unadjusted", "Area 12 PVW", "Area 12 0.15*unadjusted"), bg="white", cex=0.8)

# Assembling final harvest rate estimates
# SP: Pieter vanWIll could not yet provide harvest rates for 2010, so I have extrapolated.
# EA: Have estimates from P. Van Will (via Brendan) for 1954-2014 and have used those estimates to extrapolate for 2015-17.
harvest.estimates <- data.frame(year=seq(1950,2017,1),
                                area=rep(12, 68),
                                species=rep("chum",68),
                                calculated.harvest=harvest.rate,
                                steph=rep(NA, 68),
                                brendan=rep(NA, 68), 
                                final=rep(NA, 68),
                                source=rep(NA,68))
                      
harvest.estimates$brendan[5:65] <- brendanharvest[brendanharvest$area==12,]$harvest.rate[1:61]
harvest.estimates$steph[31:65] <- catch$PieterVW[1:35]
# harvest.estimates <- transform(harvest.estimates, mean=rowMeans(harvest.estimates[,-c(1,2,3,4)], na.rm=TRUE))
# harvest.estimates[harvest.estimates$mean=="NaN",]$mean<-NA

# Filling in missing years
harvest.estimates$final[5:65] <- harvest.estimates$brendan[5:65]
# For 1950-1953: use mean harvest rate for the rest of the decade for these missing years
# harvest.estimates$final[1:4] <- mean(harvest.estimates$brendan[1:10], na.rm=TRUE)
# For 2015-2017:
harvest.estimates$final[66:68] <- mean.estimate.brendan*harvest.rate[66:68]

# Adding column for data sources 
harvest.estimates$source[5:65] <- "P. Van Will (via Brendan Connors)"
harvest.estimates$source[66:68] <- "Calculated harvest adjusted using Brendan's data"

# e) Write datafile 
setwd(dir.gen)
write.csv(harvest.estimates, file="area_12_chum_harvest_rates_1950-2017.csv") 

####################################################################################
## PART 2: Age at return
####################################################################################
setwd(dir.raw)
AAR<-read.csv("area_12_chum_age_data_compiled_2018.csv", header=TRUE, na.string="")

# If using data from Ryall et al. (1999)
#AAR[AAR$area==12,3:5]<-matrix(c(0.124, 0.716, 0.16), nrow=length(AAR[AAR$area==12,3]), ncol=3, byrow=TRUE)
#-----------------------------------------------------------------------------------
# a) Assuming average age distribution for missing years/areas
AAR_1<-AAR

# # Impute missing values for area 8 with mean from that area
# AAR_1[AAR_1$area==8&is.na(AAR$age3)==TRUE,3:5]<-matrix(apply(AAR[AAR$area==8,3:5], 2, mean, na.rm=TRUE), nrow=length(which(AAR_1$area==8&is.na(AAR$age3)==TRUE)), ncol=3, byrow=TRUE)
# 
# # Impute area 7 AAR with AAR from area 8 (no data available for area 7)
# AAR_1[AAR_1$area==7,3:5]<-AAR_1[AAR_1$area==8,3:5]
# 
# # Impute missing values for area 9 with mean from that area
# # ** exlude 1984, because this year had anomalously high percentage of 3-year olds and was collected from dead pitch only
# AAR_1[AAR_1$area==9&is.na(AAR$age3)==TRUE,3:5]<-matrix(apply(AAR[AAR$area==9&AAR$return_year!=1984,3:5], 2, mean, na.rm=TRUE), nrow=length(which(AAR_1$area==9&is.na(AAR$age3)==TRUE)), ncol=3, byrow=TRUE)
# 
# # Impute missing values for area 10 with mean from that area
# AAR_1[AAR_1$area==10&is.na(AAR$age3)==TRUE,3:5]<-matrix(apply(AAR[AAR$area==10,3:5], 2, mean, na.rm=TRUE), nrow=length(which(AAR_1$area==10&is.na(AAR$age3)==TRUE)), ncol=3, byrow=TRUE)
# 
# # Impute area 11 AAR with AAR from area 10 (no data available for area 11)
# AAR_1[AAR_1$area==11,3:5]<-AAR_1[AAR_1$area==10,3:5]

# # Impute missing values for area 12 with AAR from area 10 (no data available for area 11)
# AAR_1[AAR_1$area==12,3:5]<-AAR_1[AAR_1$area==10,3:5]

# AAR_1[AAR_1$area==12&is.na(AAR$age3)==TRUE,3:5]<-matrix(apply(AAR[AAR$area==12,3:5], 2, mean, na.rm=TRUE), nrow=length(which(AAR_1$area==12&is.na(AAR$age3)==TRUE)), ncol=3, byrow=TRUE)

# EA: For years with missing age data, assume average age structure for Area 12 #
average.age <- c(0.209, 0.693, 0.096, 0.002) # These age data are all from Pieter Van Will - some new data received in 2018 and some received in 2017

AAR_1[c(1:8, 68),3] <- average.age[1]
AAR_1[c(1:8, 68),4] <- average.age[2]
AAR_1[c(1:8, 68),5] <- average.age[3]
AAR_1[c(1:8, 68),6] <- average.age[4]

#-----------------------------------------------------------------------------------
# b) Assuming all 4-year for missing years/areas
AAR_2<-AAR
AAR_2[is.na(AAR_2$age3)==TRUE,3:6]<-matrix(c(0,1,0,0), nrow=sum(is.na(AAR_2$age3)==TRUE), ncol=4, byrow=TRUE)

# c) Write datafile for whichever AAR imputing method you want to use
setwd(dir.gen)
write.csv(AAR_2, file="area_12_chum_age_at_return_imputed_4y.csv")
write.csv(AAR_1, file="area_12_chum_age_at_return_imputed_avg.csv")

####################################################################################
## PART 3: Select populations with acceptable data
####################################################################################
rm(list=ls()) # Clear workspace

dir.raw <- "C:/Users/Emma Atkinson/Desktop/Research/SoS/Analysis/Data-raw"
dir.gen <- "C:/Users/Emma Atkinson/Desktop/Research/SoS/Analysis/Data-generated"

setwd(dir.gen)

# a) Load data
age<-read.csv("area_12_chum_age_at_return_imputed_avg.csv", header=TRUE, na.string="NA")
harvest<-read.csv("area_12_chum_harvest_rates_1950-2017.csv", header=TRUE, na.string="NA")

data <- read.csv("updated_nuseds_2017.csv", header=TRUE, na.string="NA")
data <- filter(data, SPECIES=="Chum")

# z <- z[,c(2,3,1,4)]
# names(z) <- c("Area", "River", "Yr", "Escapement")

# Transposing chum data in order to compile S-R pairs with Steph's code #
z <- data[,-c(1,8,9)] # removing estimate classification/method columns and extra colum at the beginning 

require(reshape2)
z <- dcast(z, AREA + WATERBODY + SPECIES + POPULATION + POP_ID + GFE_ID ~ ANALYSIS_YR, value.var="MAX_ESTIMATE")
z <- z[order(z$WATERBODY),] # Ordering dataframe alphabetically for consistent indexing 
z<-z[,c(1:6, 28:95)] # Select years 1950-2010

# Create vector of population names to keep track!
names<-strsplit(as.character(z[,4]), split=" ")
river.names<-character(length=98) # 98 = number of unique populations
for(i in 1:98) river.names[i]<-paste(names[[i]][1], names[[i]][2])

# b) Select populations
# Criteria: at least 2/3 of the years 1980-2010 must have escapement estimates in order to use the population in the analysis (>= 20/31 years).
# EA: If going to filter using this critera, need to check... Ask Andrew whether he used the filtered or unfiltered data - presumably the filtered?

# keep<-which((length(1980:2010)-as.numeric(apply(is.na(z[,9:39]), 1, sum)))>=20)
# river.names2<-river.names[keep]
# print(river.names2)
# z2<-z[keep,]

# EA: Adjusted criteria to include all data from 1950-2017
keep<-which((length(1950:2017)-as.numeric(apply(is.na(z[,7:74]), 1, sum)))>=1)
river.names2<-river.names[keep]
print(river.names2)
z2<-z[keep,]

# # c) Remove populations in the "exposed" area whose level of exposure was ambiguous (i.e., outside the Broughton Archipelago).  These rivers are:
# cat("To be removed: \n", river.names2[is.element(c(1:length(keep)), c(75, 81, 82))])
# river.names3<-river.names2[is.element(c(1:length(keep)), c(75,81,82))==FALSE]
# z3<-z2[is.element(c(1:length(keep)), c(75,81,82))==FALSE,]
# 
# # d) Remove populations (in the north) that may have been exposed, but we don't know the lice levels.
# cat("To be removed: \n", paste(z3[is.element(c(1:dim(z3)[1]), c(30, 26, 13, 5, 14, 18, 2, 16, 22, 27, 25, 3, 6, 10, 33, 17, 21, 12)), 1], river.names3[is.element(c(1:dim(z3)[1]), c(30, 26, 13, 5, 14, 18, 2, 16, 22, 27, 25, 3, 6, 10, 33, 17, 21, 12))], "\n"))
# z4<-z3[is.element(c(1:dim(z3)[1]), c(30, 26, 13, 5, 14, 18, 2, 16, 22, 27, 25, 3, 6, 10, 33, 17, 21, 12))==FALSE,]
# 
# # e) Remove Four Lakes Creek (area 8) because it has a significantly different growth rate
# z5<-z4[c(1:20,22:dim(z4)[1]),]
# n<-dim(z5)[1]
# z6<-matrix(nrow=n, ncol=31)
# for(i in 1:n){z6[i,]<-as.numeric(z5[i,9:39])}

z5 <- z2 # EA: renaming z2 for following code
n <- dim(z5)[1]
z6 <- matrix(nrow=n, ncol=68) # set ncol to total number of years
for(i in 1:n){z6[i,]<-as.numeric(z5[i,7:74])}


# f) List of included sites 
FullRiverName<-character(n)
for(i in 1:n) FullRiverName[i]<-strsplit(as.character(z5$POPULATION), split="Chum")[[i]][1]

RiverName<-character(n)
AreaName<-character(n)
for(i in 1:n){
	j<-strsplit(FullRiverName, " ")[[i]]
	AreaName[i]<-paste(j[(length(j)-1)], j[length(j)])
	if(length(j)==4) RiverName[i]<-paste(j[1], j[2])
	if(length(j)==5) RiverName[i]<-paste(j[1], j[2], j[3])
	if(length(j)==6) RiverName[i]<-paste(j[1], j[2], j[3], j[4])
}

# EA - changed "RiverName" to "FullRiverName"	
#InclRivers<-data.frame(n=as.integer(1:n), AreaNum=z5$area, AreaName=AreaName, CU=z5$CU.name, River=FullRiverName)
InclRivers<-data.frame(n=as.integer(1:n), AreaNum=z5$AREA, AreaName=AreaName, River=FullRiverName)

####################################################################################
## PART 4: Create spawner recruit pairs
####################################################################################
# a) Create vectors to store data

# EA: Should last year be 2017 or 2013 give that we are indexing by brood year? #
site<-rep(1:n, each=length(1950:2017))
escapement<-numeric(length(1950:2017)*n)
year<-rep(1950:2017, n)
area<-rep(z5[,1], each=length(1950:2017))
#CU<-rep(z5[,3], each=length(1950:2017))
#CU.name<-rep(z5[,2], each=length(1950:2017))
river <- rep(z5[,2], each=length(1950:2017))

# b) Calculate survival = ln(R/S)
for(y in 1950:2017){
	ind<-which(c(1950:2017)==y)
	escapement[which(year==y)]<-z6[,ind]
}

returns<-numeric(length(escapement))
for(i in 1:max(site)){
	hr<-harvest$final[harvest$area==area[site==i][1]]
	returns[site==i]<-escapement[site==i]/(1-hr)
		}

# If using age at return data:
recruits<-rep(NA,length(escapement))
for(i in 1:max(site)){
	aar<-age[age$area==area[site==i][1]&is.element(age$return_year, c(1950:2017)),is.element(names(age), c("age3", "age4", "age5"))]
	for(y in 1:63){
		Y<-c(1950:2017)[y]
		recruits[site==i&year==Y]<-aar[y+3,1]*returns[year==(Y+3)&site==i]+aar[y+4,2]*returns[year==(Y+4)&site==i]+aar[y+5,3]*returns[year==(Y+5)&site==i]
		}
	}
	
#If assuming constant x-year age at return	
# x<-4
# recruits<-rep(NA,length(escapement))
# for(i in 1:max(site)){
	# for(y in 1:26){
		# Y<-c(1980:2010)[y]
		# recruits[site==i&year==Y]<-returns[year==(Y+x)&site==i]		}
	# }

survival<-log(recruits/escapement)

# Create dataframe. EA edit - added "river" and "CU.name"
#Z<-data.frame(area=as.factor(area), CU=as.factor(CU), CU.name, river,  site=as.factor(site), year=as.factor(year), escapement, returns, recruits, survival)
Z<-data.frame(area=as.factor(area), river,  site=as.factor(site), year=as.factor(year), escapement, returns, recruits, survival)

# # EA - CSV to check if got river names right
# dir.out <- "C:/Users/Emma Atkinson/Desktop"
# setwd(dir.out) #EA
# write.csv(Z, "Chum_stock_recruit_edited_names.csv") #EA

## EA-AB: I've left the following code here in case you want to use louse data for your stuff. ##

####################################################################################
## PART 5: Louse Covariates
####################################################################################

# # a) Farm louse covariates (from Krkosek et al. Effects of parasites from salmon farms on productivity of wild salmon. Proceedings of the National Academy of Sciences, 108(35):14700–14704, 2011.)
# L_1<-c(10.419065,8.618401,18.705716,1.412284,7.885961,4.261532,1.095941,1.442069,0.687060)# April lice on farms spring 2000-2008 (Marty et al 2010 PNAS)
# L_2<-c(0.0453000,6.4274585,11.4803591,1.1720842,2.8268919,2.3077925,1.0213965,1.2671798,0.6540545)# farm lice scenario 2 (Krkosek et al 2011)
# L_3<-c(2.2649900,7.6255525,15.0991603,1.1720842,2.8268919,2.3077925,1.0213965,1.2671798,0.6540545)# farm lice scenario 3 (Krkosek et al 2011)
# L_4<-c(2.1827792,7.6255525,14.7975935,1.1720842,2.8268919,2.3077925,1.0213965,1.2671798,0.6540545)# farm lice scenario 4 (Krkosek et al 2011)
# 
# 
# # b) Wild louse covariate (see other R file: "0 lice on wild salmon.R")
# W <-
# structure(list(X = 2001:2010, mean = c(10.7649985917623, 5.87355457516037, 
# 0.614159876223738, 6.59729687009137, 2.8201779900578, 0.719427156222749, 
# 0.882761691371703, 0.384081695624553, 0.199089660589752, 0.686396383236404
# ), LCI = c(7.51294106683191, 4.97077093909145, 0.50619723787665, 
# 5.6367973094448, 2.41013182973974, 0.608270474887731, 0.741491400097079, 
# 0.329303068159553, 0.161488046659826, 0.582643117405682), UCI = c(15.4247442712222, 
# 6.94030036187767, 0.745148976208106, 7.72146373246199, 3.29998707849327, 
# 0.850896853420809, 1.05094705569263, 0.44797259174748, 0.245446605947482, 
# 0.808625350313665)), .Names = c("year", "mean", "LCI", "UCI"), class = "data.frame", row.names = c(NA, 
# -10L))
# 
# # c) Include louse covariates in main database, Z
# Y<-c(1999,2000,2001,2002,2003,2004,2005,2006,2007) # set chum brood years to correspond to louse abundances (i.e., 1999 are the brood that migrated out in 2000)
# 
# # Assign louse abundance to populations with 0 for all references 
# # and NA for broughton from 1991-1999
# year<-as.numeric(levels(Z$year)[as.numeric(Z$year)])
# 
# Z$L1<-rep(NA, length(Z$escapement))
# Z$L1[Z$area!=12]<-0
# Z$L1[year<1990]<-0
# Z$L2<-Z$L1; Z$L3<-Z$L1; Z$L4<-Z$L1; Z$W<-Z$L1
# 
# for(i in 54:64){
# 	for(y in 1999:2005){
# 		Z$L1[Z$site==i&Z$year==y]<-L_1[Y==y]
# 		Z$L2[Z$site==i&Z$year==y]<-L_2[Y==y]
# 		Z$L3[Z$site==i&Z$year==y]<-L_3[Y==y]
# 		Z$L4[Z$site==i&Z$year==y]<-L_4[Y==y]
# 		Z$W[Z$site==i&Z$year==y]<-c(NA, W[1:8,2])[Y==y]
# 		}
# 	}
# 
# Z$year.numeric<-Z$year

### EA: Writing data to file ###
data.chum <- Z
setwd(dir.gen) 
write.csv(data.chum, "Chum_stock_recruit_unfiltered_2018.csv") 
