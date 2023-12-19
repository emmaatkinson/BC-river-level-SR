# Date: 11-Jul-2018
# Last updated: 23-Oct-2018
# Name: Emma Atkinson
# Description: Pink salmon stock-recruit data compilation.
# Notes: Data compilation (pink) adapted from Peacock et al. 2013
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
data <- filter(data, SPECIES=="Pink")

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
#steph <- read.csv("nuSEDS_PINK.csv") # Load Steph's data to compare afterwards

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

a <- 1 # Setting area to Area 12 in loop

#for(a in 1:length(unique(z$Area))){ # For each area
	z1<-subset(z, Area==unique(z$Area)[a]) #subset data
	
	#choose index streams as those that have at least 20 years data
	ind<-as.numeric(tapply(is.na(z1$Escapement), z1$River, sum)) 
	index<-unique(z1$River)[which(ind<26)] # EA: Changed criteria to max 26 missing years (1/3 = 23 years + 3 years because no rivers have data for 1950-52)
	
	test <- filter(z1, River%in%index)
	unique(test$RiverName)
	
	#The observed sum is the sum of escapement for index streams each year
	obs.sum<-as.numeric(tapply(z1$Escapement[is.element(z1$River, index)], z1$Yr[is.element(z1$River, index)], sum, na.rm=TRUE))
	
	#The decade mean escapement for each stream 
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
	
	#Calculate the sum of escapement for each decade, from index streams only
	if(length(index)==1) decade.sum<-decade.mean.esc[which(is.element(unique(z1$River),index)),]
	if(length(index)>1) decade.sum<-apply(decade.mean.esc[which(is.element(unique(z1$River),index)),], 2, sum, na.rm=TRUE)
	
	#Porportion of decade.sum that each stream contributed, for each decade
	prop.obs.total<-matrix(nrow=length(index), ncol=7)
	for(i in 1:7){prop.obs.total[,i]<-decade.mean.esc[which(is.element(unique(z1$River),index)),i]/decade.sum[i]}
	apply(prop.obs.total, 2, sum) 
	
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
				if(y>50&y<=60) esc.contribution[i,y]<-prop.obs.total[i,6] # EA: Added this for the extra decade but not sure if it is right
				if(y>60) esc.contribution[i,y]<-prop.obs.total[i,7]
				}
			}
		}
	
	#Determine expansion factor: how much of annual escapement for index streams is NA (i.e., infilling of index streams)
	expansion<-apply(esc.contribution, 2, sum)
	
	#Adjusted total escapement of the area for the year
	adj.sum<-obs.sum/expansion
	
	plot(1950:2017, obs.sum, "l", ylab="Escapement", xlab="Year", bty="n", main=paste("Area ", unique(z$Area)[a]), xlim=c(1950,2020), ylim=c(0,8*10^6))
	lines(1950:2017, adj.sum, col=2)
	
	#Analysis of 1980s baseline period for index streams contribution
	if(length(index)>1) proportion<-as.numeric(apply(decade.mean.esc[which(is.element(unique(z1$River),index)),], 2, sum, na.rm=TRUE)/apply(decade.mean.esc, 2, sum, na.rm=TRUE)) else proportion<-decade.mean.esc[which(is.element(unique(z1$River),index)),]/apply(decade.mean.esc, 2, sum, na.rm=TRUE)
	
	proportion<-c(rep(proportion[1:7], each=10), proportion[7])
	proportion <- proportion[-c(68, 69, 70)] # Removing last three years of final decade (data only go to 2017)
	
	#Observed sum of all streams (not just index) in area (i.e., infilling of non-index streams)
	obs.sum.all<-adj.sum/proportion
	lines(1950:2017, obs.sum.all, col=3)
	
	#Brian S. multiplies the observed sum by a 1.5 expansion factor, presumably for the streams that are not monitored at all.  It would be good to see how sensitive the results are to this.
	adj.sum.all<-obs.sum.all*1.5
	lines(1950:2017, adj.sum.all, col=4)

	
	zz$AdjustedEsc[zz$Area==unique(zz$Area)[a]]<-adj.sum.all
	
	#} #end area

#-------------------------------------------------------------------------------
# 3) Calculate exploitation rates - note: major edits to SP code by EA
#-------------------------------------------------------------------------------
setwd(dir.raw)

# These raw catch data have been updated by EA for 1954-2016 with data from Pieter Van Will. 
# We know that these are inflated estimates of catch because some of these pink are on their
# way towards more southern rivers and shouldn't be included in Area 12 specific analyses.
x<-read.csv("Pink_catch_data_updated_EA_2.csv", header=TRUE) 

# These are up-to-date harvest RATES (adjusted for interception) from the 2017 Inner South Coast pink run reconstructions from Pieter Van Will
emmaharvest <- read.csv("area_12_pink_harvest_rates_2018.csv", header=TRUE)
names(emmaharvest) <- c("area", "year", "harvest.rate", "source")

# These are less recent harvest RATES (adjusted for interception) from Pieter Van Will (via Brendan Connors). Spans 1954-2014.
brendanharvest <- read.csv("brendan_pink_harvest_2014.csv", header=TRUE)  
names(brendanharvest) <- c("area", "year", "harvest.rate", "source")
brendanharvest <- filter(brendanharvest[,c(1:4)], area==12)

a <- 12

### Calculating harvest rate estimates using raw catch data and adjusted escapement from above ###

zz$Catch<-numeric(dim(zz)[1])
#for(a in unique(zz$Area)){
	for(j in 1950:2017){
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

# Estimates below are from Steph who got them from Pieter Van Will in 2011 
# These are most out-of-date (relative to Brendan's and Emma's data).
Area12Exploitation<-data.frame(Exploitation=c(NA, NA, NA, NA,0.382085422, NA, 0.577450409,NA, 0.608614425, 0.609438028,0.581290831,0.624488262,0.511452327,0.53910599, 0.559106731, 0.407737058,0.709759091,0.68330814,0.702556339,0.355568067, 0.661001531, 0.417602208,0.495093597,0.466323627,0.557995733,0.307493388, 0.69227994, 0.642881521,0.535438691,0.597624993,0.445960806,0.445568795, 0.239768737, 0.439214304,0.363600223,0.521287118,0.267575097,0.503395014, 0.350754263, 0.724103169,0.664221614,0.481413686,0.315936987,0.518137595, 0.13366282, 0.337189177,0.018862085,0.316506018,0.031687856,0.001688128, 0.233618801, 0.150603921,0.108160486,0.074265328,0.06162374,0.071483675, 0.074, 0.011066875, 0.0262,0.049,0.129), ReturnYear=c(1950:2010))

zz$Steph <- NA
zz$Brendan <- NA
zz$Emma <- NA
zz$type = rep(c("E","O"), length(unique(zz$Yr))/2) # Add odd/even identifier

# Add DFO estimates from Steph, Brendan, Emma #
for(j in 1950:2010){
  zz$Steph[zz$Yr==j&zz$Area==12]<-Area12Exploitation$Exploitation[Area12Exploitation$ReturnYear==j]
}

for(j in 1954:2014){
  zz$Brendan[zz$Yr==j&zz$Area==12]<-brendanharvest$harvest.rate[brendanharvest$year==j]
}

for(j in 1954:2016){
  zz$Emma[zz$Yr==j&zz$Area==12]<-emmaharvest$harvest.rate[emmaharvest$year==j]
}

# EA: Delete this later. #
# Plot different catch estimates #
# plot(1,1,"n", ylim=c(0,4000000), xlim=c(1950,2020), bty="n", ylab="", xlab="return year", las=1)
# lines(zz[!is.na(zz$Catch),]$Yr, zz[!is.na(zz$Catch),]$Catch, col=gray(0.6), lwd=1.5)
# lines(zz[!is.na(zz$Catch),]$Yr, (zz[!is.na(zz$Catch),]$Exploitation)*(zz[!is.na(zz$Catch),]$Catch), col=alpha("red", 0.5), lwd=1.5)
# lines(zz[!is.na(zz$Brendan),]$Yr, (zz[!is.na(zz$Brendan),]$Brendan)*(zz[!is.na(zz$Brendan),]$Catch), col=alpha("blue", 0.5), lwd=1.5)
# 

# Plot harvest rate estimates to compare #
setwd(dir.plot)
png(filename="pink_area_12_exploitation_estimates.png", width=1000, height=1000, units="px", pointsize=12, res=200)

plot(1,1, "n", ylim=c(0,1), xlim=c(1950,2020), bty="n", ylab="harvest rate", xlab="return year", main="Area 12 pink exploitation rate estimates", las=1)
lines(zz[!is.na(zz$Exploitation),]$Yr, zz[!is.na(zz$Exploitation),]$Exploitation, col=alpha("black", 0.5), lwd=1.5)
lines(zz[!is.na(zz$Steph),]$Yr, zz[!is.na(zz$Steph),]$Steph, col=alpha("red",0.5), lwd=1.5)
lines(zz[!is.na(zz$Brendan),]$Yr, zz[!is.na(zz$Brendan),]$Brendan, col=alpha("blue",0.5), lwd=1.5)
lines(zz[!is.na(zz$Emma),]$Yr, zz[!is.na(zz$Emma),]$Emma, col=alpha("green",0.8), lwd=2, lty=2)
legend(1950, 1, col=c(alpha("black", 0.6), "red", "blue", "green"), c("Unadjusted", "DFO (Steph)", "DFO (Brendan)", "DFO (Emma)"), lty=c(1,1,1,2), lwd=c(1.5, 1.5, 1.5, 2), bg="white", cex=0.7)

dev.off()

### ---- MAYBE DELETE THIS MAMMOTH --- #
### Use the relationship between calculated and DFO harvest rates
### to extrapolate for years without DFO estimates. 

# Alternative: looking at differences between final catch estimates rather than rates #
# catch <- data.frame(year= c(1950:2017),
#                    catch= zz$Catch,
#                    brendan= c((zz$Brendan*zz$AdjustedEsc)/(1-zz$Brendan)), # Catch according to Brendan's exploitation estimate
#                    bc.rate= c(zz$Brendan),
#                    calculated= c((zz$Exploitation*zz$AdjustedEsc)/(1-zz$Exploitation)), # should be same as zz$catch
#                    calc.rate= c(zz$Exploitation),
#                    log.bc= rep(NA, 68),
#                    log.calc = rep(NA,68),
#                    log.catch = rep(NA, 68),
#                    type = rep(c("E","O"), 34))
# 
# # Take the log of catch estimates #
# catch$log.bc <- log(catch$brendan)
# catch$log.calc <- log(catch$calculated)
# catch$log.catch <- log(catch$catch)
# 
# # Plot log(catch) estiimates over time #
# windows(width=10, height=5)
# par(mfrow=(c(1,2)))
# 
# plot(1,1, "n", ylim=c(-15,16), xlim=c(1950,2020), bty="n", xlab="return year", ylab="log(catch)", las=1)
# lines(catch$year[catch$type=="E"], catch$log.calc[catch$type=="E"], col="red")
# lines(catch$year[catch$type=="E"], catch$log.bc[catch$type=="E"], col="blue")
# text(1961, -12, "EVEN CATCH")
# legend(1950, -5, col=c("red", "blue", gray(0.6)), lty=c(1,1,1), c("Raw catch", "Using adjusted DFO harvest rate"), bg="white", cex=0.7)
# 
# 
# plot(1,1, "n", ylim=c(-15,16), xlim=c(1950,2020), bty="n", xlab="return year", ylab="log(catch)", las=1)
# lines(catch$year[catch$type=="O"], catch$log.calc[catch$type=="O"], col="red")
# lines(catch$year[catch$type=="O"], catch$log.bc[catch$type=="O"], col="blue")
# text(1961, -12, "ODD CATCH")
# 
# # Plotting estimate vs. estimate #
# windows(width=10, height=10)
# par(mfrow=(c(2,2)))
# 
# # Plotting even data #
# #ggplot(catch, aes(calc.rate, bc.rate, colour=year)) + geom_point() # Plotting with point coloured by years
# 
# plot(1,1, "n", xlim=c(-13,20), ylim=c(-10,20), bty="n", xlab="log(unadjusted catch)", ylab="log(Brendan catch)", las=1)
# points(catch$log.catch[catch$type=="E"], catch$log.bc[catch$type=="E"], col=alpha("blue", 0.4), pch=20)
# text(-6, 18, "EVEN RAW VS. DFO-ADJUSTED")
# abline(h=0, lty=2, col=grey(0.6))
# abline(v=0, lty=2, col=grey(0.6))
# 
# 
# # Plotting odd data #
# plot(1,1, "n", xlim=c(-13,20), ylim=c(-10,20), bty="n", xlab="log(unadjusted catch)", ylab="log(Brendan catch)", las=1)
# points(catch$log.catch[catch$type=="O"], catch$log.bc[catch$type=="O"], col=alpha("blue", 0.4), pch=20)
# text(-6, 18, "ODD RAW VS. DFO-ADJUSTED")
# abline(h=0, lty=2, col=grey(0.6))
# abline(v=0, lty=2, col=grey(0.6))
# 
# # There seems to be a fairly tight relationship between log(raw catch) and log(DFO catch).
# # Fit linear model to this relationship to facilitate extrapolation.
# oddcatchmod <- lm(catch$log.bc[catch$type=="O"] ~ catch$log.catch[catch$type=="O"])
# evencatchmod <- lm(catch$log.bc[catch$type=="E"] ~ catch$log.catch[catch$type=="E"])
# 
# # Alternative: fit to relationship between RATES #
# oddratemod <- lm(catch$bc.rate[catch$type=="O"] ~ catch$calc.rate[catch$type=="O"])
# evenratemod <- lm(catch$bc.rate[catch$type=="E"] ~ catch$calc.rate[catch$type=="E"])
# 
# # Re-plot with model fit line #
# # Plotting log(CATCH) estimate vs. log(CATCH) estimate #
# windows(width=10, height=5)
# par(mfrow=(c(1,2)))
# 
# plot(1,1, "n", xlim=c(-13,20), ylim=c(-10,20), bty="n", xlab="log(raw catch)", ylab="log(Brendan catch)", las=1)
# lines(c(-13:20), evencatchmod$coefficients[[1]]+evencatchmod$coefficients[[2]]*c(-13:20))
# points(catch$log.catch[catch$type=="E"], catch$log.bc[catch$type=="E"], col=alpha("blue", 0.4), pch=20)
# text(-6, 18, "EVEN CATCH")
# abline(h=0, lty=2, col=grey(0.6))
# abline(v=0, lty=2, col=grey(0.6))
# 
# plot(1,1, "n", xlim=c(-13,20), ylim=c(-10,20), bty="n", xlab="log(unadjusted catch)", ylab="log(Brendan catch)", las=1)
# lines(c(-13:20), oddcatchmod$coefficients[[1]]+oddcatchmod$coefficients[[2]]*c(-13:20))
# points(catch$log.catch[catch$type=="O"], catch$log.bc[catch$type=="O"], col=alpha("blue", 0.4), pch=20)
# text(-6, 18, "ODD CATCH")
# abline(h=0, lty=2, col=grey(0.6))
# abline(v=0, lty=2, col=grey(0.6))
# 
# # Plotting RATE estimate vs. RATE estimate #
# windows(width=10, height=5)
# par(mfrow=(c(1,2)))
# 
# seq <- seq(0,1,.05)
# 
# plot(1,1, "n", xlim=c(0,1), ylim=c(0,1), bty="n", xlab="Calculated rate", ylab="DFO-adjusted rate", las=1)
# lines(seq, evenratemod$coefficients[[1]]+evenratemod$coefficients[[2]]*seq)
# points(catch$calc.rate[catch$type=="E"], catch$bc.rate[catch$type=="E"], col=alpha("blue", 0.4), pch=20)
# text(.25, .9, "EVEN CATCH")
# 
# plot(1,1, "n", xlim=c(0,1), ylim=c(0,1), bty="n", xlab="Calculated rate", ylab="DFO-adjusted rate", las=1)
# lines(seq, oddratemod$coefficients[[1]]+oddratemod$coefficients[[2]]*seq)
# points(catch$calc.rate[catch$type=="O"], catch$bc.rate[catch$type=="O"], col=alpha("blue", 0.4), pch=20)
# text(.25,.9, "ODD CATCH")
# 
# 
# # The relationship between log(raw CATCH) and log(DFO-adjusted CATCH)
# # is better so we'll use that relationship to extrapolate for years
# # with missing DFO data. 
# zz$extrapolated <- c(NA)
# zz$final <- c(NA)
# catch.temp <- NA
# 
# for (i in 1950:2017){
#   if(zz$type[zz$Yr==i]=="E") {
#     catch.temp <- (exp(evencatchmod$coefficients[[1]]+evencatchmod$coefficients[[2]]*log(zz$Catch[zz$Yr==i])))
#     zz$extrapolated[zz$Yr==i] <- catch.temp/(zz$AdjustedEsc[zz$Yr==i]+catch.temp)
#     }
#   else if (zz$type[zz$Yr==i]=="O") {
#     catch.temp <- (exp(oddcatchmod$coefficients[[1]]+oddcatchmod$coefficients[[2]]*log(zz$Catch[zz$Yr==i])))
#     zz$extrapolated[zz$Yr==i] <- catch.temp/(zz$AdjustedEsc[zz$Yr==i]+catch.temp)
#   }
#   }

# --- END OF SECTION TO DELETE --- #

# I use the most recently updated set of harvest rate estimates as my final estimate #
zz$Final <- NA

for(i in 1950:2017){
    zz$Final[zz$Yr==i] <- zz$Emma[zz$Yr==i]
  }

# Plot catch estimates #
setwd(dir.plot)
png(filename="pink_area_12_catch_estimates.png", width=2000, height= 2000, res=300, units="px", pointsize=12)
plot(1,1, "n", ylim=c(0,3700000), xlim=c(1950,2020), bty="n",yaxt="n", ylab="Estimated catch (millions of fish)", xlab="Return year", main="Area 12 pink catch estimates", las=1)
lines(zz[!is.na(zz$Catch),]$Yr, zz[!is.na(zz$Catch),]$Catch, col=alpha("black", 0.5), lwd=1.5)
lines(zz[!is.na(zz$Steph),]$Yr, zz[!is.na(zz$Steph),]$Steph*zz[!is.na(zz$Steph),]$AdjustedEsc, col=alpha("red",0.5), lwd=1.5)
lines(zz[!is.na(zz$Brendan),]$Yr, zz[!is.na(zz$Brendan),]$Brendan*zz[!is.na(zz$Brendan),]$AdjustedEsc, col=alpha("blue",0.5), lwd=1.5)
lines(zz[!is.na(zz$Emma),]$Yr, zz[!is.na(zz$Emma),]$Emma*zz[!is.na(zz$Emma),]$AdjustedEsc, col=alpha("green",0.8), lwd=2, lty=2)
axis(2, at=seq(0,3700000, 500000), lab=seq(0,3.7,.5), las=1)
legend(1950, 3500000, col=c(alpha("black", 0.5), alpha("blue", 0.5), alpha("red", 0.5), alpha("green", 0.8)), lty=c(1,1,1,2), lwd=c(1.5, 1.5, 1.5, 2), c("Area 12 unadjusted catch", "DFO adjusted (Steph)", "DFO adjusted (Brendan)", "DFO adjusted (Emma)"), bg="white", cex=0.7)

dev.off()

################################################################################
## B. Compiling final spawner-recruit data
################################################################################

#-------------------------------------------------------------------------------
# 1) Read in data
#-------------------------------------------------------------------------------
#z<-read.csv("nuSEDS_PINK.csv") # EA: need to update this #
setwd(dir.gen)
data <- read.csv("updated_nuseds_2017.csv")
data <- filter(data, SPECIES=="Pink")

z <- data %>% 
  select(ANALYSIS_YR, AREA, WATERBODY, MAX_ESTIMATE) 

z <- z[,c(2,3,1,4)]
names(z) <- c("Area", "River", "Yr", "Escapement")

x<-zz # Exploitation data #

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
Z<-data.frame(Yr=yr1, Area=area1, River=river1); Z$Escapement<-NA; Z$Returns <- NA; Z$Recruits <- NA; Z$Expl<-NA;

# c) Add exploitation rates to dataframe
for(a in unique(Z$Area)){
	for(y in 1950:2017){
		Z$Expl[which(Z$Yr==y & Z$Area==a)]<-x$Final[which(x$Yr==y & x$Area==a)]
	}
}

# d) Add the nuSEDs escapement values (takes a minute)
for(i in 1:length(Z$Yr)){
	E<-z$Escapement[which(z$Yr==Z$Yr[i] & z$River==Z$River[i])]; 
	if(length(E)==1) Z$Escapement[i]<-E;
	if(length(E)>1) E<-E[which(E>0)]; if(length(E)!=0) Z$Escapement[i]<-E;
	}

# e) Replace 0s with NAs
Z$Escapement[which(Z$Escapement==0)]<-NA;

# f) Returns estimates Returns = N/(1-u)
Z$Returns<-Z$Escapement/(1-Z$Expl)

# Input recruits for each brood. The recruits of spawners in year t are the returns in year t+2.
for (i in 1950:2015) {
  Z$Recruits[Z$Yr==i] <- Z$Returns[Z$Yr==i+2]
}

# g) Spawner estimates to pair with recruitment (S(t-2) corresponds to R(t))
# EA: Code below has been commented out so as to index by brood year (spawners) instead of return year.
# Now, S(t) corresponds to R(t-2)

# Z$S<-NA; Z$S[3:length(Z$S)]<-Z$Escapement[1:(length(Z$S)-2)];
# Z$S[which(Z$Yr==1960 | Z$Yr==1961)]<-NA;

# h) Calculate survival as log(R/S)
Z$Survival <- c(NA)
Z$Survival <- log(Z$Recruits/Z$Escapement)

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
# 4) Specify odd and even year populations within the same river
#-------------------------------------------------------------------------------

ZZ$EO<-NA; ZZ$EO[which(ZZ$Yr %% 2 ==0)]<-"E"; ZZ$EO[which(ZZ$Yr %% 2 ==1)]<-"O";

# Populations
ZZ$Popn<-NA; N<-unique(ZZ$River); j<-1;
for(i in 1:length(N)){
	ZZ$Popn[which(ZZ$River==N[i] & ZZ$EO=="O")]<-j; j<-j+1;
	ZZ$Popn[which(ZZ$River==N[i] & ZZ$EO=="E")]<-j; j<-j+1;
	}
ZZ$Popn<-factor(ZZ$Popn)

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

#Separate out even and odd year populations 
ZZ$Population<-as.factor(paste(ZZ$River, ZZ$EO, sep=" "))

#Only keep those entries with estimates of survival
# Z1<-subset(ZZ, is.na(ZZ$Survival)==FALSE)

# cat("Final dataset: \n Total number of populations (even/odd): ", length(unique(Z1$Population)), "\n Total number of S-R pairs: ", dim(Z1)[1], "\n Total number of rivers: ", length(unique(Z1$River)))
cat("Final dataset: \n Total number of populations (even/odd): ", length(unique(ZZ$Population)), "\n Total number of S-R pairs: ", dim(ZZ)[1], "\n Total number of rivers: ", length(unique(ZZ$River)))

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
data.pink=ZZ
write.csv(data.pink, "Pink_stock_recruit_unfiltered_2018.csv") 


