## Script: Salmon Data Analysis - Recruit
## 
## DESCRIPTION: This script estimates recruits from spawners, returns and exploitation rate 
##   at the river level for chum, pink even and odd years salmon species
##
##
## Last edited: 2023-07-20
## Author: Joao Braga (Ecofish Research Ltd. - Victoria)

# Data Sources ----
# * PSF - From Eric Hertz: ----
# ** SCVI_age_2019-04-13.csv -> CU-level data for area 13 and WCVI pink and chum 
# ** NuSEDS 
# ** Conservation units

#Clear workspace
# rm(list = ls())

#Required packages
list.of.packages <- c("tidyverse", "extrafont", "lubridate", "sf", "readxl", "data.table")

#What packages need to be installed?
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]

# Install missing packages
if (length(new.packages)) {
  install.packages(new.packages, dependencies = TRUE, repos = c("http://cran.rstudio.com/", "http://R-Forge.R-project.org"))
}

# load libraries now
lapply(list.of.packages, require, character.only = TRUE)

#### Read in CU - level Data ----
# Age table for DFO area 13
# It includes Exploitation rates for CM (10 and 11) (and age), PKO (2,3,4,5 and 7) and PKE (1 and 4)
age_tb_13 <- read_csv("Data/From PSF/SCVI_age_2019-04-13.csv") %>%
  filter(!is.na(Escape) & !is.na(Total), SpeciesId %in% c("CM", "PKE", "PKO")) %>%
  mutate(ER_source = "Data/From PSF/SCVI_age_2019-04-13.csv",
         Age_source = "Data/From PSF/SCVI_age_2019-04-13.csv")

# Age Table, Total Stock and Exploitation for South Coast BC (Data from Pieter Van Will) - For CM only
# Only AT CU/DFO level. We'll use age and ER from these data
# Age table (notice that it only starts at 1958)
age_CM_SC <- read_excel(path = "Data/From PSF/AgeComp(2018).xlsx", sheet = "AgeComp(2018)") %>%
  mutate(Age_source = "Data/From PSF/AgeComp(2018).xlsx")

# Explotation rates per DFO management area and CU
ER_CM_SC <- read_excel(path = "Data/From PSF/CU based Return year Reconstructions.xlsx", sheet = "Reconstruction", range = "A106:BO123")
names(ER_CM_SC) <- c("CU_NAME", "AREA", 1954:2018) # Start at 1954
ER_CM_SC <- reshape2::melt(data = ER_CM_SC, id.vars = c("CU_NAME", "AREA"))
names(ER_CM_SC) <- c("CU_NAME", "AREA","Year", "Total.ER")

ER_CM_SC <- ER_CM_SC %>%
  mutate(ER_source =  "Data/From PSF/CU based Return year Reconstructions.xlsx")

# ER_CM_SC %>% group_by(CU_NAME, AREA) %>% 
#  summarise(obs = n(), yrs = min(as.numeric(as.character(Year))), yrs_mx = max(as.numeric(as.character(Year))))

# Merging ER with Age information
Age_ER_CM_SC <- ER_CM_SC %>%
  as_tibble(.) %>%
  mutate(Year = as.numeric(as.character(Year))) %>%
  left_join(., age_CM_SC, by = "Year") %>%
  rename(BroodYear = Year) %>% # For sake of consistency among names
  mutate(SpeciesId = "CM")     # For sake of consistency among names

#### NuSEDS data base ----
# NuSEDS <- read.csv("Data/NuSEDS/NuSEDS_20200720.csv")
NuSEDS <- read.csv("Data/NuSEDS/NuSEDS_20220309.csv") %>%
  mutate(Stock_source = "Data/NuSEDS/NuSEDS_20220309.csv")

CU_data <- read.csv("Data/NuSEDS/Conservation_Unit_Data.csv")

# ** Adding CU information not present in NuSEDS per spp and reduce table to relevant variables ----
PKE_CU_data <- CU_data %>%
  as_tibble() %>%
  filter(SPECIES_QUALIFIED  == "PKE")  %>%
  select(WATERSHED_CDE, POP_ID,AREA, CU_INDEX, SPECIES_QUALIFIED, GEOGRAPHICAL_EXTNT_OF_ESTIMATE, CU_NAME) %>%
  unique() 

PKO_CU_data <- CU_data %>%
  filter(SPECIES_QUALIFIED  == "PKO") %>%
  select(WATERSHED_CDE, POP_ID, AREA, CU_INDEX, SPECIES_QUALIFIED, GEOGRAPHICAL_EXTNT_OF_ESTIMATE, CU_NAME) %>%
  unique() 

CM_CU_data <- CU_data %>%
  filter(SPECIES_QUALIFIED  == "CM") %>%
  select(WATERSHED_CDE, POP_ID, AREA, CU_INDEX, SPECIES_QUALIFIED, GEOGRAPHICAL_EXTNT_OF_ESTIMATE, CU_NAME) %>%
  unique()

# ** Subsetting NuSEDS per species ----
CM_NuSEDS <- NuSEDS %>%
  filter(SPC_ID  == 4)  

PKE_NuSEDS <- NuSEDS %>%  
  filter(SPC_ID  == 3)  %>%
  filter(ANALYSIS_YR %% 2 == 0 ) # Filter by Even years

PKO_NuSEDS <- NuSEDS %>%  
  filter(SPC_ID  == 3)  %>%
  filter(ANALYSIS_YR %% 2 == 1 ) # Filter by Odd years

# ** Merging CU info to CM NuSEDS ----
add.CU.NuSEDS <- function(CU.DATA, NuSEDS.DATA) {
  res <-  NuSEDS.DATA %>%
    full_join(., CU.DATA, by = c("WATERSHED_CDE", "POP_ID")) %>%
    select(GFE_ID, ANALYSIS_YR, GEOGRAPHICAL_EXTNT_OF_ESTIMATE, WATERBODY, SPECIES_QUALIFIED, SPC_ID, WATERSHED_CDE, AREA.x, AREA.y, CU_INDEX, MAX_ESTIMATE, CU_NAME, Stock_source) %>%
    mutate(StatArea.CU = paste0(SPECIES_QUALIFIED, "-", CU_INDEX)) %>%
    rename(BroodYear = ANALYSIS_YR) %>%
    select(-AREA.x) %>%
    rename(AREA = AREA.y)
  
  return(res)
}

CM_CU_NuSEDS_rd <- add.CU.NuSEDS(CU.DATA = CM_CU_data, NuSEDS.DATA = CM_NuSEDS)

PKO_CU_NuSEDS_rd <- add.CU.NuSEDS(CU.DATA = PKO_CU_data, NuSEDS.DATA = PKO_NuSEDS)

PKE_CU_NuSEDS_rd <- add.CU.NuSEDS(CU.DATA = PKE_CU_data, NuSEDS.DATA = PKE_NuSEDS)

rm(CU_data)
rm(NuSEDS)

#### Estimating recruits DFO area with available information CU level information and NuSEDS ----
# There are 9 CUs that we have data available, but no recruits calculated by PSF. 
# I assumed age structure (For CM) and ER (CM, PKO and PKE) to be similar for rivers within same CU

# * For PKO ----
# Exploitation rate Data from CUs 2, 3, 4, 5, 7
PKO_target_CU <- c("PKO-2", "PKO-3", "PKO-4", "PKO-5", "PKO-7")

unique(age_tb_13$StatArea.CU)

PKO_age_tb_13 <- age_tb_13 %>%   # Contains Total exploitation rate for Pink
  mutate(StatArea.CU = gsub("(?<![0-9])0+", "", StatArea.CU, perl = TRUE)) %>%
  filter(StatArea.CU %in% PKO_target_CU)

# Expanding Database from 1950:2020 to have same number of year across waterbodies
PKO_dat <- PKO_CU_NuSEDS_rd %>%    # Combine information
  as_tibble() %>%
  filter(StatArea.CU %in% PKO_target_CU) %>%
  select(GFE_ID, WATERBODY, BroodYear, WATERSHED_CDE, SPECIES_QUALIFIED, StatArea.CU, AREA, CU_NAME, MAX_ESTIMATE, ends_with("_source")) %>%
  group_by(GFE_ID, WATERBODY, WATERSHED_CDE, SPECIES_QUALIFIED, StatArea.CU, CU_NAME, AREA) %>%
  complete(BroodYear = seq(1951, to = 2019, by = 2)) %>%
  ungroup() %>%
  # Adding explotation rates
  left_join(., PKO_age_tb_13, by = c("StatArea.CU", "BroodYear")) %>%
  arrange(., GFE_ID, BroodYear) %>%
  filter(WATERSHED_CDE != "")  %>%       # 5 Waterbodies without watershed code removed. There's no way to link to ECA variables (watershed level).
  filter(BroodYear >= 1950)


# ** Estimation starts here: ----
#' This assumes that the only mortality is only due to fisheries.
#' 
PKO_db <- PKO_dat %>%
  mutate(Returns = MAX_ESTIMATE / (1 - Total.ER), 
         Recruits = NA)


#'JFB: We assume no marine mortality. Thus all returns in year t were the recruits in year t - 2 (pink)
for(R in unique(PKO_db$WATERBODY)){
  for(WS in c(unique(PKO_dat[PKO_dat$WATERBODY %in% R, "WATERSHED_CDE"]))[[1]]) {
    for(y in unique(PKO_db$BroodYear)) {
      if(y == 2019) next
      PKO_db$Recruits[which(PKO_db$WATERBODY == R & PKO_db$WATERSHED_CDE == WS & PKO_db$BroodYear  == y )] <- PKO_db$Returns[which(PKO_db$WATERBODY == R & PKO_db$WATERSHED_CDE == WS & PKO_db$BroodYear  == (y + 2))]
    }
  }
}


# * For PKE ----
# Pulling data from CU 1,4
PKE_target_CU <- c("PKE-1", "PKE-4")

PKE_age_tb_13 <- age_tb_13 %>%   # Contains Total exploitation rate for pink
  mutate(StatArea.CU = gsub("(?<![0-9])0+", "", StatArea.CU, perl = TRUE)) %>%
  filter(StatArea.CU %in% PKE_target_CU)

# Expanding Database
PKE_dat <- PKE_CU_NuSEDS_rd %>%    # Combine information
  as_tibble() %>%
  filter(StatArea.CU %in% PKE_target_CU) %>%   
  select(GFE_ID, WATERBODY, BroodYear, WATERSHED_CDE, SPECIES_QUALIFIED, StatArea.CU, AREA, CU_NAME, MAX_ESTIMATE, ends_with("_source")) %>%
  group_by(GFE_ID, WATERBODY, WATERSHED_CDE, SPECIES_QUALIFIED, StatArea.CU, CU_NAME, AREA) %>%
  complete(BroodYear = seq(1950, to = 2020, by = 2)) %>%
  ungroup() %>%
# Adding the explotation rate  
  left_join(., PKE_age_tb_13, by = c("StatArea.CU", "BroodYear")) %>%   #JFB: causing NAs. Needs to be clean.
  arrange(., GFE_ID, BroodYear)   %>%
  filter(BroodYear >= 1950)

PKE_dat %>% group_by(WATERBODY ) %>% summarise(WB_c = n_distinct(WATERSHED_CDE)) %>% filter(WB_c != 1)

unique(PKE_dat %>% group_by(WATERBODY, StatArea.CU) %>%  filter(WATERBODY == "GRAY CREEK") %>% pull(WATERSHED_CDE))
unique(PKE_dat %>% group_by(WATERBODY, StatArea.CU) %>%  filter(WATERBODY == "SEYMOUR RIVER") %>% pull(WATERSHED_CDE))

# ** Estimation starts here: ----
PKE_db <- PKE_dat %>%
  mutate(Returns = MAX_ESTIMATE / (1 - Total.ER), 
  Recruits = NA)  

for(R in unique(PKE_db$WATERBODY)){
  for(WS in c(unique(PKE_dat[PKE_dat$WATERBODY %in% R, "WATERSHED_CDE"]))[[1]]) {
    for(y in unique(PKE_db$BroodYear)) {
      if(y == 2020) next
      PKE_db$Recruits[which(PKE_db$WATERBODY == R & PKE_db$WATERSHED_CDE == WS & PKE_db$BroodYear  == y)] <- PKE_db$Returns[which(PKE_db$WATERBODY == R & PKE_db$WATERSHED_CDE == WS & PKE_db$BroodYear  == y + 2)] 
    }
  }
}

# * For CM ----
CM_missing_CU <- c("CM-10", "CM-11")

CM_age_tb_13 <- age_tb_13 %>% # Contains Total Escapement, Exploitation rate and age tables for Chum # per CU - however Age6 and Age7 columns are empty
  mutate(StatArea.CU = gsub("(?<![0-9])0+", "", StatArea.CU, perl = TRUE)) %>%
  filter(StatArea.CU %in% CM_missing_CU)

CM_dat <- CM_CU_NuSEDS_rd %>%
  as_tibble() %>%
  filter(StatArea.CU %in% CM_missing_CU) %>%
  select(GFE_ID, WATERBODY, BroodYear, WATERSHED_CDE, SPECIES_QUALIFIED, StatArea.CU, AREA, CU_NAME, MAX_ESTIMATE, ends_with("_source")) %>%
  group_by(GFE_ID, WATERBODY, WATERSHED_CDE, SPECIES_QUALIFIED, StatArea.CU, CU_NAME, AREA) %>%
  complete(BroodYear = seq(1950, to = 2020, by = 1)) %>%
  ungroup() %>%
  left_join(., CM_age_tb_13, by = c("StatArea.CU", "BroodYear")) %>%   
  arrange(., GFE_ID, BroodYear) 


# ** Estimation starts here: ----
# Removing any duplicates
nrow(CM_dat)

CM_db <- CM_dat %>%
  group_by(GFE_ID, WATERBODY, BroodYear, WATERSHED_CDE, SPECIES_QUALIFIED, StatArea.CU, CU_NAME, AREA, Total.ER, Age3, Age4, Age5, Age6, Stock_source, Age_source, ER_source) %>% # Age 6 all NAs
  summarise( flag = ifelse(n() > 1, TRUE, FALSE),
    MAX_ESTIMATE = ifelse(sum(is.na(MAX_ESTIMATE)) == n(), NA, sum(MAX_ESTIMATE, na.rm = TRUE)) ) %>%  
  ungroup() %>%
  mutate(Recruits = NA) %>%
  filter(BroodYear >= 1950)

nrow(CM_db)

CM_db <- CM_db %>%
  mutate(Returns = MAX_ESTIMATE / (1 - Total.ER))

unique(CM_db$Age6) # All NAs

for(R in unique(CM_db$WATERBODY)){
  for(y in 1950:2020) {
    if(y %in% c(2015:2020)) next
    
    CM_db$Recruits[which(CM_db$WATERBODY == R & CM_db$BroodYear  == y)] <- {
      (CM_db$Returns[which(CM_db$WATERBODY == R & CM_db$BroodYear  == y + 3)] * CM_db$Age3[which(CM_db$WATERBODY == R & CM_db$BroodYear  == y + 3)]) +
        (CM_db$Returns[which(CM_db$WATERBODY == R & CM_db$BroodYear  == y + 4)] * CM_db$Age4[which(CM_db$WATERBODY == R & CM_db$BroodYear  == y + 4)]) +
        (CM_db$Returns[which(CM_db$WATERBODY == R & CM_db$BroodYear  == y + 5)] * CM_db$Age5[which(CM_db$WATERBODY == R & CM_db$BroodYear  == y + 5)]) 
      
    } 
  }
}

#### Estimating recruits Pieter Reconstruction with available information CU level ----
# For CU areas: CM 4 to CM 9
# I assumed age structure (For CM) to be the same for all of these CUs and ER to be similar for same DFO/Stat Area
# * For CM ----
CM_missing_CU <- paste0("CM-", 4:9)

Age_ER_CM_SC <- Age_ER_CM_SC %>%   # Age6 has data in it
  mutate(CU_NAME = toupper(CU_NAME),
         AREA =  gsub(pattern = "Area ",replacement =  "",x = AREA)) %>%
  mutate(CU_NAME = gsub(pattern = "NORTH EAST",replacement = "NORTHEAST", x = CU_NAME))

# unique(Age_ER_CM_SC$CU_NAME)  # CU of intest are the first 5
# unique((CM_CU_NuSEDS_rd %>% filter(StatArea.CU %in% CM_missing_CU))$CU_NAME)
# unique((CM_CU_NuSEDS_rd %>% filter(StatArea.CU %in% CM_missing_CU))$CU_NAME) %in% unique(Age_ER_CM_SC$CU_NAME)
CM_dat_CU59 <- CM_CU_NuSEDS_rd %>%
  as_tibble() %>%
  filter(StatArea.CU %in% CM_missing_CU) %>%
  group_by(GFE_ID, WATERBODY, WATERSHED_CDE,GEOGRAPHICAL_EXTNT_OF_ESTIMATE , SPECIES_QUALIFIED , SPC_ID, StatArea.CU, AREA, CU_INDEX, CU_NAME, Stock_source) %>%
  complete(BroodYear = seq(1950, to = 2020, by = 1)) %>%
  ungroup() %>%
  
  mutate(AREA = gsub(x = AREA, pattern = "[A-Z]", replacement = "") ) %>%
  
  left_join(., Age_ER_CM_SC, by = c("CU_NAME", "AREA","BroodYear")) %>%
  arrange(., GFE_ID, BroodYear) 

# Waterbody  has unique shed codes. using waterbody instead to estimate returns
CM_dat_CU59 %>% group_by(WATERBODY) %>% summarise(WB_c = n_distinct(WATERSHED_CDE)) %>% filter(WB_c != 1)

CM_dat_CU59 %>% group_by(WATERBODY, WATERSHED_CDE) %>% summarise(WB_c = n_distinct(BroodYear)- n()) %>% filter(WB_c != 0)

# ** Estimation starts here: ----
# Removing any duplicates
nrow(CM_dat_CU59)
CM_db_CU59 <- CM_dat_CU59 %>%
  group_by(GFE_ID, WATERBODY, BroodYear, WATERSHED_CDE, SPECIES_QUALIFIED, StatArea.CU, CU_NAME, AREA, Total.ER, Age3, Age4, Age5, Age6, Stock_source, Age_source, ER_source) %>% # Age 6 all NAs
  summarise(flag = ifelse(n() > 1, TRUE, FALSE),
            MAX_ESTIMATE = ifelse(sum(is.na(MAX_ESTIMATE)) == n(), NA, sum(MAX_ESTIMATE, na.rm = TRUE))) %>%  
  ungroup() %>%
  mutate(Recruits = NA) %>%
  filter(BroodYear >= 1950)
nrow(CM_db_CU59)

CM_db_CU59 <- CM_db_CU59 %>%
  mutate(Returns = MAX_ESTIMATE / (1 - Total.ER))

# Contains more than NAs
unique(CM_db_CU59$Age6)

CM_db_CU59 <- CM_db_CU59 %>%
  mutate(Age6 = ifelse(!is.na(Age3) & is.na(Age6), 0, Age6))  # If there's age data, but Age6 is NA, assume zero.

# Estimating recruits
for(R in unique(CM_db_CU59$WATERBODY)){
  for(y in 1950:2020) {
    if(y %in% c(2015:2020)) next
    CM_db_CU59$Recruits[which(CM_db_CU59$WATERBODY == R & CM_db_CU59$BroodYear  == y)] <- {
      CM_db_CU59$Returns[which(CM_db_CU59$WATERBODY == R & CM_db_CU59$BroodYear  == y + 3)] * CM_db_CU59$Age3[which(CM_db_CU59$WATERBODY == R & CM_db_CU59$BroodYear  == y + 3)] +
        CM_db_CU59$Returns[which(CM_db_CU59$WATERBODY == R & CM_db_CU59$BroodYear  == y + 4)] * CM_db_CU59$Age4[which(CM_db_CU59$WATERBODY == R & CM_db_CU59$BroodYear  == y + 4)] +
        CM_db_CU59$Returns[which(CM_db_CU59$WATERBODY == R & CM_db_CU59$BroodYear  == y + 5)] * CM_db_CU59$Age5[which(CM_db_CU59$WATERBODY == R & CM_db_CU59$BroodYear  == y + 5)] + 
        CM_db_CU59$Returns[which(CM_db_CU59$WATERBODY == R & CM_db_CU59$BroodYear  == y + 6)] * CM_db_CU59$Age6[which(CM_db_CU59$WATERBODY == R & CM_db_CU59$BroodYear  == y + 6)]
    } 
  }
}

# Rbind with previous CM estimation
CM_db_comp <- bind_rows(CM_db, CM_db_CU59)

CM_db_comp %>% distinct(StatArea.CU)

CM_CU_NuSEDS_rd %>% distinct(StatArea.CU)

# Saving outputs ----
summary(CM_db_comp)
CM_db_comp <- CM_db_comp %>% arrange(WATERBODY, BroodYear)
write_csv(x = CM_db_comp, file = "Data/Processed/CM_recruits_DFO13_source.csv")

summary(PKO_db)
PKO_db <- PKO_db %>% arrange(WATERBODY, BroodYear)
write_csv(x = PKO_db, file = "Data/Processed/PKO_recruits_DFO13_source.csv")

summary(PKE_db)
PKE_db <- PKE_db %>% arrange(WATERBODY, BroodYear)
write_csv(x = PKE_db, file = "Data/Processed/PKE_recruits_DFO13_source.csv")

# End ----