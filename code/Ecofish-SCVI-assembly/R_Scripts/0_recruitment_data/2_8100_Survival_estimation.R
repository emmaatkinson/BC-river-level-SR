## ---------------------------
## Project:   8100 - Phase 5 publication
##
## Script name: 8100_Survival_estimation.R
##
## Purpose of script: Estimate survival for each salmon population across BC watersheds through time
##
## Author: Joao Braga
##
## Date Created: 2020-09-25
##
## Ecofish Research (Victoria Office), 2020
## Email: jbraga@ecofishresearch.com
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------
##
## Data sources: ----
##   * From Eric Hertz
##    ** NCC_streams_SR_data.csv: S R for majority of BC watersheds

##   * From 8100_Salmon)estimation_CU.R 
##    ** CM_recruits_DFO13.csv:  CM  R estimation from NuSEDS for CUs (and rivers therein) CM - 10 & 11
##    ** PKO_recruits_DFO13.csv: PKO R estimation from NuSEDS for CUs (and rivers therein) PKO- 2,3,4,5 & 7
##    ** PKE_recruits_DFO13.csv: PKE R estimation from NuSEDS for CUs (and rivers therein) PKE- 1 & 4
## ---------------------------

# rm(list= ls())

## Required packages
list.of.packages <- c("tidyverse", "extrafont", "lubridate", "ggplot2")

## Install missing packages
if (length(new.packages)) {
  install.packages(new.packages, dependencies = TRUE, repos = c("http://cran.rstudio.com/", "http://R-Forge.R-project.org"))
}

## Loading libraries
lapply(list.of.packages, require, character.only = TRUE)

# Loading data ----
# * From PSF
PSF_export <- read_csv("Data/From PSF/NCC_streams_SR_data.csv") %>%
  mutate(Stock_source = "Data/From PSF/NCC_streams_SR_data.csv",
         ER_source = "Data/From PSF/NCC_streams_SR_data.csv",
         Age_source = "Data/From PSF/NCC_streams_SR_data.csv")

# * From NuSEDS estimation
CM <- read_csv("Data/Processed/CM_recruits_DFO13_source.csv") %>%
  select(GFE_ID, BroodYear, WATERBODY, SPECIES_QUALIFIED, AREA, StatArea.CU, MAX_ESTIMATE, Returns, Recruits, ends_with("_source")) %>%
  rename(River = WATERBODY,
         Species = SPECIES_QUALIFIED,
         StatArea = AREA,
         CU =  StatArea.CU,
         Spawners = MAX_ESTIMATE) %>%
  mutate(StatArea = as.character(StatArea))

PKO <- read_csv("Data/Processed/PKO_recruits_DFO13_source.csv") %>%
  select(GFE_ID, BroodYear, WATERBODY, SPECIES_QUALIFIED, AREA, StatArea.CU, MAX_ESTIMATE, Returns, Recruits, ends_with("_source")) %>%
  rename(River = WATERBODY,
         Species = SPECIES_QUALIFIED,
         StatArea = AREA,
         CU =  StatArea.CU,
         Spawners = MAX_ESTIMATE)

PKE <- read_csv("Data/Processed/PKE_recruits_DFO13_source.csv") %>%
  select(GFE_ID, BroodYear, WATERBODY, SPECIES_QUALIFIED, AREA, StatArea.CU, MAX_ESTIMATE, Returns, Recruits, ends_with("_source")) %>%
  rename(River = WATERBODY,
         Species = SPECIES_QUALIFIED,
         StatArea = AREA,
         CU =  StatArea.CU,
         Spawners = MAX_ESTIMATE)


# Merging all available information ----
salmon.dat <- bind_rows(CM, PKO, PKE)
nrow(salmon.dat)

nrow(PSF_export)

salmon.dat <- salmon.dat %>%
  bind_rows(., PSF_export)

#### Recruit per spawner estimation (survival) ----
Spp <- c("CM", "PKE", "PKO")

Salmon_NCC_stream <- salmon.dat %>%
  mutate(ln_RS = log(Recruits/Spawners),
         recruitPerSpawner = Recruits/Spawners) 

write_csv(Salmon_NCC_stream, "Data/Processed/Salmon_RS_river_CU_FULL_Data_source.csv")

Salmon_NCC_stream <- salmon.dat    %>%
  filter(!is.na(Spawners) & !is.na(Recruits)) %>%
  mutate(ln_RS = log(Recruits/Spawners),
         recruitPerSpawner = Recruits/Spawners) %>%
  group_by(CU, River) %>%
  mutate(N_Years = n_distinct(BroodYear),
         StartYear = min(BroodYear),
         EndYear = max(BroodYear)) 

write_csv(Salmon_NCC_stream, "Data/Processed/Salmon_RS_river_CU_souce.csv")

# QA Tables ----
# Filter by DFO areas of interest (1:15 and 20:27)
DFO_area <- c(1:15, 20:27)
Spp <- c("CM", "PKE", "PKO")

SALMON_QA <- Salmon_NCC_stream %>%
  filter(Species %in% Spp) %>%
  mutate(DFO = as.numeric(gsub("([0-9]+).*$", "\\1", StatArea))) %>% # Some DFO have Letters representing subdivisions
  # filter(DFO %in% DFO_area) %>%
  group_by(Species, River, DFO, CU) %>%
  summarise(N_Years = n_distinct(BroodYear),
            Age_source = unique(Age_source),
            ER_source = unique(ER_source),
            Stock_source = unique(Stock_source)) %>%
  ungroup() %>%
  rename(STOCK_NAME = River) %>%
  mutate(Suf_N_Years = if_else(N_Years >= 10, true = TRUE, false = FALSE),
         DFO_interest = if_else(DFO %in% DFO_area, true = TRUE, false = FALSE))

write_csv(SALMON_QA, file =  "R_Output/Salmon_Sum.csv")

SALMON_QA %>% 
  group_by(Species) %>%
  filter(DFO_interest & Suf_N_Years) %>%
  summarise(Years = mean(N_Years),
            N_pops = n_distinct(STOCK_NAME),
            CUs = paste0(CU, collapse = ","))



# end ----