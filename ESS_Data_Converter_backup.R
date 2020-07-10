{
library(foreign)
library(sets)
library(dplyr)
library(haven)
library(ggplot2)
library(expss)
library(grid)
library(gridExtra)
library(gdata)
library(splitstackshape)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
} # Load in libraries

{
  setwd("~/ESS_Data/Timing_of_Life")
  
  ess9 <- as.data.frame(read.spss("ESS9.sav"))
  
  which(colnames(ess9)=="evpdemp") #167 - Start of Timing of Life
  which(colnames(ess9)=="plnftr")  #202 - End of Timing of Life
  tol9 <- ess9[167:202]
  
  
  ess3 <- as.data.frame(read.spss("ESS3.sav"))
  
  which(colnames(ess3)=="evpdemp") #182 - Start of Timing of Life
  which(colnames(ess3)=="svclvo")  #236 - End of Timing of Life
  tol3 <- ess3[182:236]
  
  relevant <- intersect(colnames(tol3),colnames(tol9))
  
  tol3 <- select(tol3, all_of(relevant))
  tol9 <- select(tol9, all_of(relevant))
  
  ##### Add weights and year #####
  
  tol3 <- mutate(tol3, year = 2006, dweight = ess3$dweight, pweight = ess3$pweight, gender = ess3$gndr,
                 cntry = ess3$cntry, agea = ess3$agea, ballot = ess3$icsbfm, yrbrn = ess3$yrbrn, edu = ess3$eisced)

  tol9 <- mutate(tol9, year = 2018, dweight = ess9$dweight, pweight = ess9$pweight, gender = ess9$gndr,
                 cntry = ess9$cntry, agea = ess9$agea, ballot = ess9$admge, yrbrn = ess9$yrbrn, edu = ess9$eisced)
  
  tol <- rbind(tol3, tol9) #Combine the data into a single data frame
  
  write_sav(tol, "timing_of_life.sav") #Save as spss file
  
  #write_sav(tol, "ESS-app/data/tol.sav") #Save as spss file into app directory
} # SPSS_convert

rm(list = ls()) # Clear work environment

{
  tol <- as.data.frame(read.spss("timing_of_life.sav")) %>% 
    select(agea,yrbrn,gender,cntry,ballot,iagpnt,tygpnt,tochld,year,dweight,pweight,edu)
  
  factor(tol$cntry, levels = sort(unique(tol$cntry)))
  
  backup <- tol # Dummy for messing about with tol
  
  tol$year <- as.factor(tol$year)
  tol$agea <- as.numeric(as.character(tol$agea))
  tol$yrbrn <-as.numeric(as.character(tol$yrbrn))
  
  # Recode variables (ONLY RUN THIS ONCE)
  
  tol$ballot <- drop.levels(
    recode(tol$ballot, "Ask about girls, women"~1, "Ask about boys, men" ~ 2, "Group 1"~1, "Group 2" ~ 2))
  tol$cntry <- drop.levels(
    recode(tol$cntry,  "Austria" ~ "AT", "Belgium" ~ "BE", "Bulgaria" ~ "BG", "Switzerland"~ "CH",
           "Cyprus" ~ "CY",  "Germany" ~ "DE", "Denmark"  ~ "DK", "Estonia"    ~ "EE",
           "Spain" ~ "ES",   "Finland" ~ "FI", "France" ~"FR",    "United Kingdom" ~"UK",
           "Hungary"~"HU",   "Ireland"~"EI",   "Netherlands" ~"NL",
           "Norway"~"NO",    "Poland"~"PL",    "Portugal"~"PT",   "Russian Federation" ~ "RU",
           "Sweden"~"SE",    "Slovenia"~"SL",  "Slovakia"~"SK",   "Ukraine"~"UA",
           "Czechia"~"CZ",   "Italy"~"IT",     "Serbia"~"RS"))
  
  tol$edu <- drop.levels(
    recode(tol$edu, "Not possible to harmonise into ES-ISCED"~0, 
           "ES-ISCED I , less than lower secondary"~1,
           "ES-ISCED II, lower secondary" ~ 2,
           "ES-ISCED IIIa, upper tier upper secondary"~3,
           "ES-ISCED IIIb, lower tier upper secondary"~4,
           "ES-ISCED IV, advanced vocational, sub-degree"~5,
           "ES-ISCED V1, lower tertiary education, BA level"~6,
           "ES-ISCED V2, higher tertiary education, >= MA level"~7,
           "No answer"~0,
           "Refusal"~0,
           "Don't know"~0,
           "Other"~0))
  
  tol$gender[(which(tol$gender == "No answer"))] <- NA
  tol$gender <- drop.levels(tol$gender)
  
  tol$iagpnt <- as.numeric(as.character(tol$iagpnt)) # Here I treat "No ideal age" as missing data (for now)
  tol$tygpnt <- as.numeric(as.character(tol$tygpnt))
  tol$tochld <- as.numeric(as.character(tol$tochld))
  
  #tol <- na.omit(tol) # Lose ~1/4th of my data!
  
  tol$cohort <- tol$yrbrn
  tol$cohort <- as.factor(recode(tol$yrbrn, 1900:1959 ~ 3, 1960:1989 ~ 2, 1990:2020 ~ 1))
  
  write_sav(tol, "ESS-app/data/tol.sav") #Save as spss file into app directory
  
  setwd("~/ESS_Data/Timing_of_Life/ESS-app")
} # tol

rm(list = ls()) # Clear work environment

