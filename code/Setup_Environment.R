## ASHA Migration Special Issues Brief
## Winter 2023

## Create seperate runs by age (75-1yr, 75 5-yr, 60_70 1-yr) and geography (State, CBSA)
## First pass: by state; 1-year data
## Second pass: by state; 5-year data
## Third pass: by cbsa; 1-year data (ELIMINATE; MOE too high)
## Fourth pass: by cbsa; 5-year data


######################################
####### Packages #####################
######################################


##install.packages("tidyverse")
library(tidycensus)
library(tidyverse)
options(scipen = 999)
library(scales)
library(tigris)
options(tigris_use_cache = TRUE)
library(magrittr)
## install.packages("tmap")
library(tmap)
install.packages("ggrepel")
library(ggrepel)
library(maps)
install.packages("censusapi")
library(censusapi)


##install.packages("devtools")
##devtools::install_github("hrecht/censusapi")
##library(censusapi)


census_api_key("1852cfa09349d41909bfbf393344c5bf9b29410a", install=TRUE, overwrite = TRUE)
# Add key to .Renviron
Sys.setenv(CENSUS_KEY="1852cfa09349d41909bfbf393344c5bf9b29410a")
# Reload .Renviron
readRenviron("~/.Renviron")
# Check to see that the expected key is output in your R console
Sys.getenv("CENSUS_KEY")



getwd()
setwd("C:/Users/FrankRockwood/Desktop/ASHA_Project/WorkingDirectory")
