library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
library(sp)
library(rgdal)
library(tidyr)


rm(list = ls())
options(stringsAsFactors = FALSE) 
options(scipen=999)

setwd("/Users/zurich/Documents/TEMP-FILES/lbp_project/final_lbp")
source("r_functions/fn_load_areal_unit.R")
source("r_functions/fn_create_auckland_df.R")
source("r_functions/fn_carg.R")


# CODE SYNOPSIS
# Read Consents
# Read Dwellings
# Join Consents with Dwellings
# Groom matched consents (remove NULLs, filter in 2011 and 2015 only)
# CREATE spatial data.frame
# ASSIGN to areal_unit
# CREATE indicator for Auckland
# CALCULATE Summaries
# SUBSET and save the Auckland Consent Polygons

# ======================
# Read Consents information
# read in 945,921 records
df_consents <- read.csv("data/consent_data/consents.csv", header = FALSE)
names(df_consents) <- c("fk", "type", "value", "date", "desc", "guid")
df_consents$guid <- NULL
nrow(df_consents) == 945921
# convert to date
df_consents$date <- as.Date(df_consents$date, "%Y-%m-%d")

# ======================
# Read Dwellings information
# read in 1,511,685 records
df_dwellings <- read.csv("data/consent_data/dwellings.csv", header = FALSE)
names(df_dwellings) <- c("skey", "qpid", "build_id", "address", "street_name", 
                         "street_suffix", "suburb", "town", "post_code", "ta", 
                         "ta_name", "bed_rooms", "year_built", "land_area", 
                         "owner_occupier", "matched_to_house", 
                         "long", "lat", "rec_begin_dte","rec_end_date", "rec_curr_ind")

# extract relevant columns
df_dwellings <- df_dwellings[, names(df_dwellings) %in% 
                               c("qpid", "address", "suburb", "town", 
                                 "post_code", "long", "lat")]

nrow(df_dwellings) == 1511685

# ======================
# Join dwellings (ONE) and consents (MANY)

# Assert: primary key is qpid
length(unique(df_dwellings$qpid)) == nrow(df_dwellings)

# Number of unique consents 
length(unique(df_consents$fk)) == 640022

# check that all consents are contained in dwellings (nice bit of code!)
unique(df_consents$fk) %in% df_dwellings$qpid %>% all()

# join the two data sets
df_matched_consents <- df_consents[, ! names(df_consents) %in% c("desc")] %>%  
  inner_join(df_dwellings, c("fk" = "qpid"))

# check that nothing got lost : "Besuche die Join" #rows = 945921
nrow(df_matched_consents) == 945921

# ======================
# Groom matched consents & sample some values

# Get rid of NULL lat / longs (lose 311 records)
df_matched_consents <- df_matched_consents[df_matched_consents$long != "NULL" & 
                                             df_matched_consents$lat != "NULL",]
# new record count = 945610
nrow(df_matched_consents)  ==  945921 - 311

# Filter to include 2011 OR 2015 only (85445)
df_filt_consents <- df_matched_consents %>% 
  filter(lubridate::year(date) == 2015 | 
           lubridate::year(date) == 2011) 

nrow(df_filt_consents) == 85445

# reconcile what you just did:

# get some counts for each year
df_year_months_counts <- df_matched_consents %>% 
  group_by(year = lubridate::year(date), month = lubridate::month(date)) %>% 
  summarise(count = n()) %>% arrange(year, month) %>% 
  filter(year >= 2010) %>% tidyr::spread(month, count)

# print to screen
df_year_months_counts

df_year_sum <- data.frame(year = df_year_months_counts$year,
                          total = rowSums(df_year_months_counts[, 2:13]))
# print to screen
df_year_sum

# check that this matches the above
df_filt_consents %>% group_by(year = year(date)) %>% summarise(count = n())

# 2011 = 43040, 2015 = 42405. Total = 85445

nrow(df_filt_consents) == 85445
# ================================
# CREATE Spatial Data Frame
# set lat and long to numeric
library(dplyr)
# set lat and long to numeric
df_filt_consents$long <- as.numeric(df_filt_consents$long)
df_filt_consents$lat <- as.numeric(df_filt_consents$lat)

# magic!! We have a SpatialPointsDataFrame
sp::coordinates(df_filt_consents) <- ~long+lat

# set the spatial coordinate system to wgs84
df_filt_consents@proj4string <- sp::CRS("+proj=longlat +ellps=WGS84")

# rename the object
spdf_consents_2011_2015 <- df_filt_consents; rm(df_filt_consents)

# ===================================
# ASSIGN to NZ Aerial Unit area 

# sp::over cannot work with "Spatial Points Data Frame" only Spatial Points
sp_consents_2011_2015  <- as(spdf_consents_2011_2015, 'SpatialPoints')

# Assert nothing got lost: N = 85445
dim(sp_consents_2011_2015@coords)[1] == nrow(spdf_consents_2011_2015)

# load in Areal units geometry
# expect warning message regarding z - dimension being discarded
spoly_areal_unit_wgs84 <- fn_load_areal_unit()

# intersect the points into areal units
df_point_poly <- sp::over(sp_consents_2011_2015, spoly_areal_unit_wgs84)

# Assert nothing lost
nrow(df_point_poly) == nrow(spdf_consents_2011_2015)

# 242 bad points (not interected into an Au..weird but small number)
sum(is.na(df_point_poly$AU2013)) 

# back into the spatial points data frame
spdf_consents_2011_2015@data <- cbind(spdf_consents_2011_2015@data, df_point_poly)

# print the first 10 rows.  What we are looking for is that the suburb name and the
# aerial unit name are vaguely the same ...???
head(spdf_consents_2011_2015@data, 20)

# ===================================
# CREATE Aggreations by Areal Unit

df_au_aggregation_2011 <- spdf_consents_2011_2015@data %>% 
                          filter(year(date) == 2011) %>%
                          group_by(AU2013) %>% 
                          summarise(count_2011 = n())


df_au_aggregation_2015 <- spdf_consents_2011_2015@data %>% 
                          filter(year(date) == 2015) %>%
                          group_by(AU2013) %>% 
                          summarise(count_2015 = n())
# check the totals
sum(df_au_aggregation_2011$count_2011, na.rm = TRUE) == 43040
sum(df_au_aggregation_2015$count_2015, na.rm = TRUE) == 42405
# totals here
43040 + 42405  == 85445

# 114 NAs for 2011
df_au_aggregation_2011[is.na(df_au_aggregation_2011$AU2013),"count_2011"] %>% .$count_2011 == 114
df_au_aggregation_2015[is.na(df_au_aggregation_2015$AU2013),"count_2015"] %>% .$count_2015 == 128

# total should be 242 (see above)
242 == 114 + 128

# shove things back into the spatial polygon
# print first few rows to screen
spoly_areal_unit_wgs84@data %>% head()

# add in the 2011 data
spoly_areal_unit_wgs84@data <- spoly_areal_unit_wgs84@data %>% 
                              left_join(df_au_aggregation_2011, c("AU2013" = "AU2013"))

# add in the 2015 data
spoly_areal_unit_wgs84@data <- spoly_areal_unit_wgs84@data %>% 
                              left_join(df_au_aggregation_2015, c("AU2013" = "AU2013"))

# make sure order is the same as the above
spoly_areal_unit_wgs84@data %>% head()

# check the joins (original number less NAs)
sum(spoly_areal_unit_wgs84@data$count_2011, na.rm = TRUE) == 43040 - 114
sum(spoly_areal_unit_wgs84@data$count_2015, na.rm = TRUE) == 42405 - 128

# set all the NAs to zero --- a single line of powerful code
spoly_areal_unit_wgs84@data[is.na(spoly_areal_unit_wgs84@data)] <- 0

# ASSERT that nothing got lost above:
sum(spoly_areal_unit_wgs84@data$count_2015) + sum(spoly_areal_unit_wgs84@data$count_2011) == 85203

# ===================================
# CREATE indicator for the 1911 areal units to indicate whether in Auckland
# get the data frame from the auckland au shapefile

# get the data frame from the auckland au shapefile
df_auckland <- fn_create_auckland_df()
nrow(df_auckland) == 315

# create a logical vector areal units within (arbitrary) Auckland
vct_logic_in_auckland <- spoly_areal_unit_wgs84@data$AU2013 %in% df_auckland$AU2013
length(vct_logic_in_auckland) == 1911
sum(vct_logic_in_auckland) == 315

# append the vector
spoly_areal_unit_wgs84@data$in_al <- vct_logic_in_auckland

# ==============================
# CREATE cagr for counts
vct_logic_cagr <- (spoly_areal_unit_wgs84@data$count_2011 > 0) & 
                  (spoly_areal_unit_wgs84@data$count_2015 > 0)

 
 # apply the logical vector created above
spoly_areal_unit_wgs84@data$cagr_count <-  
                    ifelse(vct_logic_cagr, 
                      fn_carg(previous_value = spoly_areal_unit_wgs84@data$count_2011, 
                      current_value = spoly_areal_unit_wgs84@data$count_2015, 
                      int_year_diff = 5), NA)
# =======================================

# CALCULATE Summaries
# All NZ -- Zenkoku
int_count_all_nz_2011 <- sum(spoly_areal_unit_wgs84@data$count_2011)
int_count_all_nz_2015 <- sum(spoly_areal_unit_wgs84@data$count_2015)

int_count_all_nz_2011 + int_count_all_nz_2015 == 85203

# Auckland Only -- 
int_count_al_only_2011 <-  spoly_areal_unit_wgs84@data %>% filter(in_al == TRUE) %>% 
                           summarise(total = sum(count_2011)) %>% .$total

int_count_al_only_2015 <-  spoly_areal_unit_wgs84@data %>% filter(in_al == TRUE) %>% 
                           summarise(total = sum(count_2015)) %>% .$total

# NON Auckland ---
int_count_NON_al_2011 <-  spoly_areal_unit_wgs84@data %>% filter(in_al != TRUE) %>% 
                              summarise(total = sum(count_2011)) %>% .$total

int_count_NON_al_2015 <-  spoly_areal_unit_wgs84@data %>% filter(in_al != TRUE) %>% 
                              summarise(total = sum(count_2015)) %>% .$total

# Reconcile
int_count_al_only_2011 + int_count_al_only_2015 + 
  int_count_NON_al_2011 + int_count_NON_al_2015 == 85203

# ================================================
# SUBSET and save the Auckland Consent Polygons

# With all this preparation. This is how we subset the spatial polygons

spoly_al_consents <- spoly_areal_unit_wgs84[spoly_areal_unit_wgs84@data$in_al == TRUE,]
nrow(spoly_al_consents) == 315

# write the sucker to disk - this is areal units (dieser file ist NZ der Alles)


# write the sucker to disk - this is areal units (this is AL only)
rgdal::writeOGR(spoly_areal_unit_wgs84, dsn = "data/polygons_1911_consents", 
                layer = "au_consents_all_nz_wgs84",
                driver="ESRI Shapefile", overwrite_layer = TRUE)


message("writing a shapefile of Auckland consent data to:")
message("data/polygons_315_unfiltered_counts_consents")







