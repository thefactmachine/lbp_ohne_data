library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
library(sp)
library(rgdal)

rm(list = ls())
options(stringsAsFactors = FALSE) 
options(scipen=999)

setwd("/Users/zurich/Documents/TEMP-FILES/lbp_project/final_lbp")
source("r_functions/fn_load_areal_unit.R")
source("r_functions/fn_create_auckland_df.R")
source("r_functions/fn_carg.R")


# data is in the format of consents and dwellings. There
# is 0..* consents for each dwelling. 


# CODE SYNOPSIS
# Read Consents
# Read Dwellings
# Join Consents with Dwellings
# Groom matched consents (remove NULLs, filter in 2011 and 2015 only)
# Remove outliers from 2011 and 2015 (include middle 96% of data)
# Remove values with 4 consecutive numbers that are the same
# Assign points to NZ AU area 
# CREATE Aggreations by Areal Unit
# Create indicator for the 1911 areal units to indicate whether in Auckland
# Calculate cagr for each of the 1911 polygons
# Calcualte summaries for reporting later
# Calculate CAGR from summaries
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

# check that nothing got lost : "Besuche die Join"
nrow(df_matched_consents) == nrow(df_consents)


# Get rid of NULL lat / longs (lose 311 records)
df_matched_consents <- df_matched_consents[df_matched_consents$long != "NULL" & 
                                             df_matched_consents$lat != "NULL",]
# Assert: new number of records is: 945610
nrow(df_matched_consents) ==  (nrow(df_consents) - 311)


# Filter to include 2011 OR 2015 only   (235712)
df_filt_consents <- df_matched_consents %>% 
  filter(lubridate::year(date) %in% c(2011, 2012, 2013, 2014, 2015))


# get some counts for each year
df_year_counts <- df_matched_consents %>% group_by(year = lubridate::year(date)) %>% 
  summarise(count = n()) %>% arrange(year) %>% as.data.frame()
df_year_counts

# reconcile the total 
df_year_counts[df_year_counts$year %in% 2011:2015, "count"] %>% sum() == 235712


# =================================================
# Exclude outlying values
nrow(df_filt_consents) == 235712

flt_threshold <- 0.02
vct_probs <- c(0 + flt_threshold, 1 - flt_threshold)
# print the quantiles to screen
quantile(df_filt_consents$value, probs = vct_probs, na.rm = TRUE)

int_min <- quantile(df_filt_consents$value, probs = vct_probs, na.rm = TRUE)[1]
int_max <- quantile(df_filt_consents$value, probs = vct_probs, na.rm = TRUE)[2]

df_filt_consents_96_pc <- df_filt_consents %>%
  filter(value >= int_min & value <= int_max)

nrow(df_filt_consents_96_pc) == 226310

# following should be approx the same as the above.
235712 * 0.96

# =========================================

# Create Spatial Data Frame
# set lat and long to numeric
nrow(df_filt_consents_96_pc) == 226310


df_filt_consents_96_pc$lat <- as.numeric(df_filt_consents_96_pc$lat)
df_filt_consents_96_pc$long <- as.numeric(df_filt_consents_96_pc$long)

# magic!! df_filt_consents_96_pc has become a SpatialPointsDataFrame
sp::coordinates(df_filt_consents_96_pc) <- ~long+lat
# set the spatial coordinate system to wgs84
df_filt_consents_96_pc@proj4string <- sp::CRS("+proj=longlat +ellps=WGS84")

# rename the object
spdf_consents_2011_to_2015 <- df_filt_consents_96_pc; rm(df_filt_consents_96_pc)

# ===================================
# Assign to NZ AU area 

# Assign to NZ AU area 
# sp::over cannot work with "Spatial Points Data Frame" only Spatial Points
sp_consents_2011_to_2015  <- as(spdf_consents_2011_to_2015, 'SpatialPoints')

# Assert nothing got lost: N = 226310
dim(sp_consents_2011_to_2015@coords)[1] == nrow(spdf_consents_2011_to_2015)

# load in Areal units geometry
# expect warning message regarding z - dimension being discarded
spoly_areal_unit_wgs84 <- fn_load_areal_unit()

# intersect the points into areal units
df_point_poly <- sp::over(sp_consents_2011_to_2015, spoly_areal_unit_wgs84)

# Assert nothing lost
nrow(df_point_poly) == nrow(spdf_consents_2011_to_2015)

# 235 bad points
sum(is.na(df_point_poly$AU2013)) == 562

# We have assigned each point to an areal unit...we shove this information
# back into the spatial points data frame
spdf_consents_2011_to_2015@data <- cbind(spdf_consents_2011_to_2015@data, df_point_poly)

# rip out the data.frame. Exclude NA's
df_au_matched <- spdf_consents_2011_to_2015@data %>% filter(!is.na(AU2013) & !is.na( AU2013_NAM))

# assert that we just lost 562 rows. New Number = 81718
nrow(df_au_matched) == (226310 - 562)

# ===========================================================

df_au_aggregation_2011 <- df_au_matched %>% filter(year(date) == 2011) %>%
                          group_by(AU2013) %>% 
                          summarise(count_2011 = n(), total_2011 = sum(value))

df_au_aggregation_2012 <- df_au_matched %>% filter(year(date) == 2012) %>%
                          group_by(AU2013) %>% 
                          summarise(count_2012 = n(), total_2012 = sum(value))

df_au_aggregation_2013 <- df_au_matched %>% filter(year(date) == 2013) %>%
                          group_by(AU2013) %>% 
                          summarise(count_2013 = n(), total_2013 = sum(value))

df_au_aggregation_2014 <- df_au_matched %>% filter(year(date) == 2014) %>%
                          group_by(AU2013) %>% 
                          summarise(count_2014 = n(), total_2014 = sum(value))

df_au_aggregation_2015 <- df_au_matched %>% filter(year(date) == 2015) %>%
                          group_by(AU2013) %>% 
                          summarise(count_2015 = n(), total_2015 = sum(value))


# we have all the components...now we need to put Humpty Dumpty back together again
# ASSERT that nothing got lost above:
sum(df_au_aggregation_2011$count_2011) + 
  sum(df_au_aggregation_2012$count_2012) +
  sum(df_au_aggregation_2013$count_2013) +
  sum(df_au_aggregation_2014$count_2014) +
  sum(df_au_aggregation_2015$count_2015)  == nrow(df_au_matched)

# Add in the Data
spoly_areal_unit_wgs84@data <- spoly_areal_unit_wgs84@data %>% 
                                left_join(df_au_aggregation_2011, c("AU2013" = "AU2013"))

spoly_areal_unit_wgs84@data <- spoly_areal_unit_wgs84@data %>% 
                                left_join(df_au_aggregation_2012, c("AU2013" = "AU2013"))

spoly_areal_unit_wgs84@data <- spoly_areal_unit_wgs84@data %>% 
                                left_join(df_au_aggregation_2013, c("AU2013" = "AU2013"))

spoly_areal_unit_wgs84@data <- spoly_areal_unit_wgs84@data %>% 
                                left_join(df_au_aggregation_2014, c("AU2013" = "AU2013"))

spoly_areal_unit_wgs84@data <- spoly_areal_unit_wgs84@data %>% 
                                  left_join(df_au_aggregation_2015, c("AU2013" = "AU2013"))

# set all the NAs to zero --- a single line of powerful code
spoly_areal_unit_wgs84@data[is.na(spoly_areal_unit_wgs84@data)] <- 0

vct_cols <- c("count_2011", "count_2012", "count_2013", "count_2014", "count_2015")

# check nothing lost
sum(spoly_areal_unit_wgs84@data[, vct_cols]) == nrow(df_au_matched)


# get the data frame from the auckland au shapefile
df_auckland <- fn_create_auckland_df()
nrow(df_auckland) == 315

# create a logical vector areal units within (arbitrary) Auckland
vct_logic_in_auckland <- spoly_areal_unit_wgs84@data$AU2013 %in% df_auckland$AU2013
length(vct_logic_in_auckland) == 1911
sum(vct_logic_in_auckland) == 315

# append the vector
spoly_areal_unit_wgs84@data$in_al <- vct_logic_in_auckland

# scale values down to millions of dollars...some numbers are too big for the 
# shapefile to handle
spoly_areal_unit_wgs84@data$total_2011 <- spoly_areal_unit_wgs84@data$total_2011 / 1e06 
spoly_areal_unit_wgs84@data$total_2012 <- spoly_areal_unit_wgs84@data$total_2012 / 1e06 
spoly_areal_unit_wgs84@data$total_2013 <- spoly_areal_unit_wgs84@data$total_2013 / 1e06 
spoly_areal_unit_wgs84@data$total_2014 <- spoly_areal_unit_wgs84@data$total_2014 / 1e06 
spoly_areal_unit_wgs84@data$total_2015 <- spoly_areal_unit_wgs84@data$total_2015 / 1e06 


message("writing a shapefile of Auckland consent data to:")
message("data/polygons_1911_consents_value_all_years")

rgdal::writeOGR(spoly_areal_unit_wgs84, dsn = "data/polygons_1911_consents_value_all_years", 
                layer = "1911_consents_value_all_years_wgs84",
                driver="ESRI Shapefile", overwrite_layer = TRUE)

rm(list = ls())











