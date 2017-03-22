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


# ======================
# Groom matched consents & sample some values

# Get rid of NULL lat / longs (lose 311 records)
df_matched_consents <- df_matched_consents[df_matched_consents$long != "NULL" & 
                                             df_matched_consents$lat != "NULL",]
# Assert: new number of records is: 945610
nrow(df_matched_consents) ==  (nrow(df_consents) - 311)

# Filter to include 2011 OR 2015 only (85445)
df_filt_consents <- df_matched_consents %>% 
                      filter(lubridate::year(date) == 2015 | 
                      lubridate::year(date) == 2011) 

# get some counts for each year
df_year_counts <- df_matched_consents %>% group_by(year = lubridate::year(date)) %>% 
                        summarise(count = n()) %>% arrange(year) %>% as.data.frame()

# reconcile the total 
df_year_counts[df_year_counts$year %in% c(2011, 2015), "count"] %>% sum() == 85445

# SAMPLE some numbers
set.seed(123)
# get 100 random values from consents (from all 1994 to 2016)
vct_samp_all_consent <- sample(1:nrow(df_matched_consents), size = 100, replace = FALSE)
df_matched_consents[vct_samp_all_consent, "value"]

# get 100 random values from filtered consents (2011 and 2015 only)
vct_samp_filt_consent <- sample(1:nrow(df_filt_consents), size = 100, replace = FALSE)
df_filt_consents[vct_samp_filt_consent, "value"]


# df_filt_consents %>% arrange(value) %>% View() 

# =================================================
# Exclude outlying values
flt_threshold <- 0.02
vct_probs <- c(0 + flt_threshold, 1 - flt_threshold)
# print the quantiles to screen
quantile(df_filt_consents$value, probs = vct_probs, na.rm = TRUE)

int_min <- quantile(df_filt_consents$value, probs = vct_probs, na.rm = TRUE)[1]
int_max <- quantile(df_filt_consents$value, probs = vct_probs, na.rm = TRUE)[2]

df_filt_consents_96_pc <- df_filt_consents %>%
                          filter(value >= int_min & value <= int_max)


# expecting the number of rows to be approx X% less than original
nrow(df_filt_consents_96_pc) - (nrow(df_filt_consents) * (1 - (2 * flt_threshold)))


# check that the excluded values came fairly evenly from each year.
df_filt_consents %>%  group_by(year(date)) %>% 
                      summarise(count = n()) %>% 
                      mutate(pc = count / sum(.$count))

df_filt_consents_96_pc %>%  group_by(year(date)) %>% 
                            summarise(count = n()) %>% 
                            mutate(pc = count / sum(.$count))

nrow(df_filt_consents_96_pc) == 82087
# ===================================
# Remove rows with 4 consecutive numbers

vct_char_consec <- c("1111", "2222", "3333","4444", "5555", "6666", "7777", "8888", "9999")

vct_value_as_char <- as.character(df_filt_consents_96_pc$value)

reg_x_vct_char_consec <- paste(vct_char_consec, collapse = "|")

vct_matches <- grep(reg_x_vct_char_consec, vct_value_as_char)

# rows to remove
nrow(df_filt_consents_96_pc[vct_matches, ]) == 134

# create logical vector to remove rows
vct_logical_remove_rows <- 1:nrow(df_filt_consents_96_pc) %in% vct_matches 

# filter out the bad rows (NOT (vct_logical_remove_rows))
df_filt_consents_96_pc <- df_filt_consents_96_pc[!vct_logical_remove_rows, ]

# Assert we have removed correct number of rows (81953)
nrow(df_filt_consents_96_pc) == 82087 - 134

# ===================================
# Create Spatial Data Frame
# set lat and long to numeric
df_filt_consents_96_pc$lat <- as.numeric(df_filt_consents_96_pc$lat)
df_filt_consents_96_pc$long <- as.numeric(df_filt_consents_96_pc$long)

# magic!! df_filt_consents_96_pc has become a SpatialPointsDataFrame
sp::coordinates(df_filt_consents_96_pc) <- ~long+lat
# set the spatial coordinate system to wgs84
df_filt_consents_96_pc@proj4string <- sp::CRS("+proj=longlat +ellps=WGS84")
# rename the object
spdf_consents_2011_2015 <- df_filt_consents_96_pc; rm(df_filt_consents_96_pc)

# ===================================
# Assign to NZ AU area 
# sp::over cannot work with "Spatial Points Data Frame" only Spatial Points
sp_consents_2011_2015  <- as(spdf_consents_2011_2015, 'SpatialPoints')

# Assert nothing got lost: N = 81953
dim(sp_consents_2011_2015@coords)[1] == nrow(spdf_consents_2011_2015)

# load in Areal units geometry
# expect warning message regarding z - dimension being discarded
spoly_areal_unit_wgs84 <- fn_load_areal_unit()

# intersect the points into areal units
df_point_poly <- sp::over(sp_consents_2011_2015, spoly_areal_unit_wgs84)

# Assert nothing lost
nrow(df_point_poly) == nrow(spdf_consents_2011_2015)

# 235 bad points
sum(is.na(df_point_poly$AU2013)) == 235

# We have assigned each point to an areal unit...we shove this information
# back into the spatial points data frame
spdf_consents_2011_2015@data <- cbind(spdf_consents_2011_2015@data, df_point_poly)

# rip out the data.frame. Exclude NA's
df_au_matched <- spdf_consents_2011_2015@data %>% filter(!is.na(AU2013) & !is.na( AU2013_NAM))

# assert that we just lost 235 rows. New Number = 81718
nrow(df_au_matched) == (81953 - 235)

# =====================================================
# CREATE Aggreations by Areal Unit
# counts, sum, average, median

df_au_aggregation_2011 <- df_au_matched %>% filter(year(date) == 2011) %>%
                          group_by(AU2013) %>% 
                          summarise(count_2011 = n(), total_2011 = sum(value),
                                    av_2011 = mean(value), med_2011 = median(value))


df_au_aggregation_2015 <- df_au_matched %>% filter(year(date) == 2015) %>%
                          group_by(AU2013) %>% 
                          summarise(count_2015 = n(), total_2015 = sum(value),
                          av_2015 = mean(value), med_2015 = median(value))

# we have all the components...now we need to put Humpty Dumpty back together again
# ASSERT that nothing got lost above:
sum(df_au_aggregation_2015$count_2015) + sum(df_au_aggregation_2011$count_2011) == 81718

# add in the 2011 data
spoly_areal_unit_wgs84@data <- spoly_areal_unit_wgs84@data %>% 
                               left_join(df_au_aggregation_2011, c("AU2013" = "AU2013"))

# 107 polygons in the 1911 areal units that we do not have data for -- NAs
spoly_areal_unit_wgs84@data %>% filter(is.na(count_2011)) %>% summarise(count = n()) %>% .$count

# add in the 2015 data
spoly_areal_unit_wgs84@data <- spoly_areal_unit_wgs84@data %>% 
                                left_join(df_au_aggregation_2015, c("AU2013" = "AU2013"))
# 111 NAs here
spoly_areal_unit_wgs84@data %>% filter(is.na(count_2015)) %>% summarise(count = n()) %>% .$count




# set all the NAs to zero --- a single line of powerful code
spoly_areal_unit_wgs84@data[is.na(spoly_areal_unit_wgs84@data)] <- 0

# ASSERT that nothing got lost above:
sum(spoly_areal_unit_wgs84@data$count_2015) + sum(spoly_areal_unit_wgs84@data$count_2011) == 81718
# =============================================================================
# CREATE indicator for the 1911 areal units to indicate whether in Auckland

# get the data frame from the auckland au shapefile
df_auckland <- fn_create_auckland_df()
nrow(df_auckland) == 315

# create a logical vector areal units within (arbitrary) Auckland
vct_logic_in_auckland <- spoly_areal_unit_wgs84@data$AU2013 %in% df_auckland$AU2013
length(vct_logic_in_auckland) == 1911
sum(vct_logic_in_auckland) == 315

# append the vector
spoly_areal_unit_wgs84@data$in_al <- vct_logic_in_auckland

# ===============================================================================
# CALCULATE cagr for each polygon. Do this for count and total. 
# Iff (count_2011 > 0) AND (count_2015 > 0) 

vct_logic_cagr <- (spoly_areal_unit_wgs84@data$count_2011 > 0) & 
                  (spoly_areal_unit_wgs84@data$count_2015 > 0)

# firstly do this for counts:
spoly_areal_unit_wgs84@data$cagr_count <-  
  ifelse(vct_logic_cagr, 
  fn_carg(previous_value = spoly_areal_unit_wgs84@data$count_2011, 
  current_value = spoly_areal_unit_wgs84@data$count_2015, 
  int_year_diff = 5),
  NA)

# secondly do this for value (ie. total):
spoly_areal_unit_wgs84@data$cagr_value <-  
  ifelse(vct_logic_cagr, 
  fn_carg(previous_value = spoly_areal_unit_wgs84@data$total_2011, 
  current_value = spoly_areal_unit_wgs84@data$total_2015, 
  int_year_diff = 5),
  NA)
# ===============================================================================
# Calculate Summaries
# 1) For All NZ
int_count_all_nz_2011 <- sum(spoly_areal_unit_wgs84@data$count_2011)
int_count_all_nz_2015 <- sum(spoly_areal_unit_wgs84@data$count_2015)

# Assert: still got our totals
int_count_all_nz_2015 + int_count_all_nz_2011 == 81718

int_value_all_nz_2011 <-  sum(spoly_areal_unit_wgs84@data$total_2011)
int_value_all_nz_2015 <-  sum(spoly_areal_unit_wgs84@data$total_2015)

# ====
# 2) For Auckland Only
int_count_al_only_2011 <- spoly_areal_unit_wgs84@data %>% filter(in_al == TRUE) %>% 
                          summarise(total = sum(count_2011)) %>% .$total

int_count_al_only_2015 <- spoly_areal_unit_wgs84@data %>% filter(in_al == TRUE) %>% 
                          summarise(total = sum(count_2015)) %>% .$total


int_value_al_only_2011 <- spoly_areal_unit_wgs84@data %>% filter(in_al == TRUE) %>% 
                          summarise(total = sum(total_2011)) %>% .$total

int_value_al_only_2015 <- spoly_areal_unit_wgs84@data %>% filter(in_al == TRUE) %>% 
                          summarise(total = sum(total_2015)) %>% .$total

# ==== 
# 3) For NON - Auckland Only
int_count_NON_al_only_2011 <- spoly_areal_unit_wgs84@data %>% filter(in_al == FALSE) %>% 
                              summarise(total = sum(count_2011)) %>% .$total

int_count_NON_al_only_2015 <- spoly_areal_unit_wgs84@data %>% filter(in_al == FALSE) %>% 
                              summarise(total = sum(count_2015)) %>% .$total


int_value_NON_al_only_2011 <- spoly_areal_unit_wgs84@data %>% filter(in_al == FALSE) %>% 
                              summarise(total = sum(total_2011)) %>% .$total

int_value_NON_al_only_2015 <- spoly_areal_unit_wgs84@data %>% filter(in_al == FALSE) %>% 
                              summarise(total = sum(total_2015)) %>% .$total

# 4) Reconcile the partitions above:
int_count_all_nz_2011 - int_count_al_only_2011 - int_count_NON_al_only_2011 == 0
int_count_all_nz_2015 - int_count_al_only_2015 - int_count_NON_al_only_2015 == 0
int_value_all_nz_2011 - int_value_al_only_2011 - int_value_NON_al_only_2011 == 0
int_value_all_nz_2015 - int_value_al_only_2015 - int_value_NON_al_only_2015 == 0

# ===============================================================================
# Calculate CAGR from summaries
# GAGR count for all NZ
fn_carg(previous_value = int_count_all_nz_2011, current_value = int_count_all_nz_2015, int_year_diff = 5)
# GAGR value for all NZ
fn_carg(previous_value = int_value_all_nz_2011, current_value = int_value_all_nz_2015, int_year_diff = 5)
# GAGR count for Auckland
fn_carg(previous_value = int_count_al_only_2011, current_value = int_count_al_only_2015, int_year_diff = 5)
# GAGR value for Auckland
fn_carg(previous_value = int_value_al_only_2011, current_value = int_value_al_only_2015, int_year_diff = 5)
# GAGR count NON Auckland
fn_carg(previous_value = int_count_NON_al_only_2011, current_value = int_count_NON_al_only_2015, int_year_diff = 5)
# GAGR value NON Auckland
fn_carg(previous_value = int_value_NON_al_only_2011, current_value = int_value_NON_al_only_2015, int_year_diff = 5)
# ===============================================================================
# SUBSET and save the Auckland Consent Polygons

# With all this preparation. This is how we subset the spatial polygons

spoly_al_consents <-spoly_areal_unit_wgs84[spoly_areal_unit_wgs84@data$in_al == TRUE,]
nrow(spoly_al_consents) == 315

message("writing a shapefile of Auckland consent data to:")
message("data/polygons_315_consent")

# write the sucker to disk - this is areal units
rgdal::writeOGR(spoly_al_consents, dsn = "data/polygons_315_consent", layer = "au_consents_wgs84",
                driver="ESRI Shapefile", overwrite_layer = TRUE)


message("writing a shapefile of Auckland consent data to:")
message("data/polygons_1911_consents_value")

rgdal::writeOGR(spoly_areal_unit_wgs84, dsn = "data/polygons_1911_consents_value", layer = "1911_consents_value_wgs84",
                driver="ESRI Shapefile", overwrite_layer = TRUE)






rm(list = ls())
# =============== finished  ==================

