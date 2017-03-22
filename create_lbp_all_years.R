library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
library(sp)
library(rgdal)



# CODE SYNOPSIS II
# load, reconcile, remove null guids
# Squash the data down -- unique guid and sum of licenses
# Create a 2011 and 2015 subset
# load lat_long and match to guid
# load NZ Stats Areal Units geometry; create points geometry from data. Assign points to AU
# GROUP BY aerial unit
# ASSIGN THE summaries to aerial unit shapefile
# WRITE to disk
# Reconcilate
# QA



# ====================
# preliminaries
options(stringsAsFactors = FALSE)
rm(list = ls())
setwd("/Users/zurich/Documents/TEMP-FILES/lbp_project/final_lbp")
source("r_functions/fn_load_areal_unit.R")

# ====================
# load, reconcile, remove null guids
# http://mako.wd.govt.nz/otcs/llisapi.dll?func=ll&objaction=overview&objid=51874514
df_raw <- read.csv("data/FilteredContact_View with headers_21_06.csv", header = TRUE)

# replace the default names
names(df_raw) <- c("guid", "license", "yr_2011", "yr_2012", "yr_2013", "yr_2014", "yr_2015", "new_id", "name")

# number of rows
nrow(df_raw) == 36445


# create some summaries (these reconcile to Rusell's email on 21-06-2016 @ 15:30)
df_check <- df_raw %>% 
  group_by(license) %>% 
  summarise(x_11 = sum(yr_2011), x_12 = sum(yr_2012), 
            x_13 = sum(yr_2013), x_14 = sum(yr_2014),
            x_15 = sum(yr_2015)) 
# print to screen
df_check 

# total licenses for each year 125432
sum(df_check[, 2:6]) == sum(df_raw[,c("yr_2011", "yr_2012",  "yr_2013", "yr_2014", "yr_2015")])
sum(df_check[, 2:6])

# reco_check :This is the benchmark 125432 what we started with

# valid guid is 36 characters (loss of 23 rows)
df_raw_sans_null <- df_raw %>% filter(nchar(guid) == 36)
nrow(df_raw_sans_null) == 36422

# how many licenses did we lose
df_raw_null <- df_raw %>% filter(nchar(guid) != 36) 
sum(df_raw_null[, 3:7]) == 49

# reco_check : we lost 49 here due to bad guid

# number of resultant licenses
sum(df_check[, 2:6]) - sum(df_raw_null[, 3:7]) == 125383

# ================
# Squash the data down -- unique guid and sum of licenses
df_unique_guid <- df_raw_sans_null %>% group_by(guid) %>% 
  summarise(y_2011 = sum(yr_2011), y_2012 = sum(yr_2012),
            y_2013 = sum(yr_2013), y_2014 = sum(yr_2014), 
            y_2015 = sum(yr_2015))
# add a sum of licenses
df_unique_guid$y_all <- df_unique_guid$y_2011 + df_unique_guid$y_2012 + df_unique_guid$y_2013 +
  df_unique_guid$y_2014 + df_unique_guid$y_2015

# verify licenses numbers are okay
sum(df_unique_guid$y_all) == 125383

# ================
# Create a 2011 and 2015 subset

# 26884 rows
df_x11_to_x15_subset <- df_unique_guid %>% select(guid, y_2011, y_2012, y_2013, y_2014, y_2015, y_all) %>%
  filter(y_2011 != 0 | y_2012 != 0 | y_2013 != 0 | y_2014 != 0 |  y_2015 != 0)

nrow(df_x11_to_x15_subset) == 26884 

length(unique(df_x11_to_x15_subset$guid)) == 26884


# 2367 rows
df_NOT_x11_to_x15_subset  <- df_unique_guid %>% select(guid, y_2011, y_2012, y_2013, y_2014, y_2015, y_all) %>%
  filter(y_2011 == 0 & y_2012 == 0 & y_2013 == 0 & y_2014 == 0 &  y_2015 == 0)
# rec back to df_unique_guid
nrow(df_unique_guid) == 26884 + 2367

# how many licenses did we lose here: 0
sum(df_NOT_x11_to_x15_subset$y_all)

# reco_check : we lost 3090 here due to not in 2011 to 2015
# reco check: current == orig - bad_guif 
sum(df_x11_to_x15_subset$y_all) == 125432 - 49 - 0 


# ================
# load lat_long and match to guid, reconcile, get rid of NAs

# data frame of latitude and longitudes. Coordinate system is WGS-84
# (created by Google API) source code not provided due to time constraints
df_lat_long <- readRDS("data/df_lat_long_28034.rds")

# hook up the lat / longs with the raw data
df_x11_to_x15_subset <- df_x11_to_x15_subset %>% dplyr::left_join(df_lat_long, c("guid" = "guid"))

# 2725 rows here ... these are NAs
df_na_check <- df_x11_to_x15_subset %>% filter(is.na(type))

# 13734 licenses due to missing lat / long
sum(df_na_check$y_all)

df_x11_to_x15_clean <- df_x11_to_x15_subset %>% filter(!is.na(type))

# Expect 24159 (original less NAs)
nrow(df_x11_to_x15_clean) == 26884 - 2725

# now we have no pesky NAs
sum(is.na(df_x11_to_x15_clean)) == 0

# 111649
sum(df_x11_to_x15_clean$y_all)
# reco_check : current total = orig_total - missing lat longs - bad_guid
sum(df_x11_to_x15_clean$y_all) == 125432 - 13734 - 49

# ================
# Clean up the work space a bit - remove everthing except..
rm(list = ls()[!ls() %in% c("df_x11_to_x15_clean",  "fn_load_areal_unit", "df_raw")])

# ========================================
# ========================================
# LOAD NZ Stats Areal Units geometry; create points geometry from data. Assign points to AU

# expect warning message regarding z - dimension being discarded
spoly_areal_unit_wgs84 <- fn_load_areal_unit()

# convert the previous data.frame to SpatialPointsDataFrame
df_x11_to_x15_clean <- as.data.frame(df_x11_to_x15_clean)
sp::coordinates(df_x11_to_x15_clean) <- ~lng+lat

# Set the coordinate system to WGS84
df_x11_to_x15_clean@proj4string <- sp::CRS("+proj=longlat +ellps=WGS84")

# rename the object to have a spdf prefix
spdf_data_group <- df_x11_to_x15_clean

# sp::over cannot work with "Spatial Points Data Frame" only Spatial Points
sp_data_group <- as(spdf_data_group, 'SpatialPoints')

# Assert nothing got lost: N = 24159
dim(sp_data_group@coords)[1] == nrow(df_x11_to_x15_clean)
# This is the fun part: intersect the points into the polygons...
# For a specific point: what polygon is it a member of
df_point_poly <- sp::over(sp_data_group, spoly_areal_unit_wgs84)
# Assert nothing lost #24159
nrow(df_point_poly) == nrow(spdf_data_group)
# 317 NA's were introduced..these would (i.e should) need to be investigated
sum(is.na(df_point_poly$AU2013)) == 317
# We have assigned each point to an areal unit...we shove this information
# back into the spatial points data frame
spdf_data_group@data <- cbind(spdf_data_group@data, df_point_poly)


# reco_check : previous total (111649) == current total
111649 == sum(spdf_data_group@data$y_all)

# reco_check : number of NAs introduced due to bad lat / longs
# bad means not intersecting with aerial units
sum(spdf_data_group@data[is.na(spdf_data_group@data$AU2013), "y_all"]) == 1510

# ===============================================

# =========================
# GROUP BY aerial unit

nrow(spdf_data_group@data) == 24159

# number of unique Ids is 1748. This includes an NA
length(unique(spdf_data_group@data$AU2013))  == 1748

df_au_totals <- spdf_data_group@data %>% group_by(AU2013, AU2013_NAM) %>% 
  summarise(yt_2011 = sum(y_2011), yt_2012 = sum(y_2012), yt_2013 = sum(y_2013),    
            yt_2014 = sum(y_2014), yt_2015 = sum(y_2015), yt = sum(y_all))

nrow(df_au_totals) == 1748

# need to do the counts separately for each period
df_au_count_x11 <- spdf_data_group@data %>% filter(y_2011 != 0) %>% 
  group_by(AU2013, AU2013_NAM) %>% summarise(cnt_11 = n())
nrow(df_au_count_x11) == 1559

df_au_count_x12 <- spdf_data_group@data %>% filter(y_2012 != 0) %>% 
  group_by(AU2013, AU2013_NAM) %>% summarise(cnt_12 = n())
nrow(df_au_count_x12) == 1721

df_au_count_x13 <- spdf_data_group@data %>% filter(y_2013 != 0) %>% 
  group_by(AU2013, AU2013_NAM) %>% summarise(cnt_13 = n())
nrow(df_au_count_x13) == 1729

df_au_count_x14 <- spdf_data_group@data %>% filter(y_2014 != 0) %>% 
  group_by(AU2013, AU2013_NAM) %>% summarise(cnt_14 = n())
nrow(df_au_count_x14) == 1736

df_au_count_x15 <- spdf_data_group@data %>% filter(y_2015 != 0) %>% 
  group_by(AU2013, AU2013_NAM) %>% summarise(cnt_15 = n())
nrow(df_au_count_x15) == 1730

# stitch the data frames back together again
df_au_totals <- df_au_totals %>% 
  dplyr::left_join(df_au_count_x11, 
                   c("AU2013" = "AU2013", "AU2013_NAM" = "AU2013_NAM"))

df_au_totals <- df_au_totals %>% 
  dplyr::left_join(df_au_count_x12, 
                   c("AU2013" = "AU2013", "AU2013_NAM" = "AU2013_NAM"))

df_au_totals <- df_au_totals %>% 
  dplyr::left_join(df_au_count_x13, 
                   c("AU2013" = "AU2013", "AU2013_NAM" = "AU2013_NAM"))

df_au_totals <- df_au_totals %>% 
  dplyr::left_join(df_au_count_x14, 
                   c("AU2013" = "AU2013", "AU2013_NAM" = "AU2013_NAM"))


df_au_totals <- df_au_totals %>% 
  dplyr::left_join(df_au_count_x15, 
                   c("AU2013" = "AU2013", "AU2013_NAM" = "AU2013_NAM"))

# reco_check : previous total == current total
111649 == sum(df_au_totals$yt)

# ASSIGN THE summaries to aerial unit shapefile
# Join "df_au_totals" to "spoly_areal_unit_wgs84"

# some totals to reconcile to
sum(df_au_totals$yt) == 111649
sum(df_au_totals[is.na(df_au_totals$AU2013), "yt"]) == 150

nrow(df_au_totals) == 1748
nrow(spoly_areal_unit_wgs84) == 1911

# print the first couple of rows..we want to make sure the order is preserved
spoly_areal_unit_wgs84@data %>% head()

# put humpty dumpty back together again
spoly_areal_unit_wgs84@data <- 
  spoly_areal_unit_wgs84@data %>% 
  dplyr::left_join(df_au_totals, c("AU2013" = "AU2013", "AU2013_NAM" = "AU2013_NAM"))

# print the rows again...should be in the same order
spoly_areal_unit_wgs84@data %>% head()

# set NAs to zero
spoly_areal_unit_wgs84@data[is.na(spoly_areal_unit_wgs84@data)] <- 0

# make sure the only thing lost are the NAs
sum(spoly_areal_unit_wgs84@data$yt) == 111649 - 1510


# load in Auckland Ids to make an Auckland Indicator
source("r_functions/fn_create_auckland_df.R")
df_auckland <- fn_create_auckland_df()
nrow(df_auckland) == 315

# create a logical vector areal units within (arbitrary) Auckland
vct_logic_in_auckland <- spoly_areal_unit_wgs84@data$AU2013 %in% df_auckland$AU2013
vct_logic_in_auckland %>% length() == 1911
vct_logic_in_auckland %>% sum() == 315

spoly_areal_unit_wgs84@data$in_al <- vct_logic_in_auckland



message("writing aerial unit polygons to disk. See:")
message("data/polygons_1911_lbp_all_years")


rgdal::writeOGR(spoly_areal_unit_wgs84, dsn = "data/polygons_1911_lbp_all_years", 
                layer = "nz_lbp_all_years", driver="ESRI Shapefile", overwrite_layer = TRUE)

rm(list = ls())


