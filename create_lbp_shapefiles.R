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

# 25360 rows
df_x11_x15_subset <- df_unique_guid %>% select(guid, y_2011, y_2015, y_all) %>%
                      filter(y_2011 != 0 | y_2015 != 0)

nrow(df_x11_x15_subset) == 25360 
sum(df_x11_x15_subset$y_all >= 1) == 25360
length(unique(df_x11_x15_subset$guid)) == 25360

# 3891 rows
df_NOT_x11_x15_subset  <- df_unique_guid %>% select(guid, y_2011, y_2015, y_all) %>%
                          filter(y_2011 == 0 & y_2015 == 0)

# rec back to df_unique_guid
nrow(df_unique_guid) == 25360 + 3891

# how many licenses did we lose here:
sum(df_NOT_x11_x15_subset$y_all)

# reco_check : we lost 3090 here due to not in 2011 and not in 2015
# reco check: current == orig - bad_guif - not in (X11 and x 15)
sum(df_x11_x15_subset$y_all) == 125432 - 49 - 3090


# ================
# load lat_long and match to guid, reconcile, get rid of NAs

# data frame of latitude and longitudes. Coordinate system is WGS-84
# (created by Google API) source code not provided due to time constraints
df_lat_long <- readRDS("data/df_lat_long_28034.rds")

# hook up the lat / longs with the raw data
df_x11_x15_subset <- df_x11_x15_subset %>% dplyr::left_join(df_lat_long, c("guid" = "guid"))

# 2639 rows here ... these are NAs
df_na_check <- df_x11_x15_subset %>% filter(is.na(type))

# 13565 licenses due to missing lat / long
sum(df_na_check$y_all)

# Partition the NAs
# 145 [in 2011 but not in 2015]
sum(df_na_check$y_2011 > 0 & df_na_check$y_2015 == 0)
# 1603 [in both]
sum(df_na_check$y_2011 > 0 & df_na_check$y_2015 > 0)
# 891 here
sum(df_na_check$y_2011 == 0 & df_na_check$y_2015 > 0)
145 + 1603 + 891 == nrow(df_na_check)

df_x11_x15_clean <- df_x11_x15_subset %>% filter(!is.na(type))

# Expect 22721 (original less NAs)
nrow(df_x11_x15_clean) == 25360 - 2639

# now we have no pesky NAs
sum(is.na(df_x11_x15_clean)) == 0

# 108728
sum(df_x11_x15_clean$y_all)
# reco_check : current total = prev_total - missing lat longs
108728 == 122293 - 13565

# ================
# Clean up the work space a bit - remove everthing except..
rm(list = ls()[!ls() %in% c("df_x11_x15_clean",  "fn_load_areal_unit", "df_raw")])
# ================ 

# ========================================
# ========================================
# load NZ Stats Areal Units geometry; create points geometry from data. Assign points to AU

# expect warning message regarding z - dimension being discarded
spoly_areal_unit_wgs84 <- fn_load_areal_unit()

# convert the previous data.frame to SpatialPointsDataFrame
df_x11_x15_clean <- as.data.frame(df_x11_x15_clean)
sp::coordinates(df_x11_x15_clean) <- ~lng+lat

# Set the coordinate system to WGS84
df_x11_x15_clean@proj4string <- sp::CRS("+proj=longlat +ellps=WGS84")
# rename the object to have a spdf prefix
spdf_data_group <- df_x11_x15_clean
# sp::over cannot work with "Spatial Points Data Frame" only Spatial Points
sp_data_group <- as(spdf_data_group, 'SpatialPoints')
# Assert nothing got lost: N = 22721
dim(sp_data_group@coords)[1] == nrow(df_x11_x15_clean)
# This is the fun part: intersect the points into the polygons...
# For a specific point: what polygon is it a member of
df_point_poly <- sp::over(sp_data_group, spoly_areal_unit_wgs84)
# Assert nothing lost
nrow(df_point_poly) == nrow(spdf_data_group)
# 289 NA's were introduced..these would (i.e should) need to be investigated
sum(is.na(df_point_poly$AU2013)) == 289
# We have assigned each point to an areal unit...we shove this information
# back into the spatial points data frame
spdf_data_group@data <- cbind(spdf_data_group@data, df_point_poly)

# reco_check : previous total == current total
108728 == sum(spdf_data_group@data$y_all)

# reco_check : number of NAs introduced due to bad lat / longs
# bad means not intersecting with aerial units
sum(spdf_data_group@data[is.na(spdf_data_group@data$AU2013), "y_all"]) == 1462


# =========================
# GROUP BY aerial unit

nrow(spdf_data_group@data) == 22721
# number of unique Ids is 1737. This includes an NA
length(unique(spdf_data_group@data$AU2013))  == 1737

df_au_totals <- spdf_data_group@data %>% group_by(AU2013, AU2013_NAM) %>% 
                      summarise(yt_2011 = sum(y_2011), yt_2015 = sum(y_2015), yt = sum(y_all))
nrow(df_au_totals) == 1737

# need to do the counts separately for each period
df_au_count_x11 <- spdf_data_group@data %>% filter(y_2011 != 0) %>% 
                   group_by(AU2013, AU2013_NAM) %>% summarise(cnt_11 = n())
nrow(df_au_count_x11) == 1559

df_au_count_x15 <- spdf_data_group@data %>% filter(y_2015 != 0) %>% 
  group_by(AU2013, AU2013_NAM) %>% summarise(cnt_15 = n())

nrow(df_au_count_x15) == 1730

# stitch the data frames back together again
df_au_totals <- df_au_totals %>% 
                dplyr::left_join(df_au_count_x11, 
                c("AU2013" = "AU2013", "AU2013_NAM" = "AU2013_NAM"))

df_au_totals <- df_au_totals %>% 
                dplyr::left_join(df_au_count_x15, 
                 c("AU2013" = "AU2013", "AU2013_NAM" = "AU2013_NAM"))
# reco_check : previous total == current total
108728 == sum(df_au_totals$yt)

# =========================
# =========================
# ASSIGN THE summaries to aerial unit shapefile
# Join "df_au_totals" to "spoly_areal_unit_wgs84"

# some totals to reconcile to
sum(df_au_totals$yt) == 108728
sum(df_au_totals[is.na(df_au_totals$AU2013), "yt"]) == 1462

nrow(df_au_totals) == 1737
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
sum(spoly_areal_unit_wgs84@data$yt) == 108728 - 1462

# eyeball one piece of data to make sure its okay
df_au_totals[df_au_totals$AU2013 == "500203",]


# ===============================
# reco_check : make sure the only thing lost are the NA
# final total is 107266
sum(spoly_areal_unit_wgs84@data$yt) == 108728 - 1462

message("writing aerial unit polygons to disk. See:")
message("data/aerial_unit_lbp_counts")

# WRITE the sucker to disk - this is areal units
rgdal::writeOGR(spoly_areal_unit_wgs84, dsn = "data/aerial_unit_lbp_counts", layer = "au_lbp_counts",
                driver="ESRI Shapefile", overwrite_layer = TRUE)

# reco_check : final reco for points data
sum(spdf_data_group$y_all) == 108728

# following creates reconcilation total for Auckland
source("r_functions/fn_create_auckland_df.R")
df_auckland <- fn_create_auckland_df()
vct_al <- df_auckland$AU2013
vct_logic_in_al <- spdf_data_group@data$AU2013 %in% vct_al
sum(spdf_data_group@data[vct_logic_in_al, "y_all"]) == 22401


message("writing raw lbp point data to disk. See:")
message("data/point_data_lbp")

# write the point file to disk 
rgdal::writeOGR(spdf_data_group, dsn = "data/point_data_lbp", layer = "lbp_points",
                driver="ESRI Shapefile", overwrite_layer = TRUE)

# ===============================
# RECONCILIATION TABLE

# Start											          125432
# Bad guid 										        -49
# Not in X11 and X15  							  -3090
# Missing lat longs								    -13565
# Notintersecting with Aerial units  	-1462

# final total  									      107266
# following sums to zero
125432 - 49 - 3090 - 13565 - 1462 - 107266




# QA QA QA QA QA QA QA QA QA QA QA QA QA QA QA QA QA QA QA QA QA QA QA QA 
# The following visually inspected in ERSI
# TWO POLYGONS
# 1
# Calculated totals: "Armour Bay" (512802) 2012 = 3; 2016 = 5

# Following by visually inspecting ESRI
# 2BA5EAE5-8AA0-E411-8A2C-005056AE0567 (2012 and 2016)
# A5271C3A-5445-4D51-99BB-6E23E08DF1FF (2012 and 2016)
# AB7B33B0-CDDF-45CF-87C8-CB3837E5CC02 (not 2012 and 2016)
# 63D5C5A6-0E19-E311-8DB8-005056AE0567 (not 2012 and 2016)
# 80066D7B-FA22-427E-9AA3-3A99ED3C4530 (2012 and 2016)

# 2
# Calculated totals: "Glenorchy (609013) 2012 = 2; 2016 = 4
# Following by visually inspecting ESRI
# 61894F2A-0E00-E511-BD3D-005056AE0567 (not 2012 and 2016)
# 1991918C-5460-4FED-B530-F1950A756835 (2012 and 2016)
# 9FA5CF1B-A4DC-4751-A336-6B5096E81BC5 (2012 and 2016)
# 3390BD47-5760-4362-BDCB-A71D704792F0 (not 2012 and 2016)

# CHECK THREE DATA POINTS


df_lat_long <- readRDS("data/df_lat_long_28034.rds")
df_data_gps <- df_raw %>% inner_join(df_lat_long, c("guid" = "guid"))

# 1
# "A5271C3A-5445-4D51-99BB-6E23E08DF1FF"
# Following from ESRI
# 174.613307, -36.974113

df_data_gps[df_data_gps$guid == "A5271C3A-5445-4D51-99BB-6E23E08DF1FF", c("name", "lat", "lng")]
# name is Paul Fisk
# Manaully check against column G (col 7) from following Mako link (search for GUID)
# http://mako.wd.govt.nz/otcs/llisapi.dll?func=ll&objaction=overview&objid=51874514
# row 19207. Name (col DQ matches Paul Fisk)
# Google Earth search for: "12 Rauhuia Crescent New Zealand"
# Matches LAT LONG ABOVE to 3 decimal places

# 2 "80066D7B-FA22-427E-9AA3-3A99ED3C4530"
# Following from ESRI
# 174.615734, -36.984698
df_data_gps[df_data_gps$guid == "80066D7B-FA22-427E-9AA3-3A99ED3C4530", c("name", "lat", "lng")]
# name is Noe Bridler
# Manaully check against column G (col 7) from following Mako link (search for GUID)
# http://mako.wd.govt.nz/otcs/llisapi.dll?func=ll&objaction=overview&objid=51874514
# row 29624. (col DQ matches Noe Bridler )
# Google Earth search for: "26 Shirley Road Parau New Zealand"
# Matches LAT LONG ABOVE to 3 decimal places

# 3 "9FA5CF1B-A4DC-4751-A336-6B5096E81BC5"
# Following from ESRI
# 168.393297, -44.849898
df_data_gps[df_data_gps$guid == "9FA5CF1B-A4DC-4751-A336-6B5096E81BC5", c("name", "lat", "lng")]
# Name is Russell Varcoe
# Manaully check against column G (col 7) from following Mako link (search for GUID)
# http://mako.wd.govt.nz/otcs/llisapi.dll?func=ll&objaction=overview&objid=51874514
# row 19174. Name (col DQ matches Russell Varcoe)
# Google Earth search for: "91 Coll Street Otago"
# Matches LAT LONG ABOVE to 3 decimal places

# clean up the environment
rm(list = ls())

