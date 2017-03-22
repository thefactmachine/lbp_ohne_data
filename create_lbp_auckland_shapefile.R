library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
library(sp)
library(rgdal)

# CODE SYNOPSIS
# get a data.frame of Auckland au polygon names and census06 & 13 counts of people
# get the 1911 au polgons (spatial data) with counts of LBP for each polygon
# subsets the spatial polygons with just the Auckland data
# create CAGR for Auckland polygons. 
# create CAGR for Total NZ , AL Only, Non-AL.
# save the Auckland shapefile.

options(stringsAsFactors = FALSE)
setwd("/Users/zurich/Documents/TEMP-FILES/lbp_project/final_lbp")
rm(list = ls())

source("r_functions/fn_create_auckland_df.R")
source("r_functions/fn_carg.R")

# get the data frame from the auckland au shapefile this is just a generic set
# of unique identifiers for AL polygons

df_auckland <- fn_create_auckland_df()
nrow(df_auckland) == 315

# this is the shapefile created by create_lbp_shapefiles.R
spoly_au_counts <- rgdal::readOGR(dsn = "data/aerial_unit_lbp_counts",
                             layer = "au_lbp_counts")
# total from step 1
sum(spoly_au_counts@data$yt) == 107266

# Assert: Number of aerial units are 1911
nrow(spoly_au_counts) == 1911
# Assert: is(PK, AU2013) == TRUE
length(unique(spoly_au_counts@data$AU2013)) == nrow(spoly_au_counts) 


# create a logical vector areal units within (arbitrary) Auckland
vct_logic_in_auckland <- spoly_au_counts@data$AU2013 %in% df_auckland$AU2013
length(vct_logic_in_auckland) == 1911
sum(vct_logic_in_auckland) == 315


# With all this preparation. This is how we subset the spatial polygons
spoly_au_counts_al <- spoly_au_counts[vct_logic_in_auckland, ]

# Assert: our aerial units data is 315 polygons
nrow(spoly_au_counts_al) == 315


# calculate Compound Annual Growth Rate (cagr) :

# 1) create a logical vector if (2012 == 0) OR (2016 == 0)
vct_logic_zero_count <- spoly_au_counts_al@data$cnt_11== 0 | spoly_au_counts_al@data$cnt_15 == 0
# number of NAs introduced
sum(vct_logic_zero_count) == 38

# 2) only calculate cagr if both values are non-zero
spoly_au_counts_al@data$cagr <- ifelse(vct_logic_zero_count, NA, 
                                       fn_carg(previous_value = spoly_au_counts_al@data$cnt_11, 
                                               current_value = spoly_au_counts_al@data$cnt_15,  
                                               int_year_diff = 5))

# == Calculate cagr for Auckland Total ============
int_total_lbp_2011 <- sum(spoly_au_counts_al@data$cnt_11)
int_total_lbp_2015 <- sum(spoly_au_counts_al@data$cnt_15)

flt_cagr_al <- fn_carg(previous_value = int_total_lbp_2011, current_value = int_total_lbp_2015, 5)
# 25.65%
flt_cagr_al

# == Calculate CAGR for NON-Auckland
spoly_au_counts_non_al <- spoly_au_counts[!vct_logic_in_auckland, ]

# 1596 polygons here. Check that the 1911 superset partitions out ok
nrow(spoly_au_counts_non_al) == (1911 - 315)

int_total_non_al_2011 <- sum(spoly_au_counts_non_al$cnt_11)
int_total_non_al_2015 <- sum(spoly_au_counts_non_al$cnt_15)

flt_cagr_non_al <- fn_carg(previous_value = int_total_non_al_2011, current_value = int_total_non_al_2015, 5)

# 17.8 %
flt_cagr_non_al

# == Calculate CAGR for ALL New Zealand
int_total_NZ_2011 <- sum(spoly_au_counts@data$cnt_11)
int_total_NZ_2015 <- sum(spoly_au_counts@data$cnt_15)  


flt_cagr_NZ <- fn_carg(previous_value = int_total_NZ_2011, current_value = int_total_NZ_2015, 5)
# 19.9 %
flt_cagr_NZ
# Check the totals add up
(int_total_NZ_2011 - int_total_non_al_2011 - int_total_lbp_2011) == 0
(int_total_NZ_2015 - int_total_non_al_2015 - int_total_lbp_2015) == 0

# ========================================================
# ========================================================
nrow(spoly_au_counts_al@data)
# some total from the census for Auckland
al_census_pop_06 <- sum(df_auckland$c_06_tot)
al_census_pop_13 <- sum(df_auckland$c_13_tot)
# ============================
# Save the shapefile : spoly_au_counts_al
message("writing auckland subset of aerial units to disk. See:")
message("data/polygons_315_auckland_counts_wgs84")

rgdal::writeOGR(spoly_au_counts_al, dsn = "data/polygons_315_auckland_counts_wgs84", 
                layer = "al_poly_counts", driver="ESRI Shapefile", overwrite_layer = TRUE)

rm(list = ls())

