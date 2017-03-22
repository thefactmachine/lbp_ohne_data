library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
library(sp)
library(rgdal)
library("openxlsx")
library("dplyr")

rm(list = ls())
options(stringsAsFactors = FALSE) 
options(scipen=999)


setwd("/Users/zurich/Documents/TEMP-FILES/lbp_project/final_lbp")


df_sp_pop_raw <- read.xlsx("data/stats_nz/stats_NZ_population_counts_area_unit_all_years.xlsx", 
                           sheet = 1, startRow = 7, 
                           colNames = FALSE)

# drop the second column...it contains nothing.
df_sp_pop_raw <- df_sp_pop_raw[, -2]

# assign some better names
vct_c_names <- c("area", "c_96_tot", "c_01_tot", "c_06_tot", "c_13_tot", "c_96_male", 
                 "c_01_male", "c_06_male", "c_13_male", "c_96_fmale", "c_01_fmale", "c_06_fmale", "c_13_fmale")

names(df_sp_pop_raw) <- vct_c_names

# get rid of the last 3 rows
df_sp_pop_raw <- df_sp_pop_raw[1:4134, ]

# used this to work out which columns had non numerics
aa <- grepl("^\\d+$", df_sp_pop_raw$c_13_fmale)
bb <- df_sp_pop_raw[!aa , "c_13_fmale"]
unique(bb)
# from the above found that it was: 
# "c_01_male", "c_06_male", "c_13_male", "c_01_fmale", "c_06_fmale", "c_13_fmale"
# they all had ".."

df_sp_pop_raw$c_01_male <- ifelse(df_sp_pop_raw$c_01_male == "..", 0,  df_sp_pop_raw$c_01_male)
df_sp_pop_raw$c_06_male  <- ifelse(df_sp_pop_raw$c_06_male == "..", 0,  df_sp_pop_raw$c_06_male)
df_sp_pop_raw$c_13_male  <- ifelse(df_sp_pop_raw$c_13_male == "..", 0,  df_sp_pop_raw$c_13_male)
df_sp_pop_raw$c_01_fmale <- ifelse(df_sp_pop_raw$c_01_fmale == "..", 0,  df_sp_pop_raw$c_01_fmale)
df_sp_pop_raw$c_06_fmale <- ifelse(df_sp_pop_raw$c_06_fmale == "..", 0,  df_sp_pop_raw$c_06_fmale)
df_sp_pop_raw$c_13_fmale <- ifelse(df_sp_pop_raw$c_13_fmale == "..", 0,  df_sp_pop_raw$c_13_fmale)

df_sp_pop_raw$c_01_male <- as.numeric(df_sp_pop_raw$c_01_male)
df_sp_pop_raw$c_06_male <- as.numeric(df_sp_pop_raw$c_06_male)
df_sp_pop_raw$c_13_male <- as.numeric(df_sp_pop_raw$c_13_male)

df_sp_pop_raw$c_01_fmale <- as.numeric(df_sp_pop_raw$c_01_fmale)
df_sp_pop_raw$c_06_fmale <- as.numeric(df_sp_pop_raw$c_06_fmale)
df_sp_pop_raw$c_13_fmale <- as.numeric(df_sp_pop_raw$c_13_fmale)


# ==============================================================

lgl_area <- grepl("^\\s+[0-9]{6}:", df_sp_pop_raw$area)
au_string <- unique(df_sp_pop_raw[ lgl_area, "area"])
df_au_sting_lu <- data.frame(orig_au = au_string , code = substr(au_string, start = 5, stop = 10))

df_sp_pop_raw_six_code <- df_sp_pop_raw %>% inner_join(df_au_sting_lu, c("area" = "orig_au"))
df_sp_pop_raw_NOT_six_code <- df_sp_pop_raw %>% anti_join(df_au_sting_lu, c("area" = "orig_au"))
nrow(df_sp_pop_raw_six_code) + nrow(df_sp_pop_raw_NOT_six_code) == nrow(df_sp_pop_raw)

df_sp_pop_raw_six_code %>% arrange(code) %>% head()

# 2012 rows here  
nrow(df_sp_pop_raw_six_code %>% group_by(code) %>% summarise(count = n()) %>% filter(count == 2)) == 2012

# zero rows here
nrow(df_sp_pop_raw_six_code %>% group_by(code) %>% summarise(count = n()) %>% filter(count == 1)) == 0

# zero rows here TOO
nrow(df_sp_pop_raw_six_code %>% group_by(code) %>% summarise(count = n()) %>% filter(count > 2)) == 0

# remove all the duplicates
df_areal_unit_counts <- df_sp_pop_raw_six_code %>% distinct()

# ============================
# RECONCILIATION
# get a vector of totals
vct_totals <- df_areal_unit_counts %>% select(-c(area, code)) %>% summarise_each(funs(sum)) %>% as.vector()
vct_totals_orig <- df_sp_pop_raw[1, 2:13] %>% as.vector()
# Differences (not much)
vct_totals_orig  - vct_totals
# maximum difference
max(abs(vct_totals_orig  - vct_totals))
# =====================

# POST-CONDITION
# df_areal_unit_counts 2012 rows of total / male / female [3 variables] for four censuses.

ls()[!(ls() %in% c('df_areal_unit_counts'))]
rm(list= ls()[!(ls() %in% c('df_areal_unit_counts'))])

# check uniqueness of pk
length(unique(df_areal_unit_counts$code)) == nrow(df_areal_unit_counts)
# 2012
nrow(df_areal_unit_counts)
# load in Auckland:
source("r_functions/fn_create_auckland_df.R")
df_auckland <- fn_create_auckland_df()


logic_in_auckland <- df_areal_unit_counts$code %in% df_auckland$AU2013

df_areal_unit_counts$in_al <- logic_in_auckland


# STUFF for 2013
int_people_nz_2013 <- sum(df_areal_unit_counts$c_13_tot)
int_people_nz_2013

int_people_auckland_2013 <- sum(df_areal_unit_counts$c_13_tot[df_areal_unit_counts$in_al == TRUE])
int_people_auckland_2013

int_people_not_in_auckland_2013 <-  sum(df_areal_unit_counts$c_13_tot[df_areal_unit_counts$in_al == FALSE])
int_people_not_in_auckland_2013

# Rec the above:
int_people_nz_2013 == int_people_auckland_2013 + int_people_not_in_auckland_2013

# STUFF for 2006:
int_people_nz_2006 <- sum(df_areal_unit_counts$c_06_tot)
int_people_nz_2006

int_people_auckland_2006 <- sum(df_areal_unit_counts$c_06_tot[df_areal_unit_counts$in_al == TRUE])
int_people_auckland_2006

int_people_not_in_auckland_2006 <-  sum(df_areal_unit_counts$c_06_tot[df_areal_unit_counts$in_al == FALSE])
int_people_not_in_auckland_2006

# Rec the above:
int_people_nz_2006 == int_people_auckland_2006 + int_people_not_in_auckland_2006


source("r_functions/fn_carg.R")

# CAGR auckland
fn_carg(previous_value = int_people_auckland_2006, current_value = int_people_auckland_2013, int_year_diff = 7)

# CAGR non auckland
fn_carg(previous_value = int_people_not_in_auckland_2006, current_value = int_people_not_in_auckland_2013, int_year_diff = 7)








