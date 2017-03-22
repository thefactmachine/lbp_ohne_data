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

spoly_lbp_all <- rgdal::readOGR(dsn = "data/polygons_1911_lbp_all_years",
                             layer = "nz_lbp_all_years")


# 2011 figures
y_2011_al <- spoly_lbp_all@data %>% filter(in_al == 1) %>% summarise(tot = sum(cnt_11)) %>% .$tot
y_2011_non_al <- spoly_lbp_all@data %>% filter(in_al == 0) %>% summarise(tot = sum(cnt_11)) %>% .$tot

sum(spoly_lbp_all@data$cnt_11) == y_2011_al + y_2011_non_al

# pc in Auckland (2011)
y_2011_al / sum(spoly_lbp_all@data$cnt_11) 


# 2015 figures (
y_2015_al <- spoly_lbp_all@data %>% filter(in_al == 1) %>% summarise(tot = sum(cnt_15)) %>% .$tot
y_2015_non_al <- spoly_lbp_all@data %>% filter(in_al == 0) %>% summarise(tot = sum(cnt_15)) %>% .$tot

sum(spoly_lbp_all@data$cnt_15) == y_2015_al + y_2015_non_al

y_2015_al / sum(spoly_lbp_all@data$cnt_15) 








