library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
library(sp)
library(rgdal)
library(tidyr)
library(Cairo)



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

# ===============================
df_data <- df_raw[, c("guid", "license", "yr_2011", "yr_2012" , "yr_2013", "yr_2014" ,"yr_2015")]



int_2011 <- df_data %>% filter(yr_2011 != 0) %>% .$guid %>% unique() %>% length()
int_2012 <- df_data %>% filter(yr_2012 != 0) %>% .$guid %>% unique() %>% length()
int_2013 <- df_data %>% filter(yr_2013 != 0) %>% .$guid %>% unique() %>% length()
int_2014 <- df_data %>% filter(yr_2014 != 0) %>% .$guid %>% unique() %>% length()
int_2015 <- df_data %>% filter(yr_2015 != 0) %>% .$guid %>% unique() %>% length()

vct_counts <- c(int_2011, int_2012, int_2013, int_2014, int_2015)
vct_years <- 2011:2015

df_plot <- data.frame (year = vct_years, count = vct_counts)
df_plot


p <- ggplot(df_plot, aes(x = year, y = count))
p <- p + geom_bar(fill = "#2EB6E0", width = 0.7, stat = "identity")
p <- p + theme_light(11, base_family = "Calibri")
p <- p + theme(panel.grid.minor.y = element_blank())
p <- p + theme(panel.grid.minor.x = element_blank())
p <- p + theme(panel.grid.major.x = element_blank())
p <- p + labs(x = "Year")
p <- p + labs(y = "Number of licencees")
p <- p + scale_y_continuous(label = scales::comma)
p <- p + theme(legend.text = element_text(lineheight = 0.6), 
               legend.key.height = grid::unit(0.8, "cm"), 
               legend.position = c(0.8, 0.88))
p


cairo_pdf("final_images/plot_licencees.pdf", width = 11.69, height = 8.27)
print(p)
dev.off() 

rm(list = ls())

