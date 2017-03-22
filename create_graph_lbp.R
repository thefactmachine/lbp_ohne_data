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
df_data <- df_raw[, c("license", "yr_2011", "yr_2012" , "yr_2013", "yr_2014" ,"yr_2015")]


df_long_data <- df_data %>% 
                group_by(license) %>% 
                summarise(y_2011 = sum(yr_2011), y_2012 = sum(yr_2012), 
                y_2013 = sum(yr_2013), y_2014 = sum(yr_2014),
                y_2015 = sum(yr_2015)) %>%
                tidyr::gather(year, count, -license) %>%
                mutate(year = gsub("y_", "", .$year) %>% as.numeric()) %>%
                as.data.frame()

# check total 
sum(df_long_data$count) == sum(df_check[, 2:6])

# club three categories together:
vct_other <- c("Bricklaying and Blocklaying", "External Plastering", "Foundations")
vct_logical_in_other <- df_long_data$license %in% vct_other
df_long_data$license <- ifelse(vct_logical_in_other, "Other", df_long_data$license)

# squash things down ... so there are no duplicates
df_long_data <- df_long_data %>% group_by(license, year) %>%
                summarise(count = sum(count)) 

sum(df_long_data$count) == sum(df_check[, 2:6])

vct_sort <- df_long_data %>% 
            group_by(license) %>% 
            summarise(count = sum(count)) %>% 
            arrange(desc(count)) %>% .$license


q_fact <- factor(df_long_data$license, levels = vct_sort, ordered = TRUE)

df_long_data$fact <- q_fact
head(df_long_data)

df_long_data$sort <- as.integer(df_long_data$fact)

df_new <- df_long_data[order(df_long_data$sort),] %>% as.data.frame()
df_new


# get a sort order for the factor

# bright cols
 vct_colours <- c("#e41a1c","#377eb8","#4daf4a","#984ea3", "#ff7f00")
# blues
# vct_colours <- c("#ffffcc","#a1dab4","#41b6c4","#2c7fb8","#253494") %>% rev()
# pastel
# vct_colours <- c("#7fc97f","#beaed4","#fdc086","#ffff99","#386cb0")

p <- ggplot(df_new, aes(x = year, y = count, fill = fact))
p <- p + geom_bar(width = 0.7, stat = "identity", position = "stack")
p <- p + theme_light(13, base_family = "Calibri")
p <- p + theme(panel.grid.minor.y = element_blank())
p <- p + theme(panel.grid.minor.x = element_blank())
p <- p + theme(panel.grid.major.x = element_blank())

p <- p + theme(panel.border = element_blank())
p <- p + theme(panel.background = element_blank())
p <- p + theme(axis.line = element_line(colour = "black"))

p <- p + labs(x = "Year")
p <- p + labs(y = "LBP Licenses")
p <- p + scale_y_continuous(label = scales::comma)
p <- p + scale_x_continuous(breaks = c(2011, 2012, 2013, 2014, 2015))
p <- p + scale_fill_manual(values = vct_colours)
p <- p + guides(fill=guide_legend(title="License Type", reverse = TRUE))
p <- p + theme(legend.text = element_text(lineheight = 0.6), 
               legend.key.height = grid::unit(0.8, "cm"), 
               legend.position = c(0.12, 0.80))
p


cairo_pdf("final_images/plot_licenses.pdf", width = 11.69, height = 8.27)
print(p)
dev.off() 

y_2011 <- df_new %>% filter(year == 2011) %>% summarise(tot = sum(count)) %>% .$tot
y_2012 <- df_new %>% filter(year == 2012) %>% summarise(tot = sum(count)) %>% .$tot
y_2015 <- df_new %>% filter(year == 2015) %>% summarise(tot = sum(count)) %>% .$tot

source("r_functions/fn_carg.R")

fn_carg(previous_value = y_2011, current_value = y_2012, int_year_diff = 1)
fn_carg(previous_value = y_2012, current_value = y_2015, int_year_diff = 3)


