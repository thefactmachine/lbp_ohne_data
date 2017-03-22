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

df_lbp_licenses <- df_new %>% group_by(year) %>% summarise(count = sum(count))
sum(df_lbp_licenses$count) == sum(df_new$count)

# ============================================
# ============================================
df_lbp_licenses

# ============================================
# ============================================
rm(list=(ls()[ls()!="df_lbp_licenses"]))


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
df_people <- df_plot

# =============================================
# =============================================
# =============================================

# make the indexes
# these two are the relevant things:
df_lbp_licenses
df_people


int_base_licenses <- df_lbp_licenses$count[df_lbp_licenses$year == 2011]
df_lbp_licenses$index <-  df_lbp_licenses$count / int_base_licenses
df_lbp_licenses$id <- "licenses"

int_base_people <- df_people$count[df_people$year == 2011]
df_people$index <- df_people$count / int_base_people
df_people$id <- "people"

df_compile <- rbind(df_lbp_licenses, df_people)

# ============================================

df_compile$id <- factor(df_compile$id)

df_compile$index <- df_compile$index * 100

vct_colours <- c("#1F7779", "#5EB31c")






p <- ggplot(df_compile, aes(x = year, y = index, fill = id))
p <- p + geom_bar(width = 0.7, stat = "identity", position = "dodge")
p <- p + theme_light(13, base_family = "Calibri")
p <- p + theme(panel.grid.minor.y = element_blank())
p <- p + theme(panel.grid.minor.x = element_blank())
p <- p + theme(panel.grid.major.x = element_blank())

p <- p + theme(panel.border = element_blank())
p <- p + theme(panel.background = element_blank())
p <- p + theme(axis.line = element_line(colour = "black"))



p <- p + labs(x = "Year")
p <- p + labs(y = "Index (base = 2011)")
p <- p + scale_y_continuous(label = scales::comma, breaks = c(100,200))
p <- p + scale_x_continuous(breaks = c(2011, 2012, 2013, 2014, 2015))
p <- p + scale_fill_manual(values = vct_colours)
p <- p + guides(fill=guide_legend(title="Classification", reverse = TRUE))
p <- p + theme(legend.text = element_text(lineheight = 0.6), 
               legend.key.height = grid::unit(0.8, "cm"), 
               legend.position = c(0.12, 0.72))
p

cairo_pdf("final_images/plot_index.pdf", width = 11.69, height = 8.27)
print(p)
dev.off() 


# Following for the commentary

y_2011_licenses <- df_compile %>% 
                    filter(as.character(id) == "licenses") %>%
                    filter(year == 2011) %>% .$count
y_2011_licenses
y_2015_licenses <- df_compile %>% 
                    filter(as.character(id) == "licenses") %>%
                    filter(year == 2015) %>% .$count
y_2015_licenses

y_2011_people <- df_compile %>% 
                    filter(as.character(id) == "people") %>%
                    filter(year == 2011) %>% .$count
y_2011_people

y_2015_people <-  df_compile %>% 
                  filter(as.character(id) == "people") %>%
                  filter(year == 2015) %>% .$count

y_2015_people

# =======
# licenses per person
# 2011
y_2011_licenses / y_2011_people

# 2015
y_2015_licenses / y_2015_people






