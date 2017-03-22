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
source("r_functions/fn_rescale.R")
source("r_functions/fn_quantile_divs.R")
source("r_functions/fn_quantile.R")
source("r_functions/fn_fuzz.R")



# get the data frame from the auckland au shapefile this is just a generic set
# of unique identifiers for AL polygons

df_auckland <- fn_create_auckland_df()
nrow(df_auckland) == 315

# ======================================================
# LOAD IN LBP

# this is the shapefile created by create_lbp_shapefiles.R
spoly_au_lbp_counts <- rgdal::readOGR(dsn = "data/aerial_unit_lbp_counts",
                                  layer = "au_lbp_counts")
# total from step 1
sum(spoly_au_lbp_counts@data$yt) == 107266

# subset to counts being greater than 5
vct_logic_valid <- spoly_au_lbp_counts@data$cnt_11 > 5 & spoly_au_lbp_counts@data$cnt_15 > 5
sum(vct_logic_valid)

df_valid <- spoly_au_lbp_counts@data[vct_logic_valid, ]
nrow(df_valid)
sum(df_valid$yt)
#% percent included
sum(df_valid$yt) / sum(spoly_au_lbp_counts@data$yt)

# get rid of some cols
df_valid <- df_valid[, c(1,2,6,7)]

# add in cagr
df_valid$lbp_cagr  <- fn_carg(previous_value = df_valid$cnt_11, current_value = df_valid$cnt_15, 5)

names(df_valid) <- c("AU2013", "AU2013_NAM", "lbp_2011", "lbp_2015", "lbp_cagr")

# ======================================================

# LOAD IN Consent data
spoly_au_consent_counts <- rgdal::readOGR(dsn = "data/polygons_1911_consents",
                                      layer = "au_consents_all_nz_wgs84")

names(spoly_au_consent_counts) <- c("AU2013", "AU2013_NAM", "consent_2011", "consent_2015", "AL", "consent_cagr")


df_compile <- df_valid %>% inner_join(spoly_au_consent_counts@data, 
                                      c("AU2013" = "AU2013", "AU2013_NAM" = "AU2013_NAM"))

# get rid of NAs
df_compile <- df_compile[!is.na(df_compile$lbp_cagr) & !is.na(df_compile$consent_cagr), ]

df_compile$Region <- factor(df_compile$AL, levels = c(1,0), 
                             labels = c("Auckland", "Not Auckland"), ordered = TRUE)

df_compile$lbp_2011 <- NULL
df_compile$lbp_2015 <- NULL
df_compile$AU2013 <- NULL
df_compile$AL <- NULL


# ===================================
# Scaling for the point size
vct_quant <- fn_quantile(vct_input = df_compile$consent_2011 + df_compile$consent_2015,
                         int_num_divisions = 20) 
max_point_size <- 4
df_compile$pt_size <- fn_rescale(vct_quant, new_min = 0.1, new_max = max_point_size)

# ===================================
# this gets rid of three exteme values
df_compile <- df_compile %>% filter(consent_cagr < 0.5)
# Re sort the data frame

df_compile <- df_compile %>% arrange(as.character(Region) == "Not Auckland")
# Add some fuzz to lbp_cagr
df_compile$lbp_cagr_fuzz <- fn_fuzz(df_compile$lbp_cagr, flt_pc_fuzz = 0.15)

vct_coef_all <- coef(lm(lbp_cagr ~ consent_cagr, data = 
                          df_compile))

vct_coef_not_al <- coef(lm(lbp_cagr ~ consent_cagr, data = 
                          df_compile[as.character(df_compile$Region) == "Not Auckland",]))

vct_coef_al <- coef(lm(lbp_cagr ~ consent_cagr, data = 
                             df_compile[as.character(df_compile$Region) == "Auckland",]))


# ========================================
# ========================================
# GRAPH TIME
#=========================================


# ======================= ALL

df_compile <- df_compile %>% arrange(desc(pt_size))

p <- ggplot(df_compile, aes(consent_cagr, lbp_cagr_fuzz,  color = Region))
p <- p + scale_color_manual(values = c("#28A7DB", "#f03b20"))
p <- p + geom_abline(intercept = vct_coef_all[1], slope = vct_coef_all[2], 
                     colour = "#636363", size = 0.5, linetype = 2, alpha = 0.5)
# p <- p + geom_abline(intercept = vct_coef_not_al[1], slope = vct_coef_not_al[2], colour = "#0000FF")
#p <- p + geom_abline(intercept = vct_coef_al[1], slope = vct_coef_al[2], colour = "#FF0000")
p <- p + geom_point(aes(size = pt_size), alpha = I(4/5))
p <- p + scale_size_area(max_size = max_point_size, guide = 'none')
p <- p + theme_light(13, base_family = "Calibri")


p <- p + theme(panel.border = element_blank())
p <- p + theme(panel.background = element_blank())
p <- p + theme(axis.line = element_line(colour = "black"))

p <- p + labs(x = "Number of consents CAGR (2011 - 2015)")
p <- p + scale_x_continuous(limits = c(-0.38, 0.45), breaks = c(-0.25, 0, 0.25), labels = scales::percent)
p <- p + scale_y_continuous(limits = c(-0.03, 0.47), breaks = c(0, 0.2, 0.4), labels = scales::percent)
p <- p + labs(y = "Number of LBP licensees CARG (2011 - 2015)")
p <- p + theme(panel.grid.minor.y = element_blank())
p <- p + theme(panel.grid.minor.x = element_blank())
p <- p + guides(fill=guide_legend(title="Region", reverse = TRUE))
# p <- p + facet_wrap(~ Region)
p <- p + theme(legend.text = element_text(lineheight = 0.6), 
               legend.key.height = grid::unit(0.8, "cm"), 
               legend.position = c(0.84, 0.83))
p



cairo_pdf("final_images/scatter_all.pdf", width = 11.69, height = 8.27)
print(p)
dev.off() 
# print regresion coefficients:
# intercept
vct_coef_all[1]
# slope
vct_coef_all[2]


# ========================================================
# Non Auckland
# c("#28A7DB", "#f03b20"))

df_non_ak <- df_compile %>% filter(as.character(Region) == "Not Auckland")

p <- ggplot(df_non_ak, aes(consent_cagr, lbp_cagr_fuzz,  color = Region))
p <- p + scale_color_manual(values = c("#f03b20"), guide = FALSE)

p <- p + geom_abline(intercept = vct_coef_not_al[1], slope = vct_coef_not_al[2], colour = "#636363", 
                     size = 0.5, linetype = 2, alpha = 0.5)
p <- p + geom_point(aes(size = pt_size), alpha = I(4/5))
p <- p + scale_size_area(max_size = max_point_size, guide = 'none')
p <- p + theme_light(11, base_family = "Calibri")

p <- p + theme(panel.border = element_blank())
p <- p + theme(panel.background = element_blank())
p <- p + theme(axis.line = element_line(colour = "black"))


p <- p + labs(x = "Number of consents CAGR (2011 - 2015)")
p <- p + scale_x_continuous(limits = c(-0.38, 0.45), breaks = c(-0.25, 0, 0.25), labels = scales::percent)
p <- p + scale_y_continuous(limits = c(-0.03, 0.47), breaks = c(0, 0.2, 0.4), labels = scales::percent)
p <- p + labs(y = "Number of LBP licensees CARG (2011 - 2015)")
p <- p + theme(panel.grid.minor.y = element_blank())
p <- p + theme(panel.grid.minor.x = element_blank())
p <- p + guides(fill=guide_legend(title="Region", reverse = TRUE))
# p <- p + facet_wrap(~ Region)
p <- p + theme(legend.text = element_text(lineheight = 0.6), 
               legend.key.height = grid::unit(0.8, "cm"), 
               legend.position = c(0.84, 0.83))
p

cairo_pdf("final_images/scatter_non_auckland.pdf", width = 11.69, height = 8.27)
print(p)
dev.off() 

# print regresion coefficients:
# intercept
vct_coef_not_al[1]
# slope
vct_coef_not_al[2]

# ========================================================
# Auckland
# "#f03b20"
df_ak <- df_compile %>% filter(as.character(Region) == "Auckland")

p <- ggplot(df_ak, aes(consent_cagr, lbp_cagr_fuzz,  color = Region))
p <- p + scale_color_manual(values = c("#28A7DB"), guide = FALSE)
p <- p + geom_abline(intercept = vct_coef_al[1], slope = vct_coef_al[2], colour = "#636363", 
                     size = 0.5, linetype = 2, alpha = 0.5)
p <- p + geom_point(aes(size = pt_size), alpha = I(4/5))
p <- p + scale_size_area(max_size = max_point_size, guide = 'none')
p <- p + theme_light(13, base_family = "Calibri")

p <- p + theme(panel.border = element_blank())
p <- p + theme(panel.background = element_blank())
p <- p + theme(axis.line = element_line(colour = "black"))


p <- p + labs(x = "Number of consents CAGR (2011 - 2015)")
p <- p + scale_x_continuous(limits = c(-0.38, 0.45), breaks = c(-0.25, 0, 0.25), labels = scales::percent)
p <- p + scale_y_continuous(limits = c(-0.03, 0.47), breaks = c(0, 0.2, 0.4), labels = scales::percent)
p <- p + labs(y = "Number of LBP licensees CARG (2011 - 2015)")
p <- p + theme(panel.grid.minor.y = element_blank())
p <- p + theme(panel.grid.minor.x = element_blank())
p <- p + guides(fill=guide_legend(title="Region", reverse = TRUE))
# p <- p + facet_wrap(~ Region)
p <- p + theme(legend.text = element_text(lineheight = 0.6), 
               legend.key.height = grid::unit(0.8, "cm"), 
               legend.position = c(0.84, 0.83))
p

cairo_pdf("final_images/scatter_auckland.pdf", width = 11.69, height = 8.27)
print(p)
dev.off() 

# print regresion coefficients:
# intercept
vct_coef_al[1]
# slope
vct_coef_al[2]

rm(list = ls())

