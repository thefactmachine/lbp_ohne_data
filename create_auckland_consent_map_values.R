library(ggplot2)
library(rgdal)
library(rgeos)
library(maptools)
library(ggmap)
library(dplyr)
library(Cairo)

# CODE SYNOPSIS
# get a data.frame of Auckland au polygon names and census06 & 13 counts of people

options(stringsAsFactors = FALSE)
setwd("/Users/zurich/Documents/TEMP-FILES/lbp_project/final_lbp")
rm(list = ls())
# This function creates quantiles
source("r_functions/fn_quantile.R")
source("r_functions/fn_quantile_divs.R")



# Load in Auckland polygon data 
spoly_al_consent <- rgdal::readOGR(dsn = "data/polygons_315_consent", 
                                   layer =  "au_consents_wgs84")

# spoly_al_consent@data %>% arrange(cagr_value) %>% View()
# CODE SYNOPSIS
# Create logical vectors
# Subset according to logical vectors


# 12 NAs

# MISSING

# ===============================
# subset according to logical vectors
# MISSING

# ================
# split the data into good (i.e zero nas and  no good == nas)
df_data_good <- spoly_al_consent@data[!is.na(spoly_al_consent@data$cagr_value),]
# 12 NAs
df_data_na <- spoly_al_consent@data[is.na(spoly_al_consent@data$cagr_value),]

# rec the two
nrow(df_data_good) + nrow(df_data_na) == 315

# split up the good data into three: (no change (21), positive (197) and negative (85))
df_no_change <- df_data_good[abs(df_data_good$cagr_value) < 0.01,]
df_positive <- df_data_good[(df_data_good$cagr_value) >= 0.01, ]
df_negative <- df_data_good[(df_data_good$cagr_value) <= -0.01, ]

# will split up the data into 7 components:
# 1) Insufficient data [ie. NA]
# 2) Strong negative growth
# 3) Low negative growth
# 4) No change (-1% ~ 1%)
# 5) Low positive growth
# 6) Mild positive growth
# 7) High positive growth

df_data_na$q <- 1

# NEGATIVE (
fn_quantile_divs(df_negative$cagr_value, 2) 
# add 1 to ensure that the codes are from 2..3 (not 1..2)
df_negative$q <- fn_quantile(df_negative$cagr_value, 2) + 1

df_no_change$q <- 4
# POSITIVE (5 .. 7; need to add 4)
# fn_quantile(df_positive$cagr_value, 3) %>% table()
fn_quantile_divs(df_positive$cagr_value, 3) 
df_positive$q <-  fn_quantile(df_positive$cagr_value, 3) + 4

df_compile <- rbind(df_data_na, df_negative, df_no_change, df_positive)
nrow(df_compile) == 315

vct_relevant_cols <-c("AU2013", "AU2013_NAM", "total_2011", "total_2015", "cagr_count", "cagr_value", "q")
# make things easier to view
df_compile <- df_compile[, vct_relevant_cols]

# print to screen
df_compile[20:30,]

# we now put things back together. Use a join rather than
# smash the data back in. We need to preserve the original
# row order the first argument in the inner_join determines
# this row order

spoly_al_consent@data <- spoly_al_consent@data[, 1:2] %>% 
                         dplyr::inner_join(df_compile, 
                          c("AU2013" = "AU2013", "AU2013_NAM" = "AU2013_NAM"))

spoly_al_consent@data %>% head()


# create the factor
vct_labs <- c("Insufficient data", 
              "Strong negative growth (< -10%)", 
              "Low negative growth (-1 ~ -10%)",
              "No change (-1 ~ 1%) ", 
              "Low positive growth (1 ~ 10%)", 
              "Medium positive growth (10 ~ 26%)",
              "Strong positive growth (> 26 %)")



q_fact <- factor(spoly_al_consent@data$q, 
                 sort(unique(spoly_al_consent@data$q)), 
                 labels = vct_labs, ordered = TRUE)

spoly_al_consent@data$q_fact <- q_fact

# print a sample ... make sure things matched.
spoly_al_consent@data[1:10, c("AU2013", "q", "q_fact")]

# grey(1) reds (x2 - descending) yellow (1), greens (x3 - ascending) 
vct_colors  <- c("#999999", "#d73027", "#fc8d59", "#ffffbf","#d9ef8b", "#91cf60","#1a9850")


# fortify and then add back the data
df_al_counts_f <- ggplot2::fortify(model = spoly_al_consent, region = "AU2013")
df_al_counts_f_data <- df_al_counts_f %>% dplyr::inner_join(spoly_al_consent@data, c("id" = "AU2013"))

nrow(df_al_counts_f_data) == nrow(df_al_counts_f)

# check the join:
df_al_counts_f %>% dplyr::anti_join(spoly_al_consent@data, c("id" = "AU2013")) %>% nrow()


b <- bbox(spoly_al_consent)
b[1, ] <- (b[1, ] - mean(b[1, ])) * 1.05 + mean(b[1, ])
b[2, ] <- (b[2, ] - mean(b[2, ])) * 1.05 + mean(b[2, ])

# zoom 11 is about 12 tiles
auck.b <- ggmap(get_map(location = b, source = "stamen", maptype = "toner", crop = T, zoom = 12))


p2 <- auck.b
# add the coloured polygons
p2 <- p2 + geom_polygon(data = df_al_counts_f_data,  
                        aes(x = long, y = lat, group = group,  fill = q_fact),
                        alpha = 1.0)

p2 <- p2 + geom_path(data = df_al_counts_f,  
                     aes(x = long, y = lat, group = group),
                     colour="black", size = 0.2,
                     alpha = 1.0)
# coord_equal()
p2 <- p2 + coord_equal()
p2 <- p2 + theme(axis.text.x = element_blank(), 
                 axis.text.y = element_blank(),
                 axis.ticks = element_blank())
p2 <- p2 + scale_fill_manual(values = vct_colors )
p2 <- p2 + labs(x = "", y = "", fill = "Compound annual growth rate  (2011 - 2015)")
p2 <- p2 + theme(
  legend.title = element_text(colour = "white", size = 14, lineheight = 24),
  legend.text = element_text(lineheight = 0.6, colour = "white"), 
  legend.key.height = grid::unit(0.5, "cm"), 
  legend.position = c(0.6, 0.9), 
  legend.key = element_rect(fill=alpha('black', 1.0), colour = "black"),
  legend.background = element_rect(fill=alpha('black', 1.0))
)

#p2 <- p2 +  ggtitle("Auckland Growth Rates in LBP")
p2
message("writing consent map to disk. See:")
message("final_images/consents_x11_x15.pdf")

cairo_pdf("final_images/consents_x11_x15.pdf", width = 11, height = 11)
print(p2)
dev.off()

rm(list = ls())



