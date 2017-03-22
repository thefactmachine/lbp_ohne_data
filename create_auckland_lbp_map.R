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

# get the data frame from the auckland au shapefile this is just a generic set
# of unique identifiers for AL polygons

# PROVENCE INFORMATION
# For the provenance of the Aerial Unit please see:
#  fn_load_areal_unit <- function()

# Load in Auckland polygon counts (wgs 84)
spoly_al_counts <- rgdal::readOGR(dsn = "data/polygons_315_auckland_counts_wgs84", 
                              layer =  "al_poly_counts")
sum(spoly_al_counts@data$yt) == 22401

# set colors ==========================
spoly_al_counts@data %>% arrange(cagr) %>% View()

nrow(spoly_al_counts@data) == 315
# number of NAs == 38
sum(is.na(spoly_al_counts@data$cagr)) == 38
# partition the data into negative and positive cagr


# Create Paritions (these are logical vectors)

# 1) Unmeasureable (NA's)
# 2) No Change (0)
# 3) Postive cagr

vct_logic_na <- is.na(spoly_al_counts@data$cagr)
# 38
sum(vct_logic_na)

vct_logic_nc <- spoly_al_counts@data$cagr == 0
vct_logic_nc[is.na(vct_logic_nc)] <- FALSE
# 6
sum(vct_logic_nc)

vct_logic_pos <- spoly_al_counts@data$cagr > 0
vct_logic_pos[is.na(vct_logic_pos)] <- FALSE
# 271
sum(vct_logic_pos)
271 + 6 + 38 == 315

# Now we create 3 partitions:
df_unmeasureable <- spoly_al_counts@data[vct_logic_na,]
df_no_change <- spoly_al_counts@data[vct_logic_nc,]
df_positive <- spoly_al_counts@data[vct_logic_pos,]
# Assert: non-overlapping and exhaustive
nrow(df_unmeasureable) + nrow(df_no_change) + nrow(df_positive)

# Assign the quantiles:
df_unmeasureable$q <- 1
df_no_change$q <- 2



# create 3 quantiles for postive growth (these range from 3 to 5)
vct_quant <- fn_quantile(df_positive$cagr, 3) + 2
table(vct_quant)
# print out the break points
fn_quantile_divs(df_positive$cagr, 3) 
df_positive$q <- vct_quant

 # ===================
# put humpty dumpty back together:
df_compile <- rbind(df_unmeasureable, df_no_change, df_positive)
nrow(df_compile) == nrow(spoly_al_counts@data) 

# create the factor
vct_labs <- c("Insufficient data", "No change", 
              "Lower growth (0 - 33%)", "Medium growth (33 - 66%)", "Highest growth ( > 66%)")


df_compile$q_fact <- factor(df_compile$q, 
                      sort(unique(df_compile$q)), 
                      labels = vct_labs, ordered = TRUE)


# join it back to the spatial polygons
head(spoly_al_counts@data)

# putting the spatial data.frame first makes sure the sort order is preseved
spoly_al_counts@data <- spoly_al_counts@data %>% 
                        inner_join(df_compile[, c("AU2013", "q", "q_fact")] , 
                           c("AU2013" = "AU2013")) 

head(spoly_al_counts@data)

vct_colors <- c("#999999","#ffffbf","#e0f3f8","#91bfdb","#4575b4")

# spoly_al_counts

# =============================================

# fortify and then add back the data
df_al_counts_f <- ggplot2::fortify(model = spoly_al_counts, region = "AU2013")

df_al_counts_f_data <- df_al_counts_f %>% 
                        dplyr::inner_join(spoly_al_counts@data, c("id" = "AU2013"))

nrow(df_al_counts_f) == nrow(df_al_counts_f_data)
# =====================


b <- bbox(spoly_al_counts)
b[1, ] <- (b[1, ] - mean(b[1, ])) * 1.05 + mean(b[1, ])
b[2, ] <- (b[2, ] - mean(b[2, ])) * 1.05 + mean(b[2, ])

# zoom 11 is about 12 tiles
auck.b <- ggmap(get_map(location = b, source = "stamen", maptype = "toner", crop = T, zoom = 11))
# auck.b  <- ggmap(get_map(location = b))


p2 <- auck.b
# add the coloured polygons
p2 <- p2 + geom_polygon(data = df_al_counts_f_data,  
                        aes(x = long, y = lat, group = group,  fill = q_fact),
                        alpha = 1.0)

p2 <- p2 + geom_path(data = df_al_counts_f_data,  
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

message("saving lbp map to disk. See:")
message("final_images/lbp_x11_x15.pdf")

cairo_pdf("final_images/lbp_x11_x15.pdf", width = 11, height = 11)
print(p2)
dev.off()


rm(list = ls())



