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


sp_poly_lbp_all <- rgdal::readOGR(dsn = "data/polygons_1911_lbp_all_years",
                             layer = "nz_lbp_all_years")

sp_poly_consent_all <- rgdal::readOGR(dsn = "data/polygons_1911_consents_value_all_years",
                             layer = "1911_consents_value_all_years_wgs84")

sp_poly_consent_unfiltered <- 
  rgdal::readOGR(dsn = "data/polygons_1911_unfiltered_consent_counts_all_years",
                layer = "1911_unfiltered_consent_counts_all_years_wgs84")

# =========================================================

# this is license and people totals for 1911 polygons
df_lbp_all <- sp_poly_lbp_all@data

# this filtered (outliers removed.  Value and # Consents removed for all years)
df_consent_all_filtered <- sp_poly_consent_all@data

# this is unfiltered consents (outliers not removed) Counts only all years.
df_unfiltered_consent <- sp_poly_consent_unfiltered@data

# =========================================================

# What we want to do here is to COMPARE the growth of 
# licenses Vs consents
#  For: All NZ, Auckland, Non - Auckland (3  basic Graphs)
# Consents will be measured by: 
# 1) Filtered Value (outliers removed) [ 3 graphs]
# 2) Unfiltered Counts (outliers not removed) [ 3 graphs]

# Total graphs will be six;
# Naming convention:
# 1 "plot_ldp_consent_index_filtered_val_all"
# 2 "plot_ldp_consent_index_filtered_val_auck"
# 3 "plot_ldp_consent_index_filtered_val_non_auck"

# 4 "plot_ldp_consent_index_unfiltered_count_all"
# 5 "plot_ldp_consent_index_unfiltered_count_auck"
# 5 "plot_ldp_consent_index_unfiltered_count_non_auck"

# ===============================================
# CREATE basic data summaries

# ============================================
# Licenses created by : source("create_ldp_all_years.R") (see line 271 )
sum(df_lbp_all$yt) == 110139 

# group by auckland 
df_lbp_all_agg <- df_lbp_all %>% group_by(in_al) %>% 
                    summarise(y_2011 = sum(yt_2011), 
                              y_2012 = sum(yt_2012),
                              y_2013 = sum(yt_2013),
                              y_2014 = sum(yt_2014),
                              y_2015 = sum(yt_2015),
                              count_check = n())

# rec it baby. Auckland count is 315
sum(df_lbp_all_agg[, 2:6]) == 110139 

df_lbp_all_agg$count_check <- NULL


df_lbp_all_agg_long <- df_lbp_all_agg %>% 
                        tidyr::gather(year, number, -in_al) %>% 
                        mutate(year = gsub("y_", "", .$year) %>% 
                        as.numeric()) %>% as.data.frame()

sum(df_lbp_all_agg_long$number) == 110139
df_lbp_all_agg_long$in_al <- as.logical(df_lbp_all_agg_long$in_al)

df_lbp_all_agg_long$type <- "licenses"
df_lbp_all_agg_long


# ============================================
# consents filtered value
# structure: in_al, year, number, type

head(df_consent_all_filtered)

vct_ss <- c("total_2011", "total_2012", "total_2013", "total_2014", "total_2015")

# total in millions of dollars to reconcile to (floating point number has an error)
abs(sum(df_consent_all_filtered[, vct_ss]) - 20326.04) < 0.1

df_filterd_consent_value_agg <- df_consent_all_filtered  %>%
                                group_by(in_al) %>%
                                summarise(y_2011 = sum(total_2011), 
                                          y_2012 = sum(total_2012),
                                          y_2013 = sum(total_2013),
                                          y_2014 = sum(total_2014),
                                          y_2015 = sum(total_2015)) %>%
                                 tidyr::gather(year, number, -in_al) %>%
                                 mutate(year = gsub("y_", "", .$year) %>% 
                                 as.numeric()) %>% as.data.frame()
  
df_filterd_consent_value_agg$in_al <- as.logical(df_filterd_consent_value_agg$in_al)
df_filterd_consent_value_agg$type <- "consent value"
abs(sum(df_filterd_consent_value_agg$number)) - 20326.04 < 0.1

df_filterd_consent_value_agg

# ============================================
# consents unfiltered counts
head(df_unfiltered_consent)
sum(df_unfiltered_consent[, 3:7]) == 235125

df_unfilterd_consent_count_agg <- df_unfiltered_consent  %>%
                                group_by(in_al) %>%
                                summarise(y_2011 = sum(count_2011), 
                                y_2012 = sum(count_2012),
                                y_2013 = sum(count_2013),
                                y_2014 = sum(count_2014),
                                y_2015 = sum(count_2015)) %>%
                                tidyr::gather(year, number, -in_al) %>%
                                mutate(year = gsub("y_", "", .$year) %>% 
                                as.numeric()) %>% as.data.frame()



df_unfilterd_consent_count_agg$in_al <- as.logical(df_unfilterd_consent_count_agg$in_al)
df_unfilterd_consent_count_agg$type <- "consent count"

sum(df_unfilterd_consent_count_agg$number) == 235125

# ==============================================
# okay string our 3 data frames together
df_composite <- rbind(df_lbp_all_agg_long, 
                      df_filterd_consent_value_agg, 
                      df_unfilterd_consent_count_agg)

# We want the following:
# licenses = 110139
# consent value = 20326.04
# consent count = 235125
df_composite %>% group_by(type) %>% summarise(tot = sum(number))

# ==============================================
# GRAPHS
# GRAPHS
# ==============================================
# First the following:

# 1 "plot_ldp_consent_index_filtered_val_all"
# 2 "plot_ldp_consent_index_filtered_val_auck"
# 3 "plot_ldp_consent_index_filtered_val_non_auck"

# ======================================
# GRAPH 1
# 1 "plot_ldp_consent_index_filtered_val_all"
df_lic_all <- df_composite %>% filter(type == "licenses") %>% 
                                group_by(year) %>% 
                                summarise(tot = sum(number)) %>% 
                                mutate(type = "licenses")

df_lic_all$index <- (df_lic_all$tot / df_lic_all$tot[df_lic_all$year == 2011]) * 100
df_lic_all


df_consent_all <- df_composite %>% filter(type == "consent value") %>% 
                                group_by(year) %>% 
                                summarise(tot = sum(number)) %>% 
                                mutate(type = "consent value")

df_consent_all$index <- (df_consent_all$tot / df_consent_all$tot[df_consent_all$year == 2011]) * 100

df_consent_all

df_graph1 <- rbind(df_lic_all, df_consent_all)
df_graph1

# mbie colors (light green, dark green)
vct_colours <- c("#1F7779", "#5EB31c")
g1 <- ggplot(df_graph1, aes(x = year, y = index, fill = type))
g1 <- g1 + geom_bar(width = 0.7, stat = "identity", position = "dodge")
g1 <- g1 + theme_light(13, base_family = "Calibri")
g1 <- g1 + theme(panel.grid.minor.y = element_blank())
g1 <- g1 + theme(panel.grid.minor.x = element_blank())
g1 <- g1 + theme(panel.grid.major.x = element_blank())


g1 <- g1 + theme(panel.border = element_blank())
g1 <- g1 + theme(panel.background = element_blank())
g1 <- g1 + theme(axis.line = element_line(colour = "black"))


g1 <- g1 + labs(x = "Year")
g1 <- g1 + labs(y = "Index (base = 2011)")
g1 <- g1 + scale_y_continuous(label = scales::comma, breaks = c(100,200, 287), limits = c(0, 315))
g1 <- g1 + scale_x_continuous(breaks = c(2011, 2012, 2013, 2014, 2015))
g1 <- g1 + scale_fill_manual(values = vct_colours)
g1 <- g1 + guides(fill=guide_legend(title="Classification", reverse = TRUE))
g1 <- g1 + theme(legend.text = element_text(lineheight = 0.6), 
               legend.key.height = grid::unit(0.8, "cm"), 
               legend.position = c(0.14, 0.76))
g1





cairo_pdf("final_images/plot_ldp_consent_index_filtered_val_all.pdf", width = 11.69, height = 8.27)
print(g1)
dev.off()

# ======================================
# GRAPH 2
# 2 "plot_ldp_consent_index_filtered_val_auck"

df_lic_auck <- df_composite %>% 
                  filter(type == "licenses") %>% 
                  filter(in_al == TRUE) %>%
                  group_by(year) %>% 
                  summarise(tot = sum(number)) %>% 
                  mutate(type = "licenses")

df_lic_auck
                                    

df_lic_auck$index <- (df_lic_auck$tot / df_lic_auck$tot[df_lic_auck$year == 2011]) * 100
df_lic_auck



df_consent_all_auck <-  df_composite %>% 
                        filter(type == "consent value") %>% 
                        filter(in_al == TRUE) %>%
                        group_by(year) %>% 
                        summarise(tot = sum(number)) %>% 
                        mutate(type = "consent value")


df_consent_all_auck$index <- 
  (df_consent_all_auck$tot / df_consent_all_auck$tot[df_consent_all_auck$year == 2011]) * 100

df_consent_all_auck

df_graph2 <- rbind(df_lic_auck, df_consent_all_auck)
df_graph2

# mbie colors (dark blue, light blue)
vct_colours <- c( "#296D7A", "#2DB6E2")
g2 <- ggplot(df_graph2, aes(x = year, y = index, fill = type))
g2 <- g2 + geom_bar(width = 0.7, stat = "identity", position = "dodge")
g2 <- g2 + theme_light(13, base_family = "Calibri")
g2 <- g2 + theme(panel.grid.minor.y = element_blank())
g2 <- g2 + theme(panel.grid.minor.x = element_blank())
g2 <- g2 + theme(panel.grid.major.x = element_blank())

g2 <- g2 + theme(panel.border = element_blank())
g2 <- g2 + theme(panel.background = element_blank())
g2 <- g2 + theme(axis.line = element_line(colour = "black"))

g2 <- g2 + labs(x = "Year")
g2 <- g2 + labs(y = "Index (base = 2011)")
g2 <- g2 + scale_y_continuous(label = scales::comma, breaks = c(100,200, 287), limits = c(0, 315))
g2 <- g2 + scale_x_continuous(breaks = c(2011, 2012, 2013, 2014, 2015))
g2 <- g2 + scale_fill_manual(values = vct_colours)
g2 <- g2 + guides(fill=guide_legend(title="Classification", reverse = TRUE))
g2 <- g2 + theme(legend.text = element_text(lineheight = 0.6), 
                 legend.key.height = grid::unit(0.8, "cm"), 
                 legend.position = c(0.14, 0.76))
g2



cairo_pdf("final_images/plot_ldp_consent_index_filtered_val_auck.pdf", width = 11.69, height = 8.27)
print(g2)
dev.off()

# ======================================
# GRAPH 3
# 3 "plot_ldp_consent_index_filtered_val_non_auck"

df_lic_NON_auck <- df_composite %>% 
                    filter(type == "licenses") %>% 
                    filter(in_al == FALSE) %>%
                    group_by(year) %>% 
                    summarise(tot = sum(number)) %>% 
                    mutate(type = "licenses")

df_lic_NON_auck

df_lic_NON_auck$index <- (df_lic_NON_auck$tot / df_lic_NON_auck$tot[df_lic_NON_auck$year == 2011]) * 100
df_lic_NON_auck



df_consent_all_NON_auck <-  df_composite %>% 
                            filter(type == "consent value") %>% 
                            filter(in_al == FALSE) %>%
                            group_by(year) %>% 
                            summarise(tot = sum(number)) %>% 
                            mutate(type = "consent value")


df_consent_all_NON_auck$index <- 
  (df_consent_all_NON_auck$tot / df_consent_all_NON_auck$tot[df_consent_all_NON_auck$year == 2011]) * 100

df_consent_all_NON_auck


df_graph3 <- rbind(df_lic_NON_auck, df_consent_all_NON_auck)
df_graph3


#636363
# mbie colors (dark grey, light grey)
vct_colours <- c( "#bdbdbd", "#636363")
g3 <- ggplot(df_graph3, aes(x = year, y = index, fill = type))
g3 <- g3 + geom_bar(width = 0.7, stat = "identity", position = "dodge")
g3 <- g3 + theme_light(13, base_family = "Calibri")
g3 <- g3 + theme(panel.grid.minor.y = element_blank())
g3 <- g3 + theme(panel.grid.minor.x = element_blank())
g3 <- g3 + theme(panel.grid.major.x = element_blank())

g3 <- g3 + theme(panel.border = element_blank())
g3 <- g3 + theme(panel.background = element_blank())
g3 <- g3 + theme(axis.line = element_line(colour = "black"))


g3 <- g3 + labs(x = "Year")
g3 <- g3 + labs(y = "Index (base = 2011)")
g3 <- g3 + scale_y_continuous(label = scales::comma, breaks = c(100,200, 287), limits = c(0, 315))
g3 <- g3 + scale_x_continuous(breaks = c(2011, 2012, 2013, 2014, 2015))
g3 <- g3 + scale_fill_manual(values = vct_colours)
g3 <- g3 + guides(fill=guide_legend(title="Classification", reverse = TRUE))
g3 <- g3 + theme(legend.text = element_text(lineheight = 0.6), 
                 legend.key.height = grid::unit(0.8, "cm"), 
                 legend.position = c(0.14, 0.76))
g3



cairo_pdf("final_images/plot_ldp_consent_index_filtered_val_non_auck.pdf", width = 11.69, height = 8.27)
print(g3)
dev.off()

# RECONCILIATE -- FIRST TRANCHE OF GRAPHS ========================

df_2_3 <- rbind(df_graph2, df_graph3)
# Auckland and Non-Auckland should reconcile to Total New Zealand
sum(df_2_3$tot) == sum(df_graph1$tot)

# We want the following:
# licenses = 110139
# consent value = 20326.04
sum(df_graph1[df_graph1$type == "licenses", "tot"])
sum(df_graph1[df_graph1$type == "consent value", "tot"])




df_graph1$loc <- "all"
df_graph2$loc <- "auckland"
df_graph3$loc <- "non_auckland"


# find the max index value:
df_t1 <- rbind(df_graph1, df_graph2, df_graph3)
max_index_t1 <- max(df_t1$index)
max_index_t1
# add 10 pc
max_index_t1 * 1.1

df_t1 <- df_t1 %>% as.data.frame()


dft1_agg <- df_t1 %>% select(-tot) %>% spread(year, index) %>% select(c(1,2, 7))
dft1_agg


# licenses divided by consents
flt_all_value <-   dft1_agg$`2015`[dft1_agg$type == "consent value" & dft1_agg$loc == "all"] /
                      dft1_agg$`2015`[dft1_agg$type == "licenses" & dft1_agg$loc == "all"]
flt_all_value

flt_auck_value <-  dft1_agg$`2015`[dft1_agg$type == "consent value" & dft1_agg$loc == "auckland"] /
                        dft1_agg$`2015`[dft1_agg$type == "licenses" & dft1_agg$loc == "auckland"] 
flt_auck_value

flt_non_auck_value <-  dft1_agg$`2015`[dft1_agg$type == "consent value" & dft1_agg$loc == "non_auckland"] / 
                          dft1_agg$`2015`[dft1_agg$type == "licenses" & dft1_agg$loc == "non_auckland"] 
flt_non_auck_value

dft1_agg






# ================================================================
# ===============================================================
# ===============================================================
# ================================================================
# ===============================================================
# ===============================================================
# ================================================================
# ===============================================================
# ===============================================================
 # GRAPHS TRANCHE 2
# 4 "plot_ldp_consent_index_unfiltered_count_all"
# 5 "plot_ldp_consent_index_unfiltered_count_auck"
# 5 "plot_ldp_consent_index_unfiltered_count_non_auck"

# The following three data frames can be reused:
#  df_lic_all
#  df_lic_auck
#  df_lic_NON_auck

# =======================
# GRAPH 4

df_consent_count_all <- df_composite %>% filter(type == "consent count") %>% 
                          group_by(year) %>% 
                          summarise(tot = sum(number)) %>% 
                          mutate(type = "consent count")

df_consent_count_all$index <- 
  (df_consent_count_all$tot / df_consent_count_all$tot[df_consent_count_all$year == 2011]) * 100

df_consent_count_all

df_graph4 <- rbind(df_lic_all, df_consent_count_all)
df_graph4

# mbie colors (light green, dark green)
vct_colours <- c("#1F7779", "#5EB31c")
g4 <- ggplot(df_graph4, aes(x = year, y = index, fill = type))
g4 <- g4 + geom_bar(width = 0.7, stat = "identity", position = "dodge")
g4 <- g4 + theme_light(11, base_family = "Calibri")
g4 <- g4 + theme(panel.grid.minor.y = element_blank())
g4 <- g4 + theme(panel.grid.minor.x = element_blank())
g4 <- g4 + theme(panel.grid.major.x = element_blank())

g4 <- g4 + theme(panel.border = element_blank())
g4 <- g4 + theme(panel.background = element_blank())
g4 <- g4 + theme(axis.line = element_line(colour = "black"))



g4 <- g4 + labs(x = "Year")
g4 <- g4 + labs(y = "Index (base = 2011)")

g4 <- g4 + scale_y_continuous(label = scales::comma, breaks = c(100,200,287), limits = c(0, 315))

g4 <- g4 + scale_x_continuous(breaks = c(2011, 2012, 2013, 2014, 2015))
g4 <- g4 + scale_fill_manual(values = vct_colours)
g4 <- g4 + guides(fill=guide_legend(title="Classification", reverse = TRUE))
g4 <- g4 + theme(legend.text = element_text(lineheight = 0.6), 
                 legend.key.height = grid::unit(0.8, "cm"), 
                 legend.position = c(0.14, 0.76))
g4


cairo_pdf("final_images/plot_ldp_consent_index_unfiltered_count_all.pdf", width = 11.69, height = 8.27)
print(g4)
dev.off()


# ========================================================
# ========================================================
# graph 5

df_consent_count_auck <-  df_composite %>% 
                        filter(type == "consent count") %>% 
                        filter(in_al == TRUE) %>%
                        group_by(year) %>% 
                        summarise(tot = sum(number)) %>% 
                        mutate(type = "consent count")


df_consent_count_auck$index <- 
  (df_consent_count_auck$tot / df_consent_count_auck$tot[df_consent_count_auck$year == 2011]) * 100

df_consent_count_auck

df_graph5 <- rbind(df_lic_auck, df_consent_count_auck)
df_graph5

# mbie colors (dark blue, light blue)
vct_colours <- c( "#296D7A", "#2DB6E2")
g5 <- ggplot(df_graph5, aes(x = year, y = index, fill = type))
g5 <- g5 + geom_bar(width = 0.7, stat = "identity", position = "dodge")
g5 <- g5 + theme_light(11, base_family = "Calibri")
g5 <- g5 + theme(panel.grid.minor.y = element_blank())
g5 <- g5 + theme(panel.grid.minor.x = element_blank())
g5 <- g5 + theme(panel.grid.major.x = element_blank())
g5 <- g5 + labs(x = "Year")
g5 <- g5 + labs(y = "Index (base = 2011)")

g5 <- g5 + theme(panel.border = element_blank())
g5 <- g5 + theme(panel.background = element_blank())
g5 <- g5 + theme(axis.line = element_line(colour = "black"))


g5 <- g5 + scale_y_continuous(label = scales::comma, breaks = c(100,200,287), limits = c(0, 315))
g5 <- g5 + scale_x_continuous(breaks = c(2011, 2012, 2013, 2014, 2015))
g5 <- g5 + scale_fill_manual(values = vct_colours)
g5 <- g5 + guides(fill=guide_legend(title="Classification", reverse = TRUE))
g5 <- g5 + theme(legend.text = element_text(lineheight = 0.6), 
                 legend.key.height = grid::unit(0.8, "cm"), 
                 legend.position = c(0.14, 0.76))
g5

cairo_pdf("final_images/plot_ldp_consent_index_unfiltered_count_auck.pdf", width = 11.69, height = 8.27)
print(g5)
dev.off()

# ========================================================
# ========================================================
# graph 6


df_consent_count_NON_auck <-  df_composite %>% 
                            filter(type == "consent count") %>% 
                            filter(in_al == FALSE) %>%
                            group_by(year) %>% 
                            summarise(tot = sum(number)) %>% 
                            mutate(type = "consent count")


df_consent_count_NON_auck$index <- 
  (df_consent_count_NON_auck$tot / df_consent_count_NON_auck$tot[df_consent_count_NON_auck$year == 2011]) * 100

df_consent_count_NON_auck


df_graph6 <- rbind(df_lic_NON_auck, df_consent_count_NON_auck)
df_graph6

# mbie colors (dark grey, light grey)
vct_colours <- c( "#bdbdbd", "#636363")
g6 <- ggplot(df_graph6, aes(x = year, y = index, fill = type))
g6 <- g6 + geom_bar(width = 0.7, stat = "identity", position = "dodge")
g6 <- g6 + theme_light(11, base_family = "Calibri")
g6 <- g6 + theme(panel.grid.minor.y = element_blank())
g6 <- g6 + theme(panel.grid.minor.x = element_blank())
g6 <- g6 + theme(panel.grid.major.x = element_blank())
g6 <- g6 + labs(x = "Year")
g6 <- g6 + labs(y = "Index (base = 2011)")

g6 <- g6 + theme(panel.border = element_blank())
g6 <- g6 + theme(panel.background = element_blank())
g6 <- g6 + theme(axis.line = element_line(colour = "black"))

g6 <- g6 + scale_y_continuous(label = scales::comma, breaks = c(100,200,287), limits = c(0, 315))
g6 <- g6 + scale_x_continuous(breaks = c(2011, 2012, 2013, 2014, 2015))
g6 <- g6 + scale_fill_manual(values = vct_colours)
g6 <- g6 + guides(fill=guide_legend(title="Classification", reverse = TRUE))
g6 <- g6 + theme(legend.text = element_text(lineheight = 0.6), 
                 legend.key.height = grid::unit(0.8, "cm"), 
                 legend.position = c(0.14, 0.76))
g6

# ==============================
# ===============================
cairo_pdf("final_images/plot_ldp_consent_index_unfiltered_count_non_auck.pdf", width = 11.69, height = 8.27)
print(g6)
dev.off()


# RECONCILIATE -- SECOND TRANCHE OF GRAPHS ========================

df_5_6 <- rbind(df_graph5, df_graph6)
# Auckland and Non-Auckland should reconcile to Total New Zealand
sum(df_5_6$tot) == sum(df_graph4$tot)

# We want the following:
# licenses = 110139
# consent count = 235125
sum(df_graph4[df_graph4$type == "licenses", "tot"])
sum(df_graph4[df_graph4$type == "consent count", "tot"])


df_graph4$loc <- "all"
df_graph5$loc <- "auckland"
df_graph6$loc <- "non_auckland"


# find the max index value:
df_t2 <- rbind(df_graph4, df_graph5, df_graph6)
max_index_t2 <- max(df_t2$index)
max_index_t2
# add 10 pc
max_index_t2 * 1.1



df_t2 <- df_t2 %>% as.data.frame()

# ===== AND THAT WAS Mark hatchers last line of code at MBIE !!! ======

dft2_agg <- df_t2 %>% select(-tot) %>% spread(year, index) %>% select(c(1,2, 7))

# licenses divided by consents
flt_all <-   dft2_agg$`2015`[dft2_agg$type == "consent count" & dft2_agg$loc == "all"] /
                dft2_agg$`2015`[dft2_agg$type == "licenses" & dft2_agg$loc == "all"]
flt_all

flt_auck <-  dft2_agg$`2015`[dft2_agg$type == "consent count" & dft2_agg$loc == "auckland"] /
                  dft2_agg$`2015`[dft2_agg$type == "licenses" & dft2_agg$loc == "auckland"] 
flt_auck

flt_non_auck <-  dft2_agg$`2015`[dft2_agg$type == "consent count" & dft2_agg$loc == "non_auckland"] / 
                  dft2_agg$`2015`[dft2_agg$type == "licenses" & dft2_agg$loc == "non_auckland"] 
flt_non_auck

dft2_agg









