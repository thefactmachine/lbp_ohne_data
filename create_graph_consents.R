library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
library(sp)
library(rgdal)
library(tidyr)
library(Cairo)

rm(list = ls())
options(stringsAsFactors = FALSE) 
options(scipen=999)

setwd("/Users/zurich/Documents/TEMP-FILES/lbp_project/final_lbp")
source("r_functions/fn_load_areal_unit.R")
source("r_functions/fn_create_auckland_df.R")
source("r_functions/fn_carg.R")

df_consents <- read.csv("data/consent_data/consents.csv", header = FALSE)
names(df_consents) <- c("fk", "type", "value", "date", "desc", "guid")
df_consents$guid <- NULL
df_consents$desc <- NULL
df_consents$value <- NULL
df_consents$fk <- NULL
nrow(df_consents) == 945921
# convert to date
df_consents$date <- as.Date(df_consents$date, "%Y-%m-%d")

# lets just focus on everyting from 2000 onwards
df_data <- df_consents %>% 
            filter(year(date) > 2000) %>% 
            group_by(year = year(date), type) %>% 
            summarise(count = n()) %>%
            arrange(year, type) %>% as.data.frame()

df_data

vct_sort <- df_data %>% 
  group_by(type) %>% 
  summarise(count = sum(count)) %>% 
  arrange(desc(count)) %>% .$type

vct_sort

q_fact <- factor(df_data$type, levels = vct_sort, ordered = TRUE)

df_data$fact <- q_fact

head(df_data)

df_data$sort <- as.integer(df_data$fact)

df_new <- df_data[order(df_data$sort),] %>% as.data.frame()
df_new

vct_years <- unique(df_new$year) %>% sort()

# bright
# vct_colours <- c("#e41a1c","#377eb8")
# mbie style
vct_colours <- c("#7EB931", "#15A4CC")


p <- ggplot(df_new, aes(x = year, y = count, fill = fact))
# stack is prefferred
p <- p + geom_bar(width = 0.7, stat = "identity", position = "stack")
p <- p + theme_light(13, base_family = "Calibri")
p <- p + theme(panel.grid.minor.y = element_blank())
p <- p + theme(panel.grid.minor.x = element_blank())
p <- p + theme(panel.grid.major.x = element_blank())
p <- p + theme(panel.border = element_blank())
p <- p + theme(panel.background = element_blank())
p <- p + theme(axis.line = element_line(colour = "black"))
p <- p + labs(x = "Year")
p <- p + labs(y = "Number of consents")
p <- p + scale_y_continuous(label = scales::comma)
p <- p + scale_x_continuous(breaks = vct_years)
p <- p + scale_fill_manual(values = vct_colours)
p <- p + guides(fill=guide_legend(title="Consent type", reverse = TRUE))
p <- p + theme(legend.text = element_text(lineheight = 0.6), 
               legend.key.height = grid::unit(0.8, "cm"), 
               legend.position = c(0.8, 0.88))
p


cairo_pdf("final_images/plot_consents.pdf", width = 11.69, height = 8.27)
print(p)
dev.off()

