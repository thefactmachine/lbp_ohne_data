setwd("/Users/zurich/Documents/TEMP-FILES/lbp_project/final_lbp")

# Creates: "data/aerial_unit_lbp_counts", 
# Creates: "data/point_data_lbp",
source("create_lbp_shapefiles.R")

# Creates: data/polygons_315_auckland_counts_wgs84"
source("create_lbp_auckland_shapefile.R")

# Creates: "final_images/lbp_x11_x15.pdf" (Map growth in Auckland licenses)
source("create_auckland_lbp_map.R")

# The next two are for consent data (filtered values and counts) (Core Logic)
# Creates:  "data/polygons_315_consent"
# Creates: "data/polygons_1911_consents_value"
source("create_consent_auckland_value_shapefile.R")

# Creates: "final_images/consents_x11_x15.pdf"  (map of values)
source("create_auckland_consent_map_values.R")

# The next two are for consent data (Un filtered counts) (Core Logic)
# Creates:  data/polygons_1911_consents
source("create_consent_auckland_count_shapefile.R")

# Creates: final_images/consents_counts_x11_x15.pdf" (Map - change in consent counts)
source("create_auckland_consent_map_counts.R")

# create plots
# Creates: "final_images/plot_licenses.pdf"
source("create_graph_lbp.R")

# Creates:  "final_images/plot_consents.pdf"
source("create_graph_consents.R")
# 3 plots here

# Creates: "final_images/plot_licencees.pdf",
source("create_graph_lbp_licencees.R")

# Creates:  "final_images/scatter_all.pdf"
# Creates: "final_images/scatter_non_auckland.pdf"
# Creates: "final_images/scatter_auckland.pdf"
source("create_graph_lbp_consents_scatter.R")

# Creates: "final_images/plot_index.pdf"
source("create_graph_lbp_indices.R")

# =========
# Creates: "data/polygons_1911_lbp_all_years", 
source("create_ldp_all_years.R")

# Creates: "data/polygons_1911_consents_value_all_years",
source("create_filtered_consent_value_and_counts_all_years.R")

# Creates: "data/polygons_1911_unfiltered_consent_counts_all_years"
source("create_unfiltered_consent_counts_1911_all_years.R") 

# Creates:
# 1 "plot_ldp_consent_index_filtered_val_all"
# 2 "plot_ldp_consent_index_filtered_val_auck"
# 3 "plot_ldp_consent_index_filtered_val_non_auck"

# 4 "plot_ldp_consent_index_unfiltered_count_all"
# 5 "plot_ldp_consent_index_unfiltered_count_auck"
# 5 "plot_ldp_consent_index_unfiltered_count_non_auck"

source("create_graph_consents_lbp_index.R")  # incomplete
# need to do real consent counts for each year



