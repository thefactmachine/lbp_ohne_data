fn_create_auckland_df <- function() {
  # function reads selection of aerial unit polygons (315 of them)
  # this selection of polygons hand selected in Arc GIS
  # polygons are an arbitrary definition of Auckland
  spoly_al_315 <- rgdal::readOGR(dsn = "data/polygons_315_wgs84",
                               layer = "al_poly_315_wgs84")
  # extract the data.frame
  lcl_df_al_315 <- spoly_al_315@data
  # drop the pc column
  lcl_df_al_315$pc <- NULL
  lcl_df_al_315$count <- NULL
  return(lcl_df_al_315)
}