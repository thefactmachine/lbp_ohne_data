fn_load_areal_unit <- function() {
  # loads areal unit geometry
  # data provenance: 
  # http://www.stats.govt.nz/browse_for_stats/Maps_and_geography/Geographic-areas/digital-boundary-files.aspx
  # 2013 chosen rather than 2016 to align with 2013 census data
  
  # read the shapefile into a "SpatialPolygonsDataFrame"
  # expect warning message that Z dimension is discarded.
  sp_poly_au <- rgdal::readOGR(dsn = "data/2013_aerial_unit_clipped",
                               layer = "AU2013_GV_Clipped")
  
  # spatial co-ordinate system is "2193 - NZGD_2000_New_Zealand_Transverse_Mercator"
  # for confirmation have a look at:"http://prj2epsg.org/search"
  # UNCOMMENT the following 3 lines to look !!
  # df_EPSG <- rgdal::make_EPSG()
  # df_EPSG[df_EPSG$code == 2193 & !is.na(df_EPSG$code),]
  # sp::proj4string(sp_poly_au)
  
  # now transform the sucker to WGS84 ..b/c the address data is WGS 84
  sp_poly_au_wgs84 <- sp::spTransform(sp_poly_au, sp::CRS("+proj=longlat +ellps=WGS84"))
  return(sp_poly_au_wgs84)
}