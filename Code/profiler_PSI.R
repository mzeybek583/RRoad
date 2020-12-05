  
  ## PSI based profiler
  # Profiler v2: Edit 06.12.2020
  
  # Load Library ------------------------------------------------------------
  
  library(lidR)
  library(raster)
  library(purrr)
  library(tibble)
  library(geosphere)
  library(rgdal)
  library(spatstat)
  library(ggplot2)
  
  time <- proc.time()
  # Load data ---------------------------------------------------------------
  las <- readLAS(files = "data/profile/Extracted_road_TestArea1.las") # Road Surface Test1
  c.line <- shapefile("data/profile/Centerline_TestArea1.shp")  # Extracted centerline
  
  # Road surface TIN model --------------------------------------------------
  las@data$Classification <- 2
  dtm = grid_terrain(las, res = 0.1, algorithm = tin())
  plot(dtm, col = terrain.colors(10))
  # Extract Z values of centerline from DTM Raster --------------------------
  elevations <- extract(dtm, c.line, along=TRUE, cellnumbers=TRUE)

  #> Warning in proj4string(x): CRS object has comment, which is lost in output
  
  elev_df = purrr::map_dfr(elevations, as_data_frame, .id = "ID")
  
  transect_coords = xyFromCell(dtm, elev_df$cell)
  
  # Pairdist from Zero ------------------------------------------------------
  
  pair_dist <- pairdist(transect_coords)
  
  #elev_df$dist = c(0, cumsum(pair_dist)) 
  

# Plots -------------------------------------------------------------------
# DTM and Centerline Plot
  gg.cline <- data.frame(c.line@lines[[1]]@Lines[[1]]@coords)
  colnames(gg.cline) <- c("x", "y")
  gg.dtm <- as.data.frame(dtm, xy=TRUE)
  ggplot(gg.dtm, aes(x, y)) +
    geom_raster(aes(fill=Z))+
    scale_fill_gradientn(name="Altitude (m)",colours = terrain.colors(100))+
    theme_bw(base_size = 20)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank())+ xlab("X") + ylab("Y")+
    geom_path(data = gg.cline, 
              aes(x = x, y = y, color = "red"),
              size = 1,linetype = "twodash", show.legend = TRUE)+
    scale_color_identity(name = "Legend", labels = c("Centerline"), guide = "legend")
  ggsave("dtm_plot.png", width = 8, height=4, units = "in" )
  
# PROFILE Plot
  gg.prof <- data.frame(pair_dist[,1], elev_df$Z)
  colnames(gg.prof) <- c("Distance", "Elevation")
  
  ggplot(gg.prof, aes(x=Distance, y=Elevation))+
    geom_line()+
    #geom_point()+
    theme_bw(base_size = 20)+ xlab("Distance (m)") +  ylab("Elevation (m)")
    
  ggsave("profile_plot.png", width = 8, height=4, units = "in" )
  

  sprintf("Processing time %3.1f sn",(proc.time()-time)[3])
# END