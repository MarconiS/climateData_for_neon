#install.packages("daymetr")
daymet_per_site <- function(pt_data = "//orange/ewhite/NeonData/Daymet/PointData/field_data_with_climate.csv", 
                            pt_out="//orange/ewhite/NeonData/Daymet/SiteData/"){
  library(daymetr)
  site_tiles <- readr::read_csv(pt_data)
  tile <- tile_outlines@data 
  
  tileID<- data.frame(cbind(unique(floor(site_tiles$lat)), unique(floor(site_tiles$long))))
  colnames(tileID) <- c("long", "lat")
  # get gridded tiles
  for(i in 1:dim(tileID)[1]){
    temp_tile <- tile[which((tileID$lat[i] > tile$XMin & tileID$lat[i] <= tile$XMax) &
                                (tileID$long[i] > tile$YMin & tileID$long[i] <= tile$Ymax)),"TileID"]
    download_daymet_tiles(tiles = temp_tile,
                          start = 1980,
                          end = 2017,
                          param = "ALL",
                          path = pt_out)
  }
}
#daymet_per_site(pt_data = "./TOS_data/field_data_with_climate.csv")
daymet_per_site()

#pt_data = "//orange/ewhite/NeonData/Daymet/PointData/field_data_with_climate.csv"
#pt_out = "//orange/ewhite/NeonData/Daymet/PointData/"
#pt_out = "//orange/ewhite/NeonData/Daymet/SiteData/"
#pt_out = "//orange/ewhite/NeonData/Daymet/ContinentalData/"