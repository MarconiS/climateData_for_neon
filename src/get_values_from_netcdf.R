get_values_from_netcdf <- function(path_daymet = "/Volumes/rasters2017/CrossScale_macrosystems/Daymet_OSBS/",
                                   field_dat = "~/Documents/GitHub/shineOnNEON/data/predictions.csv"){
  library(rgeos)
  library(raster)
  library(ncdf4)
  library(tidyverse)
  library(data.table)
  list_files <- list.files(path_daymet)

  for(clim_dat in list_files){
    rm(ext)
    one_year_tile <- stack(paste(path_daymet, clim_dat, sep="/"))
    meta_daym <- unlist(strsplit(clim_dat, split = "_"))
    month_vect <- seq(as.Date("1981/1/1"), as.Date("1981/12/31"), "days")
    month_vect <- lubridate::month(month_vect)
    
    data_to_extract_from <- readr::read_csv(field_dat)
    coordinates(data_to_extract_from) <- c( "lat", "long")
    proj4string(data_to_extract_from) <- CRS("+init=epsg:4326")
    CRS.new <- CRS("+proj=lcc +lon_0=-100 +lat_0=42.5 +x_0=0 +y_0=0 +lat_1=25 +lat_2=60 +ellps=WGS84")
    data_to_extract_from <- spTransform(data_to_extract_from, CRS.new)
    data_to_extract_from <- crop(data_to_extract_from, extent(one_year_tile))
    one_year_tile <- crop(one_year_tile, extent(data_to_extract_from)) 
    ext <- raster::extract(one_year_tile,data_to_extract_from)
    out <- list()
    for(mth in 1:12){
      tmp <- ext[,month_vect == mth]
      if(meta_daym[1] %in% c("dayl", "srad", "swe", "tmax", "tmin", "vp")){
        tmp <- apply(tmp, 1, mean, na.rm=T)
      }else{
        tmp <- apply(tmp, 1, sum, na.rm=T)
      }
      out[[mth]] <- tmp
    }
  }
}