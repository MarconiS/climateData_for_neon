get_values_from_netcdf <- function(clim_dat = NULL, path_daymet = "/Volumes/rasters2017/CrossScale_macrosystems/Daymet_OSBS/",
                                   field_dat = "~/Documents/GitHub/shineOnNEON/data/predictions.csv"){
  
  library(rgeos)
  library(raster)
  library(ncdf4)
  library(tidyverse)
  
  rm(ext)
  one_year_tile <- stack(paste(path_daymet, clim_dat, sep="/"))
  meta_daym <- unlist(strsplit(clim_dat, split = "_"))
  month_vect <- seq(as.Date("1981/1/1"), as.Date("1981/12/31"), "days")
  month_vect <- lubridate::month(month_vect)
  
  data_to_extract_from <- readr::read_csv(field_dat)
  data_to_extract_from$unique_id <- 1:5145883
  coordinates(data_to_extract_from) <- c( "lat", "long")
  proj4string(data_to_extract_from) <- CRS("+init=epsg:4326")
  CRS.new <- CRS("+proj=lcc +lon_0=-100 +lat_0=42.5 +x_0=0 +y_0=0 +lat_1=25 +lat_2=60 +ellps=WGS84")
  data_to_extract_from <- spTransform(data_to_extract_from, CRS.new)
  data_to_extract_from <- crop(data_to_extract_from, extent(one_year_tile))
  if(!is.null(data_to_extract_from)){
    one_year_tile <- crop(one_year_tile, extent(data_to_extract_from)) 
    ext <- raster::extract(one_year_tile,data_to_extract_from)
    out <- list()
    out[[mth]] <- tmp
    for(mth in 1:12){
      tmp <- ext[,month_vect == mth]
      if(meta_daym[1] %in% c("dayl", "srad", "swe", "tmax", "tmin", "vp")){
        tmp <- apply(tmp, 1, mean, na.rm=T)
      }else{
        tmp <- apply(tmp, 1, sum, na.rm=T)
      }
      out[[mth]] <- tmp
    }
    out <- data.frame(out)
    out[["unique_id"]] <- data_to_extract_from@data$unique_id
    
    write_csv(out, paste("./outs/predictions_",meta_daym[1],"_",meta_daym[2], ".csv", sep="" ))
  }
}

path_daymet = "//orange/ewhite/NeonData/Daymet/SiteData/"
field_dat = "//orange/ewhite/s.marconi/final_dataset_crop.csv"
list_files <- list.files(path_daymet)
library(parallel)
no_cores <- 63
cl <- makeCluster(no_cores)

parLapply(cl, list_files, get_values_from_netcdf, path_daymet, field_dat)


stopCluster(cl)


#parLapply(cl, list_files, function(x, a = "text", b = "teeeex") print(paste(x, a = "1", b= "2")))
