get_epsg_from_utm <- function(site){
  dictionary <- cbind(32616, 32615, 32617, 32617, 32616, 32616, 32612, 32613, 32617) 
  colnames(dictionary) <- c("STEI", "CHEQ", "SCBI", "GRSM", "ORNL", "TALL", "MOAB", "JORN", "OSBS")
  return(dictionary[colnames(dictionary)==site])
}

get_lat_long <- function(field_data, epsg = 32617){
  library(daymetr)
  library(rgdal)
  new_dat <- NULL
  for(NeonSites in unique(field_data$siteID)){
    dat <- field_data[field_data$siteID %in% NeonSites,] 
    epsg <- get_epsg_from_utm(unique(dat$siteID))
    dat <- dat[complete.cases(dat$UTM_E), ]
    coordinates(dat) <- c("UTM_E", "UTM_N")
    proj4string(dat) <- CRS(paste("+init=epsg:", epsg, sep =""))
    CRS.new <- CRS("+init=epsg:4326")
    dat <- spTransform(dat, CRS.new)
    coords_dat <- dat@coords
    new_dat <- rbind(new_dat, cbind(dat@data ,dat@coords))
  }
  return(new_dat)
}

download_point_daymet <- function(listSites = "OSBS", dat_path = "./predictions/average_pred_per_km2.csv", 
                                  outpath = "./Environmental_features"){
  require(tidyverse)
  field_data <- readr::read_csv(dat_path) %>%
     dplyr::select(individualID, taxonID, siteID, domainID, stemDiameter, height, maxCrownDiameter, UTM_E,UTM_N)
  colnames(field_data) <- c("individualID", "taxonID", "siteID", "domainID","stemDiameter", 
                            "height", "maxCrownDiameter", "UTM_E","UTM_N")
  
  field_data <- field_data[field_data$siteID %in% listSites, ]
  new_dat <- get_lat_long(field_data)
  
  daymet_coords <- cbind(new_dat$individualID,new_dat$UTM_N, new_dat$UTM_E)
  readr::write_csv(data.frame(daymet_coords), './tmp/my_sites.csv', col_names=F)
  library(daymetr)
  
  df <- download_daymet_batch(file_location = './tmp/my_sites.csv',
                              start = 1980,
                              end = 2017,
                              internal = TRUE)
  
  save(df, file= paste(outpath, "Daymet_traits_preds.RData", sep="/"))
  
}
