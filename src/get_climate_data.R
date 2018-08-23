get_epsg_from_utm <- function(utm){
  dictionary <- cbind(32616, 32615, 32617, 32617, 32616, 32616, 32612, 32613, 32617) 
  colnames(dictionary) <- c("STEI", "CHEQ", "SCBI", "GRSM", "ORNL", "TALL", "MOAB", "JORN", "OSBS")
  return(dictionary[colnames(dictionary)==utm])
}

Get_climate_data <- function(dat, epsg){
  library(daymetr)
  new_dat <- NULL
  for(NeonSites in c("STEI", "SCBI", "GRSM", "ORNL", "TALL", "MOAB", "JORN", "OSBS")){
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
}
write_csv(new_dat, "~/Documents/Chapter2/data/Field_data_latlong.csv")
new_dat <- readr::read_csv("~/Documents/Chapter2/data/Field_data_latlong.csv")
daymet_coords <- cbind(new_dat$individualID,new_dat$UTM_N, new_dat$UTM_E)
readr::write_csv(data.frame(daymet_coords), './my_sites.csv')
library(daymetr)

df <- download_daymet_batch(file_location = './my_sites.csv',
                            start = 1980,
                            end = 2017,
                            internal = TRUE)

save(df, file= "~/Dropbox (UFL)/NEON_tiles/environmentalDataAdded/Daymet.RData")

climate_features <- data.frame(matrix(NA, ncol = 10, nrow = dim(daymet_coords)[1]))
#2020
#for(ii in 2:2020){
for(ii in 2021:length(df)){
  
  dat <- df[[ii]]$data %>%
    group_by(yday) %>%
    summarise_all(funs(mean))
  env_features = c(mean(dat$prcp..mm.day.), mean(dat$srad..W.m.2.), 
                mean(dat$swe..kg.m.2.), mean(dat$vp..Pa.), min(dat$tmin..deg.c.),
                max(dat$tmax..deg.c.), mean((dat$tmax..deg.c.+dat$tmin..deg.c.)/2))
  climate_features[ii,] <- c(daymet_coords[ii,1:3], env_features)
}
colnames(climate_features) <- c("individualID", "lat", "long", colnames(df[[2]]$data)[-c(1:2)])
write_csv(climate_features, "./add_climate.csv")

