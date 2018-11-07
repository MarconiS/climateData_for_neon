convert_stei <- function(dat){
  library(rgdal)
  what_to_keep <- which(dat$UTM_E > 270000)
  tmp_keep <- dat[what_to_keep,]
  dat$utmZone <- "15N"
  dat$siteID <- "CHEQ"
  coordinates(dat) <- c("UTM_E", "UTM_N")
  proj4string(dat) <- CRS("+init=epsg:32616") # WGS 84
  dat <- spTransform(dat, CRS("+init=epsg:32615"))
  new_dat <- cbind(dat@data, dat@coords)
  new_dat[what_to_keep,] <- tmp_keep
  return(new_dat)
}

get_epsg_from_utm <- function(utm){
  dictionary <- cbind(32616, 32615, 32617, 32617, 32616, 32616, 32612, 32613, 32617, 32617, 32614) 
  colnames(dictionary) <- c("STEI", "CHEQ", "SCBI", "GRSM", "ORNL", "TALL", "MOAB", "JORN", "OSBS", "MLBS", "KONZ")
  return(dictionary[colnames(dictionary)==utm])
}

inputs <- "./TOS_data/utm_dataset.csv"

library(readr)
library(dplyr)

dataset <- read_csv(inputs) %>%
  dplyr::select(individualID, taxonID, siteID, domainID,eventID, utmZone, stemDiameter, height, maxCrownDiameter, UTM_E, UTM_N) %>%
  unique

dataset <- dataset[!is.na(dataset$UTM_E),]
dataset[which(dataset$siteID=="STEI"),] <- convert_stei(dataset[which(dataset$siteID=="STEI"),])

latlong_data <- get_lat_long(dataset)
write_csv(latlong_data, "./TOS_data/latlong.csv")
