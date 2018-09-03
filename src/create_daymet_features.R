create_daymet_features <- function(path_dat = "./TOS_data/predictions_data_with_climate.csv", 
                                   path_clim  = "./Environmental_features/Daymet_traits_preds.RData", 
                                   path_out = "./TOS_data/pred_data_with_climate.csv"){
  library(readr)
  library(daymetr)
  library(tidyverse)
  library(lubridate)
  load(path_clim)
  dataset = data.frame(matrix(NA, ncol = 13, nrow = 0))
  colnames(dataset) <- c("month","ts_daylength", "ts_prec","ts_rad","ts_melt","ts_tmax","ts_tmin","ts_vp",
                         "identifierID", "lat","long","altitude","daymet_tile" )
  for(ii in 1:length(df)){
    id <- df[[ii]]
    if(class(id) == "daymetr"){
      id_clim = id$data
      colnames(id_clim) <- c("year", "month", "daylength", "prec", "srad", "snow_melt", "tmax", "tmin", "vp")
      id_clim$month <- as.Date(id_clim$month-1, origin = "1980-01-01") %>%
        month
      point_features <- id_clim %>%
        group_by(month) %>%
        summarise(ts_daylength = mean(daylength), ts_prec = sum(prec), 
                  ts_rad = mean(srad), ts_melt = mean(snow_melt), 
                  ts_tmax = max(tmax), ts_tmin = min(tmin), ts_vp = mean(vp))
      point_features$identifierID <- id$site
      point_features$lat <- id$latitude
      point_features$long <- id$longitude
      point_features$altitude <- id$altitude
      point_features$daymet_tile <- id$tile
      
      dataset = rbind(dataset, point_features)
    }
  }
  write_csv(dataset, path_out)
}
create_daymet_features(path_dat = "./pullData/TOS_data/utm_dataset.csv", 
                       path_clim  = "./pullData/Environmental_features/Daymet_traits.RData", 
                       path_out = "./pullData/TOS_data/trait_data_with_climate.csv")
