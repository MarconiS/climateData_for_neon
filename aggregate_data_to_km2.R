# dat <- readr::read_csv("~/Documents/GitHub/shineOnNEON/data/predictions.csv")
aggregate_data_to_km2 <- function(dat){
  library(tidyverse)
  tileID <- (cbind(as.character(as.integer(dat$UTM_X/1000)*1000), 
                         as.character(as.integer(dat$UTM_Y/1000)*1000)))
  tileID <- apply(tileID, 1, paste, sep ="", collapse = "_")
  dat$tile <- tileID
  tmp <- dat %>%
    group_by(tile) %>%
    summarize_all(mean)
  readr::write_csv(tmp, "./predictions/average_pred_per_km2.csv")
}