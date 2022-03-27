get.omi.datapath <- function(year, month, day) {
  datapath <- paste("data/train/", year, "/", year, month, day, "T235959_omi_dl-la-tpe_0.he5"
                    , sep = "")
  return(datapath)
}


# looking for grid ID to get associated latitude/longitude
# Exploring data
data <- nc_open(get.omi.datapath('2020', '07', '01'))

library(sf)
library(mapview)
setwd("~/repositories/or568_no2_prediction")

grid.metadata <- read.csv('data/grid_metadata.csv')
la_grid <- grid.metadata[grid.metadata$location == 'Los Angeles (SoCAB)',]

polys <- c()
for(i in 1:length(la_grid$wkt)){
  pnt <- st_as_sfc(la_grid$wkt[i], crs = 4326)
  polys[i] <- pnt
}

mapview()
mapview(polys)
pnt1 <- st_as_sfc(la_grid$wkt[1], crs = 4326)
pnt2 <- st_as_sfc(la_grid$wkt[2], crs = 4326)
pnt3 <- st_as_sfc(la_grid$wkt[3], crs = 4326)
pnt4 <- st_as_sfc(la_grid$wkt[4], crs = 4326)
pnt5 <- st_as_sfc(la_grid$wkt[5], crs = 4326)
pnt6 <- st_as_sfc(la_grid$wkt[6], crs = 4326)
pnt7 <- st_as_sfc(la_grid$wkt[7], crs = 4326)
pnt8 <- st_as_sfc(la_grid$wkt[8], crs = 4326)
pnt9 <- st_as_sfc(la_grid$wkt[9], crs = 4326)


mapview(c(pnt1,pnt2,pnt3,pnt4,pnt5,pnt6,pnt7,pnt8,pnt9))
