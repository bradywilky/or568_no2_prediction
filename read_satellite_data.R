# TODO:
# download Markdown extension, looks better and can run code in chunks with output
# in editor window

# dplyr provides various data manipulation functions
library(dplyr)
# ncdf4 reads various satellite/geographical data, like .he5
library(ncdf4)
# raster has functionality to plot sattelite data
library(raster)
library(rhdf5)
# Changing working directory to make it easier for myself
setwd("~/repositories/public_sonbox/or568_term_project")

get.omi.datapath <- function(year, month, day) {
  datapath <- paste("data/train/", year, "/", year, month, day, "T235959_omi_dl-la-tpe_0.he5"
                    , sep = "")
  return(datapath)
}

# function to read in NO2 data from netCDF file and return matrix of values for
# each 0.25 x 0.25 degree cell
get.no2.colvals <- function(year, month, day) {
  varpath <- "HDFEOS/GRIDS/ColumnAmountNO2/Data Fields/ColumnAmountNO2"
  datapath <- get.omi.datapath(year, month, day)
  dmatrix <- h5read(datapath, varpath)
  no2 <- c()
  for(i in 1:length(dmatrix)){
    no2[i] <- dmatrix[i]
  }
  return(no2)
}


get.raster <- function(y, m, d){
  varpath <- "HDFEOS/GRIDS/ColumnAmountNO2/Data Fields/ColumnAmountNO2"
  datapath <- get.omi.datapath(y, m, d)
  r <- raster(x=datapath, var=varpath)
  return(r)
}


# For a global map, you go from 90 to -90 and from -180 to +180, so the number
# of cells is equal to 180 / 0.25 * 360/0.25 = 180 * 360 * 16 = 1036800 cells.
# Since the coordinates are from (1:1440,1:720), they need to be mapped to
# latitude/longitude
# first orient x and y around the axis (0 is midpoint), then divide by 4 to get
# degrees. This gives the right corner of each cell, but since we want one
# latitude and longitude to represent each cell, we should use the midpoint of
# each cell. So we subtract by 0.125 to shift from the right corner to the
# midpoint.
x_coord <- c()
y_coord <- c()
#no2 <- c()

for(y in 1:720) {
  for(x in 1:1440) {
    i = (y-1)*1440 + x
    x_coord[i] <- x
    y_coord[i] <- y
#    no2[i] <- val
  }
}

df <- data.frame("x" = x_coord,
                 "y" = y_coord
                 #                 "no2" = no2
)

df$latitude <- (df$y - max(df$y)/2) / 4 - 0.125
df$longitude <- (df$x - max(df$x)/2) / 4 - 0.125
  
# Los Angeles: 34.0522° N, 118.2437° W
# New Delhi: 28.6139° N, 77.2090° E
# Taipei: 25.0330° N, 121.5654° E

## finding cells closest to cities' coordinates

# found online
la_coor <- c(34.0522, -118.2437)
nd_coor <- c(28.6139, 77.2090)
tp_coor <- c(25.0330, 121.5654)

# since data and city coordinates do not exactly line up, this finds the closest
# cell to the given coordinates above
get_closest_cell <- function(df, coor) {
  df$temp1 <- abs(df$latitude - coor[1])
  df$temp2 <- abs(df$longitude - coor[2])
  temp <- df %>% filter(temp1 == min(abs(latitude - coor[1])))
  temp <- temp %>% filter(temp2 == min(abs(longitude - coor[2])))
  return(subset(temp, select = -c(temp1,temp2)))
}

# gets the index to use on the get.no2.colvals matrix based on the way x and y
# are created
get_closest_cell_idx <- function(cell_df) {
  return((cell_df$y -1) * 1440 + cell_df$x)
}

# getting each city's closest cell
la_cell <- get_closest_cell(df, la_coor)
nd_cell <- get_closest_cell(df, nd_coor)
tp_cell <- get_closest_cell(df, tp_coor)

# building dataset of NO2 vertical column density per city between 
# 1 Jan 2019 and 31 Oct 2020

# instantiating vectors to later use as columns for df
no2_closest_loc <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(no2_closest_loc) <- c('date', 'location', 'lat', 'long', 'vcd_no2')

# iterating through days of available data
for(yr in c("2019", "2020")){
  for (m in c("01", "02", "03", "04", "05", "06",
             "07", "08", "09", "10", "11", "12")) {
    for (d in c(1:31)) {
      # adding '0' if single digit
      if (d < 10) {d <- paste("0", as.character(d), sep="")}
      else {d <- as.character(d)}
      
      # making sure day/month/year combos that don't exist attempt
      # to be read
      valid_date <- TRUE
      if (m %in% c("04", "06", "09", "11") & d == "31") {valid_date <- FALSE}
      else if (m == "02"){
        if (yr == "2019" & d %in% c("29", "30", "31")) {valid_date <- FALSE}
        else if (yr == "2020" & d %in% c("30", "31")) {valid_date <- FALSE}
      }
      if (valid_date) {
        print(paste(yr, m, d))
        # Hard-coding as for loop below not working
        # for (cell in c(la_cell, nd_cell, tp_cell)) {

        # la
        vcd_no2 <- get.no2.colvals(yr, m, d)[get_closest_cell_idx(la_cell)]
        date <- paste(yr, m, d)
        
        n.row <- c(date, 'Los Angeles', la_cell$latitude, la_cell$longitude, vcd_no2)
        no2_closest_loc[nrow(no2_closest_loc) + 1,] <- n.row
        
        # nd
        vcd_no2 <- get.no2.colvals(yr, m, d)[get_closest_cell_idx(nd_cell)]
        date <- paste(yr, m, d)
        
        n.row <- c(date, 'Delhi', nd_cell$latitude, nd_cell$longitude, vcd_no2)
        no2_closest_loc[nrow(no2_closest_loc) + 1,] <- n.row
        
        # tp
        vcd_no2 <- get.no2.colvals(yr, m, d)[get_closest_cell_idx(tp_cell)]
        date <- paste(yr, m, d)
        
        n.row <- c(date, 'Taipei', tp_cell$latitude, tp_cell$longitude, vcd_no2)
        no2_closest_loc[nrow(no2_closest_loc) + 1,] <- n.row        
      }
      if (paste(yr, m, d) == "2020 11 01"){break}
    }
    if (paste(yr, m, d) == "2020 11 01"){break}
  }
  # Killing entire process by stopping uppermost for-loop at end of data
  if (paste(yr, m, d) == "2020 11 01"){break}
}

# saving data
path <- ("C:/Users/15714/Documents/repositories/or568_no2_prediction/data/sat_vcd_data.csv")
write.csv(no2_closest_loc, path)