# Packages to use
library(dplyr) # data manipulation, also includes the operator %>%
library(magrittr) # explicit import to work around error with dplyr
library(stringr) # for various character vector operations

# required data for this script:
#   train_labels.csv (download)
#   grid_metadata.csv (download)
#   sat_vcd_data.csv (run read_satellite_data.R)
train_labels <- read.csv("~/repositories/or568_no2_prediction/data/train_labels.csv")
grid_meta <- read.csv("~/repositories/or568_no2_prediction/data/grid_metadata.csv")

###############################
# Transforming grid coordinates
###############################
# The goal here is to get a latitude and longitude value for each cell, so that
# location data can be joined to each cell.

# each cell's grid coordinates are stored in the wkt column, which specifies coordinates
# for a polygon to plot on a map. Extracting latitude and longitude from this column
# and storing it in our own columns.
grid_meta$temp <- grid_meta$wkt %>% str_remove(
  "POLYGON \\(\\(") %>% str_remove(
    "\\)\\)") %>% str_remove_all(
      ",") %>% strsplit(split=" ")

# indices where latitude and longitude reside
i_lon <- c(1,3,5,7)
i_lat <- c(2,4,6,8)

# setting up empty vectors for our later columns. Each cell is defined by four points:
# min latitude, max latitude, min longitude, and max longitude.
max_lat <- vector(mode="numeric")
max_lon <- vector(mode="numeric")
min_lat <- vector(mode="numeric")
min_lon <- vector(mode="numeric")

# iterating through each grid's polygon coordinates and assigning each coordinate
# to the appropriate vector
for(i in grid_meta$temp){
  max_lat <- c(max_lat, max(as.numeric(i[i_lat])))
  min_lat <- c(min_lat, min(as.numeric(i[i_lat])))
  max_lon <- c(max_lon, max(as.numeric(i[i_lon])))
  min_lon <- c(min_lon, min(as.numeric(i[i_lon])))
}

# creating new columns from vectors created above
grid_meta$max_lat <- max_lat
grid_meta$max_lon <- max_lon
grid_meta$min_lat <- min_lat
grid_meta$min_lon <- min_lon

# these columns should be the geometric center of the grid, representing its
# unofficial coordinate point. Can be used to join data with latitude and
# longitude.
grid_meta$mid_lat <- rowMeans(grid_meta[,c("max_lat", "min_lat")], na.rm=TRUE)
grid_meta$mid_lon <- rowMeans(grid_meta[,c("max_lon", "min_lon")], na.rm=TRUE)

# creating and saving grid_meta transformed
grid_meta_trns <- grid_meta %>% subset(select = -c(temp, wkt))
path <- ("~/repositories/or568_no2_prediction/data/grid_meta_trns.csv")
grid_meta_trns %>% write.csv(path, row.names=FALSE, quote=FALSE)

# joining transformed grid_meta with the training labels
train_data <- merge(train_labels, grid_meta_trns, by = "grid_id")

#########################
## Engineering Predictors
#########################

# creating month predictor
train_data$month <- substr(train_data$datetime, 6, 7)

## creating predictors with LA weather data
la_weather <- read.csv("~/repositories/or568_no2_prediction/data/LAweather_data.csv")
unique(la_weather$location)
unique(train_data$location)
train_data$location[train_data$location == 'Los Angeles (SoCAB)'] <- "Los Angeles"
la_weather$location <- "Los Angeles"


## creating predictors from NO2 vertical column density daily readings

# reading NO2 vertical column density satellite data
path <- ("~/repositories/or568_no2_prediction/data/sat_vcd_data.csv")
no2_vcd <- read.csv(path)

# transforming date column to join NO2 vertical column density on
train_data$date <- substr(train_data$datetime,1,10) %>% str_replace_all("-", " ")

# joining NO2 VCD dataframe to the labeled data
train_data <- merge(train_data, no2_vcd, by = c("location", "date"))

# saving data
path <- ("C:/Users/15714/Documents/repositories/or568_no2_prediction/data/train_data.csv")
write.csv(train_data, path)


## Predictor Pre-processing

# We still have columns that won't be used by the model. dropping all unnecessary
# columns for a regression model
train_data_nolab <- train_data %>% subset(select = -c(
  grid_id, datetime, tz, min_lat, max_lat, min_lon, max_lon,
  X, satellite_grid_lat, satellite_grid_long))

# standardizing numeric predictors
num.prdt <- c('')
train_data_nolab$mid_lat_sc <- scale(train_data_nolab$mid_lat)
train_data_nolab$mid_lon_sc <- scale(train_data_nolab$mid_lon)
train_data_nolab$vcd_no2 <- scale(train_data_nolab$vcd_no2)

# splitting data into train and test
row.number <- sample(1:nrow(train_data_nolab), 0.8*nrow(train_data_nolab))
train = train_data_nolab[row.number,]
test = train_data_nolab[-row.number,]

# creating model with all predictors
initial_mod <- lm(value~., train)
summary(initial_mod)
#plot(initial_mod)

# creating model with only nonzero NO2 label values
nonzero_train <- train[train$value > 0,]
nonzero_mod <- lm(log(value)~., nonzero_train)
summary(nonzero_mod)
#plot(nonzero_mod)

# looking for outliers to handle
t <- data.frame(
  initial_mod$residuals
  , initial_mod$fitted.values
  , train$value
  , train$date
  , train$location
)
colnames(t) <- c('res', 'fit.val', 'val', 'date', 'loc')
res_100 <- t[abs(t$res) > 100,]
res_150 <- t[abs(t$res) > 150,]

nrow(res_100)
nrow(res_150)

aggregate(data.frame(count=res_100$loc), list(value=res_100$loc), length)
# all of the high residual predictions come from Delhi. Two solutions will be
# attempting:
#  - removing top X outliers
#  - removing Delhi from the scope of the problem to see how the model performs


## Anti-joining on outliers to see how results are with new dataframe
boxplot.stats(train_data_nolab$value)$out
# many points fall outside of the boxplot, likely due to the skew.

# Looking at outliers with top percentiles. The top 99.95% of values will be removed
top_percentile <- 0.9995
cutoff <- quantile(train_data_nolab$value, top_percentile)
train_data_nolab[abs(train_data_nolab$value) > cutoff,] %>% nrow()

# grabbing rows only under the cutoff
train_data_nolab_outliers <- train_data_nolab[abs(train_data_nolab$value) > cutoff,]

# anti-joining to filter out outliers (tdnno = train_data_nolab_no_outliers)
tdnno <- anti_join(
  train_data_nolab, train_data_nolab_outliers, by=c("location", "date"))

# splitting the new data into train and test
row.number <- sample(1:nrow(tdnno), 0.8*nrow(tdnno))
train = tdnno[row.number,]
test = tdnno[-row.number,]

# creating model with all predictors
initial_mod <- lm(value~., train)
summary(initial_mod)

# One assumption of regression is that the samples are independent. As pointed out
# by Dr. Xu, this data would be better fit for a time series. However, Dr. Xu has
# advised to group the data by week or month to mitigate the assumption violation.

# Creating temporary column to group by. Need to aggregate data by week and cell
# location and get mean values.
tdnno$week_month_cell <- paste(
  substr(tdnno$date,1,7)
  , tdnno$mid_lat
  , tdnno$mid_lon
)

grouped_df <- aggregate(tdnno[, c("vcd_no2", "value")], list(tdnno$week_month_cell), mean)
names(grouped_df)[names(grouped_df) == "Group.1"] <- "week_month_cell"

add_cols <- tdnno[, c("location", "month", "mid_lat_sc", "mid_lon_sc", "week_month_cell")] %>% distinct()
new_training_data <- merge(grouped_df, add_cols, by = "week_month_cell") %>% subset(select=-week_month_cell)

# saving data
path <- ("C:/Users/15714/Documents/repositories/or568_no2_prediction/data/grouped_train_data.csv")
write.csv(new_training_data, path)

# splitting the new data into train and test
row.number <- sample(1:nrow(new_training_data), 0.8*nrow(new_training_data))
train = new_training_data[row.number,]
test = new_training_data[-row.number,]

# creating model with all predictors
initial_mod <- lm(value~., train)
summary(initial_mod)

# now that our data is set, we can explore other models