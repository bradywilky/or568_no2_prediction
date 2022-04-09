library(magrittr) # used only to import the operator %>%
library(stringr) # for various character vector operations


train_labels <- read.csv("~/repositories/or568_no2_prediction/data/train_labels.csv")
grid_meta <- read.csv("~/repositories/or568_no2_prediction/data/grid_metadata.csv")

grid_meta$temp <- grid_meta$wkt %>% str_remove(
  "POLYGON \\(\\(") %>% str_remove(
    "\\)\\)") %>% str_remove_all(
      ",") %>% strsplit(split=" ")

i_lon <- c(1,3,5,7)
i_lat <- c(2,4,6,8)

max_lat <- vector(mode="numeric")
max_lon <- vector(mode="numeric")
min_lat <- vector(mode="numeric")
min_lon <- vector(mode="numeric")

# not the best way to do this but didn't look at other solutions yet
for(i in grid_meta$temp){
  max_lat <- c(max_lat, max(as.numeric(i[i_lat])))
  min_lat <- c(min_lat, min(as.numeric(i[i_lat])))
  max_lon <- c(max_lon, max(as.numeric(i[i_lon])))
  min_lon <- c(min_lon, min(as.numeric(i[i_lon])))
}

# new cols from values created above
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


## Predictors

# creating sample predictor
train_data$month <- substr(train_data$datetime, 6, 7)

## creating predictors with LA weather data
la_weather <- read.csv("~/repositories/or568_no2_prediction/data/LAweather_data.csv")
unique(la_weather$location)
unique(train_data$location)
train_data$location[train_data$location == 'Los Angeles (SoCAB)'] <- "Los Angeles"
la_weather$location <- "Los Angeles"


## creating predictors from NO2 vertical column density daily readings
path <- ("~/repositories/or568_no2_prediction/data/sat_vcd_data.csv")
no2_vcd <- read.csv(path)

# transforming columns to join NO2 vertical column density on
train_data$date <- substr(train_data$datetime,1,10) %>% str_replace_all("-", " ")

t <- merge(train_data, no2_vcd, by = c("location", "date"))

# saving data
path <- ("C:/Users/15714/Documents/repositories/or568_no2_prediction/data/train_data.csv")
write.csv(train_data, path)#, row.names=FALSE)


## EDA
library(ggplot2)
library(dplyr)


# Explore the data
unique(train_data$location)
summary(train_data$value)

train_data %>% group_by(location) %>% summarise(count_entries = n())

# Plots
ggplot(train_data, aes(value))+ geom_density(fill="blue") + ggtitle("NO2 Value Distribution")
ggplot(train_data, aes(log(value))) + geom_density(fill="blue") + ggtitle("NO2 Log Value Distribution")
ggplot(train_data, aes(sqrt(value))) + geom_density(fill="blue") + ggtitle("NO2 Square Root Value Distribution")

gx = train_data %>% 
  group_by(location) %>% 
  summarise(value = mean(value))
gx

gx %>% ggplot(aes(x = location, y = value, color = location)) +
  geom_bar(stat = "identity", alpha = 0.3) + 
  theme_classic() + 
  labs(y = "NO2 Value",
       x = "Location")

gx = train_data %>% 
  group_by(month) %>% 
  summarise(value = mean(value))
gx

gx %>% ggplot(aes(x = month, y = value, color = month)) +
  geom_bar(stat = "identity", alpha = 0.3) + 
  theme_classic() + 
  labs(y = "NO2 Value",
       x = "Month")


### Predictor Pre-processing

# standardizing numeric predictors
num.prdt <- c('')
train_data$mid_lat_sc <- scale(train_data$mid_lat)
train_data$mid_lon_sc <- scale(train_data$mid_lon)

# dropping all unnecessary columns for a linear regression model
train_data_nolab <- train_data %>% subset(select = -c(
  grid_id, datetime, tz, min_lat, max_lat, min_lon, max_lon, mid_lon, min_lat))

# splitting data into train and test
row.number <- sample(1:nrow(train_data_nolab), 0.8*nrow(train_data_nolab))
train = train_data_nolab[row.number,]
test = train_data_nolab[-row.number,]

# creating model with all predictors. No additionally-sourced predictors have been added
# at this time
initial_mod <- lm(value~., train_data_nolab)
summary(initial_mod)
plot(initial_mod)
