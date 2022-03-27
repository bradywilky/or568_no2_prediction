library(magrittr) # used only to import the operator %>%
library(stringr) # for various character vector operations


train_labels <- read.csv("~/repositories/public_sonbox/or568_term_project/data/train_labels.csv")
grid_meta <- read.csv("~/repositories/public_sonbox/or568_term_project/data/grid_metadata.csv")

x <- grid_meta$wkt[1]

grid_meta$temp <- grid_meta$wkt %>% str_remove(
  'POLYGON \\(\\(') %>% str_remove(
    '\\)\\)') %>% str_remove_all(
      ',') %>% strsplit(split=' ')

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
grid_meta$mid_lat <- rowMeans(grid_meta[,c('max_lat', 'min_lat')], na.rm=TRUE)
grid_meta$mid_lon <- rowMeans(grid_meta[,c('max_lon', 'min_lon')], na.rm=TRUE)

# creating and saving grid_meta transformed
grid_meta_trns <- grid_meta %>% subset(select = -c(temp, wkt))
path <- ("~/repositories/public_sonbox/or568_term_project/data/grid_meta_trns.csv")
grid_meta_trns %>% write.csv(path, row.names=FALSE, quote=FALSE)

# joining transformed grid_meta with the training labels
train_data <- merge(train_labels, grid_meta_trns, by = 'grid_id')

# creating sample predictor
train_data$month <- substr(train_data$datetime, 6, 7)

# dropping all unnecessary columns for a simple linear model
train_data_nolab <- train_data %>% subset(select = -c(
  grid_id, datetime, tz, min_lat, max_lat, min_lon, max_lon))

# creating model with all predictors. No meaningful predictors have been added
# at this time
initial_mod <- lm(value~., train_data_nolab)
summary(initial_mod)


# saving data
path <- ("~/repositories/public_sonbox/or568_term_project/data/train_data.csv")
train_data %>% write.csv(path, row.names=FALSE, quote=FALSE)