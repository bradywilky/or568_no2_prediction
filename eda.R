library(ggplot2) # various plotting

## EDA
path <- path <- ("C:/Users/15714/Documents/repositories/or568_no2_prediction/data/train_data.csv")
train_data <- read.csv(path)

# Explore the data
unique(train_data$location)
summary(train_data$value)
# The data appears to have a significant left skew

train_data %>% group_by(location) %>% summarise(count_entries = n())
# There are more entries from Delhi and Los Angeles than Taipei

# Plots for pollution value distribution
ggplot(train_data, aes(value))+ geom_density(fill="blue") + ggtitle("NO2 Value Distribution")
ggplot(train_data, aes(log(value))) + geom_density(fill="blue") + ggtitle("NO2 Log Value Distribution")
ggplot(train_data, aes(sqrt(value))) + geom_density(fill="blue") + ggtitle("NO2 Square Root Value Distribution")
# These plots confirm the skewed distribution

# pollution value mean by location
gx = train_data %>% 
  group_by(location) %>% 
  summarise(value = mean(value))
gx
# LA appears to have a significantly lower mean value than Delhi and Taipei

# visualizing above means
gx %>% ggplot(aes(x = location, y = value, color = location)) +
  geom_bar(stat = "identity", alpha = 0.3) + 
  theme_classic() + 
  labs(y = "NO2 Value",
       x = "Location")

# Looking at how the value differs by month
gx = train_data %>% 
  group_by(month) %>% 
  summarise(value = mean(value))
gx

# plotting the above monthly breakdown
gx %>% ggplot(aes(x = month, y = value, color = month)) +
  geom_bar(stat = "identity", alpha = 0.3) + 
  theme_classic() + 
  labs(y = "NO2 Value",
       x = "Month")