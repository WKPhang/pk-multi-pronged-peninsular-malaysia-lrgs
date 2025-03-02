library(sf)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)

mozzi_data <- st_read("D:/Malaria/Writing/Paper 19 (Conclusion 1 knowlesi malaria)/GIS/mozzi_UTM_47N.shp")
macaca_data <- st_read("D:/Malaria/Writing/Paper 19 (Conclusion 1 knowlesi malaria)/GIS/macaca_UTM_47N.shp")
human_data <- st_read("D:/Malaria/Writing/Paper 19 (Conclusion 1 knowlesi malaria)/GIS/human_UTM_47N.shp")
mys_shp <- st_read("D:/Malaria/Writing/Paper 19 (Conclusion 1 knowlesi malaria)/GIS/Malaysia_shapefile.shp")

#Simplify dataset
colnames(mozzi_data)
colnames(macaca_data)
colnames(human_data)
moz_data <- mozzi_data [,-c(2, 3, 7, 8, 9, 16, 17)]
mac_data <- macaca_data [,-c(2, 6, 7, 8, 9, 13, 14)]
hum_data <- human_data [,c(1,19)]

moz_data$month_year <-as.Date(moz_data$month_year, "%d/%m/%y")
mac_data$month_year <-as.Date(mac_data$month_year, "%d/%m/%y")
hum_data$month_year <-as.Date(hum_data$month_year, "%d/%m/%y")  
mac_data$pk_percent <- (mac_data$pk_n)/(mac_data$macaque_n) 

rm(mozzi_data, macaca_data, human_data)

# Define the bounding box for cropping (xmin, ymin, xmax, ymax)
st_bbox(mys_shp)
bbox <- st_bbox(c(xmin = 570826.78, ymin = 96342.84, xmax = 1120000, 
                  ymax = 856683.59), crs = st_crs(mys_shp))

# Crop the shapefile using the bounding box
cropped_mys <- st_crop(mys_shp, bbox)

# Define the distance threshold (5 km)
radii <-  seq(1000, 20000, by = 1000)

# Function to count the number of hum_data points within a given radius from 
# each moz_data point
count_points_within_distance <- function(radius) {
  within_distance <- st_is_within_distance(moz_data, hum_data, dist = radius)
  sapply(within_distance, length)
}

counts_list <- lapply(radii, function(radius) {
  counts <- count_points_within_distance(radius)
  data.frame(id = 1:nrow(moz_data), radius = radius / 1000, count = counts)
})

# Combine the results into a long-format data frame
counts_long <- bind_rows(counts_list) %>%
  mutate(radius = as.factor(radius))

# Join the counts with vector_a based on point_id
moz_data_long <- left_join(moz_data, counts_long, by = "id")

# Print the first few rows to check the result
print(moz_data_long)

rm(moz_data_long)

# Reading predicted risk map TIFF file
library(raster)
mozzi_raster <- raster("D:/Malaria/Writing/Paper 19 (Conclusion 1 knowlesi malaria)/Raster map/predicted.tif")
xgb_raster <- raster("D:/Malaria/Writing/Paper 19 (Conclusion 1 knowlesi malaria)/Raster map/XGBraster_mean_final.tif")

# Min-Max Normalization for the mozzi_raster
## Get the min and max of the original raster values
min_val <- minValue(mozzi_raster)
max_val <- maxValue(mozzi_raster)

## Rescale the raster to the range (0, 1)
rescaled_mozzi_raster <- (mozzi_raster - min_val) / (max_val - min_val)

# Plot the mozzi_raster
plot(rescaled_mozzi_raster, main = "Mozzi Raster")
plot(xgb_raster)
# Extract the raster data
