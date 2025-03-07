library(sf)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)


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
xgb_pk_map <- raster("D:/Malaria/Writing/Paper 19 (Conclusion 1 knowlesi malaria)/Raster map/XGBraster_mean_final.tif")
lr_vector_map <- raster("D:/Malaria/Writing/Paper 19 (Conclusion 1 knowlesi malaria)/Raster map/predicted.tif")


writeRaster(rescaled_lr_vector, "D:/Malaria/Writing/Paper 19 (Conclusion 1 knowlesi malaria)/Raster map/rescaled_vector_prediction.tif", format = "GTiff", overwrite = TRUE)

# Crop mozzi raster
crop_extent <- extent(570826.78, 1120000, 96342.84, 856683.59)
cropped_lr_vector <- crop(lr_vector_map, crop_extent)
cropped_xgb_vector <- crop(xgb_pk_map, crop_extent)

# Min-Max Normalization for the mozzi_raster
## Get the min and max of the original raster values
min_val <- minValue(cropped_lr_vector)
max_val <- maxValue(cropped_lr_vector)

## Rescale the raster to the range (0, 1)
rescaled_lr_vector <- (cropped_lr_vector - min_val) / (max_val - min_val)





# Import raw data file
m_df <- read.csv("D:/Malaria/Writing/Paper 19 (Conclusion 1 knowlesi malaria)/macaque_dataset.csv",
                 stringsAsFactors = T)

v_df <- read.csv("D:/Malaria/Writing/Paper 19 (Conclusion 1 knowlesi malaria)/vector_dataset.csv",
                 stringsAsFactors = T)
v_df$malaria_rate <- v_df$malaria_n/v_df$an_total
v_df$lg_bin <- ifelse(v_df$lg_total == 0, 0, 1)


h_df <- read.csv("D:/Malaria/Writing/Paper 19 (Conclusion 1 knowlesi malaria)/human_dataset.csv",
                 stringsAsFactors = T)
moh_df <- read.csv("D:/Malaria/Writing/Paper 19 (Conclusion 1 knowlesi malaria)/secondary_human_dataset.csv",
                   stringsAsFactors = T)

# Import shapefiles
m_shp <- st_read("D:/Malaria/Writing/Paper 19 (Conclusion 1 knowlesi malaria)/GIS/m_UTM_47N.shp")
v_shp <- st_read("D:/Malaria/Writing/Paper 19 (Conclusion 1 knowlesi malaria)/GIS/v_UTM_47N.shp")
h_shp <- st_read("D:/Malaria/Writing/Paper 19 (Conclusion 1 knowlesi malaria)/GIS/h_UTM_47N.shp")
moh_shp <- st_read("D:/Malaria/Writing/Paper 19 (Conclusion 1 knowlesi malaria)/GIS/moh_UTM_47N.shp")
mys_shp <- st_read("D:/Malaria/Writing/Paper 19 (Conclusion 1 knowlesi malaria)/GIS/Malaysia_shapefile.shp")




ggplot() +
  geom_raster(data = r_df, aes(x = x, y = y, fill = value)) +  # Raster layer
  geom_sf(data = m_shp, color = "red", size = 2) +  # Points overlay
  scale_fill_viridis_c() +  # Better color scale for raster
  theme_minimal() +  # Clean theme
  labs(title = "Point Shapefile on Raster", fill = "Raster Value") +
  annotation_scale(location = "br", width_hint = 0.2) +  # Scale bar
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm"))

#Extract value
m_df$ext_human_pk <- extract(xgb_pk_map, m_shp)
m_df$ext_an_dist <- extract(rescaled_lr_vector, m_shp)
v_df$ext_human_pk <- extract(xgb_pk_map, v_shp)
v_df$ext_an_dist <- extract(rescaled_lr_vector, v_shp)
h_df$ext_human_pk <- extract(xgb_pk_map, h_shp)
h_df$ext_an_dist <- extract(rescaled_lr_vector, h_shp)

cor.test(m_df$pk_n, m_df$ext_human_pk,  method = "spearman")
cor.test(m_df$pk_n, m_df$ext_an_dist, method = "spearman")
cor.test(m_df$pk_rate, m_df$ext_human_pk,  method = "spearman")
cor.test(m_df$pk_rate, m_df$ext_an_dist, method = "spearman")

cor.test(m_df$malaria_n, m_df$ext_human_pk,  method = "spearman")
cor.test(m_df$malaria_n, m_df$ext_an_dist, method = "spearman")
cor.test(m_df$malaria_rate, m_df$ext_human_pk,  method = "spearman")
cor.test(m_df$malaria_rate, m_df$ext_an_dist, method = "spearman")

cor.test(v_df$an_pernite, v_df$ext_human_pk, method = "spearman")
cor.test(v_df$an_pernite, v_df$ext_an_dist, method = "spearman")
cor.test(v_df$lg_pernite, v_df$ext_human_pk, method = "spearman")
cor.test(v_df$lg_pernite, v_df$ext_an_dist, method = "spearman")
cor.test(v_df$malaria_n, v_df$ext_human_pk, method = "spearman")
cor.test(v_df$malaria_n, v_df$ext_an_dist, method = "spearman")
cor.test(v_df$malaria_rate, v_df$ext_human_pk, method = "spearman")
cor.test(v_df$malaria_rate, v_df$ext_an_dist, method = "spearman")

cor.test(h_df$pk_n, h_df$ext_human_pk, method = "spearman")
cor.test(h_df$pk_n, h_df$ext_an_dist, method = "spearman")
cor.test(h_df$pk_rate, h_df$ext_human_pk, method = "spearman")
cor.test(h_df$pk_rate, h_df$ext_an_dist, method = "spearman")

# Correlation plots

selected_df <- h_df
selected_x_var <- h_df$pk_rate
selected_x_label <- "Num. of malaria positive"

ggplot(selected_df, aes(x = selected_x_var, y = ext_human_pk)) +
  geom_point(color = "black", alpha = 0.7, size = 5) +   # Scatterplot points
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Linear fit
  labs( x = NULL, y = NULL) +
  ylim(0,1) +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15))

ggplot(selected_df, aes(x = selected_x_var, y = ext_an_dist)) +
  geom_point(color = "black", alpha = 0.7, size = 5) +   # Scatterplot points
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Linear fit
  labs( x = NULL, y = NULL) +
  ylim(0, 1) +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15))

  labs(x = selected_x_label,
       y = "Predicted human knowlesi malaria risk")
  labs(x = selected_x_label,
      y = "Predicted Anopheles Leucosphyrus Group mosquito probability of occurrence")
  
  
# Comparison of the 2020 - 2023 Pk case data
moh_df$ext_human_pk <- extract(xgb_pk_map, moh_shp)
moh_df$ext_an_dist <- extract(rescaled_lr_vector, moh_shp)

# Violin plot

filtered_moh_df <- moh_df %>%  filter(onset_year > 2017)

ggplot(filtered_moh_df, aes(x= factor(onset_year), y=ext_human_pk)) + 
  geom_violin() +
  geom_jitter(shape=16, position=position_jitter(0.2),
              alpha = 0.5, size = 1) +
  stat_summary(fun = mean, geom = "crossbar", width = 0.5, color = "red") + # Mean line
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15)) +
  labs(x = "Year",
       y = NULL)

ggplot(filtered_moh_df, aes(x= factor(onset_year), y=ext_an_dist)) + 
  geom_violin() +
  geom_jitter(shape=16, position=position_jitter(0.2),
              alpha = 0.5, size = 1) +
  stat_summary(fun = mean, geom = "crossbar", width = 0.5, color = "red") + # Mean line
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15)) +
  labs(x = "Year",
       y = NULL)


ggplot(v_df, aes(x= factor(lg_bin), y=ext_human_pk)) + 
  geom_violin() +
  geom_jitter(shape=16, position=position_jitter(0.2),
              alpha = 0.5, size = 1) +
  stat_summary(fun = mean, geom = "crossbar", width = 0.5, color = "red") + # Mean line
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15)) +
  labs(x = "Year",
       y = NULL)

ggplot(v_df, aes(x= factor(lg_bin), y=ext_an_dist)) + 
  geom_violin() +
  geom_jitter(shape=16, position=position_jitter(0.2),
              alpha = 0.5, size = 1) +
  stat_summary(fun = mean, geom = "crossbar", width = 0.5, color = "red") + # Mean line
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15)) +
  labs(x = "Year",
       y = NULL)
