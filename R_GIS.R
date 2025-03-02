library(sf)
library(ggplot2)
library(dplyr)
library(lubridate)

mozzi_data <- st_read("D:/Malaria/Writing/Paper 19 (Conclusion 1 knowlesi malaria)/GIS/mozzi_UTM_47N.shp")
macaca_data <- st_read("D:/Malaria/Writing/Paper 19 (Conclusion 1 knowlesi malaria)/GIS/macaca_UTM_47N.shp")
mys_shp <- st_read("D:/Malaria/Writing/Paper 19 (Conclusion 1 knowlesi malaria)/GIS/Malaysia_shapefile.shp")

# Define the bounding box for cropping (xmin, ymin, xmax, ymax)
st_bbox(mys_shp)
bbox <- st_bbox(c(xmin = 570826.78, ymin = 96342.84, xmax = 1120000, 
                  ymax = 856683.59), crs = st_crs(mys_shp))

# Crop the shapefile using the bounding box
cropped_mys <- st_crop(mys_shp, bbox)

ggplot() +
  geom_sf(data = cropped_mys, fill = 'lightgrey', color = 'lightgrey') +
  geom_sf(data = mozzi_data, color = 'blue', size = 3) +
  geom_sf(data = macaca_data, color = 'red', size = 3) +
  theme_minimal()

# Calculate pairwise distances
distance_matrix <- st_distance(mozzi_data, macaca_data)

# Convert distances to kilometers
distance_matrix_km <- as.numeric(distance_matrix) / 1000

# Convert the distance matrix to a long-format data frame
distance_df <- as.data.frame(as.table(as.matrix(distance_matrix_km)))

# Convert row and column identifiers to proper point IDs 

ID_list <- expand.grid(mozzi_ID = mozzi_data$id, macaca_ID = macaca_data$id)

distance_df[, 1:2] <- ID_list[, 1:2]

# Rename the columns for clarity
names(distance_df) <- c("mozzi_ID", "macaca_ID", "Distance_km")

# Filter for distances within 10 km
within_100km <- distance_df %>% filter(Distance_km <= 40)

# Visualize the points and connections within 10 km using ggplot2
lines <- within_100km %>%
  rowwise() %>%
  mutate(
    moz_coords = st_coordinates(st_as_sf(mozzi_data[mozzi_data$id == mozzi_ID, ])),
    mac_coords = st_coordinates(st_as_sf(macaca_data[macaca_data$id == macaca_ID, ]))
  )

ggplot() +
  geom_sf(data = cropped_mys, fill = 'lightgrey', color = 'lightgrey')+
  geom_sf(data = mozzi_data, color = "blue", size = 3) +
  geom_sf(data = macaca_data, color = "red", size = 3) +
  geom_segment(data = lines,
               aes(x = moz_coords[,1], y = moz_coords[,2], 
                   xend = mac_coords[,1], yend = mac_coords[,2]),
               color = "black") +
  theme_minimal()


# Combine data for analysis
my_data <- within_100km 

my_data <- my_data %>%
  left_join(mozzi_data %>% select( id, month, year, month_year,
                                  anopheles, leuco_grou, leuco_comp, 
                                  an_pernite, lg_pernite, lc_pernite), 
            by = c("mozzi_ID" = "id"))

my_data <- my_data %>%
  left_join(macaca_data %>% select( id, month, year, month_year,
                                   macaque_n, malaria_n, pk_n), 
            by = c("macaca_ID" = "id"))

my_data  <- my_data  %>% 
  select(-c(geometry.x, geometry.y))

# Calculate new variable
my_data$pk_percent <- my_data$pk_n / my_data$macaque_n
str(my_data)
my_data <- my_data %>%
  mutate(duration_gap_mth= interval(as.Date(month_year.x), 
                                    as.Date(month_year.y)) / months(1))

plot(my_data$duration_gap_mth, my_data$pk_percent,
     pch = 19, frame = FALSE)

library(plotly)
plot_ly(x = my_data$lg_pernite, 
        y = my_data$Distance_km, 
        z = my_data$pk_n, 
        type = 'scatter3d',
        marker = list(color = ~my_data$duration_gap_mth, 
                      colorscale = c('yellow', 'red'), 
                      showscale = TRUE,
                      size = 5))

#Correlation analysis

# Calculate Spearman correlation
library("ggpubr")
colnames(my_data)
var_A <- "duration_gap_mth"
var_B <- "pk_percent"

ggscatter(my_data, x = var_A, y = var_B, 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "An. caught per night", ylab = "Macaca positive n")

