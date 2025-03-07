library(sf)
library(ggplot2)
library(dplyr)
library(lubridate)
library(ggspatial)

# Import data
m_df <- read.csv("D:/Malaria/Writing/Paper 19 (Conclusion 1 knowlesi malaria)/macaque_dataset.csv",
                 stringsAsFactors = T)
v_df <- read.csv("D:/Malaria/Writing/Paper 19 (Conclusion 1 knowlesi malaria)/vector_dataset.csv",
                 stringsAsFactors = T)
h_df <- read.csv("D:/Malaria/Writing/Paper 19 (Conclusion 1 knowlesi malaria)/human_dataset.csv",
                 stringsAsFactors = T)
moh_df <- read.csv("D:/Malaria/Writing/Paper 19 (Conclusion 1 knowlesi malaria)/secondary_human_dataset.csv",
                   stringsAsFactors = T)

m_df$pct_n

### Part 1: Descriptive Visualisation ######
# Ensure full date range (Jan 2018 - Dec 2023)
all_dates <- data.frame(
  date = seq(as.Date("2018-01-01"), as.Date("2023-12-01"), by = "month")
) %>%
  mutate(
    month = month(date),
    year = year(date),
    month_year = format(date, "%m/%Y")
  )

# Aggregate the dataframe
m_df_bar <- m_df %>%
  group_by(year, month) %>%
  summarise(macaque_n = sum(macaque_n),
    positive_rate = (sum(pk_n)/sum(macaque_n))*100) %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-")))

v_df_bar <- v_df %>%
  group_by(year, month) %>%
  summarise(an_total = sum(an_total),
            leuco_rate = (sum(lg_total)/sum(an_total))*100) %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-")))

h_df_bar <- h_df %>%
  group_by(year, month) %>%
  summarise(screened_n = sum(screened_n),
            positive_rate = (sum(pk_n)/sum(screened_n))*100) %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-")))

moh_df_count <- moh_df %>%
  mutate(date = as.Date(paste(onset_year, onset_month, "01", sep = "-"))) %>%
  count(date, name = "frequency")

# Merge with full date range to ensure missing months are included
m_df_bar_new <- left_join(all_dates, m_df_bar, by = c("year","month", "date")) %>%
  arrange(date) %>%
  replace(is.na(.), 0)  # Fill missing months with zeros

v_df_bar_new <- left_join(all_dates, v_df_bar, by = c("year","month", "date")) %>%
  arrange(date) %>%
  replace(is.na(.), 0)  # Fill missing months with zeros

h_df_bar_new <- left_join(all_dates, h_df_bar, by = c("year","month", "date")) %>%
  arrange(date) %>%
  replace(is.na(.), 0)  # Fill missing months with zeros

moh_df_bar_new <-left_join(all_dates, moh_df_count, by = "date") %>%
  arrange(date) %>%
  replace(is.na(.), 0) 

# Create the bar + line chart
ggplot(m_df_bar_new, aes(x = date)) +
  geom_bar(aes(y = macaque_n), stat = "identity", fill = "blue") +
  scale_y_continuous(
    name = "Num. of macaque samples", expand = c(0, 0)) +
  labs(x = "Month-Year") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15))

ggplot(v_df_bar_new, aes(x = date)) +
  geom_bar(aes(y = an_total), stat = "identity", fill = "red") +
  scale_y_continuous(
    name = "Num. of Anopheles samples", expand = c(0, 0)) +
  labs(x = "Month-Year") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15))

ggplot(h_df_bar_new, aes(x = date)) +
  geom_bar(aes(y = screened_n), stat = "identity", fill = "darkgreen") +
  scale_y_continuous(
    name = "Num. of blood samples", expand = c(0, 0)) +
  labs(x = "Month-Year") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15))

ggplot(moh_df_bar_new, aes(x = date)) +
  geom_bar(aes(y = frequency), stat = "identity", fill = "purple") +
  scale_y_continuous(
    name = "Num. of indigenous cases", expand = c(0, 0)) +
  labs(x = "Month-Year") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15))

# Remove all dataset to save space
rm(list = ls())
######

# Convert the coordinates from EPSG:4326 to EPSG:32647
# Save and export as shapefile
m_shp <- st_as_sf(m_df, coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = 32647)

v_shp <- st_as_sf(v_df, coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = 32647)

h_shp <- st_as_sf(h_df, coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = 32647)

moh_shp <- st_as_sf(moh_df, coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = 32647)

st_write(m_shp, "D:/Malaria/Writing/Paper 19 (Conclusion 1 knowlesi malaria)/GIS/m_UTM_47N.shp", delete_layer = TRUE)
st_write(v_shp, "D:/Malaria/Writing/Paper 19 (Conclusion 1 knowlesi malaria)/GIS/v_UTM_47N.shp", delete_layer = TRUE)
st_write(h_shp, "D:/Malaria/Writing/Paper 19 (Conclusion 1 knowlesi malaria)/GIS/h_UTM_47N.shp", delete_layer = TRUE)
st_write(moh_shp, "D:/Malaria/Writing/Paper 19 (Conclusion 1 knowlesi malaria)/GIS/moh_UTM_47N.shp", delete_layer = TRUE)

# Remove all dataset to save space
rm(list = ls())

# Import all the shapefiles
m_shp <- st_read("D:/Malaria/Writing/Paper 19 (Conclusion 1 knowlesi malaria)/GIS/m_UTM_47N.shp")
v_shp <- st_read("D:/Malaria/Writing/Paper 19 (Conclusion 1 knowlesi malaria)/GIS/v_UTM_47N.shp")
h_shp <- st_read("D:/Malaria/Writing/Paper 19 (Conclusion 1 knowlesi malaria)/GIS/h_UTM_47N.shp")
moh_shp <- st_read("D:/Malaria/Writing/Paper 19 (Conclusion 1 knowlesi malaria)/GIS/moh_UTM_47N.shp")
mys_shp <- st_read("D:/Malaria/Writing/Paper 19 (Conclusion 1 knowlesi malaria)/GIS/Malaysia_shapefile.shp")

# Define the bounding box for cropping (xmin, ymin, xmax, ymax)
st_bbox(mys_shp)
bbox <- st_bbox(c(xmin = 570826.78, ymin = 96342.84, xmax = 1120000, 
                  ymax = 856683.59), crs = st_crs(mys_shp))

# Crop the shapefile using the bounding box
cropped_mys <- st_crop(mys_shp, bbox)

ggplot() +
  geom_sf(data = cropped_mys, fill = 'darkgrey', color = 'darkgrey') +
  geom_sf(data = m_shp, color = 'blue', size = 4) +
  geom_sf(data = v_shp, color = 'red', size = 4) +
  geom_sf(data = h_shp, color = 'darkgreen', size = 4) +
  annotation_north_arrow(
    location = "topright",
    style = north_arrow_orienteering(),
    height = unit(1,"cm"),
    width = unit(1,"cm"),) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18))

# Check validity of historical case data
# Filter points that are within the polygon
points_inside <- st_intersects(moh_shp, st_union(cropped_mys), sparse = F)
points_outside <- moh_shp[!points_inside, ]

ggplot() +
  geom_sf(data = cropped_mys, fill = 'darkgrey', color = 'darkgrey') +
  geom_sf(data = moh_shp, color = 'purple', size = 1) +
  annotation_north_arrow(
    location = "topright",
    style = north_arrow_orienteering(),
    height = unit(1,"cm"),
    width = unit(1,"cm"),)+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18))

# Create map of different species occurence

filtered_m_shp <- m_shp %>% filter(pfi_bin == 1)
filtered_v_shp <- v_shp %>% filter(pfi_bin == 1)

ggplot() +
  geom_sf(data = cropped_mys, fill = 'darkgrey', color = 'darkgrey') +
  geom_sf(data = filtered_m_shp, color = 'blue', size = 4) +
  geom_sf(data = filtered_v_shp , color = 'red', size = 4) +
  annotation_north_arrow(
    location = "topright",
    style = north_arrow_orienteering(),
    height = unit(1,"cm"),
    width = unit(1,"cm"),) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18))


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

