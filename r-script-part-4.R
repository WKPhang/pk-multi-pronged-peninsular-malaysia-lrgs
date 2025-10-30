library(raster)
library(sf)
library(dplyr)

# Import rasters of predicted map
xgb_pk_map <- raster("PATH/XGBraster_mean_final.tif") # human knowlesi malaria map
lr_vector_map <- raster("PATH/predicted.tif") # vector map

plot(xgb_pk_map)
plot(lr_vector_map)

# Assign raster map with EPSG:32647
crs(xgb_pk_map) <- "EPSG:32647"

crs(lr_vector_map) <- "EPSG:32647"

# Define the value range for valid pixel cell
value_range <- c(0, 1)

# Create a logical raster indicating valid cells
valid_cells_raster <- calc(raster_map, fun = function(x) { 
  x >= value_range[1] & x <= value_range[2] })

# Identify the indices of valid cells
valid_cells_indices <- which(valid_cells_raster[], arr.ind = TRUE)
length(valid_cells_indices)

# Ensure we have enough valid cells to place points
if (length(valid_cells_indices) < 20000) {
  stop("Not enough valid cells to place 20,000 points.")
}

# Randomly select 20,000 from the valid cells
selected_cells <- sample(valid_cells_indices, 20000)

# Convert cell indices to spatial coordinates
xy_coords <- xyFromCell(raster_map, selected_cells) 

# Convert to sf object
random_points <- st_as_sf(data.frame(x = xy_coords[, 1], y = xy_coords[, 2]), 
                          coords = c("x", "y"), crs = crs(raster_map))

# Plot the raster and overlay the random points
plot(raster_map, main = "Random Points within Pixel Value Range (0.0 - 1.0)")
plot(st_geometry(random_points), add = TRUE, col = "red", pch = 4, cex = 0.001)


