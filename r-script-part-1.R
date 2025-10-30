# Part 1: Data Wrangling
##########################

library(sf)
library(ggplot2)
library(lubridate)
library(ggspatial)
library(dplyr)
library(tidyr)


# Import Data for Macaque, Vector, and Human Datasets
# Note:
#   Some of the actual datasets used in this project contain sensitive
#   information and are not publicly shared for data protection
#   and ethical compliance reasons.
#
#   To reproduce this analysis, please replace the example
#   file paths below with your own dataset paths.

m_df <- read.csv("PATH/data/macaque_dataset.csv",
                 stringsAsFactors = T)
v_df <- read.csv("PATH/data/vector_dataset.csv",
                 stringsAsFactors = T)
h_df <- read.csv("PATH/data/human_dataset.csv",
                 stringsAsFactors = T)

# The secondary/historical human dataset is restricted and not publicly available.
# A placeholder is provided below for private use.
# To use your own data, supply the correct file path.
# Example:
# moh_df <- read.csv("data/secondary_human_dataset.csv", stringsAsFactors = TRUE)

# Print summary of imported data 
summary(m_df)
summary(v_df)
summary(h_df)

# Process the dataframe for later analysis
v_df$malaria_rate <- v_df$malaria_n/v_df$an_total
v_df$malaria_rate_lg <- v_df$malaria_n/v_df$lg_total
v_df$pin_rate_lg <- v_df$pin_n/v_df$lg_total
v_df$pcy_rate_lg <- v_df$pcy_n/v_df$lg_total
v_df$nonlg_pernite <- v_df$an_pernite - v_df$lg_pernite
v_df$lg_bin <- ifelse(v_df$lg_total == 0, 0, 1)

h_df$nhp_mal_n <- h_df$pk_n + h_df$pcy_n + h_df$pin_n
h_df$nhp_mal_rate <- h_df$nhp_mal_n/h_df$screened_n 

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

# Aggregate the dataframe by month
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

st_write(m_shp, "PATH/data/m_UTM_47N.shp", delete_layer = TRUE)
st_write(v_shp, "PATH/data/v_UTM_47N.shp", delete_layer = TRUE)
st_write(h_shp, "PATH/data/h_UTM_47N.shp", delete_layer = TRUE)
st_write(moh_shp, "PATH/data/moh_UTM_47N.shp", delete_layer = TRUE)

# Import shapefile
mys_shp <- st_read("PATH/data/Malaysia_shapefile.shp")

# Define the bounding box for cropping (xmin, ymin, xmax, ymax)
st_bbox(mys_shp)
bbox <- st_bbox(c(xmin = 570826.78, ymin = 96342.84, xmax = 1120000, 
                  ymax = 856683.59), crs = st_crs(mys_shp))

# Crop the shapefile using the bounding box
cropped_mys <- st_crop(mys_shp, bbox)

ggplot() +
  geom_sf(data = cropped_mys, fill = 'darkgrey', color = 'darkgrey') +
  geom_sf(data = m_shp, aes(color = lndscp_), size = 4) +
  scale_color_manual(values = c(
    "urban" = "red",
    "peridomestic_agri" = "blue",
    "peridomestic_forest" = "darkgreen"
  )) +
  annotation_north_arrow(
    location = "topright",
    style = north_arrow_orienteering(),
    height = unit(1, "cm"),
    width = unit(1, "cm")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18)
  )


# Macaque only
ggplot() +
  geom_sf(data = cropped_mys, fill = 'darkgrey', color = 'darkgrey') +
  geom_sf(data = m_shp, aes(color = m_shp$lndscp_), size = 4) +
  annotation_north_arrow(
    location = "topright",
    style = north_arrow_orienteering(),
    height = unit(1,"cm"),
    width = unit(1,"cm"),) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18))


# Check validity of historical case data
# Check which points fall within the study area polygon
points_inside <- st_intersects(moh_shp, st_union(cropped_mys), sparse = F) 
# Extract points that fall outside the polygon
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


# Calculate pairwise distances between host sampling data
m_v_dist_matrix <- st_distance(m_shp, v_shp) %>% # create distance matrix
  as.numeric()/1000 # Convert distances to kilometers

h_m_dist_matrix <- st_distance(h_shp, m_shp) %>% 
  as.numeric()/1000 # 

h_v_dist_matrix <- st_distance(h_shp, v_shp) %>% 
  as.numeric()/1000 # 

# Convert the distance matrix to a long-format data frame
m_v_dist_df <- as.data.frame(as.table(as.matrix(m_v_dist_matrix)))
h_m_dist_df <- as.data.frame(as.table(as.matrix(h_m_dist_matrix)))
h_v_dist_df <- as.data.frame(as.table(as.matrix(h_v_dist_matrix)))

# Convert row and column identifiers to proper point IDs 

ID_list1 <- expand.grid(m_id = m_shp$m_smp_d, v_id = v_shp$v_samp_id)
m_v_dist_df[, 1:2] <- ID_list1[, 1:2]

ID_list2 <- expand.grid(h_id = h_shp$h_smp_d, m_id = m_shp$m_smp_d)
h_m_dist_df[, 1:2] <- ID_list2[, 1:2]

ID_list3 <- expand.grid(h_id = h_shp$h_smp_d, v_id = v_shp$v_samp_id)
h_v_dist_df[, 1:2] <- ID_list3[, 1:2]

# Rename the columns for clarity
names(m_v_dist_df) <- c("m_id", "v_id", "dist_km")
names(h_m_dist_df) <- c("h_id", "m_id", "dist_km")
names(h_v_dist_df) <- c("h_id", "v_id", "dist_km")

#calculate number of link for each proximity constraint
library(tidyr)

thresholds <- 1:40
row_counts <- data.frame()

for (t in thresholds) {
  row_counts <- rbind(
    row_counts,
    data.frame(
      threshold = t,
      source = "m_v",
      count = m_v_dist_df %>% filter(dist_km <= t) %>% nrow()
    ),
    data.frame(
      threshold = t,
      source = "h_m",
      count = h_m_dist_df %>% filter(dist_km <= t) %>% nrow()
    ),
    data.frame(
      threshold = t,
      source = "h_v",
      count = h_v_dist_df %>% filter(dist_km <= t) %>% nrow()
    )
  )
}

# Plotting
ggplot(row_counts, aes(x = threshold, y = count, color = source)) +
  geom_line(size = 1.2) +
  labs(
    title = "Number of site pairs within proximity constraint",
    x = "Proximity constraint (km)",
    y = "Number of site pairs",
    color = "Data Source"
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.title = element_text(size = 18),     
    axis.text = element_text(size = 18),      
    legend.title = element_text(size = 14),   
    legend.text = element_text(size = 13),    
    plot.title = element_text(size = 20, face = "bold") 
  )
row_df 

# Filter for distances within 10 km
m_v_within_40km <- m_v_dist_df %>% filter(dist_km <= 10)
h_m_within_40km <- h_m_dist_df %>% filter(dist_km <= 10)
h_v_within_40km <- h_v_dist_df %>% filter(dist_km <= 10)
m_v_within_40km <- m_v_dist_df %>% filter(dist_km <= 20)
h_m_within_40km <- h_m_dist_df %>% filter(dist_km <= 20)
h_v_within_40km <- h_v_dist_df %>% filter(dist_km <= 20)
m_v_within_40km <- m_v_dist_df %>% filter(dist_km <= 40)
h_m_within_40km <- h_m_dist_df %>% filter(dist_km <= 40)
h_v_within_40km <- h_v_dist_df %>% filter(dist_km <= 40)


# Visualize the points and connections within 10, 20, and 40 km using ggplot2
# change the dataframe names accordingly 
m_v_lines <- m_v_within_40km %>%
  rowwise() %>%
  mutate(
    m_coords = st_coordinates(st_as_sf(m_shp[m_shp$m_smp_d == m_id, ])),
    v_coords = st_coordinates(st_as_sf(v_shp[v_shp$v_samp_id == v_id, ]))
  )

h_m_lines <- h_m_within_40km %>%
  rowwise() %>%
  mutate(
    h_coords = st_coordinates(st_as_sf(h_shp[h_shp$h_smp_d == h_id, ])),
    m_coords = st_coordinates(st_as_sf(m_shp[m_shp$m_smp_d == m_id, ]))
  )

h_v_lines <- h_v_within_40km %>%
  rowwise() %>%
  mutate(
    h_coords = st_coordinates(st_as_sf(h_shp[h_shp$h_smp_d == h_id, ])),
    v_coords = st_coordinates(st_as_sf(v_shp[v_shp$v_samp_id == v_id, ]))
  )

ggplot() +
  geom_sf(data = cropped_mys, fill = 'lightgrey', color = 'lightgrey')+
  geom_sf(data = v_shp, color = "red", size = 3) +
  geom_sf(data = m_shp, color = "blue", size = 3) +
  geom_sf(data = h_shp, color = "darkgreen", size = 3) +
  geom_segment(data = m_v_lines,
               aes(x = m_coords[,1], y = m_coords[,2], 
                   xend = v_coords[,1], yend = v_coords[,2]),
               color = "black") +
  geom_segment(data = h_m_lines,
               aes(x = h_coords[,1], y = h_coords[,2], 
                   xend = m_coords[,1], yend = m_coords[,2]),
               color = "black") +
  geom_segment(data = h_v_lines,
               aes(x = h_coords[,1], y = h_coords[,2], 
                   xend = v_coords[,1], yend = v_coords[,2]),
               color = "black") +
  theme_minimal()


# Combine data for analysis
my_data <- m_v_within_40km
colnames(v_df)

my_data <- my_data %>%
  left_join(m_df %>% dplyr::select( m_samp_id, month, year, month_year, 
                             macaque_n, malaria_n, malaria_rate, 
                             pk_n, pk_rate), 
            by = c("m_id" = "m_samp_id"))

my_data <- my_data %>%
  left_join(v_df %>% dplyr::select( v_samp_id, month, year, month_year, 
                             an_pernite, lg_pernite, malaria_n, 
                             mal_bin, malaria_rate), 
            by = c("v_id" = "v_samp_id"))


my_data <- my_data %>%
  left_join(h_df %>% dplyr::select( h_samp_id, month, year, month_year, 
                             malaria_n, pk_n, pk_rate), 
            by = c("h_id" = "h_samp_id"))

str(h_df)
# Calculate new variable duration gap (x-y)
# x is later
# y is earlier
my_data <- my_data %>%
  mutate(duration_gap_mth = time_length(interval(as.Date(paste(year.y, month.y, "01", sep = "-")), 
                                                 as.Date(paste(year.x, month.x, "01", sep = "-"))), "months"))



# calculate proportion of all parasites in macaques in each landscape type
str(m_df)

prop_df <- m_df %>%                                   # your data frame
  group_by(landscape_type) %>%                      # aggregate by landscape
  summarise(
    across(
      c(pk_n, pcy_n, pin_n, pct_n, pfi_n, macaque_n),
      sum,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%                                             #  convert counts → proportions
  mutate(across(
    c(pk_n, pcy_n, pin_n, pct_n, pfi_n),
    ~ .x / macaque_n
  )) %>%                                            # long format for ggplot
  dplyr::select(- macaque_n) %>%
  pivot_longer(
    cols  = pk_n:pfi_n,
    names_to  = "species",
    values_to = "prop"
  )


## UPSET Plot
library(dplyr)
library(UpSetR)

upset_data <- read.csv( "D:/Malaria/Writing/Paper 19 (Conclusion 1 knowlesi malaria)/Upset_plot_data.csv")
colnames(upset_data)[3:7] <- c("P. knowlesi", "P. cynomolgi", "P. coatneyi", "P. inui",
                               "P. fieldi")

upset_m <- upset_data %>%
  filter(host_species == "macaque")

upset_tempo <- upset_m %>%
  filter(rowSums(upset_m[, 3:7]) > 0)

nrow(upset_tempo)

upset(upset_m, nintersects = NA,
      nsets = 5, number.angles = 30, point.size = 3, line.size = 1,
      mainbar.y.label = "Frequency of mixed-species infection", 
      sets.x.label = "Frequency by species",
      text.scale = c(1, 1, 1, 1, 1.5, 1.5)
)

upset_v <- upset_data %>%
  filter(host_species == "vector")

upset(upset_v, nintersects = NA,
      nsets = 5, number.angles = 30, point.size = 3, line.size = 1,
      mainbar.y.label = "Frequency of mixed-species infection", 
      sets.x.label = "Frequency by species",
      text.scale = c(1, 1, 1, 1, 1.5, 1.5)
)
nrow(upset_v)



# ── 2. Plot ──────────────────────────────────────────────────────────────
ggplot(prop_df, aes(x = species,
                    y = prop,
                    fill = landscape_type)) +
  geom_col(position = position_dodge(width = 0.8),
           width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(),
                     expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(
    values = c(
      "urban" = "red",
      "peridomestic_agri" = "blue",
      "peridomestic_forest" = "darkgreen"
    ),
    name = "Landscape Type"
  ) +
  labs(
    x = "Parasite species",
    y = "Proportion of Positive Samples",
    title = "Proportion of Parasite‑Positive Samples by Landscape"
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# 1. Summarise total samples per landscape_type
pie_data <- m_df %>%
  group_by(landscape_type) %>%
  summarise(total_samples = sum(macaque_n, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    percent = total_samples / sum(total_samples) * 100,
    label = paste0(total_samples, " (", round(percent, 1), "%)")
  )

# 2. Pie chart
ggplot(pie_data, aes(x = "", y = total_samples, fill = landscape_type)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c(
    "urban" = "red",
    "peridomestic_agri" = "blue",
    "peridomestic_forest" = "darkgreen"
  )) +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            color = "white", size = 5) +

  theme_void(base_size = 14)

# Chi-square test for all parasite species
library(dplyr)
library(tidyr)
library(purrr)
library(stats)

# Define species columns
species_list <- c("pk_n", "pcy_n", "pin_n", "pct_n", "pfi_n")

# Get landscape combinations for pairwise comparisons
landscapes <- unique(m_df$landscape_type)
landscape_pairs <- combn(landscapes, 2, simplify = FALSE)

# Create a list to store results
chi_sq_results <- list()

for (sp in species_list) {
  
  # Step 1: Create aggregated data
  temp_df <- m_df %>%
    group_by(landscape_type) %>%
    summarise(
      infected = sum(.data[[sp]], na.rm = TRUE),
      total = sum(macaque_n, na.rm = TRUE),
      not_infected = total - infected,
      .groups = "drop"
    )
  
  # Step 2: Run overall chi-square test
  contingency_matrix <- as.matrix(temp_df[, c("infected", "not_infected")])
  rownames(contingency_matrix) <- temp_df$landscape_type
  contingency_matrix <- t(contingency_matrix)
  chisq_test <- suppressWarnings(chisq.test(contingency_matrix))
  
  # Step 3: Pairwise comparisons
  pairwise_pvals <- map_dfr(landscape_pairs, function(pair) {
    df_pair <- temp_df %>% filter(landscape_type %in% pair)
    
    pair_matrix <- matrix(
      c(df_pair$infected, df_pair$not_infected),
      nrow = 2,
      byrow = TRUE
    )
    colnames(pair_matrix) <- df_pair$landscape_type
    rownames(pair_matrix) <- c("infected", "not_infected")
    
    test <- suppressWarnings(chisq.test(pair_matrix))
    
    tibble(
      species = sp,
      group1 = pair[1],
      group2 = pair[2],
      p_value = test$p.value
    )
  })
  
  # Step 4: Adjust p-values with Bonferroni correction
  pairwise_pvals <- pairwise_pvals %>%
    mutate(p_adj = p.adjust(p_value, method = "bonferroni"))
  
  # Step 5: Save results
  chi_sq_results[[sp]] <- list(
    overall_test = chisq_test,
    posthoc = pairwise_pvals
  )
}

chi_sq_results$pk_n$overall_test  # Overall chi-square test result for P. knowlesi
chi_sq_results$pk_n$posthoc       # Pairwise comparisons with Bonferroni adjustment
chi_sq_results$pin_n$posthoc       # Pairwise comparisons with Bonferroni adjustment
chi_sq_results$pct_n$posthoc       # Pairwise comparisons with Bonferroni adjustment
