# For Partial Correlation Analysis

# Import all the shapefiles
m_shp <- st_read("D:/Malaria/Writing/Paper 19 (Conclusion 1 knowlesi malaria)/GIS/m_UTM_47N.shp")
v_shp <- st_read("D:/Malaria/Writing/Paper 19 (Conclusion 1 knowlesi malaria)/GIS/v_UTM_47N.shp")
h_shp <- st_read("D:/Malaria/Writing/Paper 19 (Conclusion 1 knowlesi malaria)/GIS/h_UTM_47N.shp")

# Calculate pairwise distances between macaque and vector data
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

# Filter out the find to shortest proximity possible
# Get the lowest dist_km and corresponding v_id for each m_id
h_h_m <- h_m_dist_df %>%
  group_by(h_id) %>%
  slice_min(order_by = dist_km, n = 1, with_ties = FALSE) %>%
  ungroup()
m_h_m <- h_m_dist_df %>%
  group_by(m_id) %>%
  slice_min(order_by = dist_km, n = 1, with_ties = FALSE) %>%
  ungroup()
h_h_v <- h_v_dist_df %>%
  group_by(h_id) %>%
  slice_min(order_by = dist_km, n = 1, with_ties = FALSE) %>%
  ungroup()
v_h_v <- h_v_dist_df %>%
  group_by(v_id) %>%
  slice_min(order_by = dist_km, n = 1, with_ties = FALSE) %>%
  ungroup()
m_m_v <- m_v_dist_df %>%
  group_by(m_id) %>%
  slice_min(order_by = dist_km, n = 1, with_ties = FALSE) %>%
  ungroup()
v_m_v <- m_v_dist_df %>%
  group_by(v_id) %>%
  slice_min(order_by = dist_km, n = 1, with_ties = FALSE) %>%
  ungroup()

# Add group label to each dataframe
h_h_m$group <- "h_h_m"
m_h_m$group <- "m_h_m"
h_h_v$group <- "h_h_v"
v_h_v$group <- "v_h_v"
m_m_v$group <- "m_m_v"
v_m_v$group <- "v_m_v"
# Combine all into one dataframe
combined_df <- bind_rows(h_h_m, m_h_m, h_h_v, v_h_v, m_m_v, v_m_v)

# Perform Shapiro-Wilk test for each group
normality_tests <- combined_df %>%
  group_by(group) %>%
  summarise(
    
  )

print(normality_tests)

# Group by and compute mean
group_stats <- combined_df %>%
  group_by(group) %>%
  summarise(
    median_dist = median(dist_km, na.rm = FALSE),
    q1_dist   = quantile(dist_km, 0.25, na.rm = FALSE),
    q3_dist   = quantile(dist_km, 0.75, na.rm = FALSE),
    p_value = tryCatch(shapiro.test(dist_km)$p.value, error = function(e) NA),
    W_statistic = tryCatch(shapiro.test(dist_km)$statistic, error = function(e) NA),
    n = n()
  )
print(group_stats)
# Plot
ggplot(combined_df, aes(x = group, y = dist_km)) +
  geom_jitter(width = 0.15, alpha = 0.6, color = "steelblue", size = 2) +  # dot plot
  stat_summary(fun = median, geom = "crossbar", width = 0.4, color = "red", fatten = 2) +  # median
  stat_summary(fun.data = ~ {
    q <- quantile(., probs = c(0.25, 0.75), na.rm = TRUE)
    data.frame(ymin = q[1], ymax = q[2], y = median(., na.rm = TRUE))
  }, geom = "errorbar", width = 0.3, color = "black") +  # IQR
  labs(x = "Group", y = "Distance (km)", title = "Dot Plot with Median and IQR") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Filter for distances within 20 km
h_m_within_10km <- h_m_dist_df %>% filter(dist_km <= 10)
h_m_within_20km <- h_m_dist_df %>% filter(dist_km <= 20)
h_m_within_40km <- h_m_dist_df %>% filter(dist_km <= 40)

h_v_within_5km <- h_v_dist_df %>% filter(dist_km <= 5)
h_v_within_10km <- h_v_dist_df %>% filter(dist_km <= 10)
h_v_within_20km <- h_v_dist_df %>% filter(dist_km <= 20)
h_v_within_40km <- h_v_dist_df %>% filter(dist_km <= 40)

m_v_within_10km <- m_v_dist_df %>% filter(dist_km <= 10)
m_v_within_20km <- m_v_dist_df %>% filter(dist_km <= 20)
m_v_within_40km <- m_v_dist_df %>% filter(dist_km <= 40)



# Combine data for analysis

join_sampling_data <- function(my_data, h_df = NULL, m_df = NULL, v_df = NULL) {

  # Join human data if provided
  if (!is.null(h_df)) {
    my_data <- my_data %>%
      left_join(
        h_df %>% dplyr::select(
          h_samp_id, month, year, month_year, malaria_rate,
          malaria_n, pk_n, pk_rate
        ),
        by = c("h_id" = "h_samp_id")
      )
  }
    
  # Join macaque data if provided
  if (!is.null(m_df)) {
    my_data <- my_data %>%
      left_join(
        m_df %>% dplyr::select(
          m_samp_id, month, year, month_year, 
          macaque_n, malaria_n, malaria_rate, 
          pk_n, pk_rate, pin_rate, pcy_rate, pfi_rate, 
        ),
        by = c("m_id" = "m_samp_id")
      )
  }
  
  # Join vector data if provided
  if (!is.null(v_df)) {
    my_data <- my_data %>%
      left_join(
        v_df %>% dplyr::select(
          v_samp_id, month, year, month_year, 
          an_pernite, lg_pernite, malaria_n, 
          mal_bin, malaria_rate, pcy_rate, pin_rate, pct_rate, pfi_rate,
          malaria_rate_lg, pcy_rate_lg, pin_rate_lg, pct_rate_lg, pfi_rate_lg,
          nonlg_pernite
        ),
        by = c("v_id" = "v_samp_id")
      )
  }
  return(my_data)
}

h_m_10km_joined <- join_sampling_data(h_m_within_10km, h_df = h_df, m_df = m_df)
h_m_20km_joined <- join_sampling_data(h_m_within_20km, h_df = h_df, m_df = m_df)
h_m_40km_joined <- join_sampling_data(h_m_within_40km, h_df = h_df, m_df = m_df)

h_v_5km_joined <- join_sampling_data(h_v_within_5km, h_df = h_df, v_df = v_df)
h_v_10km_joined <- join_sampling_data(h_v_within_10km, h_df = h_df, v_df = v_df)
h_v_20km_joined <- join_sampling_data(h_v_within_20km, h_df = h_df, v_df = v_df)
h_v_40km_joined <- join_sampling_data(h_v_within_40km, h_df = h_df, v_df = v_df)

m_v_10km_joined <- join_sampling_data(m_v_within_10km, m_df = m_df, v_df = v_df)
m_v_20km_joined <- join_sampling_data(m_v_within_20km, m_df = m_df, v_df = v_df)
m_v_40km_joined <- join_sampling_data(m_v_within_40km, m_df = m_df, v_df = v_df)

# Calculate new variable duration gap (x-y)
# x is later
# y is earlier

library(lubridate)

cal_time_gap <- function(my_data) {
  my_data %>%
    mutate(
      duration_gap_mth = time_length(
        interval(
          ymd(paste(year.y, month.y, "01", sep = "-")),
          ymd(paste(year.x, month.x, "01", sep = "-"))
        ),
        "months"
      )
    )
}

h_m_10km_joined <- cal_time_gap(h_m_10km_joined)
h_m_20km_joined <- cal_time_gap(h_m_20km_joined)
h_m_40km_joined <- cal_time_gap(h_m_40km_joined)

h_v_5km_joined <- cal_time_gap(h_v_5km_joined)
h_v_10km_joined <- cal_time_gap(h_v_10km_joined)
h_v_20km_joined <- cal_time_gap(h_v_20km_joined)
h_v_40km_joined <- cal_time_gap(h_v_40km_joined)

m_v_10km_joined <- cal_time_gap(m_v_10km_joined)
m_v_20km_joined <- cal_time_gap(m_v_20km_joined)
m_v_40km_joined <- cal_time_gap(m_v_40km_joined)


# Partial correlation analysis
library(ppcor)
str(my_data)

max(my_data$dist_km)
custom_pcor <- function(my_data, x, y) {
  pcor.test(
    my_data[[x]], 
    my_data[[y]], 
    my_data[, c("dist_km", "duration_gap_mth")], 
    method = "spearman"
  )
}

str(h_m_20km_joined)
custom_pcor(h_m_10km_joined, "pk_rate.x", "pk_rate.y")
custom_pcor(h_m_20km_joined, "pk_rate.x", "pk_rate.y")
custom_pcor(h_m_40km_joined, "pk_rate.x", "pk_rate.y")
custom_pcor(h_m_10km_joined, "pk_rate.x", "macaque_n")
custom_pcor(h_m_20km_joined, "pk_rate.x", "macaque_n")
custom_pcor(h_m_40km_joined, "pk_rate.x", "macaque_n")
custom_pcor(h_m_10km_joined, "pk_rate.x", "pk_n.y")
custom_pcor(h_m_20km_joined, "pk_rate.x", "pk_n.y")
custom_pcor(h_m_40km_joined, "pk_rate.x", "pk_n.y")
custom_pcor(h_m_10km_joined, "pk_n.x", "pk_n.y")
custom_pcor(h_m_20km_joined, "pk_n.x", "pk_n.y")
custom_pcor(h_m_40km_joined, "pk_n.x", "pk_n.y")
custom_pcor(h_m_10km_joined, "pk_n.x", "pk_rate.y")
custom_pcor(h_m_20km_joined, "pk_n.x", "pk_rate.y")
custom_pcor(h_m_40km_joined, "pk_n.x", "pk_rate.y")
custom_pcor(h_m_10km_joined, "malaria_rate.x", "malaria_rate.y")
custom_pcor(h_m_20km_joined, "malaria_rate.x", "malaria_rate.y")
custom_pcor(h_m_40km_joined, "malaria_rate.x", "malaria_rate.y")



str(h_v_20km_joined)
custom_pcor(h_v_5km_joined, "malaria_rate.x", "an_pernite")
custom_pcor(h_v_10km_joined, "malaria_rate.x", "an_pernite")
custom_pcor(h_v_20km_joined, "malaria_rate.x", "an_pernite")
custom_pcor(h_v_40km_joined, "malaria_rate.x", "an_pernite")
custom_pcor(h_v_5km_joined, "malaria_rate.x", "lg_pernite")
custom_pcor(h_v_10km_joined, "malaria_rate.x", "lg_pernite")
custom_pcor(h_v_20km_joined, "malaria_rate.x", "lg_pernite")
custom_pcor(h_v_40km_joined, "malaria_rate.x", "lg_pernite")
custom_pcor(h_v_5km_joined, "malaria_rate.x", "nonlg_pernite")
custom_pcor(h_v_10km_joined, "malaria_rate.x", "nonlg_pernite")
custom_pcor(h_v_20km_joined, "malaria_rate.x", "nonlg_pernite")
custom_pcor(h_v_40km_joined, "malaria_rate.x", "nonlg_pernite")
custom_pcor(h_v_5km_joined, "malaria_rate.x", "malaria_rate.y)")
custom_pcor(h_v_10km_joined, "malaria_rate.x", "malaria_rate.y")
custom_pcor(h_v_20km_joined, "malaria_rate.x", "malaria_rate.y")
custom_pcor(h_v_40km_joined, "malaria_rate.x", "malaria_rate.y")
custom_pcor(na.omit(h_v_10km_joined), "malaria_rate.x", "malaria_rate_lg")
custom_pcor(na.omit(h_v_10km_joined), "malaria_rate.x", "malaria_rate_lg")
custom_pcor(na.omit(h_v_20km_joined), "malaria_rate.x", "malaria_rate_lg")
custom_pcor(na.omit(h_v_40km_joined), "malaria_rate.x", "malaria_rate_lg")

custom_pcor(h_v_5km_joined, "pk_rate", "an_pernite")
custom_pcor(h_v_10km_joined, "pk_rate", "an_pernite")
custom_pcor(h_v_20km_joined, "pk_rate", "an_pernite")
custom_pcor(h_v_40km_joined, "pk_rate", "an_pernite")
custom_pcor(h_v_5km_joined, "pk_rate", "lg_pernite")
custom_pcor(h_v_10km_joined, "pk_rate", "lg_pernite")
custom_pcor(h_v_20km_joined, "pk_rate", "lg_pernite")
custom_pcor(h_v_40km_joined, "pk_rate", "lg_pernite")
custom_pcor(h_v_5km_joined, "pk_rate", "malaria_rate.y")
custom_pcor(h_v_10km_joined, "pk_rate", "malaria_rate.y")
custom_pcor(h_v_20km_joined, "pk_rate", "malaria_rate.y")
custom_pcor(h_v_40km_joined, "pk_rate", "malaria_rate.y")
custom_pcor(h_v_5km_joined, "pk_n", "an_pernite")
custom_pcor(h_v_10km_joined, "pk_n", "an_pernite")
custom_pcor(h_v_20km_joined, "pk_n", "an_pernite")
custom_pcor(h_v_40km_joined, "pk_n", "an_pernite")
custom_pcor(h_v_5km_joined, "pk_n", "lg_pernite")
custom_pcor(h_v_10km_joined, "pk_n", "lg_pernite")
custom_pcor(h_v_20km_joined, "pk_n", "lg_pernite")
custom_pcor(h_v_40km_joined, "pk_n", "lg_pernite")




str(m_v_20km_joined)
custom_pcor(m_v_10km_joined, "malaria_rate.x", "an_pernite")
custom_pcor(m_v_20km_joined, "malaria_rate.x", "an_pernite")
custom_pcor(m_v_40km_joined, "malaria_rate.x", "an_pernite")
custom_pcor(m_v_10km_joined, "malaria_rate.x", "nonlg_pernite")
custom_pcor(m_v_20km_joined, "malaria_rate.x", "nonlg_pernite")
custom_pcor(m_v_40km_joined, "malaria_rate.x", "nonlg_pernite")

custom_pcor(m_v_10km_joined, "malaria_rate.x", "lg_pernite")
custom_pcor(m_v_20km_joined, "malaria_rate.x", "lg_pernite")
custom_pcor(m_v_40km_joined, "malaria_rate.x", "lg_pernite")
custom_pcor(m_v_10km_joined, "malaria_rate.x", "malaria_rate.y")
custom_pcor(m_v_20km_joined, "malaria_rate.x", "malaria_rate.y")
custom_pcor(m_v_40km_joined, "malaria_rate.x", "malaria_rate.y")


custom_pcor(m_v_10km_joined, "pk_rate", "an_pernite")
custom_pcor(m_v_20km_joined, "pk_rate", "an_pernite")
custom_pcor(m_v_40km_joined, "pk_rate", "an_pernite")
custom_pcor(m_v_10km_joined, "pk_rate", "lg_pernite")
custom_pcor(m_v_20km_joined, "pk_rate", "lg_pernite")
custom_pcor(m_v_40km_joined, "pk_rate", "lg_pernite")
custom_pcor(m_v_10km_joined, "pk_rate", "malaria_rate.y")
custom_pcor(m_v_20km_joined, "pk_rate", "malaria_rate.y")
custom_pcor(m_v_40km_joined, "pk_rate", "malaria_rate.y")


custom_pcor(m_v_10km_joined, "macaque_n", "lg_pernite")
custom_pcor(m_v_20km_joined, "macaque_n", "lg_pernite")
custom_pcor(m_v_40km_joined, "macaque_n", "lg_pernite")



custom_pcor(m_v_10km_joined, "macaque_n", "an_pernite")
custom_pcor(m_v_20km_joined, "macaque_n", "an_pernite")
custom_pcor(m_v_40km_joined, "macaque_n", "an_pernite")
custom_pcor(m_v_10km_joined, "pk_rate", "mal_bin")
custom_pcor(m_v_20km_joined, "pk_rate", "mal_bin")
custom_pcor(m_v_40km_joined, "pk_rate", "mal_bin")
custom_pcor(m_v_10km_joined, "malaria_rate.x", "mal_bin")
custom_pcor(m_v_20km_joined, "malaria_rate.x", "mal_bin")
custom_pcor(m_v_40km_joined, "malaria_rate.x", "mal_bin")
custom_pcor(m_v_10km_joined, "macaque_n", "mal_bin")
custom_pcor(m_v_20km_joined, "macaque_n", "mal_bin")
custom_pcor(m_v_40km_joined, "macaque_n", "mal_bin")

custom_pcor(m_v_10km_joined, "pk_rate", "malaria_rate.y")
custom_pcor(m_v_20km_joined, "pk_rate", "malaria_rate.y")
custom_pcor(m_v_40km_joined, "pk_rate", "malaria_rate.y")
custom_pcor(m_v_10km_joined, "malaria_rate.x", "malaria_rate.y")
custom_pcor(m_v_20km_joined, "malaria_rate.x", "malaria_rate.y")
custom_pcor(m_v_40km_joined, "malaria_rate.x", "malaria_rate.y")
custom_pcor(m_v_10km_joined, "macaque_n", "malaria_rate.y")
custom_pcor(m_v_20km_joined, "macaque_n", "malaria_rate.y")
custom_pcor(m_v_40km_joined, "macaque_n", "malaria_rate.y")
custom_pcor(na.omit(m_v_10km_joined), "malaria_rate.x", "malaria_rate_lg")
custom_pcor(na.omit(m_v_20km_joined), "malaria_rate.x", "malaria_rate_lg")
custom_pcor(na.omit(m_v_40km_joined), "malaria_rate.x", "malaria_rate_lg")
custom_pcor(na.omit(m_v_10km_joined), "pin_rate", "pin_rate_lg")
custom_pcor(na.omit(m_v_20km_joined), "pin_rate", "pin_rate_lg")
custom_pcor(na.omit(m_v_40km_joined), "pin_rate", "pin_rate_lg")
custom_pcor(na.omit(m_v_10km_joined), "pcy_rate", "pcy_rate_lg")
custom_pcor(na.omit(m_v_20km_joined), "pcy_rate", "pcy_rate_lg")
custom_pcor(na.omit(m_v_40km_joined), "pcy_rate", "pcy_rate_lg")
custom_pcor(na.omit(m_v_10km_joined), "pin_rate", "pin_rate_lg")
custom_pcor(na.omit(m_v_20km_joined), "pin_rate", "pin_rate_lg")
custom_pcor(na.omit(m_v_40km_joined), "pin_rate", "pin_rate_lg")
custom_pcor(na.omit(m_v_10km_joined), "pcy_rate", "pin_rate_lg")
custom_pcor(na.omit(m_v_20km_joined), "pcy_rate", "pin_rate_lg")
custom_pcor(na.omit(m_v_40km_joined), "pcy_rate", "pin_rate_lg")

