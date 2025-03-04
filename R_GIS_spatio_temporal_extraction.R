radii <-  seq(1000, 20000, by = 1000)
mth_lead <- seq(-12, 12, by = 1)
expanded_list_v <- expand.grid(id = v_shp$v_samp_id, radius = radii, 
                             mth_lead = mth_lead)
t <- dmy(v_shp$month_year)

# For loop (for mosquito)
for (i in seq_len(nrow(expanded_list_v))){
  selected_id <- expanded_list_v$id[i] 
  selected_origin <- v_shp[v_shp$v_samp_id == selected_id,]
  radius <- expanded_list_v$radius[i]
  selected_date <- dmy(v_shp$month_year) %m+% months(expanded_list_v$mth_lead[i])
  filtered_hum_data <- moh_shp %>%  
    filter(dmy(mnth_yr) == selected_date[selected_id])
  within_distance <- st_is_within_distance(selected_origin, 
                                           filtered_hum_data, dist = radius)
  counted <- lengths(within_distance)
  # Update the counts column in expanded_list
  expanded_list_v$case_counts[i] <- counted
  percent <- round((i /nrow(expanded_list_v))*100, 2)
  print(paste(percent, "% done"))
}

# For macaque
expanded_list_m <- expand.grid(id = m_shp$m_smp_d, radius = radii, 
                             mth_lead = mth_lead)
# For loop (for macaque)
for (i in seq_len(nrow(expanded_list_m))){
  selected_id <- expanded_list_m$id[i] 
  selected_origin <- m_shp[m_shp$m_smp_d == selected_id,]
  radius <- expanded_list_m$radius[i]
  selected_date <- dmy(m_shp$mnth_yr) %m+% months(expanded_list_m$mth_lead[i])
  filtered_hum_data <- moh_shp %>%  
    filter(dmy(mnth_yr) == selected_date[selected_id])
  within_distance <- st_is_within_distance(selected_origin, 
                                           filtered_hum_data, dist = radius)
  counted <- lengths(within_distance)
  # Update the counts column in expanded_list
  expanded_list_m$case_counts[i] <- counted
  percent <- round((i /nrow(expanded_list_m))*100, 2)
  print(paste(percent, "% done"))
}

# For human
expanded_list_h <- expand.grid(id = h_shp$h_smp_d, radius = radii, 
                               mth_lead = mth_lead)
# For loop (for human)
for (i in seq_len(nrow(expanded_list_h))){
  selected_id <- expanded_list_h$id[i] 
  selected_origin <- h_shp[h_shp$h_smp_d == selected_id,]
  radius <- expanded_list_h$radius[i]
  selected_date <- dmy(h_shp$mnth_yr) %m+% months(expanded_list_h$mth_lead[i])
  filtered_hum_data <- moh_shp %>%  
    filter(dmy(mnth_yr) == selected_date[selected_id])
  within_distance <- st_is_within_distance(selected_origin, 
                                           filtered_hum_data, dist = radius)
  counted <- lengths(within_distance)
  # Update the counts column in expanded_list
  expanded_list_h$case_counts[i] <- counted
  percent <- round((i /nrow(expanded_list_h))*100, 2)
  print(paste(percent, "% done"))
}
write.csv(expanded_list_h, "D:/Malaria/Writing/Paper 19 (Conclusion 1 knowlesi malaria)/Extracted_human_new.csv", row.names = F)
write.csv(expanded_list_m, "D:/Malaria/Writing/Paper 19 (Conclusion 1 knowlesi malaria)/Extracted_macaque_new.csv", row.names = F)
write.csv(expanded_list_v, "D:/Malaria/Writing/Paper 19 (Conclusion 1 knowlesi malaria)/Extracted_vector_new.csv", row.names = F)


############### 
#####PAUSE#####
###############
rm(list = ls())
  
expanded_list_m <- read.csv("D:/Malaria/Writing/Paper 19 (Conclusion 1 knowlesi malaria)/Extracted_macaque_new.csv")
expanded_list_v <- read.csv("D:/Malaria/Writing/Paper 19 (Conclusion 1 knowlesi malaria)/Extracted_vector_new.csv")
expanded_list_h <- read.csv("D:/Malaria/Writing/Paper 19 (Conclusion 1 knowlesi malaria)/Extracted_human_new.csv")
m_df <- read.csv("D:/Malaria/Writing/Paper 19 (Conclusion 1 knowlesi malaria)/macaque_dataset.csv",
                 stringsAsFactors = T)
v_df <- read.csv("D:/Malaria/Writing/Paper 19 (Conclusion 1 knowlesi malaria)/vector_dataset.csv",
                 stringsAsFactors = T)
h_df <- read.csv("D:/Malaria/Writing/Paper 19 (Conclusion 1 knowlesi malaria)/human_dataset.csv",
                 stringsAsFactors = T)



# Join dataset

m_df_joined <- left_join(expanded_list_m, m_df, by = join_by("id" == "m_samp_id"))
v_df_joined <- left_join(expanded_list_v, v_df, by = join_by("id" == "v_samp_id"))
h_df_joined <- left_join(expanded_list_h, h_df, by = join_by("id" == "h_samp_id"))

# Spearman Correlation Matrix Analysis
radii <-  seq(1000, 20000, by = 1000)
mth_lead <- seq(-12, 12, by = 1) ## Change month based on selection
correlation_matrix_table <- expand.grid(radius = radii, mth_lead = mth_lead)

target_host_data <- m_df_joined # Change accordingly
colnames(target_host_data)

for (i in seq_len(nrow(correlation_matrix_table))){
  selected_radius <- correlation_matrix_table$radius[i]
  selected_mth_lead <- correlation_matrix_table$mth_lead[i]
  filtered_data <- target_host_data %>%  
    filter(radius == selected_radius & 
             mth_lead == selected_mth_lead)
  
  # Selected variables for correlation analysis
  corvar_a <- filtered_data$pk_rate
  corvar_b <- filtered_data$case_counts
  
  # Spearman's correlation
  cor_test <- cor.test(corvar_a, corvar_b, method = "spearman")
  
  # Extracting rho and p-value
  # Update the counts column in expanded_condition
  correlation_matrix_table$rho[i] <- cor_test$estimate
  correlation_matrix_table$p_value[i] <- cor_test$p.value
  
  percent <- round((i /nrow(correlation_matrix_table))*100, 2)
  print(paste(percent, "% done"))
}

summary(correlation_matrix_table)


# Heatmap creation

  
library(ggplot2)
colnames(correlation_matrix_table)

summary(correlation_matrix_table)

#### Heatmap with legend
ggplot(correlation_matrix_table, aes(x = mth_lead, y = radius, fill = rho)) +
  geom_tile(colour="darkgrey") + 
  scale_fill_gradientn(colors = c("blue3" , "white", "red3"),
                       na.value = "grey31",
                       limits = c(-0.8, 0.8),
                       breaks = seq(-0.8, 0.8, by = 0.2)) +
  labs(x = "Time (Months)", y = "Radius (m)", fill = "Spearman's rho") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text = element_text(size = 15))+
  guides(fill = guide_colorbar(barwidth = 1, barheight = 8))

# For p-value based heatmap
# p-value correction

correlation_matrix_table$fdr_p_value <- p.adjust(correlation_matrix_table$p_value, method = "fdr")

correlation_matrix_table <- correlation_matrix_table %>%
  mutate(p_group = case_when(
    fdr_p_value <= 0.01 ~ "≤ 0.01",
    fdr_p_value > 0.01 & fdr_p_value <= 0.05 ~ "0.01 - 0.05",
    is.na(fdr_p_value) ~ "NA",
    TRUE ~ "> 0.05"
  ))
         

# Plot the heatmap
ggplot(correlation_matrix_table, aes(x = mth_lead, y = radius, fill = p_group)) +
  geom_tile(colour = "grey") + 
  scale_fill_manual(values = c("≤ 0.01" = "red", "0.01 - 0.05" = "yellow", 
                               "> 0.05" = "lightblue", "NA" = "black")) +
  labs(x = "Time (Months)", y = "Radius (m)", fill = "Adj. p-value") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())


#### Heatmap without legend
ggplot(correlation_matrix_table, aes(x = mth_lead, y = radius, fill = rho)) +
  geom_tile(colour="darkgrey") + 
  scale_fill_gradientn(colors = c("blue3" , "white", "red3"),
                       na.value = "grey31",
                       limits = c(-0.8, 0.8),
                       breaks = seq(-0.8, 0.8, by = 0.2)) +
  labs(x = "Time (Months)", y = "Radius (m)", fill = "Spearman's rho") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "none",
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15))
  
ggplot(correlation_matrix_table, aes(x = mth_lead, y = radius, fill = p_group)) +
  geom_tile(colour = "darkgrey") + 
  scale_fill_manual(values = c("≤ 0.01" = "red", "0.01 - 0.05" = "yellow", 
                               "> 0.05" = "lightblue", "NA" = "grey31")) +
  labs(x = "Time (Months)", y = "Radius (m)", fill = "Adj. p-value") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "none",
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15))


### 2-Dimensional Cumulative
### Skip this if not needed
####### ----
# Filter to include only mth_lead from 0 to 12
expanded_list <- subset(expanded_list, mth_lead >= 0)
expanded_list2 <- subset(expanded_list2, mth_lead >= 0)

# Perform cumulative sum of count
expanded_list <- expanded_list %>%
  arrange(id, radius, mth_lead) %>% # Ensure data is sorted by ID and then mth_Lead
  group_by(id, radius) %>%
  mutate(Cumulative_Counts = cumsum(counts)) %>%
  ungroup()

expanded_list2 <- expanded_list2 %>%
  arrange(id, radius, mth_lead) %>% # Ensure data is sorted by ID and then mth_Lead
  group_by(id, radius) %>%
  mutate(Cumulative_Counts = cumsum(counts)) %>%
  ungroup()
#######