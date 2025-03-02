radii <-  seq(1000, 20000, by = 1000)
mth_lead <- seq(-12, 12, by = 1)
expanded_list <- expand.grid(id = moz_data$id, radius = radii, 
                             mth_lead = mth_lead)

# For loop (for mosquito)
for (i in seq_len(nrow(expanded_list))){
  selected_id <- expanded_list$id[i] 
  selected_origin <- moz_data[moz_data$id == selected_id,]
  radius <- expanded_list$radius[i]
  selected_date <- moz_data$month_year %m+% months(expanded_list$mth_lead[i])
  filtered_hum_data <- hum_data %>%  
    filter(month_year == selected_date[selected_id])
  within_distance <- st_is_within_distance(selected_origin, 
                                           filtered_hum_data, dist = radius)
  counted <- lengths(within_distance)
  # Update the counts column in expanded_list
  expanded_list$counts[i] <- counted
  percent <- round((i /nrow(expanded_list))*100, 2)
  print(paste(percent, "% done"))
}

# For macaque

expanded_list2 <- expand.grid(id = mac_data$id, radius = radii, 
                             mth_lead = mth_lead)

# For loop (for macaque)
for (i in seq_len(nrow(expanded_list2))){
  selected_id <- expanded_list2$id[i] 
  selected_origin <- mac_data[mac_data$id == selected_id,]
  radius <- expanded_list$radius[i]
  selected_date <- mac_data$month_year %m+% months(expanded_list2$mth_lead[i])
  filtered_hum_data <- hum_data %>%  
    filter(month_year == selected_date[selected_id])
  within_distance <- st_is_within_distance(selected_origin, 
                                           filtered_hum_data, dist = radius)
  counted <- lengths(within_distance)
  # Update the counts column in expanded_list
  expanded_list2$counts[i] <- counted
  percent <- round((i /nrow(expanded_list2))*100, 2)
  print(paste(percent, "% done"))
}

write.csv(expanded_list, "D:/Malaria/Writing/Paper 19 (Conclusion 1 knowlesi malaria)/Extracted_mosquito.csv", row.names = F)
write.csv(expanded_list2, "D:/Malaria/Writing/Paper 19 (Conclusion 1 knowlesi malaria)/Extracted_macaque.csv", row.names = F)

############### 
#####PAUSE#####
###############

expanded_list <-  read.csv("D:/Malaria/Writing/Paper 19 (Conclusion 1 knowlesi malaria)/Extracted_mosquito.csv")
expanded_list2 <- read.csv("D:/Malaria/Writing/Paper 19 (Conclusion 1 knowlesi malaria)/Extracted_macaque.csv")


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


moz_data_long <- right_join(moz_data, expanded_list, by = "id")
mac_data_long <- right_join(mac_data, expanded_list2, by = "id")
rm(expanded_list, expanded_list2)

# Correlation analysis
colnames(moz_data_long)

radii <-  seq(1000, 20000, by = 1000)
mth_lead <- seq(0, 12, by = 1) ## Change month based on selection
expanded_condition <- expand.grid(radius = radii, mth_lead = mth_lead)

for (i in seq_len(nrow(expanded_condition))){
  selected_radius <- expanded_condition$radius[i]
  selected_mth_lead <- expanded_condition$mth_lead[i]
  filtered_data <- moz_data_long %>%  
    filter(radius == selected_radius & 
             mth_lead == selected_mth_lead)

  # Selected variables for correlation analysis
  corvar_a <- filtered_data$lg_pernite
  corvar_b <- filtered_data$Cumulative_Counts
  
  # Spearman's correlation
  cor_test <- cor.test(corvar_a, corvar_b, method = "spearman")
  
  # Extracting rho and p-value
  # Update the counts column in expanded_condition
  expanded_condition$rho[i] <- cor_test$estimate
  expanded_condition$p_value[i] <- cor_test$p.value
  
  percent <- round((i /nrow(expanded_condition))*100, 2)
  print(paste(percent, "% done"))
}

summary(expanded_condition)


# Heatmap creation

  
library(ggplot2)
colnames(expanded_condition)

ggplot(expanded_condition, aes(x = mth_lead, y = radius, fill = rho)) +
  geom_tile(colour="grey") + 
  scale_fill_gradientn(colors = c("red", "white", "blue"),
                       values = scales::rescale(c(-1, 0, 1)),
                       na.value = "black",
                       limits = c(-1, 1)) +
  labs(x = "Time (Months)", y = "Radius (m)", fill = "Spearman's rho") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

# For p-value based heatmap
# p-value correction

expanded_condition$fdr_p_value <- p.adjust(expanded_condition$p_value, method = "fdr")

expanded_condition <- expanded_condition %>%
  mutate(p_group = case_when(
    fdr_p_value <= 0.01 ~ "≤ 0.01",
    fdr_p_value > 0.01 & fdr_p_value <= 0.05 ~ "0.01 - 0.05",
    is.na(fdr_p_value) ~ "NA",
    TRUE ~ "> 0.05"
  ))
         

# Plot the heatmap
ggplot(expanded_condition, aes(x = mth_lead, y = radius, fill = p_group)) +
  geom_tile(colour = "grey") + 
  scale_fill_manual(values = c("≤ 0.01" = "red", "0.01 - 0.05" = "yellow", 
                               "> 0.05" = "lightblue", "NA" = "black")) +
  labs(x = "Time (Months)", y = "Radius (m)", fill = "Adj. p-value") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
