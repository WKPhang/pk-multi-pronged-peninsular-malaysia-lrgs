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
