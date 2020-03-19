### TOTAL A&E ATTENDANCES TIMESERIES CLUSTERING ###

source('load_data.R')           # Get full dataframes

library(ggplot2)                # For plotting results
source('cluster_functions.R')   # Clustering and data-prep functions

### Create timeseries for this script
wip_att <- ae %>%
  dplyr::select(month, code, region, total_attendances) %>%
  dplyr::arrange(code, month) %>%
  dplyr::mutate(mon_name = paste0('mon', month),
                total_attendances = log(total_attendances + 0.5)) %>%
  dplyr::group_by(code, region) %>%
  dplyr::mutate(mean_ts = mean(total_attendances, na.rm = T),
                sd_ts = sd(total_attendances, na.rm = T)) %>%
  dplyr::mutate(tot_att_scaled = (total_attendances - mean_ts) / sd_ts)

tot_att <- wip_att %>%
  tidyr::pivot_wider(id_cols = code:region, 
                     names_from = month, 
                     values_from = tot_att_scaled) %>%
  na.omit()

# Create raw data matrix and set number of clusters to try
att_raw <- as.matrix(tot_att[, 3:ncol(tot_att)])
try_clusters <- c(3:10)

# Run clustering and select optimal number of clusters
clust <- cluster_select(att_raw, try_clusters)

# Prepare data for plotting the clusters
data_plot <- prepare_data(tot_att, 
                          att_raw,
                          clust$clusterings, 
                          clust$num_clusters_index)

# prepare medoids of each cluster
centers <- prepare_centers(clust$clusterings, 
                           clust$num_clusters_index, 
                           clust$num_clusters)

# plot the results
ggplot(data_plot, aes(Time, value, group = ID)) +
  facet_wrap(~class, ncol = 2, scales = "free_y") +
  geom_line(color = "grey10", alpha = 0.65) +
  geom_line(data = centers, aes(Time, value, group= NULL), 
            color = "firebrick1", alpha = 0.80, size = 1.2) +
  labs(x = "Time", y = "Total A&E attendances (normalised)") +
  theme_bw()
